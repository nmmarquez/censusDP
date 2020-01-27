# This is the cleaning script to make comparisons at the different levels 
# of on spine geographies from the census between the 2010 census and the DP
# files Census pl files were extracted from here
# https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/
# While the DP 2010 CENSUS was extracted from here
# https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/
rm(list=ls())
library(tidyverse)

# need to make names by hand
# come from the census on page
# https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/pl_part1_2010.sas
# and from the dp manual on PAGE 105
varNames <- c(
    P0020001 = "Total Population",
    P0020002 = "Hispanic",
    P0020005 = "White",
    P0020006 = "Black",
    P0020007 = "Asian",
    P0020008 = "AI",
    P0020009 = "PI",
    P0020011 = "Two or more Races",
    P0020010 = "Other"
)

revNames <- function(x){
    y <- names(x)
    names(y) <- x
    y
}

# directory for dp data
ddir <- "data/2010-demonstration-data-products"
plDir <- paste0(ddir, "/01-Redistricting_File--PL_94-171")
# directory for census data
cdir <- "data/2010-census/"
cplDir <- paste0(cdir, "/01-Redistricting_File--PL_94-171")

LOCS <- gsub(" ", "_", c(state.name, "District_of_Columbia"))
# Summary Levels
# 050 county
# 140 census tract
# 150 block group
# 750 block
SLEVEL <- c("050", "140", "150", "750")

for(LOC in LOCS){
    message(paste0("Extracting data for ", LOC))
    
    stDir <- paste0(plDir, "/", LOC, "/")
    cstDir <- paste0(cplDir, "/", LOC, "/")
    
    # unzip the files
    stDir %>%
        list.files(pattern = ".zip", full.names = TRUE) %>%
        unzip(exdir = stDir)
    cstDir %>%
        list.files(pattern = ".zip", full.names = TRUE) %>%
        unzip(exdir = cstDir)
    
    # Pulled data from 
    # https://www2.census.gov/programs-surveys/decennial/rdo/technical-documentation/2020Census/2018Prototype_PL94_171_TechDoc_v2.pdf
    # PAGE 14 
    geoMatch <- read_csv("data/2010-demonstration-data-products/geoMatch.csv") %>%
        mutate(type = ifelse(data_type != "N", "c", "d"))
    
    # Pulled this from 
    # https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/pl_geohd_2010.sas
    cgeoMatch <- read_csv("data/2010-census/geoMatch.csv") %>%
        mutate(type = ifelse(data_type != "N", "c", "d"))
    
    # Read the fixed with geo files for the location for DP
    geoDF <- stDir %>%
        list.files(pattern = "geo", full.names = TRUE) %>% 
        read_fwf(
            fwf_widths(geoMatch$field_size, col_names = geoMatch$ddref),
            paste(geoMatch$type, collapse = ""))
    
    # and then the same for the census
    cgeoDF <- cstDir %>%
        list.files(pattern = "geo", full.names = TRUE) %>% 
        read_fwf(
            fwf_widths(cgeoMatch$field_size, col_names = cgeoMatch$ddref),
            paste(cgeoMatch$type, collapse = ""))
    
    # Names derived from 
    # https://www2.census.gov/programs-surveys/decennial/rdo/technical-documentation/2020Census/2018Prototype_PL94_171_TechDoc_v2.pdf
    # PAGE 105
    # same for both census and dp as seen here 
    # https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/pl_part1_2010.sas
    seg1Names <- c(
        "FILEID", "STUSAB", "CHARITER", "CIFSN", "LOGRECNO",
        paste0("P00100", sprintf("%02d", 1:71)),
        paste0("P00200", sprintf("%02d", 1:73))
    )
    
    for(sl in SLEVEL){
        p1DF <- bind_rows(
            # grab the dp files
            stDir %>%
                # get only the one for Seg 01 which contains the race data
                list.files(pattern = "012010.pl", full.names = TRUE) %>%
                # read it with col name specfied above
                read_csv(col_names = seg1Names) %>%
                select(c("CHARITER", "CIFSN", "LOGRECNO", names(varNames))) %>%
                rename(!!!revNames(varNames)) %>%
                # merge on the geography values
                left_join(
                    geoDF %>%
                        # filter to just look at blocks
                        filter(SUMLEV == sl) %>%
                        mutate(LOGRECNO = sprintf("%07d", LOGRECNO)) %>%
                        select(LOGRECNO, STATE, COUNTY, TRACT, BLKGRP, BLOCK),
                    by = "LOGRECNO") %>%
                filter(!is.na(STATE)) %>%
                mutate(SOURCE = "DP"),
            # do the same for the actual census data
            cstDir %>%
                list.files(pattern = "012010.pl", full.names = TRUE) %>%
                read_csv(col_names = seg1Names) %>%
                select(c("CHARITER", "CIFSN", "LOGRECNO", names(varNames))) %>%
                rename(!!!revNames(varNames)) %>%
                left_join(
                    cgeoDF %>%
                        # filter to just look at blocks
                        filter(SUMLEV == sl) %>%
                        mutate(LOGRECNO = sprintf("%07d", LOGRECNO)) %>%
                        select(LOGRECNO, STATE, COUNTY, TRACT, BLKGRP, BLOCK),
                    by = "LOGRECNO") %>%
                filter(!is.na(STATE)) %>%
                mutate(SOURCE = "CENSUS"))

        
        # reshape the data and save to an RDS
        p1DF %>%
            select(-`Total Population`, -CHARITER, -CIFSN, -LOGRECNO) %>%
            pivot_longer(Hispanic:Other, "RACE") %>%
            pivot_wider(names_from = SOURCE, values_from = value) %>%
            saveRDS(paste0("./data/prep_data/", LOC, "_", sl, ".Rds"))
    }
}
