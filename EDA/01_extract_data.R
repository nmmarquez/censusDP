# This is the cleaning script to make comparisons at the different levels 
# of on spine geographies from the census between the 2010 census and the DP
# files Census pl files were extracted from here
# https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/
# While the DP 2010 CENSUS was extracted from here
# https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/
rm(list=ls())
library(tidyverse)
library(tidycensus)
library(ipumsr)

# need to make names by hand
# come from the census on page
# https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/pl_part1_2010.sas
# and from the dp manual on PAGE 105
varNames <- c(
    P0020001 = "Total Population",
    P0020002 = "Hispanic",
    P0020005 = "White",
    P0020006 = "Black",
    P0020007 = "AI",
    P0020008 = "Asian",
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

# https://www2.census.gov/programs-surveys/decennial/rdo/technical-documentation/CD116_TechnicalDocumentation.pdf
# page 51 or 83 firefox pdf reader is weird
# P01200XX
# additionally these guys make getting block data hella easy
# https://ciser.cornell.edu/data/data-archive/census-2010-dhc-download-center/

ageVars <- bind_rows(
    tibble(
        base = 3:25,
        id = sprintf("P01200%02d", 3:25),
        sex = "male") %>%
        mutate(age_group = case_when(
            base <= 6 ~ "Under 18",
            base > 6 & base <= 19 ~ "18-65",
            base > 19 ~ "65+"
        )),
    tibble(
        base = 27:49,
        id = sprintf("P01200%02d", 27:49),
        sex = "female") %>%
        mutate(age_group = case_when(
            base <= 30 ~ "Under 18",
            base > 30 & base <= 43 ~ "18-65" ,
            base > 43 ~ "65+"
)))

testDF <- get_decennial("block group", "P005003", year = 2010, state = "AK")

# unzip the files
# sometimes this is unreliable with large files so consider doing this in bash
"data/2010-dp-housing" %>%
    list.files(pattern = ".ZIP", full.names = TRUE) %>%
    sapply(unzip, exdir = "data/2010-dp-housing")

trueDF <- read_nhgis("data/2010-housing/nhgis0035_ds172_2010_block.csv") %>%
    set_ipums_var_attributes(
        read_ipums_codebook(
            "data/nhgis0035_csv.zip",
            "nhgis0035_csv/nhgis0035_ds172_2010_block_codebook.txt")) %>%
    mutate(`Under 18` = 
               H76003 + H76004 + H76005 + H76006 + 
               H76027 + H76028 + H76029 + H76030) %>%
    mutate(`18-65` = 
               H76007 + H76008 + H76009 + H76010 + H76011 + H76012 + H76013 +
               H76014 + H76015 + H76016 + H76017 + H76018 + H76019 +
               H76031 + H76032 + H76033 + H76034 + H76035 + H76036 + H76037 +
               H76038 + H76039 + H76040 + H76041 + H76042 + H76043) %>%
    mutate(`65+` = 
               H76020 + H76021 + H76022 + H76023 + H76024 + H76025 + 
               H76044 + H76045 + H76046 + H76047 + H76048 + H76049) %>%
    select(STATEA, COUNTYA, TRACTA, BLKGRPA, `Under 18`, `18-65`, `65+`) %>%
    group_by(STATEA, COUNTYA, TRACTA, BLKGRPA) %>%
    summarize(
        `Under 18` = sum(`Under 18`), 
        `18-65` = sum(`18-65`), 
        `65+` = sum(`65+`)) %>%
    ungroup() %>%
    rename(STATE = STATEA, COUNTY = COUNTYA, TRACT = TRACTA, BLKGRP = BLKGRPA)

HLOCS <- c(state.abb, "DC")

for(loc in HLOCS){
    cat(paste0(loc, "\n"))
    dpDF <- read_csv(
        paste0("data/2010-dp-housing/", loc, "2010DHC.CSV"),
        col_names = TRUE,
        col_types = paste0(c(rep("c", 101), rep("i", 2585)), collapse = "")) %>%
        filter(SUMLEV == "150") %>%
        select(STATE, COUNTY, TRACT, BLKGRP, !!!ageVars$id)

    # sometimes this is numeric sometimes it aint weird
    jointDF <- dpDF %>%
        pivot_longer(P0120003:P0120049, names_to = "id") %>%
        left_join(ageVars, by = "id") %>%
        group_by(STATE, COUNTY, TRACT, BLKGRP, age_group) %>%
        summarize(DP = sum(value)) %>%
        ungroup() %>%
        left_join(trueDF %>%
            mutate(BLKGRP = as.character(BLKGRP)) %>%
            filter(STATE == dpDF$STATE[1]) %>%
            pivot_longer(
                `Under 18`:`65+`, names_to = "age_group", values_to = "CENSUS"))
    
    write_csv(jointDF, paste0("data/prep_data_age/", loc, ".csv"))
}
