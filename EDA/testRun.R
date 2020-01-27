rm(list=ls())
library(tidyverse)
library(acs)
library(jsonlite)
library(tidycensus)
library(foreign)

# Summary Levels
# 050 county
# 140 census tract
# 150 block group
# 750 block
SLEVEL <- "750"
LOC <- "Washington"

apiKey <- read_json("./keys/acs.json")

# this only includes the variable names as present in the 
# dhd files :(
v2010 <- load_variables(2010, "sf1", cache=T)

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

ddir <- "data/2010-demonstration-data-products"
plDir <- paste0(ddir, "/01-Redistricting_File--PL_94-171")
stDir <- paste0(plDir, "/", LOC, "/")

cdir <- "data/2010-census/"
cplDir <- paste0(cdir, "/01-Redistricting_File--PL_94-171")
cstDir <- paste0(cplDir, "/", LOC, "/")

# Pulled data from 
# https://www2.census.gov/programs-surveys/decennial/rdo/technical-documentation/2020Census/2018Prototype_PL94_171_TechDoc_v2.pdf
# PAGE 14 
geoMatch <- read_csv("data/2010-demonstration-data-products/geoMatch.csv") %>%
    mutate(type = ifelse(data_type != "N", "c", "d"))

### SANITY CHECK (since we typed this by hand)
# looks like everything is as it should be
geoMatch %>%
    mutate(test = cumsum(field_size), test2 = lag(test) + 1) %>%
    mutate(test2 = ifelse(is.na(test2), 1, test2)) %>%
    filter(test2 != starting_pos)

# Pulled this from 
# https://www2.census.gov/census_2010/01-Redistricting_File--PL_94-171/pl_geohd_2010.sas
cgeoMatch <- read_csv("data/2010-census/geoMatch.csv") %>%
    mutate(type = ifelse(data_type != "N", "c", "d"))

### SANITY CHECK (since we typed this by hand)
# looks like everything is as it should be
cgeoMatch %>%
    mutate(test = cumsum(field_size), test2 = lag(test) + 1) %>%
    mutate(test2 = ifelse(is.na(test2), 1, test2)) %>%
    filter(test2 != starting_pos)

geoDF <- stDir %>%
    list.files(pattern = "geo", full.names = TRUE) %>% 
    read_fwf(
        fwf_widths(geoMatch$field_size, col_names = geoMatch$ddref),
        paste(geoMatch$type, collapse = ""))

cgeoDF <- cstDir %>%
    list.files(pattern = "geo", full.names = TRUE) %>% 
    read_fwf(
        fwf_widths(cgeoMatch$field_size, col_names = cgeoMatch$ddref),
        paste(cgeoMatch$type, collapse = ""))

# we want to snag the blocks(SUMLEV == "750")

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

p1DF <- bind_rows(
    stDir %>%
        list.files(pattern = "012010.pl", full.names = TRUE) %>%
        read_csv(col_names = seg1Names) %>%
        select(c("CHARITER", "CIFSN", "LOGRECNO", names(varNames))) %>%
        rename(!!!revNames(varNames)) %>%
        left_join(
            geoDF %>%
                # filter to just look at blocks
                filter(SUMLEV == SLEVEL) %>%
                mutate(LOGRECNO = sprintf("%07d", LOGRECNO)) %>%
                select(LOGRECNO, STATE, COUNTY, TRACT, BLKGRP, BLOCK),
            by = "LOGRECNO") %>%
        filter(!is.na(STATE)) %>%
        mutate(SOURCE = "DP"),
    cstDir %>%
        list.files(pattern = "012010.pl", full.names = TRUE) %>%
        read_csv(col_names = seg1Names) %>%
        select(c("CHARITER", "CIFSN", "LOGRECNO", names(varNames))) %>%
        rename(!!!revNames(varNames)) %>%
        left_join(
            cgeoDF %>%
                # filter to just look at blocks
                filter(SUMLEV == SLEVEL) %>%
                mutate(LOGRECNO = sprintf("%07d", LOGRECNO)) %>%
                select(LOGRECNO, STATE, COUNTY, TRACT, BLKGRP, BLOCK),
            by = "LOGRECNO") %>%
        filter(!is.na(STATE)) %>%
        mutate(SOURCE = "CENSUS"))

# looking good here too are counts are internally consistent 
p1DF %>%
    filter(`Total Population` != (
        Hispanic + Black + White + Asian + AI + PI + `Two or more Races` + Other))

p1DF %>%
    select(-`Total Population`, -CHARITER, -CIFSN, -LOGRECNO) %>%
    pivot_longer(Hispanic:Other, "RACE") %>%
    pivot_wider(names_from = SOURCE, values_from = value) %>%
    saveRDS(paste0("./data/prep_data/", LOC, ".Rds"))
