# our goal here is to simulate values of segregation given some sort of simple
# placement algorithm 

rm(list=ls())
library(tidyverse)
library(sf)
library(colorRamps)
library(spdep)
library(parallel)

source("./EDA/utils.R")

msaCountyDF <- read_csv(
    "https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv") %>%
    filter(!is.na(fipscountycode) & !is.na(cbsacode)) %>%
    rename(countyname = countycountyequivalent) %>%
    rename(type = metropolitanmicropolitanstatis) %>%
    rename(STATE = fipsstatecode, COUNTY = fipscountycode) %>%
    select(cbsacode, cbsatitle, STATE, COUNTY, statename, type) %>%
    mutate(
        COUNTY = paste0(STATE, COUNTY),
        statename = str_replace_all(statename, " ", "_")) %>%
    filter(statename != "Puerto_Rico")

# lets start with a test MSA for now
msa <- "Los Angeles-Long Beach-Anaheim, CA"

cat(paste0(msa, "\n"))

locDF <- msaCountyDF %>% filter(cbsatitle == msa)

DF <- bind_rows(lapply(
    str_c("data/prep_data/", unique(locDF$statename),"_750.Rds"), read_rds)) %>%
    filter(!(CENSUS == 0 & DP == 0)) %>%
    mutate(COUNTY = str_c(STATE, COUNTY)) %>%
    filter(COUNTY %in% locDF$COUNTY) %>%
    mutate(TRACT = str_c(STATE, COUNTY, TRACT)) %>%
    mutate(BLOCK = str_c(STATE, COUNTY, TRACT, BLKGRP, BLOCK)) %>%
    pivot_longer(DP:CENSUS) %>%
    select(TRACT, BLOCK, RACE, name, value) %>%
    filter(RACE %in% c("Hispanic", "White", "Black", "Asian"))

# what we want to do first is randomly assign race to individuals and keep
# placement fixed
set.seed(123)

simDF <- bind_rows(mclapply(0:99, function(i){
    tibble(
        RACE = DF %>%
            filter(name == "CENSUS") %>%
            group_by(RACE) %>%
            summarize(value = sum(value)) %>%
            {rep(.$RACE, .$value)},
        BLOCK = DF %>%
            filter(name == "CENSUS") %>%
            group_by(BLOCK) %>%
            summarize(value = sum(value)) %>%
            {rep(.$BLOCK, .$value)} %>%
            sample()) %>%
        group_by(BLOCK, RACE) %>%
        summarize(value = n()) %>%
        ungroup() %>%
        left_join(unique(select(DF, TRACT, BLOCK)), by = "BLOCK") %>%
        mutate(name = sprintf("SIM%03d", i))
    }, mc.cores = 10))

simTractStableDF <- bind_rows(mclapply(0:99, function(i){
    tracts <- unique(DF$TRACT)
    bind_rows(lapply(tracts, function(t){
        tibble(
            RACE = DF %>%
                filter(TRACT == t) %>% 
                filter(name == "CENSUS") %>%
                group_by(RACE) %>%
                summarize(value = sum(value)) %>%
                {rep(.$RACE, .$value)},
            BLOCK = DF %>%
                filter(TRACT == t) %>%
                filter(name == "CENSUS") %>%
                group_by(BLOCK) %>%
                summarize(value = sum(value)) %>%
                {rep(.$BLOCK, .$value)} %>%
                sample()) %>%
            group_by(BLOCK, RACE) %>%
            summarize(value = n()) %>%
            ungroup() %>%
            left_join(unique(select(DF, TRACT, BLOCK)), by = "BLOCK") %>%
            mutate(name = sprintf("SIM%03d", i))
        }))
    }, mc.cores = 10))
    
outTractStableDF <- bind_rows(mclapply(0:49, function(i){
    simTractStableDF %>%
        filter(name %in% sprintf("SIM%03d", c(i*2, i*2+1))) %>%
        calcH(group = T) %>%
        mutate(CBSA = msa)
}, mc.cores = 6))

outDF <- bind_rows(mclapply(0:49, function(i){
    simDF %>%
        filter(name %in% sprintf("SIM%03d", c(i*2, i*2+1))) %>%
        calcH(group = T) %>%
        mutate(CBSA = msa)
}, mc.cores = 6))

allDF <- read_csv("results/decompositionH.csv") %>%
    filter(CBSA == msa)

