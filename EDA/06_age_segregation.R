rm(list=ls())
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leafgl)
library(colorRamps)
library(spdep)

source("./EDA/utils.R")

stateDF <- tibble(
    statename = c(state.name, "District of Columbia"),
    stateabb = c(state.abb, "DC")
)

msaCountyDF <- read_csv(
    "https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv") %>%
    filter(!is.na(fipscountycode) & !is.na(cbsacode)) %>%
    rename(countyname = countycountyequivalent) %>%
    rename(type = metropolitanmicropolitanstatis) %>%
    rename(STATE = fipsstatecode, COUNTY = fipscountycode) %>%
    select(cbsacode, cbsatitle, STATE, COUNTY, statename, type) %>%
    mutate(COUNTY = paste0(STATE, COUNTY)) %>%
    filter(statename != "Puerto Rico") %>%
    left_join(stateDF, by = "statename")



# calcultite H index at top level using blocks as sub level and

if(!file.exists("results/age_BGdecompositionH.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        
        DF <- str_c("data/prep_data_age/", unique(locDF$stateabb),".csv") %>%
            lapply(read_csv, col_types = "cccccii") %>%
            bind_rows() %>%
            filter(!(CENSUS == 0 & DP == 0)) %>%
            mutate(COUNTY = str_c(STATE, COUNTY)) %>%
            filter(COUNTY %in% locDF$COUNTY) %>%
            mutate(TRACT = str_c(COUNTY, TRACT)) %>%
            mutate(BLOCK = str_c(TRACT, BLKGRP)) %>%
            pivot_longer(DP:CENSUS) %>%
            select(TRACT, BLOCK, age_group, name, value) %>%
            # This is where things get hacky, in order to run this with the same
            # function we need to relable some variables in non-intuitive ways
            rename(RACE = age_group) %>%
            mutate(RACE = ifelse(RACE == "65+", "White", RACE))
        
        calcH(DF, group = T) %>%
            mutate(CBSA = msa)
    }))
    
    write_csv(allDF, "results/age_BGdecompositionH.csv")
}

allDF <- read_csv("results/age_BGdecompositionH.csv")

plot_list <- list()

bind_rows(
    allDF %>%
        select(name, H, CBSA) %>%
        pivot_wider(CBSA, names_from = name, values_from = H) %>%
        mutate(metric = "H Index"),
    allDF %>%
        select(name, Pb, CBSA) %>%
        pivot_wider(CBSA, names_from = name, values_from = Pb) %>%
        mutate(metric = "Percent Between Tract"),
    allDF %>%
        select(name, Hw, CBSA) %>%
        pivot_wider(CBSA, names_from = name, values_from = Hw) %>%
        mutate(metric = "Within Tract H"),
    allDF %>%
        select(name, Hb, CBSA) %>%
        pivot_wider(CBSA, names_from = name, values_from = Hb) %>%
        mutate(metric = "Between Tract H")) %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique(),
        by = "CBSA") %>%
    ggplot(aes(x = CENSUS, y = DP, color = type, text = CBSA)) +
    geom_point(alpha = .6) +
    theme_classic() +
    geom_abline() +
    ggtitle("Difference in Thiels H Index of Metropolitan Areas\nBlock Group Age Segregation") +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    facet_wrap(~metric, scales = "free")

allDF %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    select(name, Pbg, CBSA, type) %>%
    pivot_wider(CBSA:type, names_from = name, values_from = Pbg) %>%
    ggplot(aes(x = CENSUS, y = DP)) +
    geom_point(aes(color = type, text = CBSA), alpha = .6) +
    theme_classic() +
    geom_abline() +
    geom_smooth() +
    ggtitle(
        paste(
            "Difference in Thiels H Attributable to Old-Young Segergation",
            "Differences of Metropolitan Areas By Block Group", sep = "\n")) +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC"))
