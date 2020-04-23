rm(list=ls())
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(leafgl)
library(mapview)
library(leaflet)
library(leafgl)
library(colorRamps)
library(spdep)
census_api_key(Sys.getenv("CENSUS_API_KEY"))

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

# calcultite H index at top level using blocks as sub level and

if(!file.exists("results/decompositionH.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
       
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
        
        calcH(DF, group = T) %>%
            mutate(CBSA = msa)
    }))
    
    write_csv(allDF, "results/decompositionH.csv")
}

if(!file.exists("results/BGdecompositionH.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){

        cat(paste0(msa, "\n"))

        locDF <- msaCountyDF %>% filter(cbsatitle == msa)

        DF <- bind_rows(lapply(
            str_c("data/prep_data/", unique(locDF$statename),"_750.Rds"), read_rds)) %>%
            filter(!(CENSUS == 0 & DP == 0)) %>%
            mutate(COUNTY = str_c(STATE, COUNTY)) %>%
            filter(COUNTY %in% locDF$COUNTY) %>%
            mutate(TRACT = str_c(STATE, COUNTY, TRACT)) %>%
            mutate(BLOCKGRP = str_c(STATE, COUNTY, TRACT, BLKGRP)) %>%
            pivot_longer(DP:CENSUS) %>%
            select(TRACT, BLOCKGRP, RACE, name, value) %>%
            filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
            group_by(TRACT, BLOCKGRP, RACE, name) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            rename(BLOCK = BLOCKGRP)

        calcH(DF, group = T) %>%
            mutate(CBSA = msa)
    }))
    
    write_csv(allDF, "results/BGdecompositionH.csv")
}

allDF_ <- read_csv("results/decompositionH.csv")

allDF_ %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    group_by(type, name) %>%
    summarize(Pb = mean(Pb, na.rm = TRUE)) %>%
    mutate(Pw = 1 - Pb) %>%
    ungroup() %>%
    pivot_longer(Pb:Pw, names_to = "metric") %>%
    mutate(Scale = ifelse(metric == "Pb", "Between Tract", "Within Tract")) %>% 
    ggplot(aes(x = name, y = value, fill = Scale)) +
    geom_col() +
    theme_classic() +
    facet_wrap(~type) +
    ggtitle("Block Level Perecent Segregation Attributable") +
    labs(x="", y = "Percent Attributable")

allDF_ %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    group_by(type, name) %>%
    summarize(Pbg = mean(Pbg, na.rm = TRUE)) %>%
    mutate(`Seg Between NW Groups` = 1 - Pbg) %>%
    ungroup() %>%
    pivot_longer(Pbg:`Seg Between NW Groups`, names_to = "metric") %>%
    mutate(Scale = ifelse(metric == "Pbg", "W-NW Seg", metric)) %>% 
    ggplot(aes(x = name, y = value, fill = Scale)) +
    geom_col() +
    theme_classic() +
    facet_wrap(~type) +
    ggtitle("White and Non-White Segregation Attributable") +
    labs(x="", y = "Percent Attributable")

allDF <- read_csv("results/BGdecompositionH.csv")

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
    ggtitle("Difference in Thiels H Index of Metropolitan Areas\nBlock Level") +
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
            "Difference in Thiels H Attributable to White-Non White",
            "Differences of Metropolitan Areas By Block", sep = "\n")) +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC"))

allDF %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    group_by(type, name) %>%
    summarize(Pbg = mean(Pbg, na.rm = TRUE)) %>%
    mutate(`Seg Between NW Groups` = 1 - Pbg) %>%
    ungroup() %>%
    pivot_longer(Pbg:`Seg Between NW Groups`, names_to = "metric") %>%
    mutate(Scale = ifelse(metric == "Pbg", "W-NW Seg", metric)) %>% 
    ggplot(aes(x = name, y = value, fill = Scale)) +
    geom_col() +
    theme_classic() +
    facet_wrap(~type) +
    ggtitle("White and Non-White Segregation Attributable") +
    labs(x="", y = "Percent Attributable")

allDF %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    group_by(type, name) %>%
    summarize(Pb = mean(Pb, na.rm = TRUE)) %>%
    mutate(Pw = 1 - Pb) %>%
    ungroup() %>%
    pivot_longer(Pb:Pw, names_to = "metric") %>%
    mutate(Scale = ifelse(metric == "Pb", "Between Tract", "Within Tract")) %>% 
    ggplot(aes(x = name, y = value, fill = Scale)) +
    geom_col() +
    theme_classic() +
    facet_wrap(~type) +
    ggtitle("Block Level Perecent Segregation Attributable") +
    labs(x="", y = "Percent Attributable")
