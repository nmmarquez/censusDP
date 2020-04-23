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
if(!file.exists("./results/metro_vars.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        cat(paste0(msa, "\n"))
    
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
    
        DF <- bind_rows(lapply(
            str_c("data/prep_data/", unique(locDF$statename),"_750.Rds"), 
            read_rds)) %>%
            filter(!(CENSUS == 0 & DP == 0)) %>%
            mutate(COUNTY = str_c(STATE, COUNTY)) %>%
            filter(COUNTY %in% locDF$COUNTY)
    
        ## variables to create
        # ratio of non white to white population
        # total population size
        # number of blocks
        # average block size
        # percent race groups maybe?
        DF %>%
            mutate(nw = RACE != "White") %>%
            mutate(BLOCK = str_c(STATE, COUNTY, TRACT, BLKGRP, BLOCK)) %>%
            summarize(
                total_pop = sum(CENSUS),
                nw_w_ratio =  sum(CENSUS * nw) /  sum(CENSUS * !nw),
                block_count = length(unique(BLOCK)),
                p_white = sum(CENSUS * (RACE == "White")) /  sum(CENSUS),
                p_black = sum(CENSUS * (RACE == "Black")) /  sum(CENSUS),
                p_asian = sum(CENSUS * (RACE == "Asian")) /  sum(CENSUS),
                p_hispanic = sum(CENSUS * (RACE == "Hispanic")) /  sum(CENSUS)
                ) %>%
            mutate(CBSA = msa)
    }))
    
    write_csv(allDF, "./results/metro_vars.csv")
}

allDF <- read_csv("./results/metro_vars.csv") %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique(),
        by = "CBSA") %>%
    left_join(
        read_csv("./results/decompositionH.csv") %>%
            pivot_longer(Hw:Pbg, names_to = "metric") %>%
            arrange(CBSA, metric, name) %>%
            group_by(CBSA, metric) %>%
            summarize(error = diff(value)) %>%
            pivot_wider(names_from = "metric", values_from = "error"),
        by = "CBSA") %>%
    mutate(diversity = -p_white * log(p_white) -
               p_asian * log(p_asian) -
               p_black * log(p_black) -
               p_hispanic * log(p_hispanic))

summary(lm(H ~ type, data = allDF))
summary(lm(H ~ type + log(total_pop), data = allDF))
# pop is informative all on its own
summary(lm(H ~ log(total_pop), data = allDF))
# doesnt reallly change anything because the bias is all in the same direction
summary(lm(abs(H) ~ log(total_pop), data = allDF))
# block count understandibly has an effect here
summary(lm(abs(H) ~ log(total_pop) + log(block_count), data = allDF))
# race ratio has an impact here
summary(modrace1 <- 
    lm(H ~ log(total_pop) + log(block_count) + log(nw_w_ratio), data = allDF))
summary(modrace2 <- allDF %>%
    mutate(nw_w_ratio_sq = nw_w_ratio**2) %>%
    {lm(H ~ log(total_pop) + log(block_count) + nw_w_ratio + nw_w_ratio_sq, data = .)})
summary(modrace3 <- 
    lm(
        H ~ log(total_pop) + log(block_count) + p_asian + p_black + p_hispanic, 
        data = allDF))
summary(modrace4 <- 
    lm(
        H ~ log(total_pop) + log(block_count) + diversity, 
        data = allDF))

# perhaps unsurprisingly the more detailed model explains the most variation
BIC(modrace1, modrace2, modrace3, modrace4)

summary(pbgmodrace1 <- 
    lm(Pbg ~ log(total_pop) + log(block_count) + log(nw_w_ratio), data = allDF))
summary(pbgmodrace2 <- allDF %>%
    mutate(nw_w_ratio_sq = nw_w_ratio**2) %>%
    {lm(Pbg ~ log(total_pop) + log(block_count) + nw_w_ratio + nw_w_ratio_sq, data = .)})
summary(pbgmodrace3 <- 
    lm(
        Pbg ~ log(total_pop) + log(block_count) + p_asian + p_black + p_hispanic, 
        data = allDF))

summary(pbgmodrace4 <- lm(
    Pbg ~ log(total_pop) + log(block_count) + diversity, data = allDF))

BIC(pbgmodrace1, pbgmodrace2, pbgmodrace3, pbgmodrace4)
