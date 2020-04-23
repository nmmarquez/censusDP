rm(list = ls())
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
library(tigris)
library(plotly)
census_api_key(Sys.getenv("CENSUS_API_KEY"))
options(tigris_use_cache = TRUE)

source("./EDA/utils.R")

stateDF <- tibble(
    statename = c(state.name, "District of Columbia"),
    stateabb = c(state.abb, "DC"))

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

place_pop_df <- get_acs(
    "place",variables = c(Pop = "B01001_001"), year = 2018, moe_level = 95)

place_df <- do.call(sf:::rbind.sf, lapply(1:50, function(i){
    places(state.abb[i], class = "sf") %>%
        select(GEOID, NAME, geometry, ALAND) %>%
        mutate(STATE = state.name[i])})) %>%
    right_join(rename(place_pop_df, name_full = NAME), by = "GEOID") %>%
    mutate(density = estimate / ALAND) %>%
    arrange(-density)

if(!file.exists("results/age_BGgeodecompositionH.csv")){
    allDF2 <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        STS <- unique(locDF$statename)
        
        DF <- str_c("data/prep_data_age/", unique(locDF$stateabb),".csv") %>%
            lapply(read_csv, col_types = "cccccii") %>%
            bind_rows() %>%
            filter(!(CENSUS == 0 & DP == 0)) %>%
            mutate(COUNTY = str_c(STATE, COUNTY)) %>%
            filter(COUNTY %in% locDF$COUNTY) %>%
            mutate(TRACT = str_c(COUNTY, TRACT)) %>%
            mutate(BLOCK = str_c(TRACT, BLKGRP)) %>%
            mutate(BLKGRP = BLOCK) %>%
            pivot_longer(DP:CENSUS) %>%
            select(TRACT, STATE, BLOCK, COUNTY, age_group, name, value, BLKGRP) %>%
            # This is where things get hacky, in order to run this with the same
            # function we need to relable some variables in non-intuitive ways
            rename(RACE = age_group) %>%
            mutate(RACE = ifelse(RACE == "65+", "White", RACE))
        
        geos <- DF %>%
            mutate(COUNTY = str_sub(COUNTY, 3, -1)) %>%
            select(STATE, COUNTY) %>%
            unique()
        
        block_geo_df <- do.call(sf:::rbind.sf, lapply(1:nrow(geos), function(i){
            block_groups(
                state = geos$STATE[i], county = geos$COUNTY[i], class = "sf",
                cb = FALSE, year = 2010)})) %>%
            rename(GEOID = GEOID10) %>%
            filter(GEOID %in% unique(DF$BLKGRP))
        
        block_place_df <- block_geo_df %>%
            st_buffer(1e-5) %>%
            st_intersection(
                place_df %>%
                    filter(STATE %in% str_replace(STS, "_", " ")) %>%
                    select(name_full, geometry)) %>%
            # only take locations where area is greatest
            mutate(Area = st_area(.)) %>%
            group_by(GEOID) %>%
            filter(Area == max(Area)) %>%
            as_tibble() %>%
            select(BLKGRP=GEOID, name_full)
        
        DF2 <- DF %>%
            left_join(block_place_df, by = "BLKGRP") %>%
            mutate(name_full = ifelse(is.na(name_full), "No Place", name_full))
        
        center_place <- DF2 %>%
            group_by(name_full) %>%
            filter(name == "CENSUS") %>%
            summarize(Pop = sum(value, na.rm=TRUE)) %>%
            filter(name_full != "No Place") %>%
            arrange(-Pop) %>%
            # use head incase of ties
            head(n=1) %>%
            pull(name_full)
        
        ## Make sure we got this right
        block_geo_df %>%
            rename(BLKGRP = GEOID) %>%
            left_join(block_place_df, by = "BLKGRP") %>%
            mutate(name_full = ifelse(
                is.na(name_full), "No Place", name_full)) %>%
            mutate(center = name_full == center_place) %>%
            ggplot(aes(fill=center)) +
            geom_sf() +
            theme_void()

        block_geo_df %>%
            rename(BLKGRP = GEOID) %>%
            left_join(block_place_df, by = "BLKGRP") %>%
            mutate(name_full = ifelse(
                is.na(name_full), "No Place", name_full)) %>%
            ggplot(aes(fill=name_full)) +
            geom_sf() +
            theme_void()
        
        final_df <- DF2 %>%
            mutate(Center = ifelse(
                name_full == center_place, "Center", "Not Center")) %>%
            rename(BLOCKGRP = BLKGRP)
        
        bind_rows(
            
            final_df %>%
                mutate(TRACT = name_full) %>%
                select(TRACT, BLOCKGRP, RACE, name, value) %>%
                rename(BLOCK = BLOCKGRP) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Place"),
            
            final_df %>%
                mutate(TRACT = Center) %>%
                select(TRACT, BLOCKGRP, RACE, name, value) %>%
                rename(BLOCK = BLOCKGRP) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Center")) %>%
            mutate(CBSA = msa, center = center_place) %>%
            arrange(name, Level) %>%
            group_by(name) %>%
            mutate(seg_explained = Pb - lag(Pb, default = 0)) %>%
            ungroup()
    }))
    
    write_csv(allDF2, "results/age_BGgeodecompositionH.csv")
}

allDF <- read_csv("results/age_BGgeodecompositionH.csv")

allDF %>%
    select(name, CBSA, center, Level, seg_explained) %>%
    rbind(
        allDF %>%
            filter(Level == "Tract") %>%
            mutate(seg_explained = 1-Pb) %>%
            select(name, CBSA, center, Level, seg_explained) %>%
            mutate(Level = "Block Group") %>%
            unique()) %>% 
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    mutate(Level = factor(Level, c("Center", "Place", "Tract", "Block Group"))) %>%
    pivot_wider(names_from = "name", values_from = "seg_explained") %>%
    ggplot(aes(x = CENSUS, y = DP)) +
    geom_point() +
    theme_classic() +
    geom_abline() +
    facet_wrap(~Level)

allDF %>%
    filter(Level != "Tract") %>%
    select(name, CBSA, center, Level, seg_explained) %>%
    rbind(
        allDF %>%
            filter(Level == "Place") %>%
            mutate(seg_explained = 1-Pb) %>%
            select(name, CBSA, center, Level, seg_explained) %>%
            mutate(Level = "Block Group") %>%
            unique()) %>% 
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    mutate(Level = factor(Level, c("Center", "Place", "Tract", "Block Group"))) %>%
    pivot_wider(names_from = "name", values_from = "seg_explained") %>%
    group_by(type, Level) %>%
    summarize_if(is.numeric, mean, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_longer(CENSUS:DP) %>%
    ggplot(aes(x = name, y = value, fill = Level)) +
    geom_col() +
    theme_classic() +
    facet_wrap(~type) +
    labs(x = "", y = "Percent") +
    ggtitle("Precent Segregation Explained: Age Segregation")

plot_list <- list()




saveRDS(plot_list, file = "results/geo_plots.Rds")
