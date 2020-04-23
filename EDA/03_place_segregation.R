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

place_pop_df <- get_acs(
    "place",variables = c(Pop = "B01001_001"), year = 2018, moe_level = 95)

place_df <- do.call(sf:::rbind.sf, lapply(1:50, function(i){
    places(state.abb[i], class = "sf") %>%
        select(GEOID, NAME, geometry, ALAND) %>%
        mutate(STATE = state.name[i])})) %>%
    right_join(rename(place_pop_df, name_full = NAME), by = "GEOID") %>%
    mutate(density = estimate / ALAND) %>%
    arrange(-density)
    
cbsa_df <- core_based_statistical_areas(cb = FALSE, class = "sf")

inter_df <- st_intersection(place_df, cbsa_df)

central_df <- inter_df %>%
    select(estimate, name_full, NAMELSAD, density, GEOID, geometry) %>%
    mutate(Polygon = sapply(geometry, function(z){
        any(grepl("POLYGON", class(z)))})) %>%
    filter(Polygon) %>%
    arrange(NAMELSAD, -estimate) %>% 
    group_by(NAMELSAD) %>%
    mutate(pop_rank = 1:n()) %>%
    filter(pop_rank == 1) %>%
    select(-pop_rank, -Polygon)

if(!file.exists("results/placedecompositionH.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        
        DF <- bind_rows(lapply(
            str_c("data/prep_data/", unique(locDF$statename),"_750.Rds"), read_rds)) %>%
            filter(!(CENSUS == 0 & DP == 0)) %>%
            mutate(COUNTY = str_c(STATE, COUNTY)) %>%
            filter(COUNTY %in% locDF$COUNTY) %>%
            mutate(TRACT = str_c(COUNTY, TRACT)) %>%
            mutate(BLKGRP = str_c(TRACT, BLKGRP)) %>%
            mutate(BLOCK = str_c(BLKGRP, BLOCK))
        
        geos <- DF %>%
            mutate(COUNTY = str_sub(COUNTY, 3, -1)) %>%
            select(STATE, COUNTY) %>%
            unique()
        
        block_geo_df <- do.call(sf:::rbind.sf, lapply(1:nrow(geos), function(i){
            block_groups(
                state = geos$STATE[i], county = geos$COUNTY[i], class = "sf",
                cb = FALSE)})) %>%
            filter(GEOID %in% unique(DF$BLKGRP))
        
        place_center_df <- central_df %>%
            ungroup() %>%
            mutate(NAMELSAD = str_replace(NAMELSAD, " Metro Area", "")) %>%
            mutate(NAMELSAD = str_replace(NAMELSAD, " Micro Area", "")) %>%
            filter(NAMELSAD == msa) %>%
            select(name_full, geometry)
        
        center_block_df <- st_intersection(block_geo_df, place_center_df) %>%
            mutate(Polygon = sapply(geometry, function(z){
                any(grepl("POLYGON", class(z)))})) %>%
            filter(Polygon)

        ## Make sure we got this right
        # ggplot(center_block_df) +
        #     geom_sf(
        #         aes(fill=NULL), color = "red",
        #         data = cbsa_df %>%
        #             mutate(NAMELSAD = str_replace(NAMELSAD, " Metro Area", "")) %>%
        #             mutate(NAMELSAD = str_replace(NAMELSAD, " Micro Area", "")) %>%
        #             filter(NAMELSAD == msa)) +
        #     geom_sf(aes(fill=NULL)) +
        #     theme_void()
        
        # as a hack we can recode tract to be either in or outside of the 
        # city place that way we can use the same calcH function
        DF2 <- DF %>%
            mutate(TRACT = as.character(as.numeric(
                BLKGRP %in% center_block_df$GEOID))) %>%
            pivot_longer(DP:CENSUS) %>%
            select(TRACT, BLOCK, RACE, name, value) %>%
            filter(RACE %in% c("Hispanic", "White", "Black", "Asian"))
        
        calcH(DF2, group = F) %>%
            mutate(CBSA = msa)
    }))
    
    write_csv(allDF, "results/placedecompositionH.csv")
}

if(!file.exists("results/BGplacedecompositionH.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        
        DF <- bind_rows(lapply(
            str_c("data/prep_data/", unique(locDF$statename),"_750.Rds"), read_rds)) %>%
            filter(!(CENSUS == 0 & DP == 0)) %>%
            mutate(COUNTY = str_c(STATE, COUNTY)) %>%
            filter(COUNTY %in% locDF$COUNTY) %>%
            mutate(TRACT = str_c(COUNTY, TRACT)) %>%
            mutate(BLKGRP = str_c(TRACT, BLKGRP)) %>%
            mutate(BLOCK = str_c(BLKGRP, BLOCK))
        
        geos <- DF %>%
            mutate(COUNTY = str_sub(COUNTY, 3, -1)) %>%
            select(STATE, COUNTY) %>%
            unique()
        
        block_geo_df <- do.call(sf:::rbind.sf, lapply(1:nrow(geos), function(i){
            block_groups(
                state = geos$STATE[i], county = geos$COUNTY[i], class = "sf",
                cb = FALSE)})) %>%
            filter(GEOID %in% unique(DF$BLKGRP))
        
        place_center_df <- central_df %>%
            ungroup() %>%
            mutate(NAMELSAD = str_replace(NAMELSAD, " Metro Area", "")) %>%
            mutate(NAMELSAD = str_replace(NAMELSAD, " Micro Area", "")) %>%
            filter(NAMELSAD == msa) %>%
            select(name_full, geometry)
        
        center_block_df <- st_intersection(block_geo_df, place_center_df) %>%
            mutate(Polygon = sapply(geometry, function(z){
                any(grepl("POLYGON", class(z)))})) %>%
            filter(Polygon)
        
        ## Make sure we got this right
        # ggplot(center_block_df) +
        #     geom_sf(
        #         aes(fill=NULL), color = "red",
        #         data = cbsa_df %>%
        #             mutate(NAMELSAD = str_replace(NAMELSAD, " Metro Area", "")) %>%
        #             mutate(NAMELSAD = str_replace(NAMELSAD, " Micro Area", "")) %>%
        #             filter(NAMELSAD == msa)) +
        #     geom_sf(aes(fill=NULL)) +
        #     theme_void()
        
        # as a hack we can recode tract to be either in or outside of the 
        # city place that way we can use the same calcH function
        DF2 <- DF %>%
            mutate(TRACT = as.character(as.numeric(
                BLKGRP %in% center_block_df$GEOID))) %>%
            pivot_longer(DP:CENSUS) %>%
            select(TRACT, BLOCK, RACE, name, value) %>%
            filter(RACE %in% c("Hispanic", "White", "Black", "Asian"))
        
        calcH(DF2, group = F) %>%
            mutate(CBSA = msa)
    }))
    
    write_csv(allDF, "results/BGplacedecompositionH.csv")
}


plot_list <- list()

plot_list$central_scatter <- allDF %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    select(name, Pb, CBSA, type) %>%
    pivot_wider(CBSA:type, names_from = name, values_from = Pb) %>%
    ggplot(aes(x = CENSUS, y = DP, color = type, text = CBSA)) +
    geom_point() +
    theme_classic() +
    geom_abline() +
    ggtitle(
        paste(
            "Difference in Thiels H Attributable to City Center/Periphery",
            "Differences of Metropolitan Areas", sep = "\n")) +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC"))

plot_list$central_density <- allDF %>%
    ggplot(aes(x = Pb, fill = name)) +
    geom_density(alpha=.4) +
    theme_classic() +
    ggtitle("Distribution of Thiels H Attributable to City Center/Periphery") +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC"))

saveRDS(plot_list, file = "results/place_plots.Rds")
