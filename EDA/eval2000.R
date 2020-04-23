rm(list=ls())
library(tidyverse)
library(tidycensus)
library(ipumsr)
library(tigris)
library(sf)
census_api_key(Sys.getenv("CENSUS_API_KEY"))
options(tigris_use_cache = TRUE)

source("./EDA/utils.R")

totDF <- read_nhgis(
    "data/2000census/nhgis0034_csv.zip",
    data_layer = "nhgis0034_csv/nhgis0034_ts_geog2010_blck_grp.csv") %>%
    filter(DATAYEAR == 2000) %>%
    set_ipums_var_attributes(read_ipums_codebook(
        "data/2000census/nhgis0034_csv.zip",
        "nhgis0034_csv/nhgis0034_ts_geog2010_blck_grp_codebook.txt"
    )) %>%
    mutate(White = CY8AA, Black = CY8AB, Asian = CY8AD, Hispanic = CY8AF) %>%
    mutate(STATE = STATEA, COUNTY = str_c(STATE, COUNTYA)) %>%
    mutate(TRACT = str_c(COUNTY, TRACTA)) %>%
    mutate(BLOCKGRP = str_c(TRACT, BLCK_GRPA)) %>%
    select(White, Black, Asian, Hispanic, STATE, COUNTY, TRACT, BLOCKGRP)

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

if(!file.exists("results/BGgeodecompositionH2000.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        STS <- unique(locDF$statename)
        CTS <- unique(locDF$COUNTY)
        
        DF <- totDF %>%
            filter(COUNTY %in% CTS) %>%
            pivot_longer(
                White:Hispanic, names_to = "RACE", values_to = "CENSUS") %>%
            rename(BLKGRP = BLOCKGRP)
        
        geos <- DF %>%
            mutate(COUNTY = str_sub(COUNTY, 3, -1)) %>%
            select(STATE, COUNTY) %>%
            unique()
        
        block_geo_df <- do.call(sf:::rbind.sf, lapply(1:nrow(geos), function(i){
            block_groups(
                state = geos$STATE[i], county = geos$COUNTY[i], class = "sf",
                cb = FALSE, year = 2010)})) %>%
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
            summarize(Pop = sum(CENSUS, na.rm=TRUE)) %>%
            filter(name_full != "No Place") %>%
            arrange(-Pop) %>%
            # use head incase of ties
            head(n=1) %>%
            pull(name_full)
        
        ## Make sure we got this right
        # block_geo_df %>%
        #     rename(BLKGRP = GEOID) %>%
        #     left_join(block_place_df, by = "BLKGRP") %>%
        #     mutate(name_full = ifelse(
        #         is.na(name_full), "No Place", name_full)) %>%
        #     mutate(center = name_full == center_place) %>%
        #     ggplot(aes(fill=center)) +
        #     geom_sf() +
        #     theme_void()
        # 
        # block_geo_df %>%
        #     rename(BLKGRP = GEOID) %>%
        #     left_join(block_place_df, by = "BLKGRP") %>%
        #     mutate(name_full = ifelse(
        #         is.na(name_full), "No Place", name_full)) %>%
        #     ggplot(aes(fill=name_full)) +
        #     geom_sf() +
        #     theme_void()
        
        final_df <- DF2 %>%
            mutate(Center = ifelse(
                name_full == center_place, "Center", "Not Center")) %>%
            rename(BLOCKGRP = BLKGRP)
        
        bind_rows(
            final_df %>%
                pivot_longer(CENSUS) %>%
                select(TRACT, BLOCKGRP, RACE, name, value) %>%
                filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
                group_by(TRACT, BLOCKGRP, RACE, name) %>%
                summarise(value = sum(value)) %>%
                ungroup() %>%
                rename(BLOCK = BLOCKGRP) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Tract"),
            
            final_df %>%
                mutate(TRACT = name_full) %>%
                pivot_longer(CENSUS) %>%
                select(TRACT, BLOCKGRP, RACE, name, value) %>%
                filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
                group_by(TRACT, BLOCKGRP, RACE, name) %>%
                summarise(value = sum(value)) %>%
                ungroup() %>%
                rename(BLOCK = BLOCKGRP) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Place"),
            
            final_df %>%
                mutate(TRACT = Center) %>%
                pivot_longer(CENSUS) %>%
                select(TRACT, BLOCKGRP, RACE, name, value) %>%
                filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
                group_by(TRACT, BLOCKGRP, RACE, name) %>%
                summarise(value = sum(value)) %>%
                ungroup() %>%
                rename(BLOCK = BLOCKGRP) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Center")) %>%
            mutate(CBSA = msa, center = center_place) %>%
            arrange(name, Level) %>%
            group_by(name) %>%
            mutate(seg_explained = Pb - lag(Pb, default = 0)) %>%
            ungroup()
    }))
    
    write_csv(allDF, "results/BGgeodecompositionH2000.csv")
}

if(!file.exists("results/BGdecompositionH2000.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        
        STS <- unique(locDF$statename)
        CTS <- unique(locDF$COUNTY)
        
        DF <- totDF %>%
            filter(COUNTY %in% CTS) %>%
            pivot_longer(
                White:Hispanic, names_to = "RACE", values_to = "CENSUS")
        
        DF %>%
            pivot_longer(CENSUS) %>%
            select(TRACT, BLOCKGRP, RACE, name, value) %>%
            filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
            group_by(TRACT, BLOCKGRP, RACE, name) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            rename(BLOCK = BLOCKGRP) %>% 
            calcH(group = T) %>%
            mutate(CBSA = msa)
    }))
    
    write_csv(allDF, "results/BGdecompositionH2000.csv")
}

compDF <- bind_rows(
    read_csv("results/BGdecompositionH.csv") %>%
        mutate(Year = 2010),
    read_csv("results/BGdecompositionH2000.csv") %>%
        mutate(Year = 2000)
)

compDF %>%
    filter(Year == 2000 | name == "DP") %>%
    select(-name) %>%
    pivot_longer(Hw:Pbg) %>%
    arrange(CBSA, name, Year) %>%
    group_by(CBSA, name) %>%
    summarize(DP = diff(value)) %>%
    left_join(
        compDF %>%
            filter(Year == 2000 | name == "CENSUS") %>%
            select(-name) %>%
            pivot_longer(Hw:Pbg) %>%
            arrange(CBSA, name, Year) %>%
            group_by(CBSA, name) %>%
            summarize(CENSUS = diff(value))) %>%
    ggplot(aes(x = CENSUS, y = DP)) +
    geom_point() +
    theme_classic() +
    geom_abline() +
    facet_wrap(~name, scales = "free")

compDF %>%
    filter(Year == 2000 | name == "DP") %>%
    select(-name) %>%
    pivot_longer(Hw:Pbg) %>%
    arrange(CBSA, name, Year) %>%
    group_by(CBSA, name) %>%
    summarize(DP = diff(value)) %>%
    left_join(
        compDF %>%
            filter(Year == 2000 | name == "CENSUS") %>%
            select(-name) %>%
            pivot_longer(Hw:Pbg) %>%
            arrange(CBSA, name, Year) %>%
            group_by(CBSA, name) %>%
            summarize(CENSUS = diff(value))) %>%
    filter(name == "Pb") %>%
    ungroup() %>%
    select(DP, CENSUS) %>%
    pivot_longer(DP:CENSUS) %>%
    group_by(name) %>%
    summarize(
        mu = round(mean(value*100, na.rm = TRUE), 2),
        pplus = mean((sign(value)+1)/.02, na.rm = TRUE)) %>%
    mutate(t=paste0(
        "Percent w/ increase: ", round(pplus),
        "\nAvg Percentage Point Change: ", mu)) %>%
    ggplot(aes(x=name, y=pplus)) +
    geom_col() +
    geom_label(aes(label=t), nudge_y = -10) +
    theme_classic() +
    labs(x="", y="Percent") +
    ggtitle(
        "Percent Metro Areas With Increases in Census Tract Level Segregation")

compGEODF <- bind_rows(
    read_csv("results/BGgeodecompositionH.csv") %>%
        mutate(Year = 2010),
    read_csv("results/BGgeodecompositionH2000.csv") %>%
        mutate(Year = 2000)) %>%
    filter(Level != "Tract")

compGEODF %>%
    select(Year, name, CBSA, center, Level, seg_explained) %>%
    rbind(
        compGEODF %>%
            filter(Level == "Place") %>%
            mutate(seg_explained = 1-Pb) %>%
            select(Year, name, CBSA, center, Level, seg_explained) %>%
            mutate(Level = "Block Group") %>%
            unique()) %>% 
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    filter(Year == 2000 | name == "DP") %>%
    arrange(type, CBSA, Level, Year) %>%
    group_by(type, CBSA, Level) %>%
    summarize(DP = diff(seg_explained)) %>%
    left_join(
        compGEODF %>%
            select(Year, name, CBSA, center, Level, seg_explained) %>%
            rbind(
                compGEODF %>%
                    filter(Level == "Place") %>%
                    mutate(seg_explained = 1-Pb) %>%
                    select(Year, name, CBSA, center, Level, seg_explained) %>%
                    mutate(Level = "Block Group") %>%
                    unique()) %>% 
            left_join(
                msaCountyDF %>%
                    rename(CBSA = cbsatitle) %>%
                    select(CBSA, type) %>%
                    unique()) %>%
            filter(Year == 2000 | name == "CENSUS") %>%
            arrange(type, CBSA, Level, Year) %>%
            group_by(type, CBSA, Level) %>%
            summarize(CENSUS = diff(seg_explained))) %>%
    mutate(Level = factor(Level, c("Center", "Place", "Tract", "Block Group"))) %>%
    ggplot(aes(x = CENSUS, y = DP)) +
    geom_point() +
    theme_classic() +
    geom_abline() +
    facet_wrap(~Level) +
    ggtitle(
        "Percentage Change between 2000 and 2010 of Geographic Segregation Explanation")
