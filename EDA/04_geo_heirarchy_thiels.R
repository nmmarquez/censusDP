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

if(!file.exists("results/geodecompositionH.csv")){
    allDF <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        STS <- unique(locDF$statename)
        
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
                name_full == center_place, "Center", "Not Center"))
        
        bind_rows(
            final_df %>%
                pivot_longer(DP:CENSUS) %>%
                select(TRACT, BLOCK, RACE, name, value) %>%
                # I reversed AI and ASIAN so this is a dumb hack to get Asian
                filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Tract"),
            
            final_df %>%
                mutate(TRACT = name_full) %>%
                pivot_longer(DP:CENSUS) %>%
                select(TRACT, BLOCK, RACE, name, value) %>%
                # I reversed AI and ASIAN so this is a dumb hack to get Asian
                filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Place"),
                
            final_df %>%
                mutate(TRACT = Center) %>%
                pivot_longer(DP:CENSUS) %>%
                select(TRACT, BLOCK, RACE, name, value) %>%
                # I reversed AI and ASIAN so this is a dumb hack to get Asian
                filter(RACE %in% c("Hispanic", "White", "Black", "Asian")) %>%
                calcH(group = FALSE) %>%
                mutate(Level = "Center")) %>%
            mutate(CBSA = msa, center = center_place) %>%
            arrange(name, Level) %>%
            group_by(name) %>%
            mutate(seg_explained = Pb - lag(Pb, default = 0)) %>%
            ungroup()
    }))
    
    write_csv(allDF, "results/geodecompositionH.csv")
}

if(!file.exists("results/BGgeodecompositionH.csv")){
    allDF2 <- bind_rows(lapply(unique(msaCountyDF$cbsatitle), function(msa){
        
        cat(paste0(msa, "\n"))
        
        locDF <- msaCountyDF %>% filter(cbsatitle == msa)
        STS <- unique(locDF$statename)
        
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
                pivot_longer(DP:CENSUS) %>%
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
                pivot_longer(DP:CENSUS) %>%
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
                pivot_longer(DP:CENSUS) %>%
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
    
    write_csv(allDF2, "results/BGgeodecompositionH.csv")
}

allDF <- read_csv("results/BGgeodecompositionH.csv")

allDF %>%
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
    ggplot(aes(x = CENSUS, y = DP)) +
    geom_point() +
    theme_classic() +
    geom_abline() +
    facet_wrap(~Level)

plot_list <- list()

ggplotly(plot_list$tract_scatter <- allDF %>%
    filter(Level == "Tract") %>%
    mutate(text_ = paste0("CBSA: ", CBSA, "\nCenter: ", center)) %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    select(name, Pb, text_, type) %>%
    pivot_wider(text_:type, names_from = name, values_from = Pb) %>%
    ggplot(aes(x = CENSUS, y = DP, color = type, text = text_)) +
    geom_point(alpha=.6) +
    theme_classic() +
    geom_abline() +
    xlim(0, 1) +
    ylim(0, 1) +
    ggtitle(
        paste(
            "Difference in Thiels H Attributable to Tract",
            "Differences of Metropolitan Areas", sep = "\n")) +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$tract_alone_scatter <- allDF %>%
     filter(Level == "Tract") %>%
     mutate(text_ = paste0("CBSA: ", CBSA, "\nCenter: ", center)) %>%
     left_join(
         msaCountyDF %>%
             rename(CBSA = cbsatitle) %>%
             select(CBSA, type) %>%
             unique()) %>%
     select(name, seg_explained, text_, type) %>%
     pivot_wider(text_:type, names_from = name, values_from = seg_explained) %>%
     ggplot(aes(x = CENSUS, y = DP, color = type, text = text_)) +
     geom_point(alpha=.6) +
     xlim(0, 1) +
     ylim(0, 1) +
     theme_classic() +
     geom_abline() +
     ggtitle(
         paste(
             "Difference in Thiels H Attributable to Tract Alone",
             "Differences of Metropolitan Areas", sep = "\n")) +
     scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
     scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$center_scatter <- allDF %>%
     filter(Level == "Center") %>%
     mutate(text_ = paste0("CBSA: ", CBSA, "\nCenter: ", center)) %>%
     left_join(
         msaCountyDF %>%
             rename(CBSA = cbsatitle) %>%
             select(CBSA, type) %>%
             unique()) %>%
     select(name, Pb, text_, type) %>%
     pivot_wider(text_:type, names_from = name, values_from = Pb) %>%
     ggplot(aes(x = CENSUS, y = DP, color = type, text = text_)) +
     geom_point(alpha=.6) +
     xlim(0, 1) +
     ylim(0, 1) +
     theme_classic() +
     geom_abline() +
     ggtitle(
         paste(
             "Difference in Thiels H Attributable to City Center/Preiphery",
             "Differences of Metropolitan Areas", sep = "\n")) +
     scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
     scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$center_alone_scatter <- allDF %>%
    filter(Level == "Center") %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    select(name, seg_explained, CBSA, type) %>%
    pivot_wider(CBSA:type, names_from = name, values_from = seg_explained) %>%
    ggplot(aes(x = CENSUS, y = DP, color = type, text = CBSA)) +
    geom_point(alpha=.6) +
    theme_classic() +
    geom_abline() +
    xlim(0, 1) +
    ylim(0, 1) +
    ggtitle(
        paste(
            "Difference in Thiels H Attributable to City Center/Preiphery Alone",
            "Differences of Metropolitan Areas", sep = "\n")) +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$place_scatter <- allDF %>%
     filter(Level == "Place") %>%
     mutate(text_ = paste0("CBSA: ", CBSA, "\nCenter: ", center)) %>%
     left_join(
         msaCountyDF %>%
             rename(CBSA = cbsatitle) %>%
             select(CBSA, type) %>%
             unique()) %>%
     select(name, Pb, text_, type) %>%
     pivot_wider(text_:type, names_from = name, values_from = Pb) %>%
     ggplot(aes(x = CENSUS, y = DP, color = type, text = text_)) +
     geom_point(alpha=.6) +
     theme_classic() +
     xlim(0, 1) +
     ylim(0, 1) +
     geom_abline() +
     ggtitle(
         paste(
             "Difference in Thiels H Attributable to Place",
             "Differences of Metropolitan Areas", sep = "\n")) +
     scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
     scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$place_alone_scatter <- allDF %>%
     filter(Level == "Place") %>%
     left_join(
         msaCountyDF %>%
             rename(CBSA = cbsatitle) %>%
             select(CBSA, type) %>%
             unique()) %>%
     select(name, seg_explained, CBSA, type) %>%
     pivot_wider(CBSA:type, names_from = name, values_from = seg_explained) %>%
     ggplot(aes(x = CENSUS, y = DP, color = type, text = CBSA)) +
     geom_point(alpha=.6) +
     theme_classic() +
     geom_abline() +
     xlim(0, 1) +
     ylim(0, 1) +
     ggtitle(
         paste(
             "Difference in Thiels H Attributable to Place Alone",
             "Differences of Metropolitan Areas", sep = "\n")) +
     scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
     scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$central_density <- allDF %>%
    filter(Level == "Center") %>%
    ggplot(aes(x = Pb, fill = name)) +
    geom_density(alpha=.4) +
    theme_classic() +
    ggtitle("Distribution of Thiels H Attributable to City Center/Periphery") +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$place_density <- allDF %>%
    filter(Level == "Place") %>%
    ggplot(aes(x = Pb, fill = name)) +
    geom_density(alpha=.4) +
    theme_classic() +
    ggtitle("Distribution of Thiels H Attributable to Place") +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$tract_density <- allDF %>%
    filter(Level == "Tract") %>%
    ggplot(aes(x = Pb, fill = name)) +
    geom_density(alpha=.4) +
    theme_classic() +
    ggtitle("Distribution of Thiels H Attributable to Tract") +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$block_density <- allDF %>%
    filter(Level == "Tract") %>%
    mutate(Pb = 1 - Pb) %>%
    ggplot(aes(x = Pb, fill = name)) +
    geom_density(alpha=.4) +
    theme_classic() +
    ggtitle("Distribution of Thiels H Attributable to Block") +
    scale_fill_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")) +
    scale_color_manual(values=c("#b7a57a", "#4b2e83", "#000000", "#DCDCDC")))

ggplotly(plot_list$seg_explained <- allDF %>%
    select(name, CBSA, center, Level, seg_explained) %>%
    rbind(
        allDF %>%
            filter(Level == "Place") %>%
            mutate(seg_explained = 1-Pb) %>%
            select(name, CBSA, center, Level, seg_explained) %>%
            mutate(Level = "Block") %>%
            unique()) %>% 
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>%
    mutate(Level = factor(Level, c("Center", "Place", "Tract", "Block"))) %>%
    group_by(name, type, Level) %>%
    summarize(mean_P = mean(seg_explained, na.rm=TRUE)) %>%
    ggplot(aes(x = name, y = mean_P, fill = Level)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    facet_wrap(~type) +
    labs(x = "Source", y = "Percent Segregation Explained"))

(plot_list$H_explained <- allDF %>%
    select(name, H, CBSA, center, CBSA) %>%
    rename(Hb = H) %>%
    mutate(Level = "Block") %>%
    unique() %>%
    bind_rows(
        allDF %>%
            select(name, Hb, CBSA, center, Level)) %>%
    mutate(Level = factor(Level, c("Center", "Place", "Tract", "Block"))) %>%
    arrange(CBSA, center, name, Level) %>%
    group_by(CBSA, center, name) %>%
    mutate(H_alone = Hb - lag(Hb, default = 0)) %>%
    left_join(
        msaCountyDF %>%
            rename(CBSA = cbsatitle) %>%
            select(CBSA, type) %>%
            unique()) %>% 
    group_by(name, type, Level) %>%
    summarize(mean_H = mean(H_alone, na.rm=TRUE)) %>%
    ggplot(aes(x = name, y = mean_H, fill = Level)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    facet_wrap(~type) +
    labs(x = "Source", y = "Segregation by Level"))


saveRDS(plot_list, file = "results/geo_plots.Rds")
