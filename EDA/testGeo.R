rm(list=ls())
library(tidyverse)
library(sf)
library(leaflet)
library(leafgl)
library(mapview)

DF <- read_rds("data/prep_data/California_750.Rds")
tDF <- read_rds("data/prep_data/California_140.Rds")

sDF <- sf::read_sf("data/shapefiles/California/tl_2010_06_tabblock10.dbf") %>% 
    filter(COUNTYFP10 == "059"| COUNTYFP10 == "037") %>%
    rename(
        STATE = STATEFP10, COUNTY = COUNTYFP10,
        TRACT = TRACTCE10, BLOCK = BLOCKCE10)

DF2 <- DF %>%
    mutate(RACE2 = case_when(
        RACE %in% c("Asian", "PI") ~ "Asian",
        RACE %in% c("Other", "AI", "Two or more Races") ~ "Other",
        RACE == "Hispanic" ~ "Hispanic",
        RACE == "Black" ~ "Black",
        RACE == "White" ~ "White",
        TRUE ~ NA_character_)) %>%
    group_by(STATE, COUNTY, TRACT, BLOCK, RACE2) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS))

popDF <- DF2 %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS)) %>%
    filter(COUNTY == "059"| COUNTY == "037") %>%
    {(right_join(sDF, .))}

# if(!file.exists("data/cpointCADF.Rds")){
#     cpointDF <- st_sample(popDF, popDF$CENSUS)
#     saveRDS(cpointDF, "data/cpointCADF.Rds")
#     dpointDF <- st_sample(popDF, popDF$DP)
#     saveRDS(pointDF, "data/dpointCADF.Rds")
# }
# 
# cpointDF <- read_rds("data/cpointCADF.Rds")
# dpointDF <- read_rds("data/dpointCADF.Rds")
# 
# DF3 <- DF2 %>%
#     filter(COUNTY == "059"| COUNTY == "037")
# 
# dsfDF <- st_sf(
#     tibble(race = unlist(lapply(1:nrow(DF3), function(i){
#         rep(DF3$RACE2[i], DF3$DP[i])}))), 
#     geometry = dpointDF) %>%
#     mutate(clrs=case_when(
#         race == "White" ~ "Blue",
#         race == "Black" ~ "Green",
#         race == "Asian" ~ "Red",
#         race == "Hispanic" ~ "Yellow",
#         TRUE ~ "Brown"
# ))
# 
# csfDF <- st_sf(
#     tibble(race = unlist(lapply(1:nrow(DF3), function(i){
#         rep(DF3$RACE2[i], DF3$CENSUS[i])}))), 
#     geometry = cpointDF) %>%
#     mutate(clrs=case_when(
#         race == "White" ~ "Blue",
#         race == "Black" ~ "Green",
#         race == "Asian" ~ "Red",
#         race == "Hispanic" ~ "Yellow",
#         TRUE ~ "Brown"
# ))
# 
# cminisfDF <- sample_frac(csfDF, .1)
# colMat <- t(col2rgb(cminisfDF$clrs))/255
# 
# #options(viewer = NULL) # view in browser
# 
# (m <- leaflet() %>%
#         addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
#         addGlPoints(
#             data = cminisfDF, group = "pts", opacity = .3,
#             weight = 3, color = colMat) %>%
#         setView(lng = -118.2, lat = 34.1, zoom = 8))
# 
# mapshot(m, "data/cdotmapCA.html", selfcontained = FALSE)
# 
# dminisfDF <- sample_frac(dsfDF, .1)
# dcolMat <- t(col2rgb(dminisfDF$clrs))/255
# 
# (dm <- leaflet() %>%
#         addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
#         addGlPoints(
#             data = dminisfDF, group = "pts", opacity = .3,
#             weight = 3, color = dcolMat) %>%
#         setView(lng = -118.2, lat = 34.1, zoom = 8))
# 
# mapshot(dm, "data/ddotmapCA.html", selfcontained = FALSE)

DF %>%
    mutate(deltaPop = CENSUS - DP) %>%
    filter(RACE == "Hispanic") %>% 
    ggplot(aes(x = deltaPop)) +
    geom_density() +
    theme_classic() +
    facet_wrap(~RACE)

DF %>%
    filter(CENSUS != 0) %>%
    mutate(deltaPop = DP - CENSUS) %>%
    arrange(abs(deltaPop))

DF %>%
    mutate(deltaPop = DP - CENSUS) %>%
    group_by(RACE) %>%
    summarize(mean(deltaPop))

# look at tract level entropy changes
tDF %>%
    filter(RACE %in% c("Black", "White", "Asian", "Hispanic")) %>%
    group_by(STATE, COUNTY, TRACT) %>%
    mutate(DP = DP/sum(DP), CENSUS = CENSUS/sum(CENSUS)) %>%
    mutate(DP = DP * log(1/DP), CENSUS = CENSUS * log(1/CENSUS)) %>%
    mutate(
        DP = ifelse(is.finite(DP), DP, 0), 
        CENSUS = ifelse(is.finite(CENSUS), CENSUS, 0)) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS)) %>%
    ungroup() %>%
    pivot_longer(DP:CENSUS) %>% 
    ggplot(aes(x=value, fill = name)) +
    geom_density(alpha=.3) +
    theme_classic()

DF %>%
    filter(RACE %in% c("Black", "White", "Asian", "Hispanic")) %>%
    group_by(STATE, COUNTY, TRACT, BLOCK) %>%
    mutate(DP = DP/sum(DP), CENSUS = CENSUS/sum(CENSUS)) %>%
    mutate(DP = DP * log(1/DP), CENSUS = CENSUS * log(1/CENSUS)) %>%
    mutate(
        DP = ifelse(is.finite(DP), DP, 0), 
        CENSUS = ifelse(is.finite(CENSUS), CENSUS, 0)) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS)) %>%
    ungroup() %>%
    pivot_longer(DP:CENSUS) %>% 
    ggplot(aes(x=value, fill = name)) +
    geom_density(alpha=.3) +
    theme_classic()

tDF %>%
    filter(RACE %in% c("Black", "White", "Asian", "Hispanic")) %>%
    group_by(STATE, COUNTY, TRACT) %>%
    mutate(Pop = CENSUS, DP = DP/sum(DP), CENSUS = CENSUS/sum(CENSUS)) %>%
    mutate(DP = DP * log(1/DP), CENSUS = CENSUS * log(1/CENSUS)) %>%
    mutate(
        DP = ifelse(is.finite(DP), DP, 0), 
        CENSUS = ifelse(is.finite(CENSUS), CENSUS, 0)) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS), Pop = sum(Pop)) %>%
    ungroup() %>%
    ggplot(aes(x = CENSUS, y = DP, size=log(Pop))) +
    geom_point(alpha=.3) +
    theme_classic()

DF %>%
    filter(RACE %in% c("Black", "White", "Asian", "Hispanic")) %>%
    filter(COUNTY == "059"| COUNTY == "037") %>%
    group_by(STATE, COUNTY, TRACT, BLOCK) %>%
    mutate(Pop = CENSUS, DP = DP/sum(DP), CENSUS = CENSUS/sum(CENSUS)) %>%
    mutate(DP = DP * log(1/DP), CENSUS = CENSUS * log(1/CENSUS)) %>%
    mutate(
        DP = ifelse(is.finite(DP), DP, 0), 
        CENSUS = ifelse(is.finite(CENSUS), CENSUS, 0)) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS), Pop = sum(Pop)) %>%
    ungroup() %>%
    ggplot(aes(x = CENSUS, y = DP, size=log(Pop))) +
    geom_point(alpha=.01) +
    theme_classic()

DF %>%
    mutate(erasure = (DP == 0) & (CENSUS != 0), diff=DP-CENSUS) %>%
    group_by(STATE, COUNTY, TRACT, BLKGRP, BLOCK) %>%
    mutate(p = CENSUS/sum(CENSUS)) %>%
    filter(erasure) %>%
    ggplot(aes(x=p)) +
    geom_histogram() +
    theme_classic() +
    facet_wrap(~RACE)

DF %>%
    mutate(erasure = (DP == 0) & (CENSUS != 0), diff=DP-CENSUS) %>%
    mutate(creation = (DP != 0) & (CENSUS == 0)) %>%
    group_by(RACE) %>%
    summarise(mean(erasure), sum(erasure*diff), mean(creation), sum(creation*diff))

tDF %>%
    mutate(erasure = (DP == 0) & (CENSUS != 0), diff=DP-CENSUS) %>%
    group_by(STATE, COUNTY, TRACT) %>%
    mutate(p = CENSUS/sum(CENSUS)) %>%
    filter(erasure) %>%
    ggplot(aes(x=p)) +
    geom_histogram() +
    theme_classic() +
    facet_wrap(~RACE, scales = "free")

tDF %>%
    mutate(erasure = (DP == 0) & (CENSUS != 0), diff=DP-CENSUS) %>%
    mutate(creation = (DP != 0) & (CENSUS == 0)) %>%
    group_by(RACE) %>%
    summarise(mean(erasure), sum(erasure*diff), mean(creation), sum(creation*diff))

tDF %>%
    mutate(erasure = (DP == 0) & (CENSUS != 0), diff=DP-CENSUS) %>%
    mutate(creation = (DP != 0) & (CENSUS == 0)) %>%
    filter(erasure) %>%
    arrange(diff)

tDF %>%
    filter(DP == 0 & CENS) %>%
    group_by(STATE, COUNTY, TRACT) %>%
    mutate(p = CENSUS/sum(CENSUS))

eDF <- DF %>%
    filter(RACE %in% c("Black", "White", "Asian", "Hispanic")) %>%
    group_by(STATE, COUNTY, TRACT, BLOCK) %>%
    mutate(DP = DP/sum(DP), CENSUS = CENSUS/sum(CENSUS)) %>%
    mutate(DP = DP * log(1/DP), CENSUS = CENSUS * log(1/CENSUS)) %>%
    mutate(
        DP = ifelse(is.finite(DP), DP, 0), 
        CENSUS = ifelse(is.finite(CENSUS), CENSUS, 0)) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS)) %>%
    ungroup() %>%
    filter(!(DP == 0 & CENSUS == 0)) %>%
    pivot_longer(DP:CENSUS)

sDF %>%
    right_join(eDF) %>%
    filter(COUNTY == "059") %>%
    ggplot(aes(fill=value)) +
    geom_sf(lwd = 0) +
    theme_void() +
    facet_wrap(~name) +
    scale_fill_distiller(palette = "Spectral", direction = 1)
    
etDF <- DF %>%
    filter(RACE %in% c("Black", "White", "Asian", "Hispanic")) %>%
    group_by(STATE, COUNTY, TRACT, BLOCK) %>%
    mutate(DP = DP/sum(DP), CENSUS = CENSUS/sum(CENSUS)) %>%
    mutate(DP = DP * log(1/DP), CENSUS = CENSUS * log(1/CENSUS)) %>%
    mutate(
        DP = ifelse(is.finite(DP), DP, 0), 
        CENSUS = ifelse(is.finite(CENSUS), CENSUS, 0)) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS)) %>%
    ungroup() %>%
    filter(!(DP == 0 & CENSUS == 0)) %>%
    pivot_longer(DP:CENSUS)

sDF %>%
    right_join(eDF) %>%
    filter(COUNTY == "059") %>%
    ggplot(aes(fill=value)) +
    geom_sf(lwd = 0) +
    theme_void() +
    facet_wrap(~name) +
    scale_fill_distiller(palette = "Spectral", direction = 1)


# Create some measures of local clustering
DF %>%
    filter(COUNTYFP10 == "059"| COUNTYFP10 == "037") %>%
    mutate(Asian = RACE == "ASIAN") %>%
    group_by()
