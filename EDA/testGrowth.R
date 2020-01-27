rm(list=ls())
library(tidyverse)
library(sf)
library(leaflet)
library(leafgl)
library(mapview)
library(leaflet)
library(leafgl)
library(colorRamps)
library(spdep)

stVec <- rev(str_replace_all(c(state.name, "District of Columbia"), " ", "_"))

DF <- bind_rows(lapply(stVec, function(z){
    
    print(z)
    
    df_ <- read_rds(paste0("data/prep_data/", z,"_750.Rds"))
    
    whiteDF <- filter(df_, RACE == "White") %>%
        rename(White = CENSUS) %>%
        select(STATE, COUNTY, TRACT, BLKGRP, BLOCK, White)
    
    df_ %>%
        arrange(STATE, COUNTY, TRACT, BLKGRP, BLOCK, RACE) %>%
        group_by(STATE, COUNTY, TRACT, BLKGRP, BLOCK) %>%
        summarize(cPop = sum(CENSUS), dPop = sum(DP)) %>%
        ungroup() %>%
        left_join(whiteDF) %>%
        mutate(popChange = dPop - cPop) %>%
        mutate(pWhite = White / cPop) %>%
        filter(cPop != 0) %>%
        mutate(increase = popChange > 0)
}))

summary(testLM2 <- glm(
    increase ~ pWhite, data = DF, family = "binomial"))

summary(testLM1 <- lm(
    popChange ~ pWhite, data = DF))

testDF %>%
    ggplot(aes(x = pWhite, y=popChange)) +
    geom_point() +
    theme_classic()

DF %>%
    mutate(pwidth = cut(pWhite, c(-Inf, 1:10)/10)) %>%
    group_by(pwidth) %>%
    summarize(popChange = sum(popChange))

DF %>%
    mutate(pwidth = cut(pWhite, c(-Inf, 1:100)/100)) %>%
    group_by(pwidth) %>%
    summarize(popChange = sum(popChange)) %>%
    print(n=200)
