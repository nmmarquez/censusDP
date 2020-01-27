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
library(INLA)
inla.setOption(pardiso.license = "~/pardiso.lic")

counties <- c(MI="079")
#counties <- c(LA="037", OC="059")

DF <- read_rds("data/prep_data/Wisconsin_750.Rds") %>%
    filter(!(CENSUS == 0 & DP == 0)) %>%
    filter(COUNTY %in% counties) %>%
    mutate(TRACT = str_c(STATE, COUNTY, TRACT)) %>%
    mutate(BLOCK = str_c(STATE, COUNTY, TRACT, BLKGRP, BLOCK)) %>%
    pivot_longer(DP:CENSUS) %>%
    select(TRACT, BLOCK, RACE, name, value) %>%
    filter(RACE %in% c("Hispanic", "White", "Black", "Asian"))

# multi level entropy calculations
DF %>%
    pivot_longer(DP:CENSUS) %>%
    group_by(name, TRACT, BLKGRP, BLOCK, RACE) %>%
    summarize(N = sum(value)) %>%
    mutate(Total = sum(N), Qr = N / Total, Qrt = Qr * log(1/Qr)) %>%
    mutate(Qrt = ifelse(is.na(Qrt), 0, Qrt)) %>%
    summarize(E = sum(Qrt) / log(8)) %>%
    group_by(name) %>%
    summarize(meanE = mean(E))

DF %>%
    pivot_longer(DP:CENSUS) %>%
    group_by(name, TRACT, RACE) %>%
    summarize(N = sum(value)) %>%
    mutate(Total = sum(N), Qr = N / Total, Qrt = Qr * log(1/Qr)) %>%
    mutate(Qrt = ifelse(is.na(Qrt), 0, Qrt)) %>%
    summarize(E = sum(Qrt) / log(8)) %>%
    group_by(name) %>%
    summarize(meanE = mean(E))

DF %>%
    pivot_longer(DP:CENSUS) %>%
    group_by(name, RACE) %>%
    summarize(N = sum(value)) %>%
    mutate(Total = sum(N), Qr = N / Total) %>%
    summarize(E = sum(Qr * log(1/Qr)) / log(8))

# calcultite H index at top level using blocks as sub level and

calcH <- function(DF){

    blockEDF <- DF %>%
        group_by(name, TRACT, BLOCK, RACE) %>%
        summarize(N = sum(value)) %>%
        mutate(Total = sum(N), Qr = N / Total, Qrt = Qr * log(1/Qr)) %>%
        mutate(Qrt = ifelse(is.na(Qrt), 0, Qrt)) %>%
        summarize(E = sum(Qrt) / log(exp(1)), Pop = sum(N)) %>%
        ungroup()
    
    topEDF <- DF %>%
        group_by(name, RACE) %>%
        summarize(N = sum(value)) %>%
        mutate(Pop = sum(N), Qr = N / Pop) %>%
        summarize(totalE = sum(Qr * log(1/Qr)) / log(exp(1)))
    
    indexHDF <- blockEDF %>%
        left_join(topEDF, by = "name") %>%
        group_by(name) %>%
        mutate(totalPop = sum(Pop), numer = Pop / totalPop * (totalE - E)) %>%
        summarize(H = sum(numer), totalE = first(totalE), totalPop = sum(Pop)) %>%
        mutate(H = H/totalE)
    
    tractHDF <- DF %>%
        group_by(name, TRACT, RACE) %>%
        summarize(N = sum(value)) %>%
        mutate(Total = sum(N), Qr = N / Total, Qrt = Qr * log(1/Qr)) %>%
        mutate(Qrt = ifelse(is.na(Qrt), 0, Qrt)) %>%
        summarize(tractE = sum(Qrt) / log(exp(1)), tractPop = sum(N)) %>%
        ungroup() %>%
        right_join(blockEDF, by = c("name", "TRACT")) %>% 
        group_by(name, TRACT) %>%
        mutate(tractPop = sum(Pop), numer = Pop / tractPop * (tractE - E)) %>%
        summarize(
            tractH = sum(numer), tractE = first(tractE), tractPop = sum(Pop)) %>%
        mutate(tractH = tractH/tractE)
    
    tractHDF %>%
        left_join(indexHDF, by = "name") %>%
        mutate(tractH = ifelse(is.na(tractH), 1, tractH)) %>%
        group_by(name) %>%
        summarize(Hw = sum((tractE * tractH * tractPop) / (totalE * totalPop))) %>%
        left_join(indexHDF, by = "name") %>%
        mutate(Hb = H-Hw, Pw = Hw/H)
}

calcH(DF)

tibble(
    RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
    value = c(10, 10, 10, 10, 10, 10, 10, 10),
    TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
    BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
    mutate(name="test") %>%
    calcH()

tibble(
    RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
    value = c(10, 0, 0, 10, 10, 0, 0, 10),
    TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
    BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
    mutate(name="test") %>%
    calcH()

tibble(
    RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
    value = c(10, 0, 10, 0, 0, 10, 0, 10),
    TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
    BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
    mutate(name="test") %>%
    calcH()

tibble(
    RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
    value = c(10, 1, 10, 9, 5, 10, 5, 10),
    TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
    BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
    mutate(name="test") %>%
    calcH()


tibble(
    RACE = c("B", "W", "B", "W", "B", "W", "B", "W"),
    value = c(15, 5, 15, 5, 5, 15, 5, 15),
    TRACT = c("1", "1", "1", "1", "2", "2", "2", "2"),
    BLOCK = c("1", "1", "2", "2", "3", "3", "4", "4")) %>%
    mutate(name="test") %>%
    calcH()
