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

counties <- c(LA="037")
#counties <- c(LA="037", OC="059")

DF <- read_rds("data/prep_data/California_750.Rds")
#tDF <- read_rds("data/prep_data/California_140.Rds")

if(!file.exists("data/shapefiles/California/subPolySimp.Rds")){
    sDF <- "data/shapefiles/California/tl_2010_06_tabblock10.dbf" %>%
        sf::read_sf() %>% 
        filter(COUNTYFP10 %in% counties) %>%
        rename(
            STATE = STATEFP10, COUNTY = COUNTYFP10,
            TRACT = TRACTCE10, BLOCK = BLOCKCE10)
    
    for(i in 1:nrow(sDF)){
        sDF$geometry[[i]] <- st_cast(sDF$geometry[[i]], "POLYGON")
    }
    
    saveRDS(sDF, "data/shapefiles/California/subPolySimp.Rds")
}

sDF <- read_rds("data/shapefiles/California/subPolySimp.Rds")
    
DF2 <- DF %>%
    mutate(RACE2 = case_when(
        RACE %in% c("Asian", "PI") ~ "Asian",
        RACE %in% c("Other", "AI", "Two or more Races") ~ "Other",
        RACE == "Hispanic" ~ "Hispanic",
        RACE == "Black" ~ "Black",
        RACE == "White" ~ "White",
        TRUE ~ NA_character_)) %>%
    group_by(STATE, COUNTY, TRACT, BLOCK, RACE2) %>%
    summarize(DP = sum(DP), CENSUS = sum(CENSUS)) %>%
    filter(COUNTY %in% counties)

eths <- c("Asian", "Black", "Hispanic", "Other")
names(eths) <- eths

ethpDFList <- lapply(eths, function(eth){
    DF2 %>%
        group_by(STATE, COUNTY, TRACT, BLOCK) %>%
        summarise(
            dpY = sum((RACE2 == eth) * DP),
            dpN = sum(DP),
            cY = sum((RACE2 == eth) * CENSUS),
            cN = sum(CENSUS)) %>%
        filter(!(dpN == 0 & cN == 0)) %>%
        mutate(cP = cY / cN, dpP = dpY / dpN) %>%
        right_join(sDF, ., by = c("STATE", "COUNTY", "TRACT", "BLOCK"))
})


cols <- t(grDevices::col2rgb(
    matlab.like2(20)[as.numeric(cut_interval(ethpDFList$Black$cP, 20))]))

m = leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
    addGlPolygons(data = ethpDFList$Black, 
                  color = cols, 
                  popup = "cP") %>% 
    #addMouseCoordinates() %>%
    setView(lng = -118, lat = 34, zoom = 9) 
    #addLayersControl(overlayGroups = "pols")

spDF <- as(sDF, "Spatial")
spDF$GEOID10_2 <- spDF$GEOID10

if(!file.exists("data/shapefiles/California/adjMat.Rds")){
    adjMat <- poly2nb(spDF, queen=TRUE)
    saveRDS(adjMat, "data/shapefiles/California/adjMat.Rds")
}

adjMat <- read_rds("data/shapefiles/California/adjMat.Rds")

formula1 <- dpY ~ f(GEOID10, model="besag",graph=adjMat) +
    f(GEOID10_2, model="iid")

testDF <- sDF %>%
    left_join(
        select(as_tibble(ethpDFList$Black), GEOID10, cY, cN, dpY, dpN)) %>%
    mutate(GEOID10 = 1:n()) %>%
    mutate(GEOID10_2 = GEOID10)

result3 <- inla(formula1, family="binomial", data=testDF, Ntrials=dpN)

formula2 <- cY ~ f(GEOID10, model="besag",graph=adjMat) +
    f(GEOID10_2, model="iid")

result4 <- inla(formula2, family="binomial", data=testDF, Ntrials=cN)

rbind(summary(result4)$hyperpar[1,], summary(result3)$hyperpar[1,]) %>%
    as_tibble() %>%
    mutate(method = c("DP", "Census")) %>%
    ggplot(
        aes(x = method, y = mean, ymin = `0.025quant`, ymax = `0.975quant`)) +
    geom_point() +
    geom_errorbar() +
    coord_flip() +
    theme_classic()

longDF <- bind_rows(
    result3$summary.random$GEOID10 %>%
        as_tibble() %>%
        mutate(id=1:n(), method = "Census", GEOID10 = sDF$GEOID10),
    result4$summary.random$GEOID10 %>%
        as_tibble() %>%
        mutate(id=1:n(), method = "DP", GEOID10 = sDF$GEOID10))

longDF %>%
    select(ID, mean, method) %>%
    pivot_wider(values_from = mean, names_from = method) %>%
    ggplot(aes(x=Census, y=DP)) +
    geom_point(alpha=.3) +
    theme_classic()

sDF %>%
    right_join(longDF, by = "GEOID10") %>%
    ggplot() +
    geom_sf(aes(fill=boot::inv.logit(mean)), lwd=0) +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    theme_void() +
    facet_wrap(~method)

# ethpDFList$Black %>%
#     mutate(cP = cY / cN) %>%
#     {right_join(sDF, .)} %>%
#     ggplot() +
#     geom_sf(aes(fill=cP), lwd=0) +
#     scale_fill_distiller(palette = "Spectral", direction = 1) +
#     theme_void()
# 
# ethpDFList$Black %>%
#     {right_join(sDF, .)} %>%
#     ggplot() +
#     geom_sf(aes(fill=cP), lwd=0) +
#     scale_fill_distiller(palette = "Spectral", direction = 1) +
#     theme_void()