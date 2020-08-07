rm(list=ls())
library(tidyverse)
library(data.table)

# Link to data home and documentation
# https://nhgis.org/privacy-protected-demonstration-data
# Variable names taken from documentation
var_rename <- c(
    "H7Z003" = "White", 
    "H7Z004" = "Black", 
    "H7Z005" = "AIAN",
    "H7Z006" = "Asian",
    "H7Z007" = "NHOPI",
    "H7Z008" = "Other",
    "H7Z009" = "Two or More",
    "H7Z010" = "Hispanic"
)

# create a vector of names and abbreviations
state_vec <- c(str_replace(state.name, " ", "_"), "District_of_Columbia")
names(state_vec) <- c(state.abb, "DC")

# select the columns to keep
col_select <- c(
    "gisjoin",
    c(sapply(c("_sf", "_dp"), function(x) str_c(names(var_rename), x))))

# loop through all state abbreviations (+DC) and downlad/clean data
for(st in names(state_vec)){
    cat(str_c("Starting Download for ", st, "\n"))
    # create temp files and diectories for the zip file and unzipping process
    td <- tempdir()
    tf <- tempfile(tmpdir = td, fileext =".zip")
    
    # base structure of download location url
    link <- "http://assets.nhgis.org/differential-privacy/v20200527/" %>% 
        str_c("nhgis_ppdd_20200527_block_", st, ".zip")
    
    # download file
    download.file(link, tf)
    
    # unzip the file
    unzip(tf, exdir = td)
    
    # get th unzipped csv location
    tff <- str_c(td, "/nhgis_ppdd_20200527_block_", st, ".csv")
    
    cat(str_c("Starting Cleaning for ", st, "\n"))
    
    # read in select columns that pertain to race and ethnicity
    DF <- fread(tff, select = col_select)
    
    # make long
    DF_long <- melt(DF, c("gisjoin"), col_select[-1])
    # census data is sf (for summary file) dp is DP
    DF_long[,Type := ifelse(str_ends(variable, "sf"), "Census", "DP")]
    # use the race variable naming above to recode variable names
    DF_long[,RACE := var_rename[str_sub(variable, 1, 6)]]
    # gisjoin id is weird so lets clean it to make it more friendly
    DF_long[,GEOID := str_c(
        str_sub(gisjoin, 2, 3), str_sub(gisjoin, 5, 7), str_sub(gisjoin, 9, -1))]
    DF_long[,variable := NULL]
    DF_long[,gisjoin := NULL]
    # make data wide on Census/DP Type
    DF_final <- dcast(DF_long, GEOID + RACE ~ Type, value.var = "value")
    # Brake up GEOID codes into constituent parts
    DF_final[,STATE := str_sub(GEOID, 1, 2)]
    DF_final[,COUNTY := str_sub(GEOID, 3, 5)]
    DF_final[,TRACT := str_sub(GEOID, 6, 11)]
    DF_final[,BLKGRP := str_sub(GEOID, 12, 12)]
    DF_final[,BLOCK := str_sub(GEOID, 13, 15)]
    DF_final[,GEOID := NULL]
    
    cat(str_c("Starting Save for ", st, "\n"))
    fwrite(
        DF_final,
        str_c("./data/prep_data_update/", state_vec[st], "_750.csv"))
}