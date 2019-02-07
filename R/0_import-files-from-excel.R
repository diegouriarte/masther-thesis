#' ---
#' title: "Importing xlsx files"
#' author: "Diego Uriarte"
#' date: "5/02/2019"
#' output: github_document
#' ---
#'
#'Data files are to heavy, so they are not uploaded to github. I'll import them
#'into R here
#'
#'

#'Packages to use
#'
library(here)
library(tidyverse)
library(readxl)
library(janitor)

data_2005_2017 <- read_excel(here("data","no-sync","1_fuel-station-price-data",
                                  "base-de-datos-precios-combustibles-2005-2017.xlsx"), 
                      sheet ="EVP_02", col_names = TRUE,
                      skip = 3,progress = readxl_progress()) 

data_2017_2018 <- read_excel(here("data","no-sync","1_fuel-station-price-data",
                                  "base-de-datos-precios-combustibles-2017-2018.xlsx"), 
                             sheet ="PRICE", col_names = TRUE,
                             skip = 3) 

saveRDS(data_2005_2017, file = here("data","processed","data_2005_2017.rds"))
saveRDS(data_2017_2018, file = here("data","processed","data_2017_2018.rds"))

