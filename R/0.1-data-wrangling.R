#' ---
#' title: "Data wrangling"
#' author: "Diego Uriarte"
#' date: "5/02/2019"
#' output: github_document
#' ---
#'
#'Now, we assign correct format to data and correct obvious mistakes
#'
#'

#'Packages to use
#'
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)


data_2005_2017 <- readRDS(file = here("data","processed","data_2005_2017.rds"))
data_2017_2018 <- readRDS(file = here("data","processed","data_2017_2018.rds"))


data_2005_2017 <- data_2005_2017 %>% clean_names()
str(data_2005_2017)

skim(data_2005_2017)
#' We found to missing values por año, let's see which they are:

data_2005_2017 %>% filter(is.na(ano))
#' We can drop those observation:
#' 
data_2005_2017 <- data_2005_2017 %>% drop_na(ano)

skim(data_2005_2017)

#' Now, ruc still has two missing values:
#' 
data_2005_2017 %>% filter(is.na(ruc)) 
#' We see that is a fuel station without ruc than only appears twice in the dataset.
#' We will drop these fuel station:
#' 
data_2005_2017 <- data_2005_2017 %>% drop_na(ruc)

skim(data_2005_2017)

#' Now, unidades has a lot of missing values:
#' 
data_2005_2017 %>% filter(is.na(unidades)) %>%
    select(codigo_osinerg, empresa, unidades)


