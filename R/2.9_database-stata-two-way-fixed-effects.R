#' ---
#' title: "Variación del precio en el tiempo explicada por efectos fijos"
#' author: "Diego Uriarte"
#' date: Wed Apr 03 18:15:02 2019
#' output: github_document
#' ---
#' 
#' Calculmos en modelo de efectos fijos por estación y en tiempo.
#' 
#' 

# Cargamos librerías ------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(plm)
library(stringr)


# Cargamos data -----------------------------------------------------------


grifos_sc_pre <- readRDS(here::here("data","processed","grifos_con_sc_pre_venta.RDS"))
grifos_sc_post <- readRDS(here::here("data","processed","grifos_con_sc_post_venta.RDS"))

grifos_sc <- grifos_sc_post %>%
    rename(sc_post = sc)
grifos_sc$sc_pre <- grifos_sc_pre$sc



df_db5 <-
    readRDS(here::here("data", "processed", "data_diesel_mensual.rds")) %>%
    filter(`año` >= 2017,
           codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin)

df_g90 <-
    readRDS(here::here("data", "processed", "data_g90_mensual.rds")) %>%
    filter(`año` >= 2017,
           codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin)

# Adecuamos la data -------------------------------------------------------

#' Incluimos una columna con mes y año
#' 
data_db5_fechas <- df_db5 %>% 
    mutate(dia = 1) %>% 
    unite(fecha, dia, mes, `año`, sep = "-", remove = FALSE) %>%
    select(-dia) %>%
    filter(mes != 13)

skimr::skim(data_db5_fechas)
# Exportamos a Stata ------------------------------------------------------

library(foreign)

write.dta(
    data_db5_fechas %>% janitor::clean_names(),
    file = here("data", "processed", "data_diesel_mensual.dta"),
    convert.factors = "string",
    convert.dates = FALSE
)

# El resto lo corremos en stata
