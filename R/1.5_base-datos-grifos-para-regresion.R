#' ---
#' title: "Base de datos de grifos limpia"
#' author: "Diego Uriarte"
#' date: Mon Mar 11 18:58:59 2019
#' output: github_document
#' ---
#' 
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
library(lubridate)
library(stringr)

grifos_datos_coding <- readRDS(here::here("data","processed","grifo_coding_raw.rds"))

glimpse(grifos_datos_coding)

grifos_datos_coding %>%
    count(bandera)


grifos_datos_coding_nombres <- grifos_datos_coding %>%
    mutate(bandera = str_to_upper(bandera)) %>%
    mutate(bandera = recode_factor(bandera, 
                                   "PECSA" = "PECSA",
                                   "PRIMAX" = "PRIMAX",
                                   "REPSOL" = "REPSOL",
                                   "PETROPERU" = "PETROPERU",
                                   "PETROPERÚ" = "PETROPERU",
                                   "REPSOL-GAZEL" = "REPSOL",
                                   "PetroPerú" = "",
                                   .default = "INDEPENDIENTE"))

grifos_datos_coding_nombres %>%   count(bandera)

grifos_datos_coding_nombres %>% count(razon_social, bandera, sort = TRUE) %>%
    filter(str_detect(razon_social, "REPSOL"))

grifos_datos_coding_nombres %>% count(razon_social, bandera, sort = TRUE) %>%
    filter(str_detect(razon_social, "PERUANA"))
#' Agregamos la variable si el grifo es propio, abanderado o independiente
#' además, una variable dicotómica por si ofrece GNV o GLP
#' 
grifo_datos_con_tipo <- grifos_datos_coding_nombres %>% 
    mutate(
        tipo = case_when(
        str_detect(razon_social, "REPSOL") ~ "PROPIA",
        str_detect(razon_social, "COESTI") ~ "PROPIA",
        str_detect(razon_social, "PERUANA DE ESTACIONES") ~ "PROPIA",
        bandera == "PETROPERU" ~ "ABANDERADA",
        bandera == "INDEPENDIENTE" ~ "INDEPENDIENTE",
        TRUE ~ "ABANDERADA"
        ),
        con_glp = if_else(!is.na(glp), 1, 0),
        con_gnv = if_else(!is.na(gnv), 1, 0),
        ruc = as.character(ruc)
    ) %>%
    separate(coordenadas, into = c("lat", "lon"), sep = ",",convert = TRUE)

grifo_datos_con_tipo %>%
    count(bandera, tipo)

glimpse(grifo_datos_con_tipo)

#' Ahora calculamos los distancia al grifo más cercano
#' 
#' Filtramos solo aquellos filtros con coordenadas

grifos_con_datos <- grifo_datos_con_tipo %>%
    filter(!is.na(lat))

grifos_con_datos

#' Hallamos la distancia al grifo más cercano
#'
matriz_distancias <- grifos_con_datos %>%
    select("name" = codigo_de_osinergmin , lat, lon) %>%
    distinct() %>%
    GeoDistanceInMetresMatrix(.) / 1000

matriz_distancias[1,]

as_tibble(matriz_distancias, rownames = "codigo_de_osinergmin") %>%
    gather(key = "grifo_distancia", value = "distancia", -codigo_de_osinergmin)%>%
    filter(distancia != 0) %>%
    group_by(codigo_de_osinergmin) %>%
    mutate(distancia_min = min(distancia)) %>%
    ungroup() %>%
    arrange(as.numeric(codigo_de_osinergmin)) %>%
    filter(distancia == distancia_min) %>%
    arrange(distancia_min) %>%
    write_excel_csv(path = here::here("data", "processed","grifos-distancia.csv"))
    
