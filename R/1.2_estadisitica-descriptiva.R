#' Estadística descriptiva.
#' 
#' 
library(here)
library(tidyverse)
library(ggplot2)
library(ggrepel)

prices <- readRDS(here::here("data","processed","data_2005_2018_clean.rds"))

distritos <- c("JESUS MARIA", "SAN ISIDRO", "SAN MIGUEL", "BREÑA")

prices_lima <- prices %>%
    filter(departamento == "LIMA", provincia == "LIMA")

prices_distritos <- prices_lima %>%
    filter(distrito %in% distritos)

year <- 2017

#' determinemos la frecuencia de actualización de precios
#' 
#' 
prices_lima %>%
    count(producto)
prices_lima %>%
    filter(year(fecha_hora) == year, producto == "DIESEL B5 S-50 UV",
           distrito %in% distritos) %>%
    count(codigo_de_osinergmin, direccion, distrito, sort = TRUE) %>% View()

prices %>%
    filter(year(fecha_hora) == year, codigo_de_osinergmin == "8332", 
           producto == "DIESEL B5 S-50 UV") %>% View()

           