#' Estadística descriptiva.
#' 
#' 
library(here)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(lubridate)

prices <- readRDS(here::here("data","processed","data_2005_2018_clean.rds"))

distritos <- c("JESUS MARIA", "SAN ISIDRO", "SAN MIGUEL", "BREÑA")

prices_lima <- prices %>%
    filter(departamento == "LIMA", provincia == "LIMA")

prices_distritos <- prices_lima %>%
    filter(distrito %in% distritos)

year <- 2018

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
    filter(year(fecha_hora) == year, codigo_de_osinergmin == "62240",
           producto == "DIESEL B5 S-50 UV")

prices_raw <- readRDS(here::here("data","processed","data_2005_2018.rds"))

  
prices_raw %>%
    filter(year(fecha_hora) == year, `Código de Osinergmin` == "62240") %>%
    count(Producto)

prices %>% filter(ruc == 20330033313, departamento == "LIMA", provincia == "LIMA") %>%
  distinct(direccion, distrito) %>%
  count(distrito)
max(prices$fecha_hora)
