#' ---
#' title: "Mapa de número de estaciones en el tiempo"
#' author: "Diego Uriarte"
#' date: Sat Mar 02 13:04:45 2019
#' output: github_document
#' ---
#' 

#' Primero, dibujamos el mapa de Lima Metropolitana de alguna forma
#' 
library(sf)
library(ggplot2)
library(tidyverse)
library(tmap)
library(tmaptools)
library(lubridate)
library(stringr)

mapa_peru <- st_read("data/data-mapa-peru/PER_adm3.shp", stringsAsFactors = FALSE)

str(mapa_peru)
ggplot(mapa_peru) + 
    geom_sf()

mapa_peru %>% filter(NAME_1 == "Lima Province") %>%
    ggplot(.) + 
    geom_sf()+
    geom_sf_label(aes(label = NAME_3))

mapa_lima <- mapa_peru %>% filter(NAME_1 == "Lima Province")

prices <- readRDS("data/processed/data_2005_2018_clean.rds")

grifos_por_distrito_year <- prices %>% filter(departamento == "LIMA", provincia == "LIMA") %>%
    distinct(codigo_de_osinergmin, distrito, year(fecha_hora)) %>%
    count(distrito, `year(fecha_hora)`)

grifos_por_distrito_year %>% View

unique(grifos_por_distrito_year$distrito)


#' Pasamos a mayusculas distritos
#' 
mapa_lima <- mapa_lima %>%
    mutate(NAME_3 = str_to_upper(NAME_3))

unique(mapa_lima$NAME_3)

############otro archivo#####

mapa_peru_distritos <- st_read("data/Limite_distrital/BAS_LIM_DISTRITOS.shp", stringsAsFactors = FALSE)

mapa_lima_distritos <- mapa_peru_distritos %>% 
    filter(NOMBDEP == "LIMA", NOMBPROV == "LIMA")

str_sort(unique(mapa_lima_distritos$NOMBDIST))

############otro archivo#####

mapa_peru_distritos <- st_read("data/distritos/DISTRITOS.shp", stringsAsFactors = FALSE)

mapa_lima_distritos <- mapa_peru_distritos %>% 
    filter(DEPARTAMEN == "LIMA", PROVINCIA == "LIMA")

str_sort(unique(mapa_lima_distritos$DISTRITO))
