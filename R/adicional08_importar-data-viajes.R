#' ---
#' title: Importaremos 
#' author: "Diego Uriarte"
#' date: Thu Apr 18 19:32:24 2019
#' output: github_document
#' ---
#' 
#' Importaremos los archivos bajados de la aplicación del AATE extraeremos el
#' número de visitantes a cada distrito

#' Cargamos las librerías
#'
 
#+ cargar-librerias, include=FALSE
library(tidyverse)


lista_archivos <-
    list.files(here::here("data", "processed", "data_viaje_distritos.rds"),
               full.names = TRUE)

big_data_aate <- map_dfr(lista_archivos, read_csv)

saveRDS(
    big_data_aate,
    here::here(
        "data",
        "demo-distrital",
        "big-data-distritos",
        "data_viajes_distritos.rds"
    )
)
