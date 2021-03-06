#' ---
#' title: Número de viajes por distrito
#' author: "Diego Uriarte"
#' date: "`r paste(date())`"
#' output: github_document
#' ---
#' 
#' Importaremos los archivos bajados de la aplicación del AATE extraeremos el
#' número de visitantes a cada distrito

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    error = TRUE,
    warning = FALSE,
    message = FALSE
)
options(tidyverse.quiet = TRUE)

#+ body
# ----
library(conflicted)
library(tidyverse)
conflict_prefer("select", "dplyr")

#+ cargamos-archivos
lista_archivos <-
    list.files(here::here("data", "demo-distrital", "big-data-distritos"),
               full.names = TRUE)

big_data_aate <- map_dfr(lista_archivos, read_csv)




#' ## Limpiamos data distritos
#' 
#+ distritos

# numero de viajes en diciembre de 2017 x distrito
# numero de vaijes x millon de habitantes
viajes_distrito <- big_data_aate %>% 
    select("distrito" = nom_dist_d, tipo_dia, horario, motivo, edad, viajes) %>% 
    group_by(distrito) %>% 
    summarise(num_viajes = sum(viajes)) %>% 
    mutate(distrito = replace(distrito, distrito == "BRENA", "BREÑA"),
           distrito = replace(distrito, distrito == "ATE VITARTE", "ATE"),
           num_viajes_millon = num_viajes/1000000)

head(viajes_distrito)

saveRDS(
    viajes_distrito,
    here::here(
        "data",
        "processed",
        "data_viajes_distritos.rds"
    )
)
