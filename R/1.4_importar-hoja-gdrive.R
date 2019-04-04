#' ---
#' title: "Importar hoja de Google Drive"
#' author: "Diego Uriarte"
#' date: Sat Mar 02 12:04:55 2019
#' output: github_document
#' ---
#' 

library(googlesheets)
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
library(janitor)

#' Importamos todo el archivo con los datos
sheet_grifos_coding <- gs_title("grifos_lima_2006_2017")

#' convertimos la primera hoja en dataframe

grifos_datos_coding <- gs_read(ss=sheet_grifos_coding, ws = "grifos_lima_2006_2017") %>%
    clean_names() %>%
    select(-starts_with("x")) %>%
    separate(coordenadas, into = c("lat", "lon"), sep = ",",convert = TRUE)

glimpse(grifos_datos_coding)

saveRDS(grifos_datos_coding, file = here::here("data","processed","grifo_coding_raw.rds"))
