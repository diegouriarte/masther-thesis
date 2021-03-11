#' ---
#' title: "Importar hoja de Google Drive"
#' author: "Diego Uriarte"
#' date: Sat Mar 02 12:04:55 2019
#' output: github_document
#' ---
#' 

library(googlesheets4)
library(tidyverse)
library(janitor)

#' Importamos todo el archivo con los datos
url_sheet <- "https://docs.google.com/spreadsheets/d/1YSP4uwGyYWQhWV_WTAmOBdgXJdmTdfATKuOqHVQaUCw/edit?usp=drive_web&ouid=106125482079984867740"
ssid <- as_sheets_id(url_sheet)
class(ssid)
unclass(ssid)
sheet_grifos_coding <- read_sheet(url_sheet, sheet = 2)
ssid
as
#' convertimos la primera hoja en dataframe

grifos_datos_coding <- sheet_grifos_coding %>%
  clean_names() %>%
  separate(coordenadas, into = c("lat", "lon"), sep = ",",convert = TRUE)

glimpse(grifos_datos_coding)

saveRDS(grifos_datos_coding, file = here::here("data","processed","grifo_coding_raw.rds"))

