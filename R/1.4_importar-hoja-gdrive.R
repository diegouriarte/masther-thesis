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
(my_sheets <- gs_ls())

#' Importamos todo el archivo con los datos
sheet_grifos_coding <- gs_title("grifos_lima_2006_2017")

#' convertimos la primera hoja en dataframe

grifos_datos_coding <- gs_read(ss=sheet_grifos_coding, ws = "grifos_lima_2006_2017") %>%
    clean_names() %>%
    select(-starts_with("x"))

glimpse(grifos_datos_coding)

saveRDS(grifos_datos_coding, file = here::here("data","processed","grifo_coding_raw.rds"))

############# SOLO CORRER UNA VEZ######################

#' Hacemos el cruce solo con aquellos abiertos a partir del 2015

# prices <- readRDS(here::here("data","processed","data_2005_2018_clean.rds"))
# 
# 
# grifos_abiertos <- prices %>% 
#     filter(departamento == "LIMA", provincia == "LIMA",
#            year(fecha_hora) >= 2015) %>%
#     group_by(codigo_de_osinergmin, direccion) %>%
#     mutate(ultimo = max(year(fecha_hora))) %>%
#     count(codigo_de_osinergmin, razon_social, ruc, direccion, distrito, ultimo) %>%
#     filter(ultimo >= 2017)
# 
# grifos_datos_coding %>% 
#     mutate(codigo_de_osinergmin = as.character(codigo_de_osinergmin)) %>%
#     semi_join(grifos_abiertos, by = c("codigo_de_osinergmin",
#                                     "direccion") ) %>%
#     write_excel_csv(path = here::here("data","processed","grifos_gsheet_consolidad.csv"),
#               na = "")
# 
# 
# grifos_datos_coding %>% 
#     mutate(codigo_de_osinergmin = as.character(codigo_de_osinergmin)) %>%
#     semi_join(grifos_abiertos, by = c("codigo_de_osinergmin",
#                                       "direccion") ) %>%
#     count(distrito) %>%
#     write_excel_csv(path = here::here("data","processed","grifos_gsheet_consolidado_distrito.csv"),
#                     na = "")
