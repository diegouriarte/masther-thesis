#' ---
#' title: "Precios de mayoristas"
#' author: "Diego Uriarte"
#' date: "19/02/2019"
#' output: github_document
#' ---
#'
#'Intentaré extraer la info que necesito de un pdf
#'

#'Packages to use
#'
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(zoo)
library(lubridate)
library(skimr)

ruta <- "data/no-sync/1_fuel-station-price-data/precios-mensuales-mayoristas-ciudad-mes_mod.XLSX"

data_2006_2009_s1 <- read_excel(ruta,sheet =1, col_names = TRUE,
                                na = c("", "-"),
                                range="B5:V828") %>% 
    clean_names() 

data_2006_2009_s1_mod <- data_2006_2009_s1 %>%
    mutate(ano = na.locf(ano),
           mes = na.locf(mes),
           dia = 01) %>%
    unite(fecha, ano, mes, dia, sep = "-", remove = TRUE) %>%
    mutate(fecha = ymd(fecha)) %>%
    drop_na(fecha)


skim(data_2006_2009_s1_mod)

#' La data está correctamente importada, seleccionamos solo Lima y productos requeridos
#' 

data_lima_2006_2009 <- data_2006_2009_s1_mod %>%
    filter(departamento == "LIMA") %>%
    select(fecha, departamento,starts_with("g"),-g100_ll,
           diesel = d2_ba) 

skim(data_lima_2006_2009)
#' Ahora con la otra hoja de 2010 a 2016
#' 

data_2010_2016_s2 <- read_excel(ruta,sheet =2, col_names = TRUE,
                                na = c("", "-"),
                                range="B5:V1357") %>% 
    clean_names() 

data_2010_2016_s2_mod <- data_2010_2016_s2 %>%
    mutate(ano = na.locf(ano),
           mes = na.locf(mes),
           dia = 01) %>%
    unite(fecha, ano, mes, dia, sep = "-", remove = TRUE) %>%
    mutate(fecha = ymd(fecha)) %>%
    drop_na(fecha)


skim(data_2010_2016_s2_mod)

#' La data está correctamente importada, seleccionamos solo Lima y productos requeridos
#' 

data_lima_2010_2016 <- data_2010_2016_s2_mod %>%
    filter(departamento == "LIMA") %>%
    select(fecha, departamento,starts_with("g"),-g100_ll,
           starts_with("d"), -ends_with("Plus"),
           -starts_with("diesel_b5")) 

skim(data_lima_2010_2016)

data_lima_2010_2016 <- data_lima_2010_2016 %>%
    mutate(diesel = ifelse(is.na(db5_s_50 ), db5_s_50_uv, db5_s_50)) %>%
    select(-db5_s_50_uv,-db5_s_50)

skim(data_lima_2010_2016)
skim(data_lima_2006_2009)

#' Ahora podemos combinar ambos dataframes que tienen las mismas columnas:
#' 
data_mayoristas <- bind_rows(data_lima_2006_2009, data_lima_2010_2016)

skim(data_mayoristas)

data_mayoristas %>% filter(is.na(g98_ba))

#' Vemos que hay algunas observaciones al inicio de 2006 que no tienen precio para 
#' G98 BA.


#' Guardamos el archivo como objeto para importar luego:
#' 
saveRDS(data_mayoristas, file = here::here("data", "processed","precios_mayoristas.rds"))
