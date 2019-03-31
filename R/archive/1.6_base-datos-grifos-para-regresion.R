#' ---
#' title: "Base de datos de grifos limpia"
#' author: "Diego Uriarte"
#' date: Mon Mar 11 18:58:59 2019
#' output: github_document
#' ---
#' 

#' Cargamos las librerías
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
library(lubridate)
library(stringr)
source(file = "R/1.5.1_funcion-auxiliar-distancia.R")

distancia_competencia <- 1.5

grifos_datos_coding <- readRDS(here::here("data","processed","grifo_coding_raw_no_duplicate.rds")) %>%
    mutate(lat = as.double(lat))

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
                                   "PETROPER?" = "PETROPERU",
                                   "REPSOL-GAZEL" = "REPSOL",
                                   "PetroPer?" = "PETROPERU",
                                   .default = "INDEPENDIENTE"))

grifos_datos_coding_nombres %>%   count(bandera)

grifos_datos_coding_nombres %>% count(razon_social, bandera, sort = TRUE) %>%
    filter(str_detect(razon_social, "REPSOL"))

grifos_datos_coding_nombres %>% count(razon_social, bandera, sort = TRUE) %>%
    filter(str_detect(razon_social, "PERUANA"))
#' Agregamos la variable si el grifo es propio, abanderado o independiente
#' adem?s, una variable dicot?mica por si ofrece GNV o GLP
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
    )

grifo_datos_con_tipo %>%
    count(bandera, tipo)

filter(grifo_datos_con_tipo, bandera == "INDEPENDIENTE", tipo == "PROPIA")
glimpse(grifo_datos_con_tipo)


#' Ahora calculamos los distancia al grifo m?s cercano
#' 
#' Filtramos solo aquellos filtros con coordenadas

grifos_con_datos <- grifo_datos_con_tipo %>%
    filter(!is.na(lat))

grifos_con_datos

#' Hallamos la distancia al grifo m?s cercano
#'
matriz_distancias <- grifos_con_datos %>%
    select("name" = codigo_de_osinergmin , lat, lon) %>%
    distinct() %>%
    GeoDistanceInMetresMatrix(.) / 1000

matriz_distancias[1,]

#' Generamos un archivo con las distancias para revisar manualmente errores de
#' codificaci?n
as_tibble(matriz_distancias, rownames = "codigo_de_osinergmin") %>%
    gather(key = "grifo_distancia", value = "distancia", -codigo_de_osinergmin) %>%
    filter(distancia != 0) %>%
    group_by(codigo_de_osinergmin) %>%
    mutate(distancia_min = min(distancia)) %>%
    ungroup() %>%
    arrange(as.numeric(codigo_de_osinergmin)) %>%
    filter(distancia == distancia_min) %>%
    arrange(distancia_min) %>%
    write_excel_csv(path = here::here("data", "processed","grifos-distancia.csv"))

#' Creamos variable con distancia m?nima al grifo competidor m?s cercano
#' Pendiente, como hacer para que no considere si el grifo m?s cercano tiene el mismo ruc
grifos_distancia_minima <- as_tibble(matriz_distancias, rownames = "codigo_de_osinergmin") %>%
    gather(key = "grifo_distancia", value = "distancia", -codigo_de_osinergmin) %>%
    filter(distancia != 0) %>%
    group_by(codigo_de_osinergmin) %>%
    mutate(distancia_min = min(distancia)) %>%
    ungroup() %>%
    arrange(as.numeric(codigo_de_osinergmin)) %>%
    filter(distancia == distancia_min) %>%
    select(-distancia)

#' Distancia promedio a grifos a menos de `distancia_competencia`
#' 
grifos_distancia_promedio <- as_tibble(matriz_distancias, rownames = "codigo_de_osinergmin") %>%
    gather(key = "grifo_distancia", value = "distancia", -codigo_de_osinergmin) %>%
    filter(distancia != 0, distancia < distancia_competencia) %>%
    group_by(codigo_de_osinergmin) %>%
    distinct(grifo_distancia, .keep_all = TRUE) %>%
    mutate(distancia_avg = mean(distancia),
           num_grifos_cerc = n()) %>%
    ungroup() %>%
    arrange(as.numeric(codigo_de_osinergmin)) %>%
    distinct(codigo_de_osinergmin, distancia_avg, num_grifos_cerc) %>%
    arrange(desc(distancia_avg))

#' Lo agregamos al archivo anterior
grifos_distancias <- full_join(grifos_distancia_minima, grifos_distancia_promedio,
                               by = "codigo_de_osinergmin") %>%
    rename("grifo_mas_cercano" = grifo_distancia) %>%
    mutate(codigo_de_osinergmin = as.numeric(codigo_de_osinergmin))

grifos_distancias %>% arrange(desc(num_grifos_cerc))

grifo_full <-
    full_join(grifos_con_datos,
              grifos_distancias,
              by = c("codigo_de_osinergmin"))

saveRDS(grifo_full, file = here::here("data","processed","grifo_coding_clean.rds"))


