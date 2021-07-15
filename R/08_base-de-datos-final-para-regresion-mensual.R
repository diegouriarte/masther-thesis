#' ---
#' title: Base de datos final para regresion
#' author: "Diego Uriarte"
#' date: "Sun Apr 21 17:48:39 2019"
#' output:
#'   html_notebook:
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' El objetivo de este script es tener la base de datos final para las regresiones.
#' 
#' No debería haber cálculos en scripts posteriores, solo filtros. 
#' 
#' 
#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    error = TRUE
)
options(tidyverse.quiet = TRUE)

#' ## Cargamos librerías
#' 
#+ cargar-librerias, include=FALSE
library(conflicted)
library(tidyverse)
library(lubridate)
conflict_prefer("filter", "dplyr")

#' 
#' ## Cargamos datos de precios y limpiamos
#' 
#+ cargar-datos

# data grifos
grifos_sc <-
    # readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))
    read_rds(here::here("data","processed","grifos_data_final.RDS"))

# cargamos los precios para 2017 de DB5
rutas_fuel <- list(
    here::here("data", "processed", "data_diesel_mensual.rds"),
    here::here("data", "processed", "data_g90_mensual.rds")
)

#' Limpiamos los precios y creamos solo un dataframe
#' 
#' 
#+ limpieza-precios
data_precios <- map_dfr(
    rutas_fuel,
    ~ readRDS(.x) %>%
        filter(
            `año` >= 2017,
            codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin,
        ) %>%
        mutate(dia = 1) %>%
        unite(fecha, dia, mes, `año`, sep = "-", remove = FALSE) %>%
        mutate(fecha = dmy(fecha)) %>% 
        select(-dia) %>%
        filter(
            precio_de_venta > 6,
            fecha <= dmy("2-10-2018")
        )
)

#' ## Data distritos
#' 
#' 
#+ data-distritos
data_distrital_raw <- read_csv(here::here("data", "demo-distrital", "data_pop_lima.csv")) %>%
    janitor::clean_names()

#Limpiamos archivo distrital:
data_distrital_clean <- data_distrital_raw %>%
    rename(
        "pop_2017" = poblacion_total_30_06_2017,
        "densidad_2017" = densidad_poblacional_hab_km2,
        "ingresos_2012" = ingreso_per_capita
    ) %>%
    mutate(
        pop_2017 = str_remove(pop_2017, " ") %>% parse_number(),
        densidad_2017 = str_remove(densidad_2017, " ") %>% parse_number(),
        distrito = str_to_upper(distrito)
    )

#' Importamos el archivo con # viajes x distrito
# numero de viajes en diciembre de 2017 x distrito
viajes_distrito <- readRDS(here::here("data",
                                      "processed",
                                      "data_viajes_distritos.rds"))

head(viajes_distrito)
#' Verificamos que solo los distritos que no son intereses no estén en el merge

anti_join(data_distrital_clean, viajes_distrito, by = "distrito")

#+ join-big-data

data_distrital_clean <- left_join(data_distrital_clean, viajes_distrito, by = "distrito")

saveRDS(data_distrital_clean, here::here("data", "processed", "data_distrital_completa.rds"))

#' ## Merge de data precios con data de distritos y características de grifos
#' 
#+ 

data_total <- data_precios %>% 
    left_join(grifos_sc, by = "codigo_de_osinergmin") %>% 
    left_join(data_distrital_clean, by = "distrito") %>% 
    mutate(
           ingresos_2012 = ingresos_2012 / 1000,
           densidad_2017 = densidad_2017/10000)


#' ## Creamos variables relacionadas con la venta:
#' Variable que toma el valor de 1 luego de que el grifo fue adquirido
#+ variable-comprada
# data_total <- data_total %>%
#     mutate(
#         COMPRADA_FE = case_when(fecha <= dmy("31-01-2018")  ~ 0,
#                              tipo_bandera == "PROPIA PECSA" ~ 1,
#                              TRUE ~ 0),
#         COMPRADA_DID = if_else(tipo_bandera == "PROPIA PECSA", 1, 0),
#         SUMINISTRO = case_when(fecha <= dmy("31-01-2018")  ~ 0,
#                                tipo_bandera == "ABANDERADA PECSA" ~ 1,
#                                TRUE ~ 0),
#         timing_did = if_else(fecha > dmy("31-01-2018"), 1, 0)
#     )



#' Variable relacionada con vecinos utilizando otra definición de vecino
#' 
#' 
#+ vecinos-thiessen-pecsa

# grifos_vecinos <- readRDS(here::here("data", "processed", "grifo_con_vecinos_pre.RDS"))
# 
# vecinos_pecsa_thissen <- grifos_vecinos %>%
#     group_by(codigo_de_osinergmin.princ) %>%
#     mutate(vecino_pecsa_thiessen = if_else(str_detect(razon_social.vec, "PERUANA DE ESTACIONES"),
#                                            1,
#                                            0
#     )) %>%
#     arrange(codigo_de_osinergmin.princ, desc(vecino_pecsa_thiessen)) %>%
#     distinct(codigo_de_osinergmin.princ, .keep_all = TRUE) %>%
#     select("codigo_de_osinergmin" = codigo_de_osinergmin.princ, vecino_pecsa_thiessen)
# 
# vecinos_primax_thissen <- grifos_vecinos %>%
#     group_by(codigo_de_osinergmin.princ) %>%
#     mutate(vecino_primax_thiessen = if_else(str_detect(razon_social.vec, "COESTI"),
#                                            1,
#                                            0
#     )) %>%
#     arrange(codigo_de_osinergmin.princ, desc(vecino_primax_thiessen)) %>%
#     distinct(codigo_de_osinergmin.princ, .keep_all = TRUE) %>%
#     select("codigo_de_osinergmin" = codigo_de_osinergmin.princ, vecino_primax_thiessen)

# solo toma el valor de 1 para las estaciones que tienen como vecino 
# una estación de Pecsa luego de la compra para las variables de la esp. efectos fijos
# y siempre para la esp. diff-in-diff

# data_total <- data_total %>% 
#     left_join(vecinos_pecsa_thissen, by = "codigo_de_osinergmin") %>% 
#     left_join(vecinos_primax_thissen, by = "codigo_de_osinergmin") %>% 
#     mutate(vecino_pecsa_dist_fe = if_else(`año` == 2017, 0, vecino_pecsa_dist),
#            vecino_pecsa_thiessen_fe = if_else(`año` == 2017, 0, vecino_pecsa_thiessen)) %>% 
#     rename("vecino_pecsa_dist_did" = vecino_pecsa_dist, 
#            "vecino_pecsa_thiessen_did" = vecino_pecsa_thiessen)


data_total <- data_total %>%
    select(
        -orden_original,
        -glp,
        -gnv,
        -llanteria,-islas_comb_liq,
        -mecanico,
        -aceite,
    ) %>%
    select(1, 3, 2, everything()) %>% 
    mutate(mes = (year(fecha)-2017)*12 + month(fecha))

#' ### Guardamos
#' 
#' 
#' 
#' Cargamos precios de lista
#' 

precios_lista <- read_rds(here::here("data","processed","precios_lista.rds"))

precio_lista_mensual <- precios_lista %>% 
    mutate(mes = (year(fecha)-2017)*12 + month(fecha)) %>% 
    group_by(mes, producto) %>% 
    summarise(precio_lista = mean(precio))

precio_lista_mensual %>% 
    filter(mes >= 1, producto == "DIESEL")

data_total %>% 
    left_join(precio_lista_mensual, by = c("producto", "mes")) %>% 
    select(codigo_de_osinergmin, fecha, producto, mes, precio_de_venta, precio_lista)

data_total_1 <- data_total %>% 
    left_join(precio_lista_mensual, by = c("producto", "mes"))

#añadimos precios por distritos

# lista de distritos

library(readxl)


#' Hay dos hojas que requieren ser importadas del primer archivo, las demas 
#' contienen data de GN o GLP

precios_distritos <- read_excel(here::here("data","lista_precios_distritos.xlsx"), 
                                    col_names = TRUE,
                                    skip = 0,
                                    progress = readxl_progress()) %>% 
    rename(precio_m2 = precio)


data_total_2 <-  data_total_1 %>% 
    left_join(precios_distritos, by = "distrito")

data_total_2 %>% 
    select(codigo_de_osinergmin, distrito, bandera, tipo, tipo_bandera, vecino_pecsa_thiessen, vecino_primax_thiessen) %>% 
    distinct(codigo_de_osinergmin, .keep_all = T)

saveRDS(data_total_2, file = here::here("data", "processed", "data-final-regresiones.rds"))

write_excel_csv(data_total_2, here::here("data", "processed", "data-final-regresiones.csv"))

