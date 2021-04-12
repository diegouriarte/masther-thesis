#' ---
#' title: Base de datos final para regresion SEMANAL
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
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS")) %>% 
    mutate(codigo_de_osinergmin = as.character(codigo_de_osinergmin))

# cargamos los precios para 2017 de DB5
rutas_fuel <- list(
    here::here("data", "processed", "data_diesel_semanal.rds"),
    here::here("data", "processed", "data_g90_semanal.rds")
)

#' Limpiamos los precios y creamos solo un dataframe
#' 
#' 

db5 <- readRDS(here::here("data", "processed", "data_diesel_semanal.rds"))

#+ limpieza-precios
data_precios <- map_dfr(
    rutas_fuel,
    ~ readRDS(.x) %>%
        filter(
            `año` >= 2017,
            codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin,
        ) %>%
        filter(
            precio_de_venta > 6,
            `año`== 2017 |(`año` == 2018 & semana <= 43) #luego ya se repiten los datos %>% 
        ) %>% 
        select(-precio_de_venta_1, -razon_social, -distrito)
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
    mutate(sc = if_else(`año` == 2017, sc_pre, sc_post),
           ingresos_2012 = ingresos_2012 / 1000,
           densidad_2017 = densidad_2017/10000)


#' ## Creamos variables relacionadas con la venta:
#' Variable que toma el valor de 1 luego de que el grifo fue adquirido
#+ variable-comprada
data_total <- data_total %>%
    mutate(
        COMPRADA_DID_PECSA = if_else(tipo_bandera == "PROPIA PECSA", 1, 0),
        COMPRADA_DID_PRIMAX = if_else(tipo_bandera == "PROPIA PRIMAX", 1, 0),
        timing_did = if_else(año == 2018 & semana >= 5, 1, 0)
    )



#' Variable relacionada con vecinos utilizando otra definición de vecino
#' 
#' 
#+ vecinos-thiessen
grifos_vecinos <- readRDS(here::here("data", "processed", "grifo_con_vecinos_pre.RDS"))

vecinos_pecsa_thissen <- grifos_vecinos %>%
  group_by(codigo_de_osinergmin.princ) %>%
  mutate(
    vecino_pecsa_thiessen = if_else(str_detect(razon_social.vec, "PERUANA DE ESTACIONES"),
      1,
      0
    ),
    codigo_de_osinergmin.princ = as.character(codigo_de_osinergmin.princ)
  ) %>%
  arrange(codigo_de_osinergmin.princ, desc(vecino_pecsa_thiessen)) %>%
  distinct(codigo_de_osinergmin.princ, .keep_all = TRUE) %>%
  select("codigo_de_osinergmin" = codigo_de_osinergmin.princ, vecino_pecsa_thiessen)

# solo toma el valor de 1 para las estaciones que tienen como vecino 
# una estación de Pecsa luego de la compra para las variables de la esp. efectos fijos
# y siempre para la esp. diff-in-diff

data_total <- data_total %>% 
    left_join(vecinos_pecsa_thissen, by = "codigo_de_osinergmin") %>% 
    rename("vecino_pecsa_dist_did" = vecino_pecsa_dist, 
           "vecino_pecsa_thiessen_did" = vecino_pecsa_thiessen)


data_total <- data_total %>%
    select(
        -orden_original,
        -glp,
        -gnv,
        -llanteria,-islas_comb_liq,
        -mecanico,
        -aceite,
        -sc_pre,
        -sc_post,
    )
head(data_total)
#' ### Guardamos
#' 
saveRDS(data_total, file = here::here("data", "processed", "data-final-regresiones_semanal.rds"))

#write_excel_csv(data_total, here::here("data", "processed", "data-final-regresiones_semanal.csv"))

