#' ---
#' title: "database para precios en corte transversal"
#' author: "Diego Uriarte"
#' date: Wed Apr 03 20:42:24 2019
#' output: github_document
#' ---
#' 
#' 
#' Preparemos una base de datos con las variables para correr regresión en corte
#' transversal antes y luego de la compra
#' 
#' 
#' 
#' 
# Cargamos librerías ------------------------------------------------------

library(here)
library(tidyverse)


# Cargamos precios y grifos -----------------------------------------------

grifos_sc_pre <- readRDS(here::here("data","processed","grifos_con_sc_pre_venta.RDS"))
grifos_sc_post <- readRDS(here::here("data","processed","grifos_con_sc_post_venta.RDS"))

grifos_sc <- grifos_sc_post %>%
    rename(sc_post = sc)
grifos_sc$sc_pre <- grifos_sc_pre$sc


#' Filtramos data para solo tener info a partir del 2017 y con data codificada de 
#' grifos.
#' 
df_db5 <-
    readRDS(here::here("data", "processed", "data_diesel_mensual.rds")) %>%
    filter(`año` >= 2017,
           codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin)

df_g90 <-
    readRDS(here::here("data", "processed", "data_g90_mensual.rds")) %>%
    filter(`año` >= 2017,
           codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin)

#' Corregimos por algún motivo hay un mes = 13
data_db5_fechas <- df_db5 %>% 
    mutate(dia = 1) %>% 
    unite(fecha, dia, mes, `año`, sep = "-", remove = FALSE) %>%
    select(-dia) %>%
    filter(mes != 13)

# Merge de data de precios y exportación a Stata --------------------------

data_db5 <- grifos_sc %>% 
    left_join(data_db5_fechas, by = "codigo_de_osinergmin") %>% 
    select(-codificador, -ruc, -direccion, -distrito:-orden_original,
           -en_avenida_principal, -revisar)

colnames(data_db5)

# Exportamos a Stata ------------------------------------------------------


foreign::write.dta(
    data_db5 %>% janitor::clean_names(),
    file = here("data", "processed", "data_db5_cross.dta"),
    convert.factors = "string",
    convert.dates = FALSE
)



#' El resto lo corremos en stata