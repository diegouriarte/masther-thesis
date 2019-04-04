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

grifos_sc <-
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))

grifos_sc <- grifos_sc %>% 
    mutate(tipo = factor(tipo, c("INDEPENDIENTE","ABANDERADA","PROPIA")),
           tipo_simp = if_else(tipo %in% c("ABANDERADA", "INDEPENDIENTE"), 
                           "NO PROPIA",
                           "PROPIA"),
           tipo_simp = factor(tipo_simp, level = c("NO PROPIA", "PROPIA"))) 



#' Filtramos data para solo tener info a partir del 2017 y con data codificada de 
#' grifos.

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


# Exportamos a Stata ------------------------------------------------------


foreign::write.dta(
    data_db5 %>% janitor::clean_names(),
    file = here("data", "processed", "data_db5_cross.dta"),
    convert.factors = "string",
    convert.dates = FALSE
)



#' El resto lo corremos en stata
#' 
#' 

# Corremos regresiones en R (corte transversal) ---------------------------

data_1_periodo <- data_db5 %>%
    filter(fecha == "1-1-2017")

data_1_periodo %>% 
    count(bandera, tipo_simp)

model1 <- lm(precio_de_venta ~ sc_pre + bandera, data_1_periodo)

summary(model1)

model2 <- lm(precio_de_venta ~ sc_pre +  tipo_simp + bandera, data_1_periodo)

summary(model2)


model_3 <- data_1_periodo %>%
    select(
        precio_de_venta,
        bandera,
        tipo_simp,
        sc_pre,
        distancia_avg,
        distancia_min,
        num_grifos_cerc,
        llanteria,
        mecanico,
        lavado,
        cajero
    ) %>%
    lm(.)

summary(model_3)
