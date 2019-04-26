#' ---
#' title: "Regresiones para tesis"
#' output: 
#'   html_notebook:
#'     toc: true
#'     toc_float: true
#' ---

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE
)
options(tidyverse.quiet = TRUE)

#' # Cargamos librerías
#' 
#+ body, warning = FALSE, message = FALSE
library(conflicted)
library(tidyverse)
library(spdep)
library(stargazer)
library(lubridate)
library(kableExtra)
library(magrittr)
'%ni%' <- Negate('%in%')
conflict_prefer("filter", "dplyr")

#' ## Cargamos datos
#' 
#+ cargar-datos

data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))

#' ## Regresión OLS por mes
#' 
#' Hacemos una regresión OLS en corte transversal para 4 periodo.
#' 
#' 
#' 
#+ ols-regression-func

reg_lineal <- function(df, fecha_char, modelo, prod) {
  data_mes <- df %>%
    filter(producto == !!prod, 
           fecha == dmy(fecha_char)) %>%
    drop_na()
  lm(modelo, data_mes)
}

#' ### Diesel

#+ diesel-modelo1

modelo_1 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 

fechas <- list("01-07-2017", "01-10-2017", "01-03-2018", "01-07-2018")

ols_modelo_1_DB5 <- map(fechas,
    ~ reg_lineal(data_total, .x, modelo_1, "DIESEL"))
names(ols_modelo_1_DB5) <- fechas

#' Hacemos otra regresión con otro modelo
#+ diesel-modelo2
modelo_2 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 + log(num_viajes)

ols_modelo_2_DB5 <- map(fechas,
                    ~ reg_lineal(data_total, .x, modelo_2, "DIESEL"))
names(ols_modelo_2_DB5) <- fechas

#' Resultados
#+tabla-ols, results = 'asis'
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
               "Abanderada Repsol", "Propia Pecsa", "Propia Primax", 
               "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
               "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
               "INGRESO", "DENPOB")

stargazer(ols_modelo_1_DB5,ols_modelo_2_DB5, type = "html",
          covariate.labels = etiquetas_cov,
          single.row = T)


#' ### G90

#+ g90-modelo1

modelo_1 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 

fechas <- list("01-07-2017", "01-10-2017", "01-03-2018", "01-07-2018")

ols_modelo_1_G90 <- map(fechas,
                    ~ reg_lineal(data_total, .x, modelo_1, "G90"))
names(ols_modelo_1_G90) <- fechas

#' Hacemos otra regresión con otro modelo
#+ g90-modelo2
modelo_2 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 + log(num_viajes)

ols_modelo_2_G90 <- map(fechas,
                    ~ reg_lineal(data_total, .x, modelo_2, "G90"))
names(ols_modelo_2_G90) <- fechas

#' Resultados
#+tabla-ols-g90, results = 'asis'
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
                  "Abanderada Repsol", "Propia Pecsa", "Propia Primax", 
                  "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
                  "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
                  "INGRESO", "DENPOB")

stargazer(ols_modelo_1_G90,ols_modelo_2_G90, type = "html",
          covariate.labels = etiquetas_cov,
          single.row = T)


#' ## Test de Anselin

#' Escribimos función para calculas pesos espaciales.
#+ func-pesos, results = 'asis', message = FALSE

calcular_weigth_matrix <- function(df, fecha_char, prod) {
  data_mes <- df %>%
    filter(producto == !!prod,
           fecha == dmy(fecha_char)) %>%
    drop_na() %>%
    select(codigo_de_osinergmin, lon, lat) %>%
    as.matrix

  grifos_nb <- tri2nb(data_mes[,2:3], row.names = data_mes[,1])
  nb2listw(grifos_nb, zero.policy = T)
}

#' ## calculamos matriz grifos
#' 
sp_grifos_DB5 <- map(fechas, ~ calcular_weigth_matrix(data_total, .x, "DIESEL"))
sp_grifos_G90 <- map(fechas, ~ calcular_weigth_matrix(data_total, .x, "G90"))
names(sp_grifos_DB5) <- fechas
names(sp_grifos_G90) <- fechas
sp_grifos <- list("DB5" = sp_grifos_DB5, "G90" = sp_grifos_G90)

#Diesel
LMtest_DB5 <- map2(ols_modelo_2_DB5, sp_grifos_DB5, ~ lm.LMtests(.x, .y, test = "all"))
names(LMtest_DB5) <- fechas
#G90
LMtest_G90 <- map2(ols_modelo_2_G90, sp_grifos_G90, ~ lm.LMtests(.x, .y, test = "all"))
names(LMtest_G90) <- fechas

resumen <- function(lista_test, test ) {
  map_dfr(lista_test,c(test, "statistic")) %>%
    gather("fecha", "statistic") %>%
    mutate(test = test) %>%
    left_join(map_dfr(lista_test,c(test, "p.value")) %>%
                gather("fecha", "p.value"),
              by = "fecha") %>%
    select(1,3,2,4)
}

nombres_test <- list("LMerr", "LMlag", "RLMerr", "RLMlag")

test_df_DB5 <- map_dfr(nombres_test, ~resumen(LMtest_DB5, .x))
test_df_G90 <- map_dfr(nombres_test, ~resumen(LMtest_G90, .x))

test_df <- inner_join(test_df_DB5, test_df_G90, 
           by = c("fecha", "test"), 
           suffix = c("_DB5", "_G90"))

#' 
kable(test_df, digits = c(0, 0, 3, 5, 3, 5))  %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

#' 
#' El test robusto nos dice que existe dependencia espacial, pero solo significativa
#' en el caso de considerar el modelo SAR. Siguiendo la literatura,
#' estimaremos el modelo completo de Durbin
#'
#' ## Modelo Durbin
#' 
#+ durbin, results = 'asis'

reg_durbin <- function(df, fecha_char, sp_grifos, durbin = T, prod) {
  modelo_2 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
    num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
    ingresos_2012 + densidad_2017 + log(num_viajes)

  data_mes <- df %>%
    filter(producto == !!prod, 
           fecha == dmy(fecha_char)) %>%
    drop_na()

  lagsarlm(formula = modelo_2,
           data = data_mes,
           listw = sp_grifos,
           Durbin = durbin,
           tol.solve = 1e-13)
  }


durbin_DB5 <- map2(fechas, sp_grifos_DB5, ~reg_durbin(data_total, .x, .y, prod = "DIESEL"))
durbin_G90 <- map2(fechas, sp_grifos_G90, ~reg_durbin(data_total, .x, .y, prod = "G90"))

names(durbin_DB5) <- fechas
names(durbin_G90) <- fechas

#' ### Corremos el modelo autoregresivo espacial:

sar_DB5 <- map2(fechas, sp_grifos_DB5, ~reg_durbin(data_total, .x, .y, 
                                                  prod = "DIESEL", durbin = F))
sar_G90 <- map2(fechas, sp_grifos_G90, ~reg_durbin(data_total, .x, .y, 
                                               prod = "G90", durbin = F))

names(sar_DB5) <- fechas
names(sar_G90) <- fechas

#' ### Corremos el modelo de errores espaciales

#+ ser

reg_errores <- function(df, fecha_char, sp_grifos, prod) {

  modelo_2 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
    num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
    ingresos_2012 + densidad_2017 + log(num_viajes)

  data_mes <- df %>%
    filter(producto == !!prod,
           fecha == dmy(fecha_char)) %>%
    drop_na()

  errorsarlm(modelo_2,
           data = data_mes,
           listw = sp_grifos,
           tol.solve = 1e-13
  )
}
SEM_DB5 <- map2(fechas, sp_grifos_DB5, ~ reg_errores(data_total, .x, .y, prod = "DIESEL"))
SEM_G90 <- map2(fechas, sp_grifos_G90, ~ reg_errores(data_total, .x, .y, prod = "G90"))

#' # Realizamos los test para distinguir entre ambos

#+ test-LR, message = FALSE, warning = FALSE

test_SAR_DB5 <- map2(durbin_DB5, sar_DB5, ~ LR.sarlm(.x, .y))
test_SEM_DB5 <- map2(durbin_DB5, SEM_DB5, ~ LR.sarlm(.x, .y))

test_SAR_G90 <- map2(durbin_G90, sar_G90, ~ LR.sarlm(.x, .y))
test_SEM_G90 <- map2(durbin_G90, SEM_G90, ~ LR.sarlm(.x, .y))

extraer_test_LR <- function(lista_con_test) {
  map_df(lista_con_test, magrittr::extract, c("statistic", "p.value", "parameter")) %>%
    bind_cols(as.data.frame(names(lista_con_test))) %>%
    select("fecha" = "names(lista_con_test)", statistic, "df" = parameter, p.value)
}

tabla_test_LR_DB5 <- extraer_test_LR(test_SEM_DB5) %>%
  mutate(Nula = "SEM") %>%
  bind_rows(extraer_test_LR(test_SAR_DB5) %>%
                      mutate(Nula = "SAR"))

tabla_test_LR_G90 <- extraer_test_LR(test_SEM_G90) %>%
  mutate(Nula = "SEM") %>%
  bind_rows(extraer_test_LR(test_SAR_G90) %>%
              mutate(Nula = "SAR"))

tabla_test_LR <- inner_join(tabla_test_LR_DB5, tabla_test_LR_G90, 
                      by = c("fecha","Nula"), 
                      suffix = c("_DB5", "_G90")) %>% 
  select(1, Nula, everything())
kable(tabla_test_LR, digits = c(0, 0, 1, 0, 5, 1, 0, 5))  %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


#' ## Modelo escogido para corte transversal
#' Ahora que hemos determinado que el mejor modelo es el autoregresivo para Diesel
#' y el de Durbin para G90, creamos su tabla

#+ sar-output, results = 'asis'
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
                  "Abanderada Repsol", "Propia Pecsa", "Propia Primax",
                  "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
                  "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
                  "INGRESO", "DENPOB")

stargazer(sar_DB5, type = "html")

summary(durbin_G90$`01-07-2017`)

#' Vemos los impactos que tiene el cuarto periodo
#'
#' Impactos para DB5
#+ impactos-4
im_4 <- impacts(sar_DB5[[4]], listw = sp_grifos_DB5[[4]], R = 100, useHESS = F)

per4 <- summary(im_4, zstats=TRUE, short = TRUE)

#' Impactor para G90

im_G90 <- impacts(sar_G90[[4]], listw = sp_grifos_G90[[4]], R = 100, useHESS = F)

perG90 <- summary(im_G90, zstats=TRUE, short = TRUE)

#' Convertimos a tablas para imprimir

simp_impactos <- function(spa_reg_list, fecha, prod) {
  impacto <- impacts(spa_reg_list[[fecha]], listw = sp_grifos[[prod]][[fecha]], R = 100, useHESS = F)
  intervalos <- summary(impacto, zstats=TRUE, short = TRUE)
  t <- tibble(attr(impacto, "bnames"),
              "directo" = impacto$res$direct,
              "indirecto" = impacto$res$indirect,
              "total" = impacto$res$total) %>%
    bind_cols(intervalos$semat %>% as.data.frame()) %>%
    bind_cols(intervalos$pzmat %>% as.data.frame())
  
  cols_SE = c("Indirect", "Direct", "Total")
  to_app_SE = ".SE"
  cols_pvalue = c("Indirect1", "Direct1", "Total1")
  to_app_pvalue = ".pvalue"
  tabla_impacto <- rename_at(t, cols_SE, funs( paste0(., to_app_SE) ) ) %>%
    rename_at(cols_pvalue, funs( paste0(., to_app_pvalue) ) )
  tabla_4_impactos
}

diesel_4_sar_imp <- simp_impactos(sar_DB5, "01-03-2018", prod = "DB5")
g90_4_durbin_imp <- simp_impactos(durbin_G90, "01-03-2018", prod = "G90")

kable(g90_4_durbin_imp, digits = 4)  %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
#'
#'


