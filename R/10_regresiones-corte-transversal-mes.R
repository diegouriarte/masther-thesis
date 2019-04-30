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

mes_nombres <- c("01" = "Ene",
                 "02" = "Feb",
                 "03"="Mar",
                 "04"="Abr",
                 "05"="May",
                 "06"="Jun",
                 "07"="Jul",
                 "08"="Ago",
                 "09"="Set",
                 "10"="Oct",
                 "11"="Nov",
                 "12" = "Dic")

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

fechas <- list("01-08-2017", "01-12-2017", "01-03-2018", "01-07-2018")

fechas_formato <- map(fechas, ~str_remove(string = .x, pattern = "01-")) %>% 
  map(., ~str_remove(string = .x, pattern = "20")) %>% 
  flatten_chr() %>% 
  str_replace_all(mes_nombres)

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

#' Controlamos por distrito
#' 
modelo_3 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 + log(num_viajes) + distrito

ols_modelo_3_DB5 <- map(fechas,
                        ~ reg_lineal(data_total, .x, modelo_3, "DIESEL"))
names(ols_modelo_3_DB5) <- fechas

map(ols_modelo_3_DB5, summary)
map(ols_modelo_2_DB5, summary)

#' Resultados
#+tabla-ols, results = 'asis'
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
               "Abanderada Repsol", "Propia Pecsa", "Propia Primax", 
               "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
               "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
               "INGRESO", "DENPOB", "LOGVIAJES")

stargazer(ols_modelo_2_DB5, type = "html",
          covariate.labels = etiquetas_cov,
          dep.var.labels=c("Precio de venta - Diésel (soles/galón)"),
          dep.var.caption = "",
          model.numbers	= F,
          no.space = T,
          column.labels =  fechas_formato, 
          single.row = T, out = here::here("doc", "tables", "ols-model2.htm"))


#' ### G90

#+ g90-modelo1

ols_modelo_1_G90 <- map(fechas,
                    ~ reg_lineal(data_total, .x, modelo_1, "G90"))
names(ols_modelo_1_G90) <- fechas

#' Hacemos otra regresión con otro modelo
#+ g90-modelo2

ols_modelo_2_G90 <- map(fechas,
                    ~ reg_lineal(data_total, .x, modelo_2, "G90"))
names(ols_modelo_2_G90) <- fechas

#' Resultados
#+tabla-ols-g90, results = 'asis'

stargazer(ols_modelo_2_G90$`01-12-2017`, ols_modelo_2_G90$`01-03-2018`, type = "html",
          covariate.labels = etiquetas_cov,
          dep.var.labels=c("Precio de venta - Gasohol 90 (soles/galón)"),
          dep.var.caption = "",
          model.numbers	= F,
          no.space = T,
          column.labels =  fechas_formato[2:3], 
          single.row = T, out = here::here("doc", "tables", "ols-model2-g90.htm"))


#' ## Test de Anselin 1996

#' Escribimos función para calculas pesos espaciales.
#+ func-pesos, message = FALSE

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

test_df %>% 
  filter(test %in% c("RLMerr", "RLMlag"),
         str_detect(fecha, "12|03")) %>% 
  kable(digits = c(0, 0, 3, 5, 3, 5)) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)

res_tabla <- test_df %>% 
  filter(test %in% c("RLMerr", "RLMlag"),
         str_detect(fecha, "12|03"))

res_tabla %>% 
  mutate_at(vars(-fecha, -test), round, digits = 3) %>% 
  mutate_at(vars(starts_with("stati")), format, digits = 2, nsmall = 2, trim = F) %>% 
  mutate_at(vars(starts_with("p.value")), format, digits = 3, nsmall = 2) %>% 
  mutate(DB5 = paste(statistic_DB5, " [", p.value_DB5, "]", sep = ""),
         G90 = paste(statistic_G90, " [", p.value_G90, "]", sep = ""),
         fecha = str_remove(fecha, "01-"),
         test = if_else(test == "RLMerr", 
                        "Test LM Robusto SEM",
                        "Test LM Robusto SAR"),
         fecha = str_replace_all(fecha, mes_nombres)) %>% 
  arrange(desc(fecha)) %>% 
  select("Fecha" = fecha, "Test [valor p]" = test, "Diésel" = DB5, "Gasohol 90" = G90) %>% 
  kable(escape = F) %>% 
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
  select(1, Nula, everything()) %>% 
  filter(fecha %in% c("01-10-2017", "01-03-2018"))


tabla_test_LR %>% 
  mutate(Nula = if_else(Nula == "SAR", 
                        "\u03B8 = 0 (SAR)",
                        "\u03B8 = -\u03c1\u03b2 (SEM)"),
         fecha = str_remove_all(fecha, "01-|20"),
         fecha = str_replace_all(fecha, mes_nombres)) %>% 
  select(-starts_with("df_")) %>% 
  mutate_at(vars(-fecha, -Nula), round, digits = 3) %>% 
  mutate_at(vars(starts_with("stati")), format, digits = 1, nsmall = 1, trim = F) %>% 
  mutate_at(vars(starts_with("p.value")), format, digits = 3, nsmall = 4) %>% 
  mutate(stat_db5 = paste(statistic_DB5, " [", p.value_DB5, "]", sep = ""),
         stat_g90 = paste(statistic_G90, " [", p.value_G90, "]", sep = "")) %>% 
  select(-starts_with("statistic_"), -starts_with("p.value_")) %>% 
  gather(key = "producto", value = "stat", -fecha, -Nula) %>% 
  mutate(producto = if_else(producto == "stat_db5", "Diésel", "Gasohol 90")) %>% 
  rename("Hip. Nula" = Nula) %>% 
  spread(key = fecha, value = stat) %>% 
  arrange(producto) %>% 
  select(1, 4, 3) %>% 
  kable( escape = F)  %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  footnote(general = "N. grados de libertad igual a 19 para todos las pruebas.",
           general_title = "Nota: ") %>% 
  add_header_above(c(" " = 1, "Estadístico [valor p]" = 2)) %>% 
  pack_rows(index = c("Diésel" = 2, "Gasohol 90" = 2))

#' ## Modelo escogido para corte transversal
#' Ahora que hemos determinado que el mejor modelo es el autoregresivo para Diesel
#' y el de Durbin para G90, creamos su tabla

#+ sar-output, results = 'asis'

lista_sar <- sar_DB5[c(2,3)]
rho <- map_dbl(lista_sar, "rho") %>% round(3)
LL <- c(sar_DB5[[2]]$LL, )

lista_sar2 <- list(sar_DB5[[2]], ols_modelo_2_DB5[[2]],
                   sar_DB5[[3]], ols_modelo_2_DB5[[3]])

LL <- c(lista_sar2[[1]]$LL, lista_sar2[[1]]$logLik_lm.model, 
        lista_sar2[[3]]$LL, lista_sar2[[3]]$logLik_lm.model) %>% 
  round(1)

s2 <- c(lista_sar2[[1]]$s2, sigma(lista_sar2[[2]])^2, 
        lista_sar2[[3]]$s2, sigma(lista_sar2[[4]])^2) %>%  
  round(3)

aic_vec <- c(AIC(lista_sar2[[1]]), lista_sar2[[1]]$AIC_lm.model, 
             AIC(lista_sar2[[1]]), lista_sar2[[3]]$AIC_lm.model) %>% 
  round(1)

stargazer(lista_sar2,
          type = "html",
          covariate.labels = etiquetas_cov,
          dep.var.labels=c("Precio de venta - Diésel (soles/galón)"),
          dep.var.caption = "",
          model.numbers	= F,
          no.space = T,
          column.labels =  rep(fechas_formato[2:3], times = c(2,2)), 
          omit.stat	= c("rsq", "adj.rsq", "f", "ll", "sigma2", "res.dev", "ser", "aic"),
          add.lines = list(append("rho", rho), append("Log.Lik", LL),
                           append("<p>&sigma;<sup>2</sub></p>", s2),
                           append("AIC", aic_vec)),
          single.row = T, out = here::here("doc", "tables", "sar_db5.htm"))

#+ durbin-g90, results = 'asis'

summary(durbin_G90$`01-03-2018`)

lista_sar <- durbin_G90[c(2,3)]
rho <- map_dbl(lista_sar, "rho") %>% round(3)
rho_se <- map_dbl(lista_sar, "rho.se") %>% round(3)
rho_char <- str_c(rho, " (", rho_se, ")")
LL <- c(durbin_G90[[2]]$LL, )

lista_sar2 <- list(durbin_G90[[2]], ols_modelo_2_G90[[2]],
                   durbin_G90[[3]], ols_modelo_2_G90[[3]])

LL <- c(lista_sar2[[1]]$LL, lista_sar2[[1]]$logLik_lm.model, 
        lista_sar2[[3]]$LL, lista_sar2[[3]]$logLik_lm.model) %>% 
  round(1)

s2 <- c(lista_sar2[[1]]$s2, sigma(lista_sar2[[2]])^2, 
        lista_sar2[[3]]$s2, sigma(lista_sar2[[4]])^2) %>%  
  round(3)

aic_vec <- c(AIC(lista_sar2[[1]]), lista_sar2[[1]]$AIC_lm.model, 
             AIC(lista_sar2[[3]]), lista_sar2[[3]]$AIC_lm.model) %>% 
  round(1)


stargazer(lista_sar2,
          type = "html",
          covariate.labels = etiquetas_cov,
          dep.var.labels=c("Precio de venta - Gasohol 90 (soles/galón)"),
          dep.var.caption = "",
          model.numbers	= F,
          no.space = T,
          column.labels =  rep(fechas_formato[2:3], times = c(2,2)), 
          omit.stat	= c("rsq", "adj.rsq", "f", "ll", "sigma2", "res.dev", "ser", "aic"),
          add.lines = list(append("<p>&rho;</p>", rho_char), 
                           append("Log.Lik", LL),
                           append("<p>&sigma;<sup>2</sub></p>", s2),
                           append("AIC", aic_vec)),
          omit = c("lag"),
          notes = "Se omiten rezagos espaciales de variables dependientes.",
          notes.label = "Notas: ",
          single.row = T, 
          out = here::here("doc", "tables", "durbin_G90.htm"))

#' Vemos los impactos que tiene el cuarto periodo
#'
#' Impactos para DB5
#' 
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
  tabla_impacto
}

diesel_4_sar_imp <- simp_impactos(sar_DB5, "01-03-2018", prod = "DB5")
g90_4_durbin_imp <- simp_impactos(durbin_G90, "01-03-2018", prod = "G90")

nombres_tabla <- c(
`tipo_banderaABANDERADA PETROPERU` = "Abanderada Petroperu",
`tipo_banderaABANDERADA PECSA` = "Abanderada Pecsa",
`tipo_banderaABANDERADA PRIMAX` = "Abanderada Primax",
`tipo_banderaABANDERADA REPSOL` = "Abanderada Repsol",
`tipo_banderaPROPIA PECSA` = "Propia Pecsa",
`tipo_banderaPROPIA PRIMAX`  = "Propia Primax",
`tipo_banderaPROPIA REPSOL` = "Propia Repsol",
sc = "SC", 
distancia_avg = "DPROM",
distancia_min = "DMIN", 
num_grifos_cerc = "NCER",
tiene_mecanico = "MECANICO",
lavado = "LAVADO",
cajero = "CAJERO",
con_gnv = "GNV",
con_glp = "GLP",
ingresos_2012 = "INGRESO", 
densidad_2017 = "DENPOB",
`log(num_viajes)` = "LOGVIAJES"
)

g90_4_durbin_imp %>% 
  rename("Variable" = 1,) %>% 
  select(-ends_with("SE")) %>% 
  bind_cols(tibble("OLS" =  ols_modelo_2_G90$`01-03-2018`$coefficients[-1],
                   "OLS_pvalue" = summary(ols_modelo_2_G90$`01-03-2018`)$coefficients[,4][-1])) %>%
  mutate_at(vars(-Variable), round, 3) %>% 
  mutate_at(vars(ends_with("pvalue")), ~case_when(
    . > 0.1 ~ '',
    . > 0.05 ~ "<sup>*</sub>",
    . > 0.01 ~ "<sup>**</sub>",
    TRUE ~ "<sup>***</sub>"
  )) %>% 
  mutate(Variable = recode(Variable, !!!nombres_tabla),
         Directo = str_c(directo, " ", Direct1.pvalue),
         Indirecto = str_c(indirecto, " ", Indirect1.pvalue),
         Total = str_c(total, " ", Total1.pvalue),
         OLS = str_c(OLS,  " ", OLS_pvalue)) %>% 
  select(-2:-7, -9, 10:12, 8) %>% 
  rename(" " = OLS) %>% 
  kable(., escape = FALSE, caption = "Comparación para Gasohol 90 (Marzo-18)")  %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  add_header_above(c("", "OLS" = 1, "Efectos - SDM" = 3 ))
#'
#' Ahora para diesel
#' 
diesel_4_sar_imp %>% 
  rename("Variable" = 1,) %>% 
  select(-ends_with("SE")) %>% 
  bind_cols(tibble("OLS" =  ols_modelo_2_DB5$`01-03-2018`$coefficients[-1],
                   "OLS_pvalue" = summary(ols_modelo_2_DB5$`01-03-2018`)$coefficients[,4][-1])) %>%
  mutate_at(vars(-Variable), round, 3) %>% 
  mutate_at(vars(ends_with("pvalue")), ~case_when(
    . > 0.1 ~ '',
    . > 0.05 ~ "<sup>*</sub>",
    . > 0.01 ~ "<sup>**</sub>",
    TRUE ~ "<sup>***</sub>"
  )) %>% 
  mutate(Variable = recode(Variable, !!!nombres_tabla),
         Directo = str_c(directo, " ", Direct1.pvalue),
         Indirecto = str_c(indirecto, " ", Indirect1.pvalue),
         Total = str_c(total, " ", Total1.pvalue),
         OLS = str_c(OLS,  " ", OLS_pvalue)) %>% 
  select(-2:-7, -9, 10:12, 8) %>% 
  rename(" " = OLS) %>% 
  kable(., escape = FALSE, caption = "Comparación para Diésel (Marzo-18)")  %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  add_header_above(c("", "OLS" = 1, "Efectos - SDM" = 3 ))


