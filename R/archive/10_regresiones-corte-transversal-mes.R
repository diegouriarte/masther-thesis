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
source(here::here("R","funcion04_formato-tablas-reproducibles.R"), encoding = "UTF-8")

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

fechas <- list("01-08-2017", "01-12-2017", "01-03-2018", "01-07-2018")


modelo_1 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 


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

stargazer(ols_modelo_2_DB5[c(2,3)], type = "html",
          covariate.labels = etiquetas_cov,
          dep.var.labels=c("Precio de venta - Diésel (soles/galón)"),
          dep.var.caption = "",
          model.numbers	= F,
          no.space = T,
          column.labels =  fechas_formato[c(2,3)], 
          single.row = T, out = here::here("doc", "tables", "ols-model2-db5.htm"))



#' ### G90

#' Hacemos otra regresión con otro modelo
#+ g90-modelo2

ols_modelo_2_G90 <- map(fechas,
                    ~ reg_lineal(data_total, .x, modelo_2, "G90"))
names(ols_modelo_2_G90) <- fechas

#' Resultados
#+tabla-ols-g90, results = 'asis'

stargazer(ols_modelo_2_G90[c(2,3)], type = "html",
          covariate.labels = etiquetas_cov,
          dep.var.labels=c("Precio de venta - Gasohol 90 (soles/galón)"),
          dep.var.caption = "",
          model.numbers	= F,
          no.space = T,
          omit.stat = c("rsq"),
          column.labels =  fechas_formato[2:3],
          notes.label = "Nota: ",
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

listar_anselin <- function(df_test) {
  tabla_word <- df_test %>% 
    mutate(statistic = round(statistic, 2) %>%  format(nsmall = 2) ,
           p.value = round(p.value, 4) %>% format(nsmall = 4),
           stat = str_c(statistic, " [", p.value, "]") %>%  format(justify = "right")) %>% 
    select(fecha, test, stat) %>% 
    spread(key = fecha, value = stat) %>% 
    bind_cols(tibble("Nombre" = c("Test LM SEM", "Test LM SAR", 
                                  "Test LM Robusto SEM", "Test LM Robusto SAR"))) %>% 
    select(6, everything(), -1) 
  print(tabla_word)
  map(list(tabla_word[1,], tabla_word[2,], tabla_word[3,], tabla_word[4,]),
                      as.vector, mode = "character") %>% 
    map(format, width = 20, justify = "right")
}

test_anselin_db5 <- listar_anselin(test_df_DB5)
#' Añadimos a tabla para word
#' 
stargazer(ols_modelo_2_DB5, type = "html",
          covariate.labels = etiquetas_cov,
          add.lines = test_anselin_db5,
          dep.var.labels=c("Precio de venta - Diésel (soles/galón)"),
          align = T,
          dep.var.caption = "",
          model.numbers	= F,
          omit = "Constant",
          omit.stat = c("rsq", "f", "sigma2", "ser"),
          no.space = T,
          notes.label = "Nota: ",
          column.labels =  fechas_formato, 
          single.row = T, 
          out = here::here("doc", "tables", "ols-model2-db5_word.htm"))



test_df_G90 <- map_dfr(nombres_test, ~resumen(LMtest_G90, .x))
test_anselin_g90 <- listar_anselin(test_df_G90)

stargazer(ols_modelo_2_G90, type = "html",
          covariate.labels = etiquetas_cov,
          add.lines = test_anselin_g90,
          align = T, 
          dep.var.labels=c("Precio de venta - Gasohol 90 (soles/galón)"),
          dep.var.caption = "",
          model.numbers	= F,
          no.space = T,
          omit = "Constant",
          omit.stat = c("rsq", "f", "sigma2", "ser"),
          column.labels =  fechas_formato,
          notes.label = "Nota: ",
          single.row = T, out = here::here("doc", "tables", "ols-model2-g90-word.htm"))

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
         fecha = str_remove_all(fecha, "01-|20"),
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

#+ sem

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

stargazer(SEM_DB5, type = "html", out = here::here("doc", "tables","sem_db5.htm"))
#' # Realizamos los test para distinguir entre ambos

#+ test-LR, message = FALSE, warning = FALSE

test_SAR_DB5 <- map2(durbin_DB5, sar_DB5, ~ LR.sarlm(.x, .y))
test_SEM_DB5 <- map2(durbin_DB5, SEM_DB5, ~ LR.sarlm(.x, .y))

test_SAR_G90 <- map2(durbin_G90, sar_G90, ~ LR.sarlm(.x, .y))
test_SEM_G90 <- map2(durbin_G90, SEM_G90, ~ LR.sarlm(.x, .y))


#' Consolidamos en un solo dataframe
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
  filter(fecha %in% c("01-12-2017", "01-03-2018"))

#' Imprimimos el dataframe

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
  rename("Hipótesis Nula" = Nula) %>% 
  spread(key = fecha, value = stat) %>% 
  arrange(producto) %>% 
  select(-producto) %>% 
  kable( escape = F)  %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  footnote(general = "N. grados de libertad igual a 19 para todos las pruebas.",
           general_title = "Nota: ") %>% 
  add_header_above(c(" " = 1, "Estadístico [valor p]" = 2)) %>% 
  pack_rows(index = c("Diésel" = 2, "Gasohol 90" = 2))

#' ## Modelos escogidos para corte transversal
#' Ahora que hemos determinado que el mejor modelo es el autoregresivo para Diesel
#' y el de Durbin para G90, creamos su tabla

#+ sar-output, results = 'asis'

imprimir_modelo(sar_DB5, ols_modelo_2_DB5, prod = "Diésel", out = "sar_db5.htm", durbin = F)
imprimir_modelo(durbin_DB5, ols_modelo_2_DB5, prod = "Diésel", out = "durbin_db5.htm", durbin = T)

#+ durbin-g90, results = 'asis'

imprimir_modelo(durbin_G90, ols_modelo_2_G90, prod = "Gasohol 90", out = "durbin_g90.htm", durbin = T)


#' Imprimimos para un mes la comparativa lado a lado de G90 y DB5
#' 
lista_spa <- list(sar_DB5[[3]], durbin_G90[[3]])
rho <- map_dbl(lista_spa, "rho") %>% round(3)
se_rho <- map_dbl(lista_spa, "rho.se") %>% round(3)
pvalue_rho <- map_dbl(lista_spa, ~c(summary(.x)$LR1$p.value)) %>% round(4)

pvalue_rho_ch <- case_when(
  pvalue_rho > 0.1 ~ '',
  pvalue_rho > 0.05 ~ "<sup>*</sub>",
  pvalue_rho > 0.01 ~ "<sup>**</sub>",
  TRUE ~ "<sup>***</sub>"
) 

rho_with_se <- str_c("<span>", rho, pvalue_rho_ch, "</span>", " (", se_rho, ")")

LL <- map_dbl(lista_spa, "LL") %>% 
  round(1)

s2 <- map_dbl(lista_spa, "s2")%>%  
  round(3)


stargazer(lista_spa,
          type = "html",
          covariate.labels = etiquetas_cov,
          dep.var.labels="Precio de venta (soles/galón)",
          dep.var.caption = "",
          model.numbers	= F,
          model.names = F,
          no.space = T,
          column.labels =  c("Diésel", "Gasohol 90"), 
          omit = c("lag", "Constant"),
          omit.stat	= c("rsq", "adj.rsq", "f", "ll", 
                        "sigma2", "res.dev", "ser", "aic",
                        "wald", "lr"),
          add.lines = list(append("rho", rho_with_se), 
                           append("Log.Lik", LL),
                           append("<p>&sigma;<sup>2</sub></p>", s2)),
          notes = "Se omiten rezagos espaciales de variables dependientes (para SDM)",
          notes.label = "Notas: ",
          single.row = T, out = here::here("doc", "tables", "comp-db5-g90.htm"))

#' Vemos los impactos que tiene el cuarto periodo
#'
#' Impactos para DB5
#' 

#' Convertimos a tablas para imprimir calculando impactos para tercer periodo

# 
# diesel_3_sar_imp <- simp_impactos(sar_DB5, "01-03-2018", prod = "DB5", rep = 1000)
# g90_3_durbin_imp <- simp_impactos(durbin_G90, "01-03-2018", prod = "G90", rep = 1000)
# diesel_3_durbin_imp <- simp_impactos(durbin_DB5, "01-03-2018", prod = "DB5", rep = 1000)
# 
# diesel_2_sar_imp <- simp_impactos(sar_DB5, "01-12-2017", prod = "DB5", rep = 1000)
# g90_2_durbin_imp <- simp_impactos(durbin_G90, "01-12-2017", prod = "G90", rep = 1000)
# diesel_2_durbin_imp <- simp_impactos(durbin_DB5, "01-12-2017", prod = "DB5", rep = 1000)
# 
# impactos_1000_rep <- list(diesel_2_sar_imp, diesel_2_durbin_imp, g90_2_durbin_imp,
#      diesel_3_sar_imp, diesel_3_durbin_imp, g90_3_durbin_imp,
#      "Simulacion con 1000 repeticiones")

#saveRDS(impactos_1000_rep, here::here("data","processed","imp_1000_rep.rds"))

# saveRDS(g90_4_durbin_imp, here::here("data","processed","2019.04.30_g90-impact-t3.rds"))
#g90_4_durbin_imp <- read_rds(here::here("data","processed","2019.04.30_g90-impact-t3.rds"))

#'
#' Imprimimos tablas de impactos
#' 

#+ diesel-tercer-periodo
imprimir_impacto(diesel_3_sar_imp, ols_modelo_2_DB5)
imprimir_impacto(diesel_3_durbin_imp, ols_modelo_2_DB5)
imprimir_impacto(diesel_2_durbin_imp, ols_modelo_2_DB5)
#+ g90-tercer-periodo
imprimir_impacto(g90_2_durbin_imp, ols_modelo_2_G90)
imprimir_impacto(g90_3_durbin_imp, ols_modelo_2_G90)
