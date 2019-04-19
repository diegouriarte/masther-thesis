#' ---
#' title: "Regresiones para tesis"
#' output: 
#'   html_notebook:
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' editor_options: 
#'   chunk_output_type: inline
#' ---
#' 
#' # Cargamos librerías
#' 
#+ cargar-librerias, include=FALSE
library(tidyverse)
library(spdep)
library(stargazer)
library(lubridate)
library(kableExtra)
library(magrittr)

#' 
#' # Cargamos datos
#' 
#+ cargar-datos
grifos_sc <-
  readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))

#' cargamos los precios para 2017 de DB5

#+ precios-cargar
rutas_fuel <- list(
  here::here("data", "processed", "data_diesel_mensual.rds"),
  here::here("data", "processed", "data_g90_mensual.rds")
)

data_precios <- map(
  rutas_fuel,
  ~ readRDS(.x) %>%
    filter(
      `año` >= 2017,
      codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin
    ) %>%
    mutate(dia = 1) %>%
    unite(fecha, dia, mes, `año`, sep = "-", remove = FALSE) %>%
    mutate(fecha = lubridate::dmy(fecha)) %>% 
    select(-dia) %>%
    filter(
      mes != 13,
      precio_de_venta > 6,
      fecha <= lubridate::dmy("2-10-2018")
    )
)


precios_db5 <- data_precios[[1]]

precios_g90 <- data_precios[[2]]

#' cargamos data de distritos

#+ data-distritos
data_distrital_raw <- read_csv(here::here("data", "demo-distrital", "data_pop_lima.csv")) %>%
  janitor::clean_names()

#' Limpiamos archivo distrital:
#' 
#+ clean-distrito
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

#' Cargamos la info de viajes
#' 
#+ distritos
big_data_viajes <- readRDS(here::here("data", 
                                      "processed", 
                                      "data_viajes_distritos.RDS"))

# numero de viajes en diciembre de 2017 x distrito
viajes_distrito <- big_data_viajes %>% 
  select("distrito" = nom_dist_d, tipo_dia, horario, motivo, edad, viajes) %>% 
  group_by(distrito) %>% 
  summarise(num_viajes = sum(viajes)) %>% 
  mutate(distrito = replace(distrito, distrito == "BRENA", "BREÑA"),
         distrito = replace(distrito, distrito == "ATE VITARTE", "ATE"),
         num_viajes_millon = num_viajes/1000000)

head(viajes_distrito)

#' Verificamos que queden excluidos distritos que no nos interesan
anti_join(data_distrital_clean, viajes_distrito, by = "distrito")

#'Todo ok, así que hacemos el merge:
#+ join-big-data

data_distrital_clean <- left_join(data_distrital_clean, viajes_distrito, by = "distrito")

#' 
#' ## Creamos archivos con info
#' 
#+merge-data-distrito 

data_total <- map(
  list(precios_db5, precios_g90),
  ~ left_join(grifos_sc, .x, by = "codigo_de_osinergmin") %>%
    left_join(., data_distrital_clean, by = "distrito") %>% 
    mutate(sc = if_else(`año` == 2017, sc_pre, sc_post),
           ingresos_2012 = ingresos_2012 / 1000,
           densidad_2017 = densidad_2017/10000)
)

data_db5 <- data_total[[1]]
data_g90 <- data_total[[2]]


#' 
#' 
#' 
#' # Regresión OLS por mes
#' 
#' Hacemos una regresión OLS pooled, clusterizando a nivel de grifo, en corte transversal para 4 periodo.
#' 
#' 
#' 
#+ ols-regression-1
#regresión sin viajes x distrito
modelo_1 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 

reg_lineal <- function(fecha_char, modelo) {
  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na()
  lm(modelo, data_mes)
}

fechas <- list("01-07-2017", "01-10-2017", "01-03-2018", "01-07-2018")

ols_modelo_1 <- map(fechas,
    ~ reg_lineal(.x, modelo_1))

modelo_2 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 + log(num_viajes)

ols_modelo_2 <- map(fechas,
                    ~ reg_lineal(.x, modelo_2))

#+tabla-ols, results = 'asis'
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
               "Abanderada Repsol", "Propia Pecsa", "Propia Primax", 
               "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
               "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
               "INGRESO", "DENPOB")

stargazer(ols_modelo_1,ols_modelo_2, type = "html",
          covariate.labels = etiquetas_cov,
          single.row = T)

#' # Test de Anselin


#+ anselin, results = 'asis', message = FALSE

calcular_weigth_matrix <- function(fecha_char) {
  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na() %>%
    select(codigo_de_osinergmin, lon, lat) %>%
    as.matrix

  grifos_nb <- tri2nb(data_mes[,2:3], row.names = data_mes[,1])
  nb2listw(grifos_nb, zero.policy = T)
}

# calculamos matriz grifos

sp_grifos <- map(fechas, ~ calcular_weigth_matrix(.x))


LMtest <- map2(ols_modelo_2, sp_grifos, ~ lm.LMtests(.x, .y, test = "all"))
names(LMtest) <- fechas

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
test_df <- map_dfr(nombres_test, ~resumen(LMtest, .x))

kable(test_df, digits = c(0, 0, 3, 5))  %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)


#' El test robusto nos dice que existe dependencia espacial, pero solo significativa
#' en el caso de considerar el modelo SAR. Siguiendo la literatura,
#' estimaremos el modelo completo de Durbin
#' 
#' # Modelo Durbin

#+ durbin, results = 'asis'

reg_durbin <- function(fecha_char, sp_grifos, durbin = T) {
  modelo_2 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
    num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
    ingresos_2012 + densidad_2017 + log(num_viajes)

  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na()

  lagsarlm(formula = modelo_2,
           data = data_mes,
           listw = sp_grifos,
           Durbin = durbin,
           tol.solve = 1e-13)
  }


durbin_fechas <- map2(fechas, sp_grifos, ~reg_durbin(.x, .y))

names(durbin_fechas) <- fechas
# Corremos el modelo autoregressivo espacial:

spatial_fechas <- map2(fechas, sp_grifos, ~reg_durbin(.x, .y, durbin = F))
names(spatial_fechas) <- fechas

#' # Corremos el modelo de errores espaciales

#+ ser

reg_errores <- function(fecha_char, sp_grifos) {

  modelo_2 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
    num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
    ingresos_2012 + densidad_2017 + log(num_viajes)

  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na()

  errorsarlm(modelo_2,
           data = data_mes,
           listw = sp_grifos,
           tol.solve = 1e-13
  )
}
errores_fechas <- map2(fechas, sp_grifos, ~ reg_errores(.x, .y))

#' # Realizamos los test para distinguir entre ambos

#+ test-LR, message = FALSE, warning = FALSE

test_SAR <- map2(durbin_fechas, spatial_fechas, ~ LR.sarlm(.x, .y))

test_SER <- map2(durbin_fechas, errores_fechas, ~ LR.sarlm(.x, .y))

extraer_test_LR <- function(lista_con_test) {
  map_df(lista_con_test, magrittr::extract, c("statistic", "p.value", "parameter")) %>% 
    bind_cols(as.data.frame(names(lista_con_test))) %>% 
    dplyr::select("fecha" = "names(lista_con_test)", statistic, "df" = parameter, p.value)
}

tabla_test_LR <- extraer_test_LR(test_SER) %>%
  mutate(Nula = "SER") %>%
  bind_rows(extraer_test_LR(test_SAR) %>%
                      mutate(Nula = "SAR"))

kable(tabla_test_LR, digits = c(0, 0, 3, 5))  %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)


#' # Modelo escogido para corte transversal
#' Ahora que hemos determinado que el mejor modelo es el autoregresivo, creamos su tabla

#+ sar-output, results = 'asis'
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
                  "Abanderada Repsol", "Propia Pecsa", "Propia Primax",
                  "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
                  "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
                  "INGRESO", "DENPOB")

stargazer(spatial_fechas, type = "html")


#' Vemos los impactos que tiene el cuarto periodo
#' 

#+ impactos-4
im_4 <- impacts(spatial_fechas[[4]], listw = sp_grifos[[4]], R = 100, useHESS = F)

per4 <- summary(im_4, zstats=TRUE, short = TRUE)


t <- tibble(attr(im_4, "bnames"),
       "directo" = im_4$res$direct, 
       "indirecto" = im_4$res$indirect,
       "total" = im_4$res$total) %>%
  bind_cols(per4$semat %>% as.data.frame()) %>% 
  bind_cols(per4$pzmat %>% as.data.frame())

cols_SE = c("Indirect", "Direct", "Total")
to_app_SE = ".SE"
cols_pvalue = c("Indirect1", "Direct1", "Total1")
to_app_pvalue = ".pvalue"
tabla_4_impactos <- rename_at(t, cols_SE, funs( paste0(., to_app_SE) ) ) %>% 
  rename_at(cols_pvalue, funs( paste0(., to_app_pvalue) ) ) 


kable(tabla_4_impactos, digits = 4)  %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)
#'
#'


