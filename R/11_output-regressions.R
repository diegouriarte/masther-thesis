#' ---
#' title: "Regresiones para tesis"
#' output: html_notebook
#' editor_options: 
#'   chunk_output_type: inline
#' ---
#' 
#' # Cargamos librerías
#' 
## ----include=FALSE-------------------------------------------------------
library(tidyverse)
library(spdep)
library(stargazer)
library(lubridate)
#' 
#' # Cargamos datos
#' 
## ------------------------------------------------------------------------
# data grifos
grifos_sc <-
  readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))

# cargamos los precios para 2017 de DB5
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

# cargamos data de distritos

data_distrital_raw <- read_csv(here::here("data", "demo-distrital", "data_pop_lima.csv")) %>%
  janitor::clean_names()

#' ## Limpiamos archivo distrital:
#' 
## ------------------------------------------------------------------------
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

#' 
#' 
#' ## Creamos archivos con info
## ------------------------------------------------------------------------

data_total <- map(
  list(precios_db5, precios_g90),
  ~ left_join(grifos_sc, .x, by = "codigo_de_osinergmin") %>%
    left_join(., data_distrital_clean, by = "distrito") %>% 
    mutate(sc = if_else(`año` == 2017, sc_pre, sc_post),
           ingresos_2012 = ingresos_2012 / 1000)
)

data_db5 <- data_total[[1]]
data_g90 <- data_total[[2]]

#' 
#' 
#' 
#' # Primera regresión OLS pooled
#' 
#' Hacemos una regresión OLS pooled, clusterizando a nivel de grifo, en corte transversal para un periodo.
#' 
#' 
#' 
## ------------------------------------------------------------------------

modelo_1 <- precio_de_venta ~ tipo_bandera + sc + distancia_avg + distancia_min +
  num_grifos_cerc + tiene_mecanico + lavado + cajero + con_gnv + con_glp +
  ingresos_2012 + densidad_2017 

data_10.2017 <- data_db5 %>%
  filter(mes == 7, `año` == 2017) %>%
  drop_na()



reg_lineal <- function(fecha_char, modelo) {
  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na()
  lm(modelo, data_mes)
}

fechas <- list("01-07-2017", "01-10-2017", "01-03-2018", "01-07-2018")

ols_meses <- map(fechas,
    ~ reg_lineal(.x, modelo_1))

#' 
## ---- results = 'asis'---------------------------------------------------
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
               "Abanderada Repsol", "Propia Pecsa", "Propia Primax", 
               "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
               "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
               "INGRESO", "DENPOB")
stargazer(ols_meses, type = "html",
          covariate.labels = etiquetas_cov,
          single.row = T)

#' Ahora la regresión en porcentajes en lugar de niveles:
#' 

#' 
#' 
#' Test de Moran para los cuatros periodos
## ------------------------------------------------------------------------


calcular_weigth_matrix <- function(fecha_char) {
  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na() %>% 
    select(codigo_de_osinergmin, lon, lat) %>% 
    as.matrix
  # 
  # grifos_nb <- tri2nb(data_mes[,2:3], row.names = data_mes[,1])
  # sp_grifos <- nb2listw(grifos_nb, zero.policy = T)
  
  list.queen<-poly2nb(chi.poly, queen=TRUE)
  W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
  
}


sp_grifos <- map(fechas, ~ calcular_weigth_matrix(.x))


LMtest <- map2(ols_meses, sp_grifos, ~ lm.LMtests(.x, .y, test = "all"))
names(LMtest) <- fechas
map_dfr(LMtest, ~.x$RLMerr$statistic)
map_dfr(LMtest,c("RLMerr", "statistic"))
map_dfr(LMtest,c("RLMerr", "p.value"))

map(LMtest, "RLMerr")
map(LMtest, "RLMlag")

# LMtest


#' 
#' Aparentemente, el modelo con lag es el significativo, así que usaremos ese. Estiamos el modelo espacial de Durbin:
#' 
## ------------------------------------------------------------------------
reg_durbin <- function(fecha_char, modelo, sp_grifos, durbin = T) {
  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na()
  
  lagsarlm(modelo,
           data = data_mes,
           listw = sp_grifos,
           Durbin = durbin
  )
  }


durbin_fechas <- map2(fechas, sp_grifos, ~reg_durbin(.x, modelo_1, .y))
names(durbin_fechas) <- fechas



summary(durbin_fechas$`01-07-2017`)

#'
#' Corremos el modelo de rezagos espaciales:
#'
## ------------------------------------------------------------------------
spatial_fechas <- map2(fechas, sp_grifos, ~reg_durbin(.x, modelo_1, .y, durbin = F))
names(spatial_fechas) <- fechas

#'
#'
#'Corremos el modelo de errores espaciales
#'
reg_errores <- function(fecha_char, modelo, sp_grifos) {
  data_mes <- data_db5 %>%
    filter(fecha == dmy(fecha_char)) %>%
    drop_na()
  
  errorsarlm(modelo,
           data = data_mes,
           listw = sp_grifos
  )
}

errores_fechas <- map2(fechas, sp_grifos, ~ reg_errores(.x, modelo_1, .y))


## ------------------------------------------------------------------------
#' Realizamos los test para distinguir entre ambos

map2(durbin_fechas, spatial_fechas, ~ LR.sarlm(.x, .y))

map2(durbin_fechas, errores_fechas, ~ LR.sarlm(.x, .y))

#' Ahora que hemos determinado que el mejor modelo es el autoregresivo, creamos su tabla

## ---- results = 'asis'---------------------------------------------------
etiquetas_cov = c("Abanderada Petroperu", "Abanderada Pecsa", "Abanderada Primax",
                  "Abanderada Repsol", "Propia Pecsa", "Propia Primax", 
                  "Propia Repsol", "SC", "DPROM", "DMIN", "NCERC",
                  "MECANICO", "LAVADO", "CAJERO",  "GNV", "GLP",
                  "INGRESO", "DENPOB")
stargazer(spatial_fechas, type = "html",
          single.row = T)

summary(spatial_fechas[[4]])


impacts(spatial_fechas[[4]], listw = sp_grifos[[4]])

im_4 <- impacts(spatial_fechas[[4]], listw = sp_grifos[[4]], R = 100, useHESS = T)
sums_4<-summary(im_4,  zstats=T)
data.frame(sums_4$res)
data.frame(sums_4$pzmat)

summary(im_4, zstats=TRUE, short = TRUE)

#'
#'

# probamos con otra forma de definir vecinos ------------------------------
data_feb <- data_db5 %>% 
  filter(fecha <= dmy("01-03-2018")) %>% 
  distinct(codigo_de_osinergmin, .keep_all = T) %>% 
  select(codigo_de_osinergmin, lon, lat) %>% 
  as.matrix()

coords <- data_feb[,2:3]
rn <- data_feb[,1]
k1 <- knn2nb(knearneigh(coords))
all.linked <- max(unlist(nbdists(k1, coords)))
col.nb.0.all <- dnearneigh(coords, 0, 3, row.names=rn, longlat = TRUE)
nb_dist <- nb2listw(col.nb.0.all, zero.policy = T)
summary(col.nb.0.all, coords)
plot(col.nb.0.all, coords)

impacts(spatial_fechas[[3]], listw = nb_dist, R = 100)

