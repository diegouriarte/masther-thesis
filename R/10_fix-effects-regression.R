#' ---
#' title: Regresión panel
#' author: "Diego Uriarte"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_notebook:
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' 

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    error = TRUE
)
options(tidyverse.quiet = TRUE)

#+ body, warning = FALSE, message = FALSE
# ----
library(conflicted)
library(tidyverse)
library(splm)
library(lmtest)
library(plm)
library(lubridate)
'%ni%' <- Negate('%in%')

conflict_prefer("filter", "dplyr")
# ----


#' 
#' ## Cargamos datos
#' 
#+ cargar-datos

data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))

#' ## Corremos regresión de efectos fijos:


#' Ahora hacemos la regresión por efectos fijos 10 meses y 3 meses antes y después:
    
inicio <- list("01-03-2017", "01-10-2017")
fin <- list("01-10-2018", "01-03-2018")

#' El modelo es:
#' 
modelo2 <- precio_de_venta ~ COMPRADA  + SUMINISTRO + vecino_pecsa_thiessen + sc + fecha


#' Función para hacer la regresión de efectos fijos filtrando solo las estaciones que
#' estuvieron activas durante todo el periodo
calc_fe_fechas <- function(df, inicio, fin, modelo, prod) {
    grifos_creados_luego <- df %>% 
        filter(producto == !!prod) %>% 
        count(codigo_de_osinergmin) %>% 
        arrange(n) %>% 
        filter(n < max(n)) %>% 
        pull(codigo_de_osinergmin)
    
    df_panel_balanceado <- df %>% 
        filter(codigo_de_osinergmin %ni% grifos_creados_luego)
    
    panel_df <- df_panel_balanceado %>% 
        filter(producto == !!prod) %>% 
        filter(fecha >= dmy(inicio), fecha <= dmy(fin)) %>% 
        pdata.frame(., index = c("codigo_de_osinergmin", "fecha"))
    
    fe <- plm(modelo, data= panel_df, model="within")
    fe
}

#' ### FE para DB5

modelos_fe_db5 <-
    map2(inicio,
         fin,
         ~ calc_fe_fechas(data_total, .x, .y, modelo2, "DIESEL"))

names(modelos_fe_db5) <- inicio

map(modelos_fe_db5, ~summary(.x))


#' Calculos errores estándares clusterizados

map(modelos_fe_db5, 
    ~coeftest(.x, vcov = vcovHC(.x, type = "sss", cluster = "group")))

#' ### FE para G90

modelos_fe_g90 <-
    map2(inicio,
         fin,
         ~ calc_fe_fechas(data_total, .x, .y, modelo2, "G90"))

names(modelos_fe_g90) <- inicio

map(modelos_fe_g90, ~summary(.x))

#' Calculos errores estándares clusterizados

map(modelos_fe_g90, 
    ~coeftest(.x, vcov = vcovHC(.x, type = "sss", cluster = "group")))


#' ## Spatial fixed effects:


#' Creamos matriz de pesos para ambos casos:
#

crear_spatial_w <- function(df, prod) {
    #solo grifos que operaron todo la ventana
    grifos_creados_luego <- df %>%
        filter(producto == !!prod) %>%
        count(codigo_de_osinergmin, sort = T) %>%
        filter(n < max(n)) %>%
        pull(codigo_de_osinergmin)
    
    df_balanceado <- df %>%
        filter(producto == !!prod,
               codigo_de_osinergmin %ni% grifos_creados_luego) 
    
    coords_db5 <- df_balanceado %>%
        distinct(codigo_de_osinergmin, .keep_all = T) %>%
        select(codigo_de_osinergmin, lon, lat) %>%
        as.matrix()
    
    #creamos la matriz de distancias
    grifos_nb <- tri2nb(coords_db5[,2:3], row.names = coords_db5[,1])
    
    sp_grifos <- nb2listw(grifos_nb)
    sp_grifos
}

spatial_w <- map(c("DIESEL", "G90"), ~ crear_spatial_w(data_total, .x))
names(spatial_w) <- c("DIESEL", "G90")


fm <- precio_de_venta ~ COMPRADA  + vecino_pecsa_thiessen + sc + fecha

#' ### Regresión de efectos fijos
#'
#corremos regresión
#' Creamos una función para balancear el panel (removemos grifos que no abrieron o 
#' cerrarone en la ventana, son menos de 10)

balancear_panel <- function(df, prod) {
    grifos_creados_luego <- df %>%
        filter(producto == !!prod) %>%
        count(codigo_de_osinergmin, sort = T) %>%
        filter(n < max(n)) %>%
        pull(codigo_de_osinergmin)
    
    df_balanceado <- df %>%
        filter(producto == !!prod) %>%
        filter(codigo_de_osinergmin %ni% grifos_creados_luego)
    
    df_balanceado
}

#' Regresión por efectos fijos con el modelo completo
sararfemod <- spml(formula = fm, data = balancear_panel(data_total, "DIESEL"), index = NULL,
                   listw = spatial_w$DIESEL, lag = TRUE, spatial.error = "b", model = "within",
                   effect = "individual", method = "eigen", na.action = na.fail,
                   quiet = TRUE, zero.policy = NULL,
                   tol.solve = 1e-13, control = list(), legacy = FALSE)
summary(sararfemod)

imp1 <- impacts(sararfemod, listw = spatial_w$DIESEL, time = 21, R = 200)
summary(imp1, zstats = TRUE, short = T)

#' Solo spatial error tipo b 
summary(verdoorn_SEM_FE<- spml(fm, data = balancear_panel(data_total, "DIESEL"),
                               listw = spatial_w$DIESEL, lag=FALSE,model="within", effect="individual", spatial.error="b"))

#' Solo spatial error tipo kkp 

summary(verdoorn_SEM_FE<- spml(fm, data = balancear_panel(data_total, "DIESEL"),
                               listw = spatial_w$DIESEL, lag=FALSE,model="within", effect="individual", spatial.error="kkp"))

#' ### Regresión de efectos aleatorios

# no corre
# sararremod <- spml(formula = fm, data = balancear_panel(data_total, "DIESEL"), index = NULL,
#                    listw = spatial_w$DIESEL, model = "random", lag = TRUE, spatial.error = "b")


# no corre el random de errores
# summary(verdoorn_SEM_Ran<- spml(fm, data = balancear_panel(data_total, "DIESEL"),
                               # listw = spatial_w$DIESEL, lag=FALSE,model="random", 
                               # effect="individual", spatial.error="kkp"))

#' Hacemos test de Hausman espacial para modelo con lag
#'
test_lag <- sphtest(x = fm, data = balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL,
                  spatial.model = "lag", method = "ML")

test_lag

#' ### Tests para determinar modelo correcto
#'
#'
#'
# Hausman test (plm)
print(hausman_panel <- phtest(fm, data = balancear_panel(data_total, "DIESEL")))

# Hausman test robust to spatial autocorrelation (splm) for error model
print(
    spat_hausman_ML_SEM <- sphtest(
        fm,
        data = balancear_panel(data_total, "DIESEL"),
        listw = spatial_w$DIESEL,
        spatial.model = "error",
        method = "ML"
    )
)

# Hausman test robust to spatial autocorrelation (splm) for sarar model
print(
    spat_hausman_ML_SEM <- sphtest(
        fm,
        data = balancear_panel(data_total, "DIESEL"),
        listw = spatial_w$DIESEL,
        spatial.model = "sarar",
        method = "ML"
    )
)

# Fixed effects model
# Test 1
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lml",
        model="within")
# Test 2
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lme",
        model="within")
# Test 3
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlml",
        model="within")
# Test 4
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlme",
        model="within")

# Random effects model
# Test 1
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lml",
        model="random")
# Test 2
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lme",
        model="random")
# Test 3
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlml",
        model="random")
# Test 4
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlme",
        model="random")
