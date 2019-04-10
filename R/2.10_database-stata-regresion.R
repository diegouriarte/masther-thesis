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

library(tidyverse)
library(spdep)


# Cargamos precios y grifos -----------------------------------------------

grifos_sc <-
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))



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
    select(-direccion, -distrito:-orden_original)


# Exportamos a Stata ------------------------------------------------------


foreign::write.dta(
    data_db5 %>% janitor::clean_names(),
    file = here::here("data", "processed", "data_db5_cross.dta"),
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
    count(bandera)

model1 <- lm(precio_de_venta ~ sc_pre + bandera, data_1_periodo)

summary(model1)

model2 <- lm(precio_de_venta ~ sc_pre +  tipo_bandera, data_1_periodo)

summary(model2)


model_3 <- data_1_periodo %>%
    select(
        precio_de_venta,
        tipo_bandera,
        sc_pre,
        distancia_avg,
        distancia_min,
        num_grifos_cerc,
        llanteria,
        mecanico,
        lavado,
        cajero,
        con_gnv,
        con_glp
    ) %>%
    drop_na() %>%
    lm(.)

summary(model_3)

model_3$residuals
data_1_periodo

# prueba de I-test --------------------------------------------------------

ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv", sep=",", header=T)
head(ozone, n=10)
ozone.dists <- as.matrix(dist(cbind(ozone$Lon, ozone$Lat)))

ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0

ozone.dists.inv[1:5, 1:5]

ape::Moran.I(ozone$Av8top, ozone.dists.inv)


# Ahora sí para la data ---------------------------------------------------
data_1_periodo_na <- data_1_periodo %>% 
    drop_na()
grifos_distancias <- as.matrix(dist(cbind(data_1_periodo_na$lon, data_1_periodo_na$lat)))
grifos_distancias_inv <- 1/grifos_distancias
diag(grifos_distancias_inv) <- 0

length(model_3$residuals)
dim(grifos_distancias_inv)
ape::Moran.I(model_3$residuals, grifos_distancias_inv)


# Con función correcta ----------------------------------------------------

#' Hacemos el moran test

data <- data_1_periodo %>%
    select(
        precio_de_venta,
        tipo_bandera,
        sc_pre,
        distancia_avg,
        distancia_min,
        num_grifos_cerc,
        llanteria,
        mecanico,
        lavado,
        cajero,
        con_gnv,
        con_glp,
        lat,
        lon,
        codigo_de_osinergmin
    ) %>%
    drop_na()

modelo_data <- lm(data %>% select(-lat, -lon, -codigo_de_osinergmin))
summary(modelo_data)

coords <- cbind(data$lon, data$lat)

grifos <- data$codigo_de_osinergmin

grifos_nb <- tri2nb(coords, row.names = grifos)

sp_grifos <- nb2listw(grifos_nb)

lm.morantest(modelo_data, sp_grifos)

lm.LMtests(modelo_data, sp_grifos, test="all")
