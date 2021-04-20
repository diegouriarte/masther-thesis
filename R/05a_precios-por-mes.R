#' En este archivo tomaremos un distrito y convertiremos los datos a mensuales
#' asumiendo que si no se observa cambios en la base de datos, el precio del combustible
#' no ha cambiado. Luego, graficaremos superponiendo con el precio promedio de la materia prima
#' 
#' Solo dos productos para empezar. G90 y Diesel


# Cargamos librerías ------------------------------------------------------


library(here)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(lubridate)
'%ni%' <- Negate('%in%')


# Cargamos precios --------------------------------------------------------

prices_lima <- readRDS(here::here("data","processed","data_prices_2005_2018_no_duplicates.rds")) %>%
    filter(departamento == "LIMA", provincia == "LIMA",
           year(fecha_hora) >= 2016)

# Cálculo de precio mensual -----------------

precios_promedios_mensuales <- prices_lima %>%
    select(-ruc:-provincia,-direccion, -unidad,-distrito, -razon_social,
           -precio_de_venta_1) %>%
    mutate(mes = month(fecha_hora),
           `año` = year(fecha_hora)) %>%
    distinct() %>%
    arrange(codigo_de_osinergmin, fecha_hora, producto) %>%
    group_by_at(.vars = vars(-precio_de_venta,-fecha_hora)) %>%
    summarize(precio_de_venta = mean(precio_de_venta)) %>%
    ungroup() %>%
    arrange(codigo_de_osinergmin, `año`, mes)

precios_promedios_mensuales %>% 
    filter(producto == "DIESEL",
           año >= 2017) %>% 
    count(codigo_de_osinergmin, sort = T) %>% 
    pull(n) %>% hist()

cod_menos_cinco <- precios_promedios_mensuales %>% 
    filter(producto == "DIESEL",
           año >= 2018) %>% 
    count(codigo_de_osinergmin, sort = T) %>% 
    filter(n <= 5) %>% 
    pull(codigo_de_osinergmin)

# al menos dos valores en el 2018 y dos ven el 2017
dos_en_2018 <- precios_promedios_mensuales %>% 
    filter(producto == "DIESEL",
           año == 2018) %>% 
    count(codigo_de_osinergmin, sort = T) %>% 
    filter(n >= 2) %>% 
    select(codigo_de_osinergmin)

dos_en_2017 <- precios_promedios_mensuales %>% 
    filter(producto == "DIESEL",
           año == 2017) %>% 
    count(codigo_de_osinergmin, sort = T) %>% 
    filter(n >= 2) %>% 
    select(codigo_de_osinergmin)

lista_grifos_2 <- inner_join(dos_en_2018, dos_en_2017)

precios_promedios_mensuales %>% 
    filter(producto == "DIESEL",
           año >= 2017,
           codigo_de_osinergmin %in% cod_menos_cinco) %>% 
    arrange(codigo_de_osinergmin, año, mes) %>% 
    view()

prices_lima %>% 
    filter(codigo_de_osinergmin == 6808, 
           producto == "DIESEL") %>% View()

# El problema con el grifo es que tiene meses vacíos


# PRUEBA PARA UN GRIFO, PRECIOS mensuales ---------------------------------
# 
# 
# grifo <- precios_promedios_mensuales %>%
#     filter(codigo_de_osinergmin == "9584", producto == "DIESEL")
# 
# grifo %>% View

# Como solo figuran promedio para los meses donde había data, jalaremos el último 
# dato hasta la siguiente vez

# for (year in c(min(grifo$año):2018)) {
#     if (year == min(grifo$año)) {
#         for (month in (min(grifo[grifo$año == year,]$mes) + 1):12) {
#             if (month %ni% grifo[grifo$año == year, ]$mes) {
#                 grifo <- bind_rows(
#                     grifo,
#                     filter(grifo,
#                            `año` == year,
#                            mes == month - 1) %>%
#                         mutate(mes = month)
#                 )
#             }
#         }
#     } else {
#     
#     for (month in c(1:12)) {
#         if (month %ni% grifo[grifo$año == year,]$mes) {
#             if (month == 1) {
#                 grifo <- bind_rows(
#                     grifo,
#                     filter(grifo,
#                            `año` == year - 1,
#                            mes == max(grifo[grifo$año == year -
#                                                          1, ]$mes)) %>%
#                         mutate(mes = month,
#                                `año` = year)
#                 )
#             } else {
#                 grifo <- bind_rows(grifo,
#                                    filter(grifo,
#                                           `año` == year,
#                                           mes == month - 1) %>%
#                                        mutate(mes = month))
#                 
#             }
#         } 
#     }
# }
# } 


# grifo %>% arrange(codigo_de_osinergmin, año, mes) %>% View

# Este código funciona para un producto! Escribamos una función y probemos


# Función para rellenar meses sin precios reportados por grifo ------------



# llenar_precios_mensuales <- function(grifo) {
#     # Función toma el dataframe de un grifo con los precios solo de un producto
#     # y te devuelve el dataframe sin huecos en los meses
#     
#     for (year in c(min(grifo$año):2018)) {
#         if (year == min(grifo$año)) {
#             for (month in (min(grifo[grifo$año == year,]$mes) + 1):12) {
#                 if (month %ni% grifo[grifo$año == year, ]$mes) {
#                     grifo <- bind_rows(
#                         grifo,
#                         filter(grifo,
#                                `año` == year,
#                                mes == month - 1) %>%
#                             mutate(mes = month)
#                     )
#                 }
#             }
#         } else {
#             
#             for (month in c(1:12)) {
#                 if (month %ni% grifo[grifo$año == year,]$mes) {
#                     if (month == 1) {
#                         grifo <- bind_rows(
#                             grifo,
#                             filter(grifo,
#                                    `año` == year - 1,
#                                    mes == max(grifo[grifo$año == year -
#                                                         1, ]$mes)) %>%
#                                 mutate(mes = month,
#                                        `año` = year)
#                         )
#                     } else {
#                         grifo <- bind_rows(grifo,
#                                            filter(grifo,
#                                                   `año` == year,
#                                                   mes == month - 1) %>%
#                                                mutate(mes = month))
#                         
#                     }
#                 } 
#             }
#         }
#     } 
#     
#     
#     grifo %>% arrange(codigo_de_osinergmin, año, mes)
# }

llenar_precios_mensuales_1 <- function(grifo) {
    grifo %>%
        complete(
            codigo_de_osinergmin,
            año,
            producto,
            mes = 1:12
        ) %>%
        fill(precio_de_venta, .direction = "downup") 
    
}

grifo <- precios_promedios_mensuales %>%
    filter(codigo_de_osinergmin == "6740", producto == "DIESEL")

llenar_precios_mensuales_1(grifo)

llenar_precios_mensuales(grifo) %>% view #pareciera que hubiese problema
## Todo ok, ahora hagamos para todo lima!

# Cálculos de precios promedios para Diesel ----------------------------------

list_db5 <- precios_promedios_mensuales %>%
    filter(producto == "DIESEL",
           codigo_de_osinergmin %in% pull(lista_grifos_2)) %>%
    split(.$codigo_de_osinergmin)

df_db5 <- purrr::map_dfr(.x = list_db5, .f = llenar_precios_mensuales_1)

# Cálculos de precios promedios para G90 ----------------------------------

# Creamos un dataframe por grifo, y ponemos en lista

list_g90 <- precios_promedios_mensuales %>%
    filter(producto == "G90",
           codigo_de_osinergmin %in% pull(lista_grifos_2)) %>%
    split(.$codigo_de_osinergmin)

df_g90 <- purrr::map_dfr(.x = list_g90, .f = llenar_precios_mensuales_1)



# Guardamos los dos dataframes para ser usados luego ----------------------

saveRDS(df_db5, file = here::here("data","processed","data_diesel_mensual.rds"))
saveRDS(df_g90, file = here::here("data","processed","data_g90_mensual.rds"))

