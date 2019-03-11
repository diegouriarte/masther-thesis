#' ---
#' title: "Gráficas de efecto de compra de Pecsa"
#' author: "Diego Uriarte"
#' date: Tue Mar 05 17:24:48 2019
#' output: github_document
#' ---
#' 

library(tidyverse)
library(lubridate)
library(ggplot2)
#' Importamos data.
#' 
df_diesel_semana <- readRDS(here::here("data","processed","data_diesel_semanal.rds"))
df_g90_semana <- readRDS(here::here("data","processed","data_g90_semanal.rds"))


#' Filtramos alrededor de la fecha de compra (1/02/2018)
#' 
#' 
fecha_compra <- ymd("2018-02-01")
week_compra <- week(fecha_compra)
fecha_inicio <- fecha_compra - dyears(1)
semana_inicio <- week(fecha_inicio)

df_diesel_semana %>%
    filter((año >= 2017 & semana >= semana_inicio) | año >= 2018) %>%
    filter(distrito == "SAN MIGUEL") %>%
    mutate(bandera_prelim = recode_factor(razon_social, "COESTI S.A." = "PRIMAX", "PERUANA DE ESTACIONES DE SERVICIOS S.A.C." = "PECSA",
                                   "REPSOL COMERCIAL S.A.C." = "REPSOL",
                                   .default = "INDEPENDIENTE")) %>%
    group_by(bandera_prelim, semana, año) %>%
    summarize(p_promedio = mean(precio_de_venta)) %>%
    arrange(año, semana, bandera_prelim) %>%
    mutate(fecha = dmy(paste("1","1", año, sep = "-")) + days(semana*7-1)) %>%
    ggplot(aes(x = fecha, y = p_promedio, color = bandera_prelim)) +
    geom_point()
   

df_diesel_semana %>%
    filter((año >= 2017 & semana >= semana_inicio | año >= 2018)) %>%
    filter(distrito == "SAN MIGUEL") %>%
    View()


df_g90_semana %>%
    filter((año == 2017 & semana >= semana_inicio) | (año == 2018 & semana <= 40)) %>%
    filter(distrito == "SAN MIGUEL") %>%
    mutate(bandera_prelim = recode_factor(razon_social, "COESTI S.A." = "PRIMAX", "PERUANA DE ESTACIONES DE SERVICIOS S.A.C." = "PECSA",
                                          "REPSOL COMERCIAL S.A.C." = "REPSOL",
                                          .default = "INDEPENDIENTE")) %>%
    group_by(bandera_prelim, semana, año) %>%
    summarize(p_promedio = mean(precio_de_venta)) %>%
    arrange(año, semana, bandera_prelim) %>%
    mutate(fecha = dmy(paste("1","1", año, sep = "-")) + days(semana*7-1)) %>%
    ggplot(aes(x = fecha, y = p_promedio, color = bandera_prelim)) +
    geom_point()

#' Breña
df_g90_semana %>%
    filter((año == 2017 & semana >= semana_inicio) | (año == 2018 & semana <= 40)) %>%
    filter(distrito == "BREÑA") %>%
    mutate(bandera_prelim = recode_factor(razon_social, "COESTI S.A." = "PRIMAX", "PERUANA DE ESTACIONES DE SERVICIOS S.A.C." = "PECSA",
                                          "REPSOL COMERCIAL S.A.C." = "REPSOL",
                                          .default = "INDEPENDIENTE")) %>%
    group_by(bandera_prelim, semana, año) %>%
    summarize(p_promedio = mean(precio_de_venta)) %>%
    arrange(año, semana, bandera_prelim) %>%
    mutate(fecha = dmy(paste("1","1", año, sep = "-")) + days(semana*7-1)) %>%
    ggplot(aes(x = fecha, y = p_promedio, color = bandera_prelim)) +
    geom_point()

