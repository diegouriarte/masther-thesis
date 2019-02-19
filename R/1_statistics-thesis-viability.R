#' ---
#' title: "Data visualization test"
#' author: "Diego Uriarte"
#' date: "5/02/2019"
#' output: github_document
#' ---
#'
#'The purpose of this R file is to explore how complete is the data and compare 
#'trends in price
#'

#'Packages to use
#'
library(here)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)

prices <- readRDS(here::here("data","processed","data_2005_2018_clean.rds"))

#' número de grifos de jesus maría
#' 

glimpse(prices)
grifos_jesus_maria <- prices %>% 
    filter(departamento == "LIMA", distrito == "JESUS MARIA") %>%
    distinct(codigo_de_osinergmin, razon_social, direccion) %>%
    pull(codigo_de_osinergmin)
grifos_jesus_maria

prices %>%
    filter(codigo_de_osinergmin %in% grifos_jesus_maria) %>%
    ggplot(aes(x = fecha_hora, y = precio_de_venta, color = producto)) +
    geom_point() +
    facet_wrap(~codigo_de_osinergmin)


prices %>%
    filter(codigo_de_osinergmin %in% grifos_jesus_maria,
           year(fecha_hora) <= 2011) %>%
    ggplot(aes(x = fecha_hora, y = precio_de_venta, color = producto)) +
    geom_point() +
    facet_wrap(~codigo_de_osinergmin)

prices %>%
    filter(departamento == "ANCASH", distrito == "CHIMBOTE",
           year(fecha_hora) <= 2017) %>%
    ggplot(aes(x = fecha_hora, y = precio_de_venta, color = producto)) +
    geom_point() +
    facet_wrap(~codigo_de_osinergmin)

prices %>%
    filter(codigo_de_osinergmin == 8398)

prices %>%
    filter(departamento == "PIURA", provincia == "TALARA",
           year(fecha_hora) <= 2017) %>%
    ggplot(aes(x = fecha_hora, y = precio_de_venta, color = producto)) +
    geom_point() +
    facet_wrap(~codigo_de_osinergmin)

prices %>%
    filter(departamento == "LIMA", provincia == "LIMA") %>%
    distinct(direccion)

distritos 

prices %>% 
    filter(departamento == "LIMA", provincia == "LIMA", producto == "GASOHOL 90",
           distrito %in% c("JESUS MARIA", "SAN ISIDRO", "COMAS")) %>%
    group_by(distrito, "año" = year(fecha_hora), direccion) %>%
    summarize(precio_promedio = mean(precio_de_venta)) %>% 
    summarize(precio_promedio_G90 = mean(precio_promedio), num_grifos = n()) %>%
    ggplot(aes(x = num_grifos, y = precio_promedio_G90, label = distrito)) + 
    geom_point() +
    geom_text(aes(label = distrito), hjust = 0, vjust=0)+
    facet_wrap(~`año`)

prices %>% 
    filter(departamento == "LIMA", provincia == "LIMA", producto == "GASOHOL 90",
           distrito %in% c("JESUS MARIA", "SAN ISIDRO", "COMAS",
                           "BREÑA", "LINCE", "ATE", "SAN MARTIN DE PORRES",
                           "LOS OLIVOS", "SURCO", "MIRAFLORES", "SURQUILLO",
                           "PUEBLO LIBRE"),
           year(fecha_hora) >= 2010) %>%
    group_by(distrito, "año" = year(fecha_hora), direccion) %>%
    summarize(precio_promedio = mean(precio_de_venta)) %>% 
    summarize(precio_promedio_G90 = mean(precio_promedio), num_grifos = n()) %>%
    ggplot(aes(x = num_grifos, y = precio_promedio_G90, label = distrito)) + 
    geom_point() +
    geom_text_repel(aes(label = distrito), hjust = 0, vjust=0, size = 1.5)+
    labs(y = "Precio promedio G90", x = "Número de grifos",
         title = "Relación entre precio promedio de la Gasolina de 90 octanos
         y el número de grifos") +
    facet_wrap(~`año`)


ggsave(here::here("doc","plots","g90_grifos_precio_2010-2017.png"))

prices%>%
    filter(distrito == "BREÑA") %>%
    distinct(direccion)

prices%>%
    filter(provincia == "LIMA") %>%
    distinct(distrito) %>% View()
