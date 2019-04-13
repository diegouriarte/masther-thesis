#' Extraer grifos por distritos en Lima
#' 

library(here)
library(tidyverse)
library(ggplot2)

prices <- readRDS(here::here("data","processed","data_2005_2018_clean.rds"))

prices %>% filter(codigo_de_osinergmin == "14646") %>%
    count(razon_social,year(fecha_hora))

prices %>% 
    filter(departamento == "LIMA", provincia == "LIMA") %>%
    count(codigo_de_osinergmin, razon_social, ruc, direccion, distrito) %>%
    write_csv(path = here::here("data","processed","grifos_lima_2006_2017.csv"))

prices %>% 
    filter(departamento == "LIMA", provincia == "LIMA") %>%
    count(codigo_de_osinergmin, razon_social, ruc, direccion, distrito) %>%
    count(distrito, sort = TRUE) %>%
    write_csv(path = here::here("data","processed","distritos-lima.csv"))


prices %>%
    filter(codigo_de_osinergmin == 14693) %>%
    ggplot(aes(x = fecha_hora, y = precio_de_venta, color = producto))+
    geom_point() + 
    facet_wrap(~razon_social)

    prices %>%
    filter(codigo_de_osinergmin == 7047) %>%
    arrange((fecha_hora))


prices %>% 
    filter(departamento == "LIMA", provincia == "LIMA",
           year(fecha_hora) >= 2015) %>%
    count(codigo_de_osinergmin, razon_social, ruc, direccion, distrito) %>%
    write_csv(path = here::here("data","processed","grifos_lima_2015_2018.csv"))
