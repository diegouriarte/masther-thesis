#' En este archivo tomaremos un distrito y convertiremos los datos a semanales
#' asumiendo que si no se observa cambios en la base de datos, el precio del combustible
#' no ha cambiado. Luego, graficaremos superponiendo con el precio promedio de la materia prima
#' 
#' Solo dos productos para empezar. G90 y Diesel


library(here)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(lubridate)
'%ni%' <- Negate('%in%')


prices_lima <- readRDS(here::here("data","processed","data_2005_2018_clean.rds")) %>%
    filter(departamento == "LIMA", provincia == "LIMA")
distrito <- "BREÑA"
productos <- c("DIESEL", "G90")

#' Pasemos a semanal toda la data disponible en Breña para el grifo 17944 
#' 
#' 
#' 
#' Si en la semana hay dos o más nos quedamos con el promedio
#' 
#' 
# cantidad de semanas en cada año

for (year in 2005:2018) {
    print (isoweek(ymd(paste(as.character(year),"12","31"))))
}
grifo_17944 <- prices_lima %>%
    filter(distrito == "BREÑA", producto == "DIESEL") %>%
    select(-ruc:-provincia,-direccion, -unidad) %>%
    mutate(semana = week(fecha_hora), `año` = year(fecha_hora),
           semana_inicio = semana + (`año` - 2005)*53) %>%
    arrange(codigo_de_osinergmin, fecha_hora,producto) %>%
    filter(codigo_de_osinergmin== "17944") %>%
    distinct() %>%
    group_by_at(.vars = vars(-precio_de_venta,-fecha_hora)) %>%
    summarize(precio_de_venta = mean(precio_de_venta)) %>%
    ungroup() %>%
    arrange(codigo_de_osinergmin, `año`, semana)

grifo_17944 


#' Otra prueba para tener precios x semana
for (week in min(grifo_17944$semana_inicio):max(grifo_17944$semana_inicio)) {
        if (week %ni% grifo_17944$semana_inicio) {
            grifo_17944<- bind_rows(grifo_17944,
                                    filter(grifo_17944, semana_inicio == week-1) %>% 
                                        mutate(semana_inicio = week, 
                                               semana = ifelse(semana_inicio %% 54 == 0, 1, semana_inicio - (`año` - 2005)*53)))
        }
    }  


grifo_17944 %>% arrange(codigo_de_osinergmin, `año`, semana) %>% View()

#' Escribimos una primera función para quedarnos con un precio por semana, 
#' promediando en caso haya más de uno:

name <- function(df) {
    df %>%
        select(-ruc:-provincia,-direccion, -unidad) %>%
        mutate(semana = week(fecha_hora), `año` = year(fecha_hora),
               semana_inicio = semana + (`año` - 2010)*52) %>%
        arrange(codigo_de_osinergmin, fecha_hora, producto) %>%
        filter(codigo_de_osinergmin== "17944") %>%
        distinct() %>%
        group_by_at(.vars = vars(-precio_de_venta,-fecha_hora)) %>%
        summarize(precio_de_venta = mean(precio_de_venta)) %>%
        ungroup()
}

prices_lima %>%
    filter(distrito == "BREÑA") %>%
    select(-ruc:-provincia,-direccion, -unidad) %>%
    mutate(semana = week(fecha_hora), `año` = year(fecha_hora),
           semana_inicio = semana + (`año` - 2005)*52) %>%
    arrange(codigo_de_osinergmin, fecha_hora,producto) %>%
    distinct() %>%
    group_by_at(.vars = vars(-precio_de_venta,-fecha_hora)) %>%
    summarize(precio_de_venta = mean(precio_de_venta)) %>%
    arrange(codigo_de_osinergmin, producto, semana_inicio) %>%
    filter(codigo_de_osinergmin == "17944")
    ungroup()

