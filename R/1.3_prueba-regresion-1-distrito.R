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
productos <- c("DIESEL B5 S-50 UV", "GASOHOL 90")

brena_2017 <- prices_lima %>%
    filter(year(fecha_hora) %in% c(2017), 
           distrito == "BREÑA", producto %in% productos)

brena_2017 %>% 
    ggplot(aes(x = fecha_hora, y = precio_de_venta, color = razon_social))+
    geom_point()+
    facet_wrap(~producto)

#' Ahora convertimos los datos y tomemos un precio x semana. Si no hay precio reportado
#' en la semana, tomamos el precio anterior. Si hay varios precios en la semana, tomamos el promedio
#' 
brena_2017 %>% count(codigo_de_osinergmin, producto)

grifo_17944 <- brena_2017 %>% filter(codigo_de_osinergmin == "17944", producto == "DIESEL B5 S-50 UV") %>%
    mutate(semana = week(fecha_hora), year = year(fecha_hora)) %>%
    select(-ruc:-direccion, -unidad, -fecha_hora)%>% 
    distinct()

grifo_17944 


#' Prueba para tener precios x semana
grifo_17944_semana <- grifo_17944
for (week in 13:52) {
    if (week %ni% grifo_17944_semana$semana) {
        grifo_17944_semana <- bind_rows(grifo_17944_semana,
                filter(grifo_17944_semana, semana == week-1) %>% 
                    mutate(semana = week))
        
    }
}


#' Además, necesitamos el último precio del año anterior
#' 

prices_lima %>%
    filter(year(fecha_hora) == 2016, 
           distrito == "BREÑA", producto %in% productos) %>%
    arrange(codigo_de_osinergmin, producto, desc(fecha_hora)) %>%
    distinct(codigo_de_osinergmin, producto, .keep_all = TRUE) %>%
    mutate(semana = 1, year = year(fecha_hora)+1) %>%
    select(-ruc:-direccion, -unidad, -fecha_hora)

#' Lo convertimos función
#' 
UltimoPrecioAno <- function(df, ano, dist, productos) {
    df %>%
        filter(year(fecha_hora) == ano,
               distrito == dist,
               producto %in% productos) %>%
        arrange(codigo_de_osinergmin, producto, desc(fecha_hora)) %>%
        distinct(codigo_de_osinergmin, producto, .keep_all = TRUE) %>%
        mutate(semana = 1, year = year(fecha_hora) + 1) %>%
        select(-ruc:-direccion, -unidad, -fecha_hora) %>%
        print()
}    


UltimoPrecioAno(prices_lima, 2016, "BREÑA", productos)


#' Además, necesitamos el último precio del año anterior
#' 

prices_lima %>%
    filter(year(fecha_hora) == 2016, 
     producto %in% productos) %>%
    arrange(codigo_de_osinergmin, producto, desc(fecha_hora)) %>%
    distinct(codigo_de_osinergmin, producto, .keep_all = TRUE) %>%
    mutate(semana = 1, year = year(fecha_hora)+1) %>%
    select(-ruc:-direccion, -unidad, -fecha_hora, distrito) %>%
    arrange(distrito,codigo_de_osinergmin, producto)


#' Pasemos a semanal toda la data disponible en Breña para el grifo 17944 
#' 
#' 
#' 
#' Si en la semana hay dos o más nos quedamos con el promedio
#' 
grifo_17944 <- prices_lima %>%
    filter(distrito == "BREÑA", producto == "DIESEL B5 S-50 UV") %>%
    select(-ruc:-provincia,-direccion, -unidad) %>%
    mutate(semana = week(fecha_hora), `año` = year(fecha_hora),
           semana_inicio = semana + (`año` - 2010)*52) %>%
    arrange(codigo_de_osinergmin, fecha_hora,producto) %>%
    filter(codigo_de_osinergmin== "17944") %>%
    distinct() %>%
    group_by_at(.vars = vars(-precio_de_venta,-fecha_hora)) %>%
    summarize(precio_de_venta = mean(precio_de_venta)) %>%
    ungroup()

grifo_17944 %>% arrange(codigo_de_osinergmin, `año`, semana)


#' Prueba para tener precios x semana
for (year in min(grifo_17944$`año`):2018) {
    for (week in 1:52) {
        if (week %ni% (filter(grifo_17944, `año` == year) %>% pull(semana))) {
            grifo_17944<- bind_rows(grifo_17944,
                                    filter(grifo_17944, `año` == year,semana == week-1) %>% 
                                        mutate(semana = week))
            
        }
    }  
}

#' Otra prueba para tener precios x semana
for (week in min(grifo_17944$semana_inicio):max(grifo_17944$semana_inicio)) {
        if (week %ni% grifo_17944$semana_inicio) {
            grifo_17944<- bind_rows(grifo_17944,
                                    filter(grifo_17944, semana_inicio == week-1) %>% 
                                        mutate(semana_inicio = week, 
                                               semana = week - (`año` - 2010)*52))
            
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

