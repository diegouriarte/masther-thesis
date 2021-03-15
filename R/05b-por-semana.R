#' En este archivo tomaremos un distrito y convertiremos los datos a semanales
#' asumiendo que si no se observa cambios en la base de datos, el precio del combustible
#' no ha cambiado. Luego, graficaremos superponiendo con el precio promedio de la materia prima
#' 
#' Solo dos productos para empezar. G90 y Diesel


library(here)
library(tidyverse)
library(ggplot2)
#library(ggrepel)
library(lubridate)
'%ni%' <- Negate('%in%')


prices_lima <- readRDS(here::here("data","processed","data_2005_2018_clean.rds")) %>%
    filter(departamento == "LIMA", provincia == "LIMA")

#' Pasemos a semanal toda la data disponible en Breña para el grifo 17944 
#' 
#' 
#' 
#' Si en la semana hay dos o más nos quedamos con el promedio
#' 
#' 
# cantidad de semanas en cada año


#' Función para que quede un valor por semana:
#' INPUT: DF de un solo grifo
#' OUTPU: DF del mismo grifo con un solo valor x semana x producto
PrecioPromedioPorSemana <- function(df_grifo) {
    df_grifo <- df_grifo %>%
        select(-ruc:-provincia, -direccion,-unidad) %>%
        mutate(semana = week(fecha_hora),
               `año` = year(fecha_hora)) %>%
        arrange(codigo_de_osinergmin, fecha_hora, producto) %>%
        distinct() %>%
        group_by_at(.vars = vars(-precio_de_venta, -fecha_hora)) %>%
        summarize(precio_de_venta = mean(precio_de_venta)) %>%
        ungroup() %>%
        arrange(codigo_de_osinergmin, `año`, semana)
    
}

########## CASO DIESEL ##################

#' Sacamos un precio por semana para cada grifo en el caso del diesel

lista_grifos <- prices_lima %>% distinct(codigo_de_osinergmin) %>% pull()

list_diesel <- list()

for (grifo in lista_grifos) {
    list_diesel[[grifo]] <- prices_lima %>% 
        filter(producto == "DIESEL", codigo_de_osinergmin == grifo) %>%
        PrecioPromedioPorSemana(.)
    
}

#Unimos todo en un df
df_diesel <- bind_rows(list_diesel)

df_diesel

############### PRUEBA PARA UN GRIFO, PRECIOS SEMANALES ############

grifo <- prices_lima %>%
    filter(codigo_de_osinergmin == "9584", producto == "DIESEL") %>%
    PrecioPromedioPorSemana(.)

grifo %>% View

#' El problema con el grifo es que tiene semanas sin precios

for (year in c(min(grifo$año):2018)) {
    if (year == min(grifo$año)) {
        for (week in (min(grifo[grifo$año == year,]$semana) + 1):53) {
            if (week %ni% grifo[grifo$año == year, ]$semana) {
                grifo <- bind_rows(
                    grifo,
                    filter(grifo,
                           `año` == year,
                           semana == week - 1) %>%
                        mutate(semana = week)
                )
            }
        }
    } else {
    
    for (week in c(1:53)) {
        if (week %ni% grifo[grifo$año == year,]$semana) {
            if (week == 1) {
                grifo <- bind_rows(
                    grifo,
                    filter(grifo,
                           `año` == year - 1,
                           semana == max(grifo[grifo$año == year -
                                                         1, ]$semana)) %>%
                        mutate(semana = week,
                               `año` = year)
                )
            } else {
                grifo <- bind_rows(grifo,
                                         filter(grifo, 
                                                `año` == year, 
                                                semana == week - 1) %>%
                                             mutate(semana = week))
                
            }
        } 
    }
}
} 


grifo %>% arrange(codigo_de_osinergmin, año, semana) %>% View

## Este código funciona! Escribamos una función y probemos


PreciosSemanaGrifo2 <- function(grifo) {
    for (year in c(min(grifo$año):2018)) {
        if (year == min(grifo$año)) {
            for (week in (min(grifo[grifo$año == year, ]$semana) + 1):53) {
                if (week %ni% grifo[grifo$año == year,]$semana) {
                    grifo <- bind_rows(
                        grifo,
                        filter(grifo,
                               `año` == year,
                               semana == week - 1) %>%
                            mutate(semana = week)
                    )
                }
            }
        } else {
            for (week in c(1:53)) {
                if (week %ni% grifo[grifo$año == year, ]$semana) {
                    if (week == 1) {
                        grifo <- bind_rows(
                            grifo,
                            filter(
                                grifo,
                                `año` == year - 1,
                                semana == max(grifo[grifo$año == year -
                                                        1,]$semana)
                            ) %>%
                                mutate(semana = week,
                                       `año` = year)
                        )
                    } else {
                        grifo <- bind_rows(
                            grifo,
                            filter(grifo,
                                   `año` == year,
                                   semana == week - 1) %>%
                                mutate(semana = week)
                        )
                        
                    }
                }
            }
        }
    }
    
    
    grifo %>% arrange(codigo_de_osinergmin, año, semana)
}

grifo <- prices_lima %>%
    filter(codigo_de_osinergmin == "100155", producto == "DIESEL") %>%
    PrecioPromedioPorSemana(.)
grifo %>% View()

grifo <- PreciosSemanaGrifo2(grifo)

## Todo ok, ahora hagamos para todo lima!

list_diesel_semana <- list()
i <- 1
for (grifo in lista_grifos) {
    if (nrow(df_diesel[df_diesel$codigo_de_osinergmin == grifo,])>0) {
        list_diesel_semana[[grifo]] <- df_diesel %>% 
            filter(codigo_de_osinergmin == grifo) %>%
            PreciosSemanaGrifo2(.) 
        i <- i+1
        print(c(grifo, i)) 
    }

}

df_diesel_semana <- bind_rows(list_diesel_semana)


######### CASO G90 #############

#' Sacamos un precio por semana para cada grifo en el caso del diesel

lista_grifos <- prices_lima %>% distinct(codigo_de_osinergmin) %>% pull()

list_g90 <- list()

for (grifo in lista_grifos) {
    list_g90[[grifo]] <- prices_lima %>% 
        filter(producto == "G90", codigo_de_osinergmin == grifo) %>%
        PrecioPromedioPorSemana(.)
}

#Unimos todo en un df
df_g90 <- bind_rows(list_g90)

#Ahora un precio por semana, tomando el precio de la semana anterior en caso no haya

list_g90_semana <- list()
i <- 1
start_time <- Sys.time()

for (grifo in lista_grifos) {
    if (nrow(df_g90[df_g90$codigo_de_osinergmin == grifo,])>0) {
        list_g90_semana[[grifo]] <- df_g90 %>% 
            filter(codigo_de_osinergmin == grifo) %>%
            PreciosSemanaGrifo2(.) 
        i <- i+1
        print(c(grifo, i)) 
    }
    
}
end_time <- Sys.time()
end_time-start_time
df_g90_semana <- bind_rows(list_g90_semana)

## Guardamos los dos dataframes para ser usados luego

saveRDS(df_diesel_semana, file = here::here("data","processed","data_diesel_semanal.rds"))
saveRDS(df_g90_semana, file = here::here("data","processed","data_g90_semanal.rds"))

