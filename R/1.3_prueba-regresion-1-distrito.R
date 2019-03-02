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

#' Pasemos a semanal toda la data disponible en BreÃ±a para el grifo 17944 
#' 
#' 
#' 
#' Si en la semana hay dos o más nos quedamos con el promedio
#' 
#' 
# cantidad de semanas en cada año


grifo_17944 <- prices_lima %>%
    filter(distrito == "BREÑA", producto == "DIESEL") %>%
    select(-ruc:-provincia,-direccion, -unidad) %>%
    mutate(semana = week(fecha_hora), `año` = year(fecha_hora)) %>%
    arrange(codigo_de_osinergmin, fecha_hora,producto) %>%
    filter(codigo_de_osinergmin== "17944") %>%
    distinct() %>%
    group_by_at(.vars = vars(-precio_de_venta,-fecha_hora)) %>%
    summarize(precio_de_venta = mean(precio_de_venta)) %>%
    ungroup() %>%
    arrange(codigo_de_osinergmin, `año`, semana)

grifo_17944 %>% View()


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

grifo_17944_fun <- prices_lima %>%
    filter(codigo_de_osinergmin == "17944") %>%
    PrecioPromedioPorSemana(.)

grifo_17944_fun %>% View
#' Otra prueba para tener precios x semana
#' 
#' 

for (year in c(min(grifo_17944$año):2018)) {
  for (week in c(1:52)) {
      if (week %ni% grifo_17944[grifo_17944$año == year,]$semana) {
          if (week == 1) {
              grifo_17944 <- bind_rows(
                  grifo_17944,
                  filter(grifo_17944,
                         `año` == year - 1,
                         semana == max(grifo_17944[grifo_17944$año == year -
                                                       1, ]$semana)) %>%
                      mutate(semana = week,
                             `año` = year)
              )
          } else {
              grifo_17944 <- bind_rows(grifo_17944,
                                       filter(grifo_17944, 
                                              `año` == year, 
                                              semana == week - 1) %>%
                                        mutate(semana = week))
                                           
                                       }
      } 
  }
      
} 

grifo_17944 %>% arrange(codigo_de_osinergmin, año, semana) %>% View


#' Creamos una función para hacerlo por grifo:
#' 
PreciosSemanaGrifo <- function(df_grifo) {
    for (year in c(min(df_grifo$año):2018)) {
        for (week in c(1:52)) {
            if (week %ni% df_grifo[df_grifo$año == year,]$semana) {
                if (week == 1) {
                    df_grifo <- bind_rows(
                        df_grifo,
                        filter(df_grifo,
                               `año` == year - 1,
                               semana == max(df_grifo[df_grifo$año == year -
                                                             1, ]$semana)) %>%
                            mutate(semana = week,
                                   `año` = year)
                    )
                } else {
                    df_grifo <- bind_rows(df_grifo,
                                             filter(df_grifo, 
                                                    `año` == year, 
                                                    semana == week - 1) %>%
                                                 mutate(semana = week))
                    
                }
            } 
        }
        
    } 
    
    df_grifo %>% arrange(codigo_de_osinergmin, año, semana) 
}



grifo_17944_fun <- PreciosSemanaGrifo(grifo_17944_fun)
grifo_17944_fun %>% View()



#' Sacamos un precio por semana para cada grifo en el caso del diesel

lista_grifos <- prices_lima %>% distinct(codigo_de_osinergmin) %>% pull()
list_diesel <- list()
for (grifo in lista_grifos) {
    list_diesel[[grifo]] <- prices_lima %>% 
        filter(producto == "DIESEL", codigo_de_osinergmin == grifo) %>%
        PrecioPromedioPorSemana(.)
    
}




#' Sacamos un precio por semana para cada grifo en el caso del diesel

lista_grifos <- prices_lima %>% distinct(codigo_de_osinergmin) %>% pull()
list_diesel <- list()
for (grifo in lista_grifos) {
    list_diesel[[grifo]] <- prices_lima %>% 
        filter(producto == "DIESEL", codigo_de_osinergmin == grifo) %>%
        PrecioPromedioPorSemana(.)
    
}

#' 

df_diesel <- bind_rows(list_diesel)

df_diesel

#####################


grifo_100155 <- prices_lima %>%
    filter(codigo_de_osinergmin == "9584", producto == "DIESEL") %>%
    PrecioPromedioPorSemana(.)

grifo_100155_semana <- PreciosSemanaGrifo(grifo_100155)
grifo_100155_semana <- PreciosSemanaGrifo2(grifo_100155)

grifo_100155_semana %>% View()
#' Todo OK con la prueba.
#' 
#' Repetimos para Diesel, y todos los grifos

list_diesel_semana <- list()
i <- 1
for (grifo in lista_grifos) {
    list_diesel_semana[[grifo]] <- df_diesel %>% 
        filter(codigo_de_osinergmin == grifo) %>%
        PreciosSemanaGrifo(.) 
    i <- i+1
    print(c(grifo, i))
}

#' 

bind_rows(df_list) 

grifo_100155_semana %>% View
prices_lima %>%
    filter(distrito == "BREÑA") %>%
    select(-ruc:-provincia,-direccion, -unidad) %>%
    mutate(semana = week(fecha_hora), `año` = year(fecha_hora)) %>%
    arrange(codigo_de_osinergmin, fecha_hora, producto) %>%
    distinct() %>%
    group_by_at(.vars = vars(-precio_de_venta,-fecha_hora)) %>%
    summarize(precio_de_venta = mean(precio_de_venta)) %>%
    arrange(codigo_de_osinergmin, producto, año, semana) %>%
    ungroup()

prices_lima %>%
    filter(codigo_de_osinergmin == "100012", producto == "DIESEL") %>%
    View()


###############otra prueba############

grifo <- prices_lima %>%
    filter(codigo_de_osinergmin == "9584", producto == "DIESEL") %>%
    PrecioPromedioPorSemana(.)

grifo %>% View
#' Otra prueba para tener precios x semana
#' 
#' 

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
    filter(codigo_de_osinergmin == "9592", producto == "DIESEL") %>%
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

