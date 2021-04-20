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


prices_lima <- readRDS(here::here("data", "processed", "data_2005_2018_clean.rds")) %>%
  filter(
    departamento == "LIMA",
    provincia == "LIMA",
    year(fecha_hora) >= 2016
  ) %>%
  select(-ruc:-provincia, -razon_social,-direccion,
         -unidad, -precio_de_venta_1,
         -distrito) %>% #retiro razon social y distrito, pq hay algunos códigos q no son consistentes
  mutate(
    semana = week(fecha_hora),
    `año` = year(fecha_hora)
  ) %>%
  arrange(codigo_de_osinergmin, fecha_hora, producto)

#' Máximo número de semanas en la base de datos:
#' 
prices_lima %>% 
  filter(año == 2018) %>% 
  arrange(codigo_de_osinergmin, desc(semana)) %>% 
  distinct(codigo_de_osinergmin, .keep_all = T) %>% 
  arrange(desc(semana))

#Es 44 semanas, lo máximo que hay en la data, no extrapolar más allá
#' Pasemos a semanal toda la data disponible en Breña para el grifo 17944 
#' 
#' 
#' 
#' Si en la semana hay dos o más nos quedamos con el promedio
#' 
#' 
# cantidad de semanas en cada año


precios_semanales <- prices_lima %>% 
    distinct() %>% 
    group_by_at(.vars = vars(-precio_de_venta, -fecha_hora)) %>%
    summarize(precio_de_venta = mean(precio_de_venta), .groups = "keep") %>% 
    ungroup() %>% 
    arrange(codigo_de_osinergmin, producto, `año`, semana)

grifos_10mas <- precios_semanales %>% 
  filter(año >= 2017, producto == "DIESEL") %>% 
  arrange(codigo_de_osinergmin, desc(semana)) %>% 
  count(codigo_de_osinergmin, sort = T) %>% 
  filter(n >= 10) %>% 
  pull(codigo_de_osinergmin)

precios_semanales <- precios_semanales %>% 
  filter(codigo_de_osinergmin %in% grifos_10mas)

lista_grifos <- precios_semanales %>% distinct(codigo_de_osinergmin) %>% pull()

# precios_semanales %>%
#   filter(codigo_de_osinergmin == 100012, producto == "DIESEL") %>% 
#   complete(
#     codigo_de_osinergmin,
#     razon_social, 
#     distrito, 
#     año,
#     producto,
#     semana = 1:53
#   ) %>% view("sin completar")
# 
# precios_semanales %>%
#   filter(codigo_de_osinergmin == 100012, producto == "DIESEL") %>% 
#   complete(
#     codigo_de_osinergmin,
#     razon_social, 
#     distrito, 
#     año,
#     producto,
#     semana = 1:53
#   ) %>% 
#   fill(precio_de_venta, .direction = "downup") %>% 
#   view("completo2")
# 
# precios_semanales_completo <- 
#   precios_semanales %>% 
#   filter(producto == "DIESEL") %>% 
#   complete(
#     codigo_de_osinergmin,
#     razon_social, 
#     distrito, 
#     año,
#     producto,
#     semana = 1:53
#   ) %>% 
#   fill(precio_de_venta, .direction = "downup")

# grifo <- precios_semanales %>% filter(codigo_de_osinergmin == 100155)
# grifo
#' El problema con el grifo es que tiene semanas sin precios
# grifo <- grifo %>% filter(producto == "DIESEL")
# for (year in c(min(grifo$año):2018)) {
#     if (year == min(grifo$año)) {
#         for (week in (min(grifo[grifo$año == year,]$semana) + 1):53) {
#             if (week %ni% grifo[grifo$año == year, ]$semana) {
#                 grifo <- bind_rows(
#                     grifo,
#                     filter(grifo,
#                            `año` == year,
#                            semana == week - 1) %>%
#                         mutate(semana = week)
#                 )
#             }
#         }
#     } else {
#     
#     for (week in c(1:53)) {
#         if (week %ni% grifo[grifo$año == year,]$semana) {
#             if (week == 1) {
#                 grifo <- bind_rows(
#                     grifo,
#                     filter(grifo,
#                            `año` == year - 1,
#                            semana == max(grifo[grifo$año == year -
#                                                          1, ]$semana)) %>%
#                         mutate(semana = week,
#                                `año` = year)
#                 )
#             } else {
#                 grifo <- bind_rows(grifo,
#                                          filter(grifo, 
#                                                 `año` == year, 
#                                                 semana == week - 1) %>%
#                                              mutate(semana = week))
#                 
#             }
#         } 
#     }
# }
# } 


# grifo %>% arrange(codigo_de_osinergmin, año, semana) %>% View

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

# grifo <- precios_semanales %>% 
#     filter(codigo_de_osinergmin == 100155,
#            producto == "DIESEL")
# 
# 
# grifo <- PreciosSemanaGrifo2(grifo)

## Todo ok, ahora hagamos para todo lima! DIESEL

df_diesel <- precios_semanales %>% 
    filter(producto =="DIESEL")

list_diesel_semana <- vector("list", length(lista_grifos))
i <- 1
for (grifo in lista_grifos) {
    if (nrow(df_diesel[df_diesel$codigo_de_osinergmin == grifo,])>0) {
        list_diesel_semana[[grifo]] <- df_diesel %>% 
            filter(codigo_de_osinergmin == grifo) %>%
           # PreciosSemanaGrifo2(.) 
            complete(
                codigo_de_osinergmin,
                año,
                producto,
                semana = 1:53
              ) %>%
              fill(precio_de_venta, .direction = "downup")
        i <- i+1
        print(c(grifo, i)) 
    }

}

# precios_semanales %>%
#   filter(codigo_de_osinergmin == 100012, producto == "DIESEL") %>% 
#   complete(
#     codigo_de_osinergmin,
#     razon_social, 
#     distrito, 
#     año,
#     producto,
#     semana = 1:53
#   ) %>% 
#   fill(precio_de_venta, .direction = "downup")

df_diesel_semana <- bind_rows(list_diesel_semana) %>% 
  filter(año == 2017 | (año == 2018 & semana <= 44))


######### CASO G90 #############

#' Sacamos un precio por semana para cada grifo en el caso del diesel



# for (grifo in lista_grifos) {
#     list_g90[[grifo]] <- prices_lima %>% 
#         filter(producto == "G90", codigo_de_osinergmin == grifo) %>%
#         PrecioPromedioPorSemana(.)
# }
# 
# #Unimos todo en un df
# df_g90 <- bind_rows(list_g90)

#Ahora un precio por semana, tomando el precio de la semana anterior en caso no haya

list_g90_semana <- vector("list", length(lista_grifos))
i <- 1
start_time <- Sys.time()

df_g90 <- precios_semanales %>% 
    filter(producto =="G90")

for (grifo in lista_grifos) {
    if (nrow(df_g90[df_g90$codigo_de_osinergmin == grifo,])>0) {
        list_g90_semana[[grifo]] <- df_g90 %>% 
            filter(codigo_de_osinergmin == grifo) %>%
          # PreciosSemanaGrifo2(.) 
            complete(
              codigo_de_osinergmin,
              año,
              producto,
              semana = 1:53
            ) %>%
          fill(precio_de_venta, .direction = "downup")       
        i <- i+1
        print(c(grifo, i)) 
    }
    
}
end_time <- Sys.time()
end_time-start_time
df_g90_semana <- bind_rows(list_g90_semana) %>% 
  filter(año == 2017 | (año == 2018 & semana <= 44))
## Guardamos los dos dataframes para ser usados luego

saveRDS(df_diesel_semana, file = here::here("data","processed","data_diesel_semanal.rds"))

# df_diesel_semana <- readRDS(here::here("data", "processed", "data_diesel_semanal.rds")) 

saveRDS(df_g90_semana, file = here::here("data","processed","data_g90_semanal.rds"))

# df_g90_semana <- readRDS(here::here("data","processed","data_g90_semanal.rds"))
