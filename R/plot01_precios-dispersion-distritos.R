#' ---
#' title: "Gráfica de dispersión de precios de combustible"
#' author: "Diego Uriarte"
#' date: "Tue Apr 23 16:55:17 2019"
#' output:
#'   html_notebook:
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 
#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk(
    collapse = TRUE,
    comment = "#>",
    error = TRUE
)
options(tidyverse.quiet = TRUE)

#' ## Cargamos librerías
#' 
#+ cargar-librerias, include=FALSE
library(conflicted)
library(tidyverse)
library(ggplot2)
library(sf)
library(maptools)
library(tmap)
'%ni%' <- Negate('%in%')

conflict_prefer("filter", "dplyr")

#' ## Importamos data de precios:
#' 
prices_lima <- readRDS(here::here("data","processed","data_prices_2005_2018_no_duplicates.rds")) %>%
    filter(departamento == "LIMA", provincia == "LIMA")

prices_lima

#' ## Importamos mapa de Lima
#' 
crs <- CRS("+init=epsg:32718")

distritos <- st_read(here::here("data","shapes-files","data-mapa-peru", "PER_adm3.shp"))

# distritos_no <- c("Ancon", "San Juan de Lurigancho", "Carabayllo", 
#                   "Puente Piedra", "Comas", "Villa Maria del Triunfo", 
#                   "Cieneguilla", "Pachacamac", "Punta Hermosa", "Punta Negra",
#                   "San Bartolo", "Santa Maria del Mar", "Santa Rosa", "Lurin",
#                   "Pucusana", "Lurigancho", "Chaclacayo")

distritos_lima <- subset(distritos, NAME_1 == "Lima Province" & NAME_2 == "Lima")# &
#                               NAME_3 %ni% distritos_no) %>% 

distritos_lima <- distritos_lima %>% 
    select("distrito" = NAME_3) %>% 
    mutate(distrito = str_to_upper(distrito),
           distrito = if_else(distrito == "MAGDALENA VIEJA", 
                              "PUEBLO LIBRE", 
                              distrito))

plot(distritos_lima)
proj4string(distritos_lima) <- crs


tm_shape(distritos_lima) + 
    tm_polygons()


#' Calculamos precio promedio por años por distrito
#' 
precios_promedio_año <- prices_lima %>% 
    mutate(`año` = lubridate::year(fecha_hora)) %>% 
    group_by(`año`, distrito) %>% 
    summarize(Promedio = mean(precio_de_venta)) %>% 
    mutate(distrito = if_else(distrito == "MAGDALENA VIEJA", "PUEBLO LIBRE", distrito),
           num_distrito = as.integer(factor(distrito)))


#' Seleccionamos un año y añadamos al mapa
anti_join(distritos_lima, precios_promedio_año, by = "distrito")
precios_mapa <- left_join(distritos_lima, precios_promedio_año, by = "distrito")


p2017 <- precios_mapa %>% 
    filter(`año` == 2017) %>% 
    tm_shape() + 
    tm_polygons(col = "Promedio", palette = "BuGn") + 
    #tm_text(text = "num_distrito", size = 0.5, remove.overlap = T, auto.placement = T) +
    tm_layout(
              legend.title.size = 1,
              legend.text.size = 0.6,
              legend.position = c("left","bottom"),
              legend.bg.color = "white",
              legend.bg.alpha = 1)
p2017

p2018 <- precios_mapa %>% 
    filter(`año` >= 2017) %>% 
    tm_shape() + 
    tm_polygons(col = "Promedio", palette = "BuGn") + 
    #tm_text(text = "num_distrito", size = 0.5, remove.overlap = T, auto.placement = T) +
    tm_layout(
        legend.title.size = 1.5,
        legend.text.size = 1,
        legend.position = c("left","bottom"),
        legend.bg.color = "white",
        legend.bg.alpha = 1,
        legend.outside = F) + 
    tm_facets(by = "año", nrow = 1, free.scales = FALSE )
p2018
tmap_save(p2018, filename = "precios_17_18.png")

#' Ahora animación 
#' 
anim_precios_lima <- precios_mapa %>% 
    filter(`año` >= 2014) %>% 
    tm_shape() + 
    tm_polygons(col = "Promedio", palette = "BuGn") + 
    tm_facets(along = "año", free.coords = FALSE) 
    
tmap_animation(anim_precios_lima, filename = "precios_lima.gif", delay = 150,
               loop = 2, restart.delay = 150)

# Sys.setenv(PATH = paste("C:/Program Files/ImageMagick-7.0.8-Q16",
#                          Sys.getenv("PATH"), sep = ";"))
