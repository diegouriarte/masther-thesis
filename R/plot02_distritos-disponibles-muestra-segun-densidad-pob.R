#' ---
#' title: "Gráfica de distritos incluidos en el estudio
#' author: "Diego Uriarte"
#' date: "Wed Apr 24 16:00:22 2019"
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
library(sf)
library(maptools)
library(tmap)
'%ni%' <- Negate('%in%')

conflict_prefer("filter", "dplyr")

#' ## Cargamos la data
#+ cargar-datos
data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))

#' ## Trabajamos con el mapa de Lima
#' 
#' 
crs <- CRS("+init=epsg:32718")

distritos <- st_read(here::here("data","shapes-files","data-mapa-peru", "PER_adm3.shp"))
# distritos_no <- c("Ancon", "San Juan de Lurigancho", "Carabayllo", 
#                   "Puente Piedra", "Comas", "Villa Maria del Triunfo", 
#                   "Cieneguilla", "Pachacamac", "Punta Hermosa", "Punta Negra",
#                   "San Bartolo", "Santa Maria del Mar", "Santa Rosa", "Lurin",
#                   "Pucusana", "Lurigancho", "Chaclacayo")


distritos_lima <- distritos %>% 
    filter( NAME_1 == "Lima Province", NAME_2 == "Lima") %>% 
    select("distrito" = NAME_3) %>% 
    mutate(distrito = str_to_upper(distrito),
           distrito = if_else(distrito == "MAGDALENA VIEJA", 
                              "PUEBLO LIBRE", 
                              distrito))

tm_shape(distritos_lima) + 
    tm_polygons()

#' ## Distritos en la muestra
#' 

data_distrital_clean <- readRDS(here::here("data", "processed", "data_distrital_completa.rds"))
data_distrital_clean


distritos_muestra <- unique(data_total$distrito)

# Ahora los grifos
lista_grifos <- data_total %>% 
    distinct(codigo_de_osinergmin, lon, lat) %>% 
    mutate(grifo = "grifo") %>% 
    filter(codigo_de_osinergmin != 7563) #grifo inubicable
lista_grifos_sp <- lista_grifos
coordinates(lista_grifos_sp) = c("lon", "lat")
#Proyectamos en UTM
# proj4string(lista_grifos_sp) <- crs






distritos_lima <- distritos_lima %>% 
    mutate("En muestra" = if_else(distrito %in% distritos_muestra, "Sí", "No")) %>% 
    left_join(data_distrital_clean, by = "distrito")

tm1 <- tm_shape(distritos_lima) + 
    tm_polygons(col = "densidad_2017",
                title = "Habitantes por km^2") +
    tm_scale_bar(breaks = c(0, 5, 10), size = 0.5, position = c("right", "top"))
tm1


tm2 <- tm_shape(distritos_lima) + 
    tm_polygons(col = "En muestra", palette = "Greys", alpha = 0.5) +
    tm_shape(lista_grifos_sp) + 
    tm_dots(col = "red", size = 0.05, alpha = 0.5) +
    tm_scale_bar(breaks = c(0, 5, 10), size = 0.5, position = c("right", "top"))+
    tm_add_legend(type="symbol", 
                  col="red", 
                  labels=c("Ubicación de grifos"), 
                  title="")



tm_arrange <- tmap_arrange(tm1, tm2)
tm_arrange
st=format(Sys.time(), "%Y-%m-%d")


# tmap_save(tm_arrange, filename = here::here("plots", paste("muestra-distritos_",st, ".png", sep = "")),
#           width = 800, height = 500, dpi = 100 )
tmap_save(tm_arrange, filename = here::here("plots", paste("muestra-distritos_1",st, ".png", sep = "")),
          width = 1600, height = 1000, dpi = 200 )
