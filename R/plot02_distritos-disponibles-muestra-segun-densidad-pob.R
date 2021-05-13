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
    distinct(codigo_de_osinergmin, lon, lat, tipo, tipo_bandera, distrito) %>% 
    mutate(grifo = "grifo") %>% 
    filter(codigo_de_osinergmin != 7563) %>% #grifo inubicable
    mutate(tipo_r = case_when(
      tipo_bandera == "PROPIA PRIMAX" ~ "PROPIA PRIMAX",
      tipo_bandera == "PROPIA PECSA" ~ "PROPIA PECSA",
      tipo_bandera == "PROPIA REPSOL" ~ "PROPIA REPSOL",
      tipo == "ABANDERADA" ~ "ABANDERADA",
      TRUE ~ "INDEPENDIENTE"
      ),
      tipo2 = case_when(
        tipo_bandera == "PROPIA PRIMAX" ~ "PROPIA PRIMAX",
        tipo_bandera == "PROPIA PECSA" ~ "PROPIA PECSA",
        TRUE ~ "OTRA"
      ),
      alpha1 = if_else(tipo2 == "OTRA", 0.5, 1)
      )
      
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

tm2

distritos_lima %>% 
  filter(`En muestra` == "Sí") %>% 
  mutate(tipo_r = case_when(
    tipo_bandera == "PROPIA PRIMAX" ~ "PROPIA PRIMAX",
    tipo_bandera == "PROPIA PECSA" ~ "PROPIA PECSA",
    tipo_bandera == "PROPIA REPSOL" ~ "PROPIA REPSOL",
    bandera == "ABANDERADA" ~ "ABANDERADA",
    TRUE ~ "INDEPENDIENTE"
  ))
  tm_shape() + 
  tm_polygons(alpha = 0.5) +
  tm_shape(lista_grifos_sp) + 
  tm_dots(col = "tipo",  size = 0.1, alpha = 0.8,
          palette = c(INDEPENDIENTE = "green", PROPIA = "blue", ABANDERADA = "purple")) +
  tm_scale_bar(breaks = c(0, 5, 10), size = 0.5, position = c("right", "top"))+
  tm_add_legend(type="symbol", 
                col="red", 
                labels=c("Ubicación de grifos"), 
                title="")


tm_shape(distritos_lima) + 
    tm_polygons(col = "En muestra", palette = "Greys", alpha = 0.25) +
    tm_shape(lista_grifos_sp) + 
    tm_dots(col = "tipo_r",  size = 0.1, alpha = 0.5,
            palette = c(INDEPENDIENTE = "yellow", 
                        `PROPIA PRIMAX` = "orange",
                        `PROPIA PECSA` = "red",
                        `PROPIA REPSOL` = "blue",
                        ABANDERADA = "purple",
                        INDEPENDIENTE = "green")) +
    tm_scale_bar(breaks = c(0, 5, 10), size = 0.5, position = c("right", "top"))+
    tm_add_legend(type="symbol", 
                  col="red", 
                  labels=c("Ubicación de grifos"), 
                  title="")


distritos_lima %>% 
  filter(`En muestra` == "Sí") %>% 
tm_shape() + 
  tm_polygons(alpha = 0.5) +
  tm_text("distrito", size = 0.5, remove.overlap = T, 
          auto.placement = F
) +
  tm_shape(lista_grifos_sp) + 
  tm_dots(col = "tipo_r",  size = 0.1, alpha = 0.5,
          palette = c(INDEPENDIENTE = "yellow", 
                      `PROPIA PRIMAX` = "orange",
                      `PROPIA PECSA` = "red",
                      `PROPIA REPSOL` = "blue",
                      ABANDERADA = "purple",
                      INDEPENDIENTE = "green")) +
  tm_scale_bar(breaks = c(0, 1, 2), size = 0.5, position = c("right", "top"))+
  tm_add_legend(type="symbol", 
                col="red", 
                labels=c("Ubicación de grifos"), 
                title="")
tm_shape(distritos_lima) + 
  tm_polygons(col = "En muestra", palette = "Greys", alpha = 0.5) +

distritos_lima %>% 
  # filter(`En muestra` == "Sí") %>% 
  tm_shape() + 
  tm_polygons(
    col = "En muestra", palette = "Greys",
    alpha = 0.25) +
  tm_text("distrito", size = 0.5, remove.overlap = T, 
          auto.placement = F
  ) +
  tm_shape(lista_grifos_sp) + 
  tm_dots(col = "tipo2",  size = 0.1, alpha = 0.4,
          palette = c(OTRA = "darkgreen", 
                      `PROPIA PRIMAX` = "blue",
                      `PROPIA PECSA` = "red"),
          border.lwd = 1,
          border.col = "black") +
  tm_scale_bar(breaks = c(0, 5, 10), size = 0.5, position = c("right", "bottom"))

#Grafico solo algunos distritos este es el que uso en la tesis ============

lista_grifos_sp <- lista_grifos %>% 
  filter(distrito %in% c("SURQUILLO", "MIRAFLORES", "SAN ISIDRO", "SAN BORJA"
  )) %>% 
  mutate(tipo2 = fct_rev(tipo2))
coordinates(lista_grifos_sp) = c("lon", "lat")
distritos_lima %>% 
  filter(distrito %in% c("SURQUILLO", "MIRAFLORES", "SAN ISIDRO", "SAN BORJA"
                         )) %>% 
  tm_shape() + 
  tm_polygons(alpha = 0.25) +
  tm_text("distrito", size = 0.7, remove.overlap = T, 
          auto.placement = F
  ) +
  tm_shape(lista_grifos_sp) + 
  tm_dots(col = "tipo2",  size = 0.3, alpha = 0.4,
          palette = c(OTRA = "darkgreen", 
                      `PROPIA PRIMAX` = "blue",
                      `PROPIA PECSA` = "red"),
          border.lwd = 1,
          border.col = "black",
          title = "Estaciones") +
  tm_scale_bar(breaks = c(0, 1, 2), size = 0.5, position = c("right", "bottom"))+
  tm_add_legend(title="",
                )

tm_arrange <- tmap_arrange(tm1, tm2)
tm_arrange
st=format(Sys.time(), "%Y-%m-%d")


# tmap_save(tm_arrange, filename = here::here("plots", paste("muestra-distritos_",st, ".png", sep = "")),
#           width = 800, height = 500, dpi = 100 )
tmap_save(tm_arrange, filename = here::here("plots", paste("muestra-distritos_1",st, ".png", sep = "")),
          width = 1600, height = 1000, dpi = 200 )


## Gráfico vecinos thiessen ======================
#' 
grifos_thissen <- readRDS(here::here("data", "processed", "shape_file_grifos_thiessen.rds"))
lista_grifos_sp <- st_as_sf(lista_grifos_sp)  
grifos_thissen <- st_as_sf(grifos_thissen)




(thiessen_plot <-
  tm_shape(distritos_lima %>% 
             filter(distrito %in% c("SAN ISIDRO", "MIRAFLORES",
                                    "SURQUILLO", "SAN BORJA",
                                    "LA VICTORIA"))) +
  tm_borders(col = NULL) + 
  tm_text("distrito", col = "black", ymod = 3.5, fontface = "bold", size = 0.8) + 
  tm_scale_bar(
    breaks = c(0, 0.5, 1),
    size = 0.8,
    position = c("left", "bottom")
  ) +
  tm_shape(grifos_thissen) +
  tm_polygons(
    col = "tipo_bandera",
    title = "Razon social de estación",
    palette = c("grey", "red", "blue", "orange"),
    alpha = 0.4
  ) +
  tm_layout(
    legend.outside = T,
    legend.outside.position = "bottom",
    legend.stack = "vertical",
    legend.frame = T,
    legend.bg.color = T,
    legend.text.size	= 0.9,
    legend.title.size = 1,
    legend.title.fontface = "bold"
  ) +
  tm_shape(lista_grifos_sp) +
  tm_dots(col = "red", size = 0.1, alpha = 0.8) +
  tm_add_legend(
    type = "symbol",
    col = "red",
    size = 2,
    labels = c("Ubicación de grifos"),
    title = ""
  ) +
  tm_shape(filter(grifos_thissen, codigo_de_osinergmin == 6765)) +
  tm_borders(col = "red", lwd = 3)
)
DPI <- 300
width_in <- 7 / 2.54
heigth_in <- 12 / 2.54
tmap_save(thiessen_plot, filename = here::here("plots", paste("san-isidro-thiessen_",st, ".png", sep = "")),
          width = width_in*DPI, height = heigth_in*DPI , dpi = DPI, scale = 0.5)

#Tabla con estaciones por distritos

data_total %>% 
  distinct(codigo_de_osinergmin, .keep_all = T) %>% 
  select(razon_social, direccion, distrito, bandera, tipo, tipo_bandera) %>% 
  ggplot(aes(x = distrito, fill = tipo_bandera)) +
  geom_bar() + 
  coord_flip()
  
  
