#' ---
#' title: 
#' author: "Diego Uriarte"
#' date: Thu Apr 04 14:09:30 2019
#' output: github_document
#' ---
#' 


# Librerías ---------------------------------------------------------------

library(tidyverse)
library(spdep)


# Prueba de internet ------------------------------------------------------

if (require(rgdal, quietly = TRUE)) {
    NY8 <- readOGR(system.file("shapes/NY8_utm18.shp", package = "spData"))
     } else {
         require(maptools, quietly = TRUE)
         NY8 <- readShapeSpatial(system.file("shapes/NY8_utm18.shp", package = "spData"))
         }
NY_nb <- read.gal(system.file("weights/NY_nb.gal", package = "spData"),
                     region.id = row.names(NY8))
Syracuse <- NY8[NY8$AREANAME == "Syracuse city", ]

Syracuse


coords <- coordinates(Syracuse)
coords
IDs <- row.names(as(Syracuse, "data.frame"))
IDs
Sy4_nb <- tri2nb(coords, row.names = IDs)
Sy4_nb


# Lo hacemos para la data -------------------------------------------------

grifos_sc <-
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS")) %>%
    drop_na()

grifos_sc %>%
    select(codigo_de_osinergmin) %>%
    distinct()

coords <- cbind(grifos_sc$lon, grifos_sc$lat)

grifos <- grifos_sc$codigo_de_osinergmin

grifos_nb <- tri2nb(coords, row.names = grifos)

sp_grifos <- nb2listw(grifos_nb)

str(sp_grifos)

