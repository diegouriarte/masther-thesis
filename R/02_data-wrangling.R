#' ---
#' title: "Data wrangling"
#' author: "Diego Uriarte"
#' date: "5/02/2019"
#' output: github_document
#' ---
#'
#'El objetivo de este archivo R es tomar la base de datos obtenida en
#'01_import-files-from-excel.R y detectar y limpiar errores como nombres de
#'productos que no sean idénticos, precios incorrectos. También se retiran
#'precios de gas natural y GLP que no son necesarios. Se corrige para todo el
#'país, con la observación que hay varias observaciones en Provincia con precio 0, 
#'que no corrijo por no ser de interés en esta oportunidad.
#'

#'Paquetes a utilizar
#'
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(purrr)
library(skimr)
#library(readxl)

#'We load the previouly generated files that contain all information from 2005 to
#'2008
#'
data_2005_2018 <- readRDS(file = here("data","processed","data_2005_2018.rds"))

#' clean names using janitor package
data_2005_2018 <- data_2005_2018 %>% clean_names()


#' We found 5 missing values in `fecha_hora`, let's see which they are:
data_2005_2018 %>% filter(is.na(fecha_hora))

#' We can drop those observations:
#' 
data_2005_2018 <- data_2005_2018 %>% drop_na(fecha_hora)


#' Now, ruc still has  missing values:
#' 
data_2005_2018 %>% filter(is.na(ruc)) 
#' We see that is a fuel station without ruc than only appears twice in the dataset.
#' Another one appears one time. I keep this observations.


#' We only keep information about liquid fuels
#' 

productos <- data_2005_2018 %>%
    count(producto, sort = TRUE) %>%
    pull(producto)

productos

#' As the stations use different names for the fuels, we need to convert to the 
#' same
# 
# estaciones_venden_DIESEL2BA <- data_2005_2018 %>%
#   filter(departamento == "LIMA", provincia == "LIMA") %>%
#   filter(producto == "DIESEL2 BA") %>%
#   distinct(codigo_de_osinergmin) %>%
#   pull()

    
#' Este es el primer cambio al códidgo de hace años, agrego algunas keys
#' más de diesel

level_key <- c("GASOHOL 90 PLUS" = "G90", 
               "GASOHOL 84 PLUS" = "G84",
               "GASOHOL 95 PLUS" = "G95",
               "GASOLINA 84"  = "G84",
               "GASOHOL 97 PLUS" = "G97",
               "GASOLINA 90"  = "G90",
               "GASOHOL 98 PLUS"  = "G98",
               "Diesel B5 S-50" = "DIESEL",
               "DIESEL 2" = "DIESEL",
               "GASOLINA 95" = "G95",
               "GASOLINA 97" = "G97",
               "DIESEL B5" = "DIESEL",
               "Diesel B5 S-50 UV" = "DIESEL",
               "GASOLINA 98 BA" = "G98",
               "Diesel B2 S-50" = "DIESEL",
               "Diesel B2 S-50 UV" = "DIESEL",
               "Diesel 2 S-50 UV" = "DIESEL",
               "DIESEL 2 UV" = "DIESEL",
               "DIESEL B2 UV" = "DIESEL",
               "DIESEL B5 S-50" = "DIESEL",
               "GASOLINA 98" = "G98",
               "DIESEL B2" = "DIESEL",
               "DIESEL B5 UV" = "DIESEL",
               "DIESEL B2" = "DIESEL",
               "DIESEL2 BA" = "DIESEL",
               "DIESEL B2 BA" = "DIESEL")


data_2005_2018_clean_products <- data_2005_2018 %>%
    mutate(producto = recode(producto, !!!level_key)) %>%
    filter(producto != "DIESEL MARINO N° 2",
           producto != "GLP - G",
           producto != "KEROSENE (DOMÉSTICO)",
           producto != "GAS NATURAL VEHICULAR",
           producto != "GAS LICUADO DE PETROLEO")


data_2005_2018_clean_products %>% 
    filter(precio_de_venta > 1000) %>%
    ggplot(aes(precio_de_venta)) + 
    geom_histogram()

data_2005_2018_clean_products %>% 
    filter(precio_de_venta > 100000)
#' De la gráfica y filtro, hay un precio super alto, que corresponde a una estación en
#' Comas en 2005 que lo imputó incorrectamente. 
#' 
#' 



#' Function to correct prices that have been incorrectly imputed
correct_price <- function(price) {
  if (price < 30.1) {
    return(price)
  } else {
    return(correct_price(price / 10))
  }
}

#' Al hacer un prueba, se ve que se corrige la distribución
data_2005_2018_clean_products %>%
  mutate(
    precio_rev = map_dbl(
      data_2005_2018_clean_products$precio_de_venta,
      correct_price
    ),
    diff = precio_rev == precio_de_venta,
    year = year(fecha_hora)
  ) %>%
  select(-2:-8, year) %>%
  filter(diff == FALSE) %>%
  skim(precio_rev)



data_2005_2018_clean_products %>% 
    mutate(precio_rev = map_dbl(data_2005_2018_clean_products$precio_de_venta, 
                                correct_price),
           diff = precio_rev == precio_de_venta) %>%
    skim(precio_rev)


#' Everything looks normal now with the histogram, so we save this into the object
#' 
#' 
data_2005_2018_corrected_prices <- data_2005_2018_clean_products %>% 
  mutate(precio_de_venta_1 = map_dbl(data_2005_2018_clean_products$precio_de_venta, 
                                   correct_price))

skim(data_2005_2018_corrected_prices, precio_de_venta)

#' No puede ser que el mínimo sea 0, veamos que está ocurriendo:
#' 

data_2005_2018_corrected_prices %>%
  filter(precio_de_venta == 0) %>% 
  count(departamento) %>% 
  arrange(desc(n)) %>%
  pull(departamento)

#' Como el problema no está en Lima, sigamos, no tengo tiempo para corregir


data_2005_2018_corrected_prices_1 <- data_2005_2018_corrected_prices %>% 
    filter(precio_de_venta != 0,
           precio_de_venta > 4)


saveRDS(data_2005_2018_corrected_prices_1, file = here::here("data","processed","data_2005_2018_clean.rds"))

