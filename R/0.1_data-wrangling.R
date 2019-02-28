#' ---
#' title: "Data wrangling"
#' author: "Diego Uriarte"
#' date: "5/02/2019"
#' output: github_document
#' ---
#'
#'The purpose of this R file is to clean the database from mistakes such as product
#'names, incorrect prices. Also, I drop observations from LPG and Natural Gas
#'
#'

#'Packages to use
#'
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)
library(purrr)

#'We load the previouly generated files that contain all information from 2005 to
#'2008
#'
data_2005_2018 <- readRDS(file = here::here("data","processed","data_2005_2018.rds"))

#' clean names using janitor package
data_2005_2018 <- data_2005_2018 %>% clean_names()
skim(data_2005_2018)


#' We found 5 missing values in `fecha_hora`, let's see which they are:
data_2005_2018 %>% filter(is.na(fecha_hora))

#' We can drop those observation:
#' 
data_2005_2018 <- data_2005_2018 %>% drop_na(fecha_hora)


#' Now, ruc still has  missing values:
#' 
data_2005_2018 %>% filter(is.na(ruc)) 
#' We see that is a fuel station without ruc than only appears twice in the dataset.
#' Another one appears one time. I keep this observations.


skim(data_2005_2018)

#' We only keep informatio about liquid fuels
#' 
data_2005_2018 %>%
    filter(departamento == "LIMA", provincia == "LIMA",
           year(fecha_hora) < 2011) %>%
    count(producto, sort = TRUE) %>% View()



productos <- data_2005_2018 %>%
    count(producto, sort = TRUE) %>%
    pull(producto)

estaciones_venden_DIESEL2BA <- data_2005_2018 %>% 
    filter(departamento == "LIMA", provincia == "LIMA") %>%
    filter(producto == "DIESEL2 BA") %>%
    distinct(codigo_de_osinergmin) %>% pull()

data_2005_2018 %>%
    filter(codigo_de_osinergmin %in% estaciones_venden_DIESEL2BA,
           producto %in% c("DIESEL 2", "DIESEL2 BA")) %>%
    arrange(codigo_de_osinergmin, fecha_hora, producto) %>%
    

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
               "GASOLINA 98" = "G98")


data_2005_2018 %>%
    mutate(producto = recode(producto, !!!level_key)) %>%
    count(producto, sort = TRUE)


data_2005_2018_clean_products <- data_2005_2018 %>%
    mutate(producto = recode(producto, !!!level_key)) %>%
    filter(producto != "DIESEL MARINO N° 2",
           producto != "GLP - G",
           producto != "KEROSENE (DOMÉSTICO)",
           producto != "GAS NATURAL VEHICULAR",
           producto != "GAS LICUADO DE PETROLEO",
           producto != "DIESEL2 BA", 
           producto != "DIESEL B2 BA")


data_2005_2018_clean_products %>% 
    filter(precio_de_venta < 10000, precio_de_venta > 1000) %>%
    select(razon_social, departamento, precio_de_venta)

#' Function to correct prices that have been incorrectly imputed
correct_price <- function(price) {
    if (price < 30.1) {
        return (price)
    } else {
        return(correct_price(price/10))
    }
}

#' Test
data_2005_2018_clean_products %>% 
    mutate(precio_rev = map_dbl(data_2005_2018_clean_products$precio_de_venta, 
                                correct_price),
           diff = precio_rev == precio_de_venta,
           year = year(fecha_hora)) %>%
    select(-2:-8,year) %>%
    filter(diff == FALSE ) %>%
    arrange(precio_de_venta)


data_2005_2018_clean_products %>% 
    mutate(precio_rev = map_dbl(data_2005_2018_clean_products$precio_de_venta, 
                                correct_price),
           diff = precio_rev == precio_de_venta) %>%
    skim(precio_rev)


#' Everything looks normal now with the histogram, so we save this into the object
#' 
#' 
data_2005_2018_corrected_prices <- data_2005_2018_clean_products %>% 
    mutate(precio_de_venta = map_dbl(data_2005_2018_clean_products$precio_de_venta, 
                                correct_price))

skim(data_2005_2018_corrected_prices)

saveRDS(data_2005_2018_corrected_prices, file = here::here("data","processed","data_2005_2018_clean.rds"))

