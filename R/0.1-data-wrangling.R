#' ---
#' title: "Data wrangling"
#' author: "Diego Uriarte"
#' date: "5/02/2019"
#' output: github_document
#' ---
#'
#'Now, we assign correct format to data and correct obvious mistakes
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
data_2005_2017 <- readRDS(file = here::here("data","processed","data_2005_2017.rds"))
data_2017_2018 <- readRDS(file = here::here("data","processed","data_2017_2018.rds"))

#' clean names using janitor package
data_2005_2017 <- data_2005_2017 %>% clean_names()
skim(data_2005_2017)


#' We found to missing values por año, let's see which they are:
data_2005_2017 %>% filter(is.na(ano))

#' We can drop those observation:
#' 
data_2005_2017 <- data_2005_2017 %>% drop_na(ano)

skim(data_2005_2017)

#' Now, ruc still has two missing values:
#' 
data_2005_2017 %>% filter(is.na(ruc)) 
#' We see that is a fuel station without ruc than only appears twice in the dataset.
#' We will drop these fuel station:
#' 
data_2005_2017 <- data_2005_2017 %>% drop_na(ruc)

skim(data_2005_2017)

#' Now, unidades is completely missing, so I drop it:
#' 
data_2005_2017 <- data_2005_2017 %>% 
    select(-unidades, departamento = 'nomdepa',
           provincia = 'nomprov', distrito = 'nomdist',
           -ano, -mes, -dia)

data_2005_2017 <- data_2005_2017 %>%
    unite(fecha_hora, fecha_registro, hora_registro, sep = " ") %>%
    mutate(fecha_hora = dmy_hms(fecha_hora))

data_2005_2017 %>% 
    filter(precio_venta < 10000, precio_venta > 1000) %>%
    view()

correct_price <- function(price) {
    if (price < 25) {
        return (price)
    } else {
        return(correct_price(price/10))
    }
}

data_2005_2017 %>% 
    mutate(precio_rev = map_dbl(data_2005_2017$precio_venta, 
                                correct_price),
           diff = precio_rev == precio_venta) %>%
    select(-2:-8) %>%
    filter(diff == FALSE ) %>%
    arrange(precio_venta) %>% View()

data_2005_2017 %>% 
    mutate(precio_rev = map_dbl(data_2005_2017$precio_venta, 
                                correct_price),
           diff = precio_rev == precio_venta) %>%
    skim(precio_rev)

#' Everything looks normal now with the histogram, so we save this into the object
#' 
#' 
data_2005_2017 <- data_2005_2017 %>% 
    mutate(precio_venta = map_dbl(data_2005_2017$precio_venta, 
                                correct_price))

skim(data_2005_2017)


data_2005_2017 %>% count(descripcion_producto)

data_2005_2017 %>%
    filter(departamento == "LIMA", provincia == "LIMA", distrito == "BREÑA") %>%
    mutate(ano = year(fecha_hora)) %>%
    select(codigo_osinerg, empresa, direccion, ano) %>%
    distinct()

data_2005_2017 %>%
    filter(departamento == "LIMA", provincia == "LIMA") %>%
    count(distrito, direccion) %>%
    count(distrito) %>% View()
