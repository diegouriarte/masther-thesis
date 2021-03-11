#' ---
#' title: "Importing xlsx files"
#' author: "Diego Uriarte"
#' date: "5/02/2019"
#' output: github_document
#' ---
#'
#'Data files are to heavy, so they are not uploaded to github. I'll import them
#'into R here
#'
#'

#'Packages to use
#'
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(here)

#' Hay dos hojas que requieren ser importadas del primer archivo, las demas 
#' contienen data de GN o GLP

data_2005_2017_sheet1 <- read_excel(here("data","no-sync",
                                         "1_fuel-station-price-data",
                                         "base-de-datos-precios-combustibles-2005-2017.xlsx"), 
                                    sheet ="EVP_01", 
                                    col_names = TRUE,
                                    skip = 3,
                                    progress = readxl_progress()) 

data_2005_2017_sheet2 <- read_excel(here("data", "no-sync",
                                         "1_fuel-station-price-data",
                                         "base-de-datos-precios-combustibles-2005-2017.xlsx"),
                                    sheet = "EVP_02",
                                    col_names = TRUE,
                                    skip = 3,
                                    progress = readxl_progress()) 


data_2005_2017 <- bind_rows(data_2005_2017_sheet1, data_2005_2017_sheet2)

#' Complementamos con la data 2017-2018 que está en otro archivo
data_2017_2018 <- read_excel(here("data", "no-sync", 
                                  "1_fuel-station-price-data",
                                  "base-de-datos-precios-combustibles-2017-2018.xlsx"),
                             sheet = "PRICE",
                             col_names = TRUE,
                             skip = 3)

glimpse(data_2005_2017)
glimpse(data_2017_2018)

data_2005_2017_selected <- data_2005_2017 %>%
  select(-DESCRIPCION_ACTIVIDAD, -AÑO, -MES, -DIA)


colnames(data_2005_2017_selected)
colnames(data_2017_2018)

#'Uniformizamos los nombres de las columnas, ya que describen lo mismo
colnames(data_2005_2017_selected) <- colnames(data_2017_2018)


#' Creamos una sola columna con fecha y hora, para que R lo reconozca como fecha
data_2005_2017_selected <- data_2005_2017_selected %>%
    unite(fecha_hora, `Fecha de Registro`, `Hora de Registro`, sep = " ") %>%
    mutate(fecha_hora = dmy_hms(fecha_hora))

glimpse(data_2005_2017_selected)
head(data_2017_2018)

#'Lo mismo para el archivo 2017-2018
data_2017_2018_date <- data_2017_2018 %>%
    mutate(hora = hour(`Hora de Registro`),
           minuto = minute(`Hora de Registro`),
           segundo = second(`Hora de Registro`),
           fecha_hora = `Fecha de Registro` + hours(hora) + minutes(minuto) + seconds(segundo))  %>%
    select(-hora, -minuto,-hora, -segundo, - `Hora de Registro`, -`Fecha de Registro`)


data_2005_2018 <- bind_rows(data_2005_2017_selected, data_2017_2018_date)

#' Save datafile
saveRDS(data_2005_2018, file = here("data","processed","data_2005_2018.rds"))

