library(tidyverse)
library(lubridate)
'%ni%' <- Negate('%in%')

#' ## Cargamos datos
#' 
#+ cargar-datos

data_total <- readRDS(file = here::here("data", 
                                        "processed", 
                                        "data_prices_2005_2018_no_duplicates.rds"))

data_lima <- data_total %>% 
  filter(departamento == "LIMA",
         provincia == "LIMA",
         fecha_hora >= dmy("1-1-2018"))
  
data_total %>% 
  filter(codigo_de_osinergmin == 15219) %>% 
  count(producto)

prod_grifos <- data_lima %>% 
  distinct(codigo_de_osinergmin, distrito, producto)

num_grifos <- count(prod_grifos, codigo_de_osinergmin) %>% 
  count() %>% 
  pull()

count(prod_grifos, producto) %>% 
  mutate(ratio  = n / num_grifos)
# Grifos que no venden DB5
# 

grifos_db5 <- prod_grifos %>% 
  filter(producto == "DIESEL") %>% 
  pull(codigo_de_osinergmin)

prod_grifos %>% 
  filter(codigo_de_osinergmin %ni% grifos_db5)


data_raw <- readRDS(file = here::here("data", 
                                        "processed", 
                                        "data_2017_2018.rds"))

data_lima <- data_raw %>% 
  filter(Departamento == "LIMA",
         Provincia == "LIMA")
data_lima %>% 
  filter(`Código de Osinergmin` == 6821) %>% 
  count(Producto)

