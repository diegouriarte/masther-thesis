library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)
library(lubridate)
path <- here::here("data", "lista de precios 2 petro.xlsx")
all_cells <- xlsx_cells(path, sheets = "PRECIOS DE LISTA PETRO II") %>% 
  dplyr::filter(!is_blank) %>%
  select(row, col, data_type, character, numeric, local_format_id, date)



fechas <-
  all_cells %>%
  dplyr::filter(
    data_type == "date",
    col == 2
    ) %>% 
  select(row, col, fecha = date) 

planta <- all_cells %>% 
  filter(col == 2, 
         row >= 3, 
         data_type == "character") %>% 
  select(row, col, planta = character)

producto <- all_cells %>% 
  filter(row == 2, col %in% 3:10) %>% 
  select(row, col, producto = character)

data_cells <- 
  all_cells %>% 
  filter(data_type == "numeric",
         col %in% 2:10) %>% 
  select(row, col, precio = numeric)

precios_lista_1 <- data_cells %>% 
  enhead(producto, "up") %>%
  enhead(fechas, "up-left") %>% 
  enhead(planta, "left") %>% 
  select(fecha, planta, producto, precio)



path_2 <- here::here("data", "precio lista comb liq 2015-2021.xlsx") 

all_cells <- xlsx_cells(path_2, sheets = 1) %>% 
  dplyr::filter(!is_blank) %>%
  select(row, col, data_type, character, numeric, local_format_id, date)


fechas <-
  all_cells %>%
  dplyr::filter(
    data_type == "date",
    col == 2
  ) %>% 
  select(row, col, fecha = date) 

planta <- all_cells %>% 
  filter(col == 2, 
         row >= 3, 
         data_type == "character") %>% 
  select(row, col, planta = character)

producto <- all_cells %>% 
  filter(row == 2, col %in% 3:8) %>% 
  select(row, col, producto = character)

data_cells <- 
  all_cells %>% 
  filter(data_type == "numeric",
         col %in% 3:8) %>% 
  select(row, col, precio = numeric)

precios_lista_2 <- data_cells %>% 
  enhead(producto, "up") %>%
  enhead(fechas, "up-left") %>% 
  enhead(planta, "left") %>% 
  select(fecha, planta, producto, precio)

precios_lista_2 %>% 
  filter(
    planta %in% c("TALARA", "CONCHAN", "CALLAO"),
    str_detect(producto, "UV"),
    year(fecha) > 2019
  ) %>% 
  ggplot(aes(x = fecha, y = precio)) +
  geom_line(aes(color = planta))
  
precios_lista_2 %>% 
  filter(
    str_detect(producto, "UV"),
    year(fecha) > 2019
  ) %>% 
  ggplot(aes(x = fecha, y = precio)) +
  geom_line(aes(color = planta))

precios_lista_2 %>% 
  mutate(producto = factor(producto)) %>% 
  count(producto)

precios_lista_2 %>% 
  filter(
    str_detect(producto, "UV"),
    year(fecha) > 2019
  ) %>% 
  group_by(planta) %>% 
  summarise(promedio = mean(precio)) %>% 
  arrange(desc(promedio))

precios_lista_final <- precios_lista_2 %>% 
  filter(planta %in% c("CALLAO"),
         fecha >= dmy("1/11/2016") & fecha <= dmy("1/12/2018"),
         producto == "GASOHOL 90" | str_detect(producto, "DIESEL B5 UV")) %>% 
  mutate(producto = if_else(producto == "GASOHOL 90", "G90", "DIESEL")
    
  )

saveRDS(precios_lista_final, file = here::here("data", "processed", "precios_lista.rds"))
