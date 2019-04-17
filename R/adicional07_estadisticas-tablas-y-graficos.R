
library(tidyverse)

# Datos de porcejtan de estaciones Pecsa y Repsol propias---------------------------------------------

prices_lima <- readRDS(here::here("data","processed","data_prices_2005_2018_no_duplicates.rds")) %>%
    filter(departamento == "LIMA", provincia == "LIMA")


estaciones_pecsa <- prices_lima %>% 
    filter(lubridate::year(fecha_hora) >= 2017,
           str_detect(razon_social, "PERUANA DE ESTACIONES")) %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>% 
    dplyr::select(codigo_de_osinergmin, direccion, distrito) %>% 
    pull() %>% 
    length()


grifos_sc <-
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))


precios_no_duplicates <- readRDS(here::here("data", "processed", "data_prices_2005_2018_no_duplicates.rds"))
estaciones_vivas_2017 <- precios_no_duplicates %>% 
    filter(lubridate::year(fecha_hora) >= 2017,
           departamento == "LIMA", provincia == "LIMA") %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>% 
    dplyr::select(codigo_de_osinergmin) %>% 
    pull() %>% 
    length()

perc_pecsa <- estaciones_pecsa / estaciones_vivas_2017
perc_pecsa


estaciones_repsol_propias <- precios_no_duplicates %>% 
    filter(lubridate::year(fecha_hora) >= 2017,
           departamento == "LIMA", provincia == "LIMA",
           razon_social == "REPSOL COMERCIAL S.A.C." ) %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>% 
    dplyr::select(codigo_de_osinergmin) %>% 
    pull() %>% 
    length()

perc_repsol <- estaciones_repsol_propias / estaciones_vivas_2017
perc_repsol



# Gráfica de precios promedios ------------------------

# Cargamos librerías

library(tidyverse)
library(spdep)


# Cargamos datos

# data grifos
grifos_sc <-
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))

# cargamos los precios para 2017 de DB5
rutas_fuel <- list(
    here::here("data", "processed", "data_diesel_mensual.rds"),
    here::here("data", "processed", "data_g90_mensual.rds")
)

data_precios <- map(
    rutas_fuel,
    ~ readRDS(.x) %>%
        filter(
            `año` >= 2017,
            codigo_de_osinergmin %in% grifos_sc$codigo_de_osinergmin
        ) %>%
        mutate(dia = 1) %>%
        unite(fecha, dia, mes, `año`, sep = "-", remove = FALSE) %>%
        select(-dia) %>%
        filter(
            mes != 13,
            precio_de_venta > 6
        )
)


precios_db5 <- data_precios[[1]]

precios_g90 <- data_precios[[2]]

# cargamos data de distritos

data_distrital_raw <- read_csv(here::here("data", "demo-distrital", "data_pop_lima.csv")) %>%
    janitor::clean_names()

## Limpiamos archivo distrital:

data_distrital_clean <- data_distrital_raw %>%
    rename(
        "pop_2017" = poblacion_total_30_06_2017,
        "densidad_2017" = densidad_poblacional_hab_km2,
        "ingresos_2012" = ingreso_per_capita
    ) %>%
    mutate(
        pop_2017 = str_remove(pop_2017, " ") %>% parse_number(),
        densidad_2017 = str_remove(densidad_2017, " ") %>% parse_number(),
        distrito = str_to_upper(distrito)
    )



## Creamos archivos con info
grifos_sc <- grifos_sc %>%
    mutate(
        serv_mecanico = if_else(mecanico + aceite + llanteria > 0, 1, 0),
        serv_mecanico = as.factor(serv_mecanico)
    )


data_total <- map(
    list(precios_db5, precios_g90),
    ~ left_join(grifos_sc, .x, by = "codigo_de_osinergmin") %>%
        left_join(., data_distrital_clean, by = "distrito")
)

data_db5 <- data_total[[1]]
data_g90 <- data_total[[2]]

data_db5_g90 <- bind_rows(data_db5, data_g90)


p1 <- data_db5_g90 %>% 
    mutate(fecha = lubridate::dmy(fecha)) %>% 
    filter(fecha <= lubridate::dmy("2-10-2018")) %>% 
    group_by(fecha, producto, tipo) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    arrange(fecha) %>% 
    ggplot(aes(x = fecha, y = precio_promedio, color = tipo)) +
    geom_line(size = 0.8) + 
    facet_grid(~ producto) +
    theme(panel.spacing.x = unit(0.8, "cm")) +
    labs(x = "Fecha", y = "Precio promedio") 
    
    
p1    
ggsave("precios-tipo-grifo.wmf", 
           path = here::here("plots"), device = "wmf", 
            dpi = 300, scale = 0.7)
    
data_db5 %>% 
    mutate(fecha = lubridate::dmy(fecha)) %>% 
    group_by(fecha, tipo) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    arrange(fecha) %>% 
    ggplot(aes(x = fecha, y = precio_promedio, color = tipo)) +
    geom_line() + 
    theme_bw() + 
    labs(x = "Fecha", y = "Precio promedio")


# Tabla con número de estaciones propias y abanderas ----------------------

grifos_sc <-
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))


precios_no_duplicates <- readRDS(here::here("data", "processed", "data_prices_2005_2018_no_duplicates.rds"))
estaciones_vivas_2017 <- precios_no_duplicates %>% 
    filter(lubridate::year(fecha_hora) >= 2017,
           departamento == "LIMA", provincia == "LIMA") %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>% 
    dplyr::select(codigo_de_osinergmin) %>% 
    pull()

grifos_sc %>% 
    count(tipo_bandera)

grifos_all <-
    readRDS(here::here("data", "processed", "grifo_coding_raw_no_duplicate.rds"))

grifos_all %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>%
    mutate(razon = case_when(
        str_detect(razon_social, "REPSOL") ~ "REPSOL",
        str_detect(razon_social, "COESTI") ~ "PRIMAX",
        str_detect(razon_social, "PERUANA DE ESTACIONES") ~ "PECSA",
        TRUE ~ "INDEPENDIENTE")) %>% 
    count(razon)
    
## para todo el perú

precios_no_duplicates %>% 
    filter(lubridate::year(fecha_hora) >= 2017) %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>%
    mutate(razon = case_when(
        str_detect(razon_social, "REPSOL") ~ "REPSOL",
        str_detect(razon_social, "COESTI") ~ "PRIMAX",
        str_detect(razon_social, "PERUANA DE ESTACIONES") ~ "PECSA",
        TRUE ~ "INDEPENDIENTE")) %>% 
    count(razon)

#lima
precios_no_duplicates %>% 
    filter(lubridate::year(fecha_hora) >= 2017,
           departamento == "LIMA", provincia == "LIMA") %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>%
    mutate(razon = case_when(
        str_detect(razon_social, "REPSOL") ~ "REPSOL",
        str_detect(razon_social, "COESTI") ~ "PRIMAX",
        str_detect(razon_social, "PERUANA DE ESTACIONES") ~ "PECSA",
        TRUE ~ "INDEPENDIENTE")) %>% 
    count(razon)


# Gráfico de estaciones en el mapa ----------------------------------------

library(maptools)
library(sp)
crs <- CRS("+init=epsg:32718")

'%ni%' <- Negate('%in%')
distritos <- readShapePoly(here::here("data","shapes-files","data-mapa-peru", "PER_adm3.shp"))

distritos_no <- c("Ancon", "San Juan de Lurigancho", "Carabayllo", 
                  "Puente Piedra", "Comas", "Villa Maria del Triunfo", 
                  "Cieneguilla", "Pachacamac", "Punta Hermosa", "Punta Negra",
                  "San Bartolo", "Santa Maria del Mar", "Santa Rosa", "Lurin",
                  "Pucusana", "Lurigancho", "Chaclacayo")

distritos_filter <- subset(distritos, NAME_1 == "Lima Province" & NAME_2 == "Lima" &
                               NAME_3 %ni% distritos_no)

plot(distritos_filter)
proj4string(distritos_filter) <- crs



grifos_sp <- grifos_sc
coordinates(grifos_sp) = c("lon", "lat")
#Proyectamos en UTM
proj4string(grifos_sp) <- crs

plot(distritos_filter, border="darkgrey")
#text(distritos_filter, labels=distritos_filter$NAME_3, cex=0.6, font=2, offset=0.5, adj=c(0,2))


points(grifos_sp, col = alpha("red", 0.3), pch = 16)

pointLabel(coordinates(distritos_filter), labels = distritos_filter$NAME_3, col= 1, cex = 0.6,
           allowSmallOverlap = F)


