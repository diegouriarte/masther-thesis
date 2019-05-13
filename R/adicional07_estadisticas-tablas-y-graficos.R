
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
library(lubridate)

# Cargamos datos
data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))



p1 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018")) %>% 
    group_by(fecha, producto, tipo) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    arrange(fecha) %>% 
    ggplot(aes(x = fecha, y = precio_promedio, color = tipo)) +
    geom_line() +
    geom_vline(xintercept = dmy("01-02-2018"),
               linetype = "dashed",
               size = 1.2) +
    annotate("text", x = dmy("01-02-2018"), y = -Inf, vjust = -0.9, 
             hjust = -0.05, label = "Venta de Pecsa") +
    facet_grid(~ producto) +
    theme(panel.spacing.x = unit(0.8, "cm")) +
    labs(x = "Fecha", y = "Precio promedio", color = "Tipo de estación") +
    theme_bw() + 
    scale_color_brewer(palette = "Set1") + 
    theme(
        legend.background = element_rect(fill = "grey85", colour = "black", size = 1),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(colour = "black"),
        legend.key = element_rect(colour = "black", size = 0.25),
        axis.title = element_text(face = "bold")
    )
    
p1 

distritos_con_pecsa <- data_total %>% 
    distinct(distrito, tipo_bandera) %>% 
    filter(tipo_bandera == "PROPIA PECSA") %>% 
    pull(distrito)



f_labels <- data.frame(producto = c("DIESEL", "G90"), label = c("", "Venta de Pecsa"))


(p2 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018"),
           distrito %in% distritos_con_pecsa) %>% 
    group_by(fecha, producto, tipo_bandera) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    filter(tipo_bandera %in% c("PROPIA PRIMAX", "PROPIA PECSA")) %>%
    mutate(tipo_bandera = fct_relevel(tipo_bandera, "PROPIA PRIMAX")) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = tipo_bandera),
              size = 1) +
    geom_line(data = data_total %>%
                  filter(fecha <= dmy("2-10-2018"),
                         distrito %in% distritos_con_pecsa) %>%
                  group_by(fecha, producto, tipo) %>%
                  summarise(precio_promedio = mean(precio_de_venta)) ,
              aes(x = fecha, y = precio_promedio, color = tipo),
              size = 1.2) +
    facet_grid(producto ~ .) +
    geom_vline(xintercept = dmy("01-02-2018"),
               linetype = "dashed",
               size = 1.2, color = "grey55") +
    geom_text(x = dmy("01-02-2018"), y = -Inf, vjust = -0.9, 
              hjust = -0.1, aes(label = label), data = f_labels,
              color = "grey55", fontface = "bold") +
    labs(x = "Fecha", y = "Precio promedio", 
         color = "Tipo de estación",
         linetype = "Venta Pecsa") +
    theme_bw() + 
    scale_color_brewer(palette = "Set1") + 
    theme(
        panel.spacing.x = unit(0.8, "cm"),
        legend.background = element_rect(fill = "grey85", colour = "black", size = 0.5),
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(colour = "black"),
        legend.key = element_rect(colour = "black", size = 0.5),
        axis.title = element_text(face = "bold"),
        legend.position = "right"
        
    )
)

ggsave("precios-tipo-grifo.png", 
       path = here::here("plots"), device = "png", unit = "cm",
       height = 15,
       width = 15,
       dpi = 300)
    
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


