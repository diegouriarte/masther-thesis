#' Saca distintos tipos de gráficos comparando PECSA con el resto de estaciones
#' 
#' 
#' Se prueban con distintos grupos de control
#' 
#' 
# Cargamos librerías y definición de tema ---------------------------
library(tidyverse)
library(lubridate)

'%ni%' <- Negate('%in%')

tema_grafica <- theme_bw() + 
  theme(
    panel.spacing.x = unit(0.8, "cm"),
    legend.background = element_rect(fill = "grey85", colour = "black", size = 0.5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(colour = "black"),
    legend.key = element_rect(colour = "black", size = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right")
 
f_labels <- data.frame(producto = c("G90", "DIESEL"), label = c("", "Venta de Pecsa"))

formato_comun <- list(
  #ylim(10,13), 
  geom_vline(xintercept = dmy("01-02-2018"),
             linetype = "dashed",
             size = 0.5, color = "grey55"),
  geom_rect(data = data.frame(xmin = dmy("15-01-2018"),
                              xmax = dmy("16-02-2018"),
                              ymin = -Inf,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5),
  geom_text(x = dmy("01-02-2018"), y = -Inf, vjust = -0.9, 
            hjust = -0.1, aes(label = label), data = f_labels,
            color = "grey55", fontface = "bold"),
  scale_color_brewer(palette = "Set1") )

# ====================
# Cargamos datos y definimos distritos donde esta PECSA ----------------------------
data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))


#' Lista de grifos que son vecinos a estación de PECSA 
#' 
lista_vecinos <- data_total %>% 
filter(vecino_pecsa_thiessen_did == 1) %>%
  distinct(codigo_de_osinergmin) %>% 
  pull()

distritos_con_pecsa <- data_total %>% 
  distinct(distrito, tipo_bandera) %>% 
  filter(tipo_bandera == "PROPIA PECSA") %>% 
  pull(distrito)

distritos_con_primax <- data_total %>% 
  distinct(distrito, tipo_bandera) %>% 
  filter(tipo_bandera == "PROPIA PRIMAX") %>% 
  pull(distrito)
# Generamos el gráfico original ----------- 
#'Considerando todas las estaciones en distritos donde PECSA está


(p2 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018"), 
           fecha >= dmy("1-07-2017"),
           distrito %in% distritos_con_pecsa) %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL"))) %>% 
    group_by(fecha, producto, tipo_bandera) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    filter(tipo_bandera %in% c("PROPIA PRIMAX", "PROPIA PECSA")) %>%
    mutate(tipo_bandera = fct_relevel(tipo_bandera, "PROPIA PRIMAX")) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = tipo_bandera),
              size = 1) +
    geom_line(data = data_total %>%
                filter(fecha <= dmy("2-10-2018"),
                       fecha >= dmy("1-07-2017"),
                       distrito %in% distritos_con_pecsa) %>%
                mutate(producto = fct_relevel(producto,c("G90","DIESEL"))) %>% 
                group_by(fecha, producto, tipo) %>%
                summarise(precio_promedio = mean(precio_de_venta)) ,
              aes(x = fecha, y = precio_promedio, color = tipo),
              size = 1.2) +
    facet_grid(factor(producto, levels = c("G90","DIESEL")) ~ .) +
    geom_vline(xintercept = dmy("01-02-2018"),
               linetype = "dashed",
               size = 1.2, color = "grey55") +
    geom_text(x = dmy("01-02-2018"), y = -Inf, vjust = -0.9, 
              hjust = -0.1, aes(label = label), data = f_labels,
              color = "grey55", fontface = "bold") +
    labs(x = "Fecha", y = "Precio promedio", 
         color = "Tipo de estación",
         linetype = "Venta Pecsa") +
    scale_color_brewer(palette = "Set1") + 
    tema_grafica
      
    )


ggsave("precios-tipo-grifo.png", 
       path = here::here("plots"), device = "png", unit = "cm",
       height = 15,
       width = 15,
       dpi = 300)

# Generamos una gráfica sin considerar grifos de Primax, y todos los distritos----------

(p3 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018"), 
           fecha >= dmy("1-07-2017"),
           tipo_bandera != "PROPIA PRIMAX") %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
           pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "RESTO")) %>% 
    group_by(fecha, producto, pecsa_no) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = pecsa_no),
              size = 1) +
    ylim(10,13) + 
    facet_grid(factor(producto, levels = c("G90","DIESEL")) ~ .) +
    geom_vline(xintercept = dmy("01-02-2018"),
               linetype = "dashed",
               size = 1.2, color = "grey55") +
    geom_text(x = dmy("01-02-2018"), y = -Inf, vjust = -0.9, 
              hjust = -0.1, aes(label = label), data = f_labels,
              color = "grey55", fontface = "bold") +
    labs(x = "Fecha", y = "Precio promedio", 
         linetype = "Venta Pecsa") +
    scale_color_brewer(palette = "Set1") + 
    tema_grafica
  
)




# Gráfica sin grifos de Primax Propios, todos los distritos-----------
#' y sin considerar vecinos a estaciones PECSA PROPIOS
#' 
#' 

(p4 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018"), 
           fecha >= dmy("1-07-2017"),
           tipo_bandera != "PROPIA PRIMAX",
           codigo_de_osinergmin %ni% lista_vecinos) %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
           pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "RESTO")) %>% 
    group_by(fecha, producto, pecsa_no) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = pecsa_no),
              size = 1) +
    ylim(10,13) + 
    facet_grid(factor(producto, levels = c("G90","DIESEL")) ~ .) +
    geom_vline(xintercept = dmy("01-02-2018"),
               linetype = "dashed",
               size = 1.2, color = "grey55") +
    geom_text(x = dmy("01-02-2018"), y = -Inf, vjust = -0.9, 
              hjust = -0.1, aes(label = label), data = f_labels,
              color = "grey55", fontface = "bold") +
    labs(x = "Fecha", y = "Precio promedio", 
         linetype = "Venta Pecsa") +
    scale_color_brewer(palette = "Set1") + 
    tema_grafica
  
)



#' Comparación distritos donde está PECSA con distritos donde no lo está ----------
#' 

(p5 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018"), 
           fecha >= dmy("1-07-2017"),
           tipo_bandera != "PROPIA PRIMAX",
           tipo_bandera == "PROPIA PECSA" | distrito %ni% distritos_con_pecsa) %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
           pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "RESTO")) %>% 
    group_by(fecha, producto, pecsa_no) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = pecsa_no),
              size = 1) +
    ylim(10,13) + 
    facet_grid(factor(producto, levels = c("G90","DIESEL")) ~ .) +
    geom_vline(xintercept = dmy("01-02-2018"),
               linetype = "dashed",
               size = 1.2, color = "grey55") +
    geom_text(x = dmy("01-02-2018"), y = -Inf, vjust = -0.9, 
              hjust = -0.1, aes(label = label), data = f_labels,
              color = "grey55", fontface = "bold") +
    labs(x = "Fecha", y = "Precio promedio", 
         linetype = "Venta Pecsa") +
    scale_color_brewer(palette = "Set1") + 
    tema_grafica
  
)

#' Lo hacemos por paneles distintos, no se consideran las propias de PRIMAX, 
#' promediando aquellos grifos que no son vecinos

(p6 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018"), 
           fecha >= dmy("1-07-2017"),
           tipo_bandera != "PROPIA PRIMAX",
           codigo_de_osinergmin %ni% lista_vecinos) %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
           pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "RESTO")) %>% 
    filter(producto == "G90") %>% 
    group_by(fecha, producto, pecsa_no) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = pecsa_no),
              size = 1) +
    labs(x = "Fecha", 
         y = "Precio promedio", 
         linetype = "Venta Pecsa",
         title = "Efecto en los precios promedio de Gasohol 90") +
    scale_color_brewer(palette = "Set1") + 
    tema_grafica +
    formato_comun
  
)

data_total %>% 
  filter(fecha <= dmy("2-10-2018"), 
         fecha >= dmy("1-07-2017"),
         tipo_bandera != "PROPIA PECSA",
         codigo_de_osinergmin %ni% lista_vecinos) %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         primax_no = if_else(tipo_bandera == "PROPIA PRIMAX", "PRIMAX", "RESTO")) %>% 
  filter(producto == "G90") %>% 
  group_by(fecha, producto, primax_no) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = fecha, y = precio_promedio, linetype = primax_no),
            size = 1) +
  labs(x = "Fecha", 
       y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Efecto en los precios promedio de Gasohol 90 en Primax") +
  scale_color_brewer(palette = "Set1") + 
  tema_grafica +
  formato_comun

(p7 <- data_total %>% 
    filter(fecha <= dmy("2-10-2018"), 
           fecha >= dmy("1-07-2017"),
           tipo_bandera != "PROPIA PRIMAX",
           codigo_de_osinergmin %ni% lista_vecinos) %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
           pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "RESTO")) %>% 
    filter(producto == "DIESEL") %>% 
    group_by(fecha, producto, pecsa_no) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = pecsa_no),
              size = 1) +
    labs(x = "Fecha", y = "Precio promedio", 
         linetype = "Venta Pecsa",
         title = "Efecto en los precios promedio de Diésel") +
    tema_grafica + 
    formato_comun
  
)

#' Gráfica de las vecinas versus las no vecinas
#' 
#' 
#' 
#' 

(p8 <- data_total %>% 
    filter(fecha <= dmy("2-08-2018"), 
           fecha >= dmy("1-07-2017"),
           tipo_bandera != "PROPIA PRIMAX",
           tipo_bandera != "PROPIA PECSA") %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
           vecina = if_else(codigo_de_osinergmin %in% lista_vecinos, "vecina", "no vecina")) %>% 
    filter(producto == "G90") %>% 
    group_by(fecha, producto, vecina) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, linetype = vecina),
              size = 1) +
    labs(x = "Fecha", y = "Precio promedio", 
         linetype = "Venta Pecsa",
         title = "Efecto en los precios promedio de G90 para estaciones vecinas versus control") +
    tema_grafica + 
    formato_comun
  
)



(p9 <- data_total %>% 
filter(fecha <= dmy("2-10-2018"), 
       fecha >= dmy("1-07-2017"),
       tipo_bandera != "PROPIA PRIMAX",
       tipo_bandera != "PROPIA PECSA") %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         vecina = if_else(codigo_de_osinergmin %in% lista_vecinos, "vecina", "no vecina")) %>% 
  filter(producto == "DIESEL") %>% 
  group_by(fecha, producto, vecina) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = fecha, y = precio_promedio, linetype = vecina),
            size = 1) +
  labs(x = "Fecha", y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Efecto en los precios promedio de Diésel para estaciones vecinas versus control") +
  tema_grafica + 
  formato_comun

)


#Ahora con datos semanales -----------------
  
data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones_semanal.rds")) %>% 
  mutate(semana_cont = if_else(año == 2017, semana, semana + 53))



formato_comun <- list(
  ylim(10,13), 
  geom_vline(
    # xintercept = 53+5,
    xintercept = dmy("01/02/2018"),
    linetype = "dashed",
    size = 0.5, color = "grey55"),
  # geom_rect(data = data.frame(xmin = 53,
  #                             xmax = 53+10,
  #                             ymin = -Inf,
  #                             ymax = Inf),
  #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = "grey", alpha = 0.5),
  geom_text(x = dmy("01/02/2018"), y = -Inf, vjust = -0.9, 
            hjust = -0.1, aes(label = label), data = f_labels,
            color = "grey55", fontface = "bold"),
  scale_color_brewer(palette = "Set1") )

#' Lista de grifos que son vecinos a estación de PECSA 
#' 
lista_vecinos <- data_total %>% 
  filter(vecino_pecsa_thiessen_did == 1) %>%
  distinct(codigo_de_osinergmin) %>% 
  pull()

distritos_con_pecsa <- data_total %>% 
  distinct(distrito, tipo_bandera) %>% 
  filter(tipo_bandera == "PROPIA PECSA") %>% 
  pull(distrito)

distritos_con_primax <- data_total %>% 
  distinct(distrito, tipo_bandera) %>% 
  filter(tipo_bandera == "PROPIA PRIMAX") %>% 
  pull(distrito)

# Gráfica de comparativa precios PECSA versus resto

data_total %>% 
    filter(
           codigo_de_osinergmin %ni% lista_vecinos,
           fecha >= dmy("01/07/2017") & fecha <= dmy("01/11/2018")) %>% 
    mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
           pecsa_no = case_when(
             tipo_bandera == "PROPIA PECSA" ~ "PECSA",
             tipo_bandera == "PROPIA PRIMAX" ~ "PRIMAX",
             TRUE ~ "NO VECINOS")) %>% 
    filter(producto == "G90") %>% 
    group_by(fecha, producto, pecsa_no) %>% 
    summarise(precio_promedio = mean(precio_de_venta)) %>% 
    ggplot() +
    geom_line(aes(x = fecha, y = precio_promedio, color = pecsa_no),
              size = 1) +
    labs(x = "Fecha", 
         y = "Precio promedio", 
         color = "Estaciones",
         title = "Precios promedio de Gasohol 90") +
    ylim(10.5,12.5) + 
    tema_grafica +
    formato_comun  
  # scale_x_continuous(minor_breaks = seq(0, 100, 5))

data_total %>% 
  filter(
         codigo_de_osinergmin %ni% lista_vecinos,
         fecha >= dmy("01/06/2017") & fecha <= dmy("01/11/2018")) %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "NO VECINOS")) %>% 
  filter(producto == "G90") %>% 
  group_by(fecha, producto, pecsa_no) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = fecha, y = precio_promedio, linetype = pecsa_no),
            size = 1) +
  labs(x = "Fecha", 
       y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Precios promedio de Gasohol 90") +
  ylim(10.5,12.5) + 
  tema_grafica +
  formato_comun  
# scale_x_continuous(minor_breaks = seq(0, 100, 5))


data_total %>% 
  filter(tipo_bandera != "PROPIA PRIMAX",
         codigo_de_osinergmin %ni% lista_vecinos,
         fecha >= dmy("01/10/2017") & fecha <= dmy("01/06/2018")) %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "RESTO")) %>% 
  filter(producto == "DIESEL") %>% 
  group_by(fecha, producto, pecsa_no) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = fecha, y = precio_promedio, linetype = pecsa_no),
            size = 1) +
  labs(x = "Fecha", 
       y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Efecto en los precios promedio de Diesel para PECSA") +
  # ylim(10,12) + 
  tema_grafica +
  formato_comun  
# scale_x_continuous(minor_breaks = seq(0, 100, 5))

data_total %>% 
  filter(tipo_bandera != "PROPIA PRIMAX",
         codigo_de_osinergmin %ni% lista_vecinos) %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         pecsa_no = if_else(tipo_bandera == "PROPIA PECSA", "PECSA", "RESTO"),
         semana_cont = if_else(año == 2017, semana, semana + 53)) %>% 
  filter(producto == "DIESEL") %>% 
  group_by(semana_cont, producto, pecsa_no) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = semana_cont, y = precio_promedio, linetype = pecsa_no),
            size = 1) +
  labs(x = "Fecha", 
       y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Efecto en los precios promedio de DB5") +
  scale_color_brewer(palette = "Set1") + 
  tema_grafica +
  formato_comun

data_total %>% 
  filter(tipo_bandera != "PROPIA PECSA",
         codigo_de_osinergmin %ni% lista_vecinos) %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         primax_no = if_else(tipo_bandera == "PROPIA PRIMAX", "PRIMAX", "RESTO"),
         semana_cont = if_else(año == 2017, semana, semana + 53)) %>% 
  filter(producto == "G90") %>% 
  group_by(semana_cont, producto, primax_no) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = semana_cont, y = precio_promedio, linetype = primax_no),
            size = 1) +
  labs(x = "Fecha", 
       y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Efecto en los precios promedio de Primax para Gasohol 90") +
  tema_grafica +
  formato_comun

data_total %>% 
  filter(tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | distrito %ni% distritos_con_primax) %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         primax_no = if_else(tipo_bandera == "PROPIA PRIMAX", "PRIMAX", "RESTO"),
         semana_cont = if_else(año == 2017, semana, semana + 53)) %>% 
  filter(producto == "G90") %>% 
  group_by(semana_cont, producto, primax_no) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = semana_cont, y = precio_promedio, linetype = primax_no),
            size = 1) +
  labs(x = "Fecha", 
       y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Efecto en los precios promedio de Primax para Gasohol 90") +
  tema_grafica +
  scale_x_continuous(minor_breaks = seq(0, 100, 5)) +
  formato_comun  

data_total %>% 
  filter(tipo_bandera != "PROPIA PECSA",
         codigo_de_osinergmin %ni% lista_vecinos) %>% 
  mutate(producto = fct_relevel(producto,c("G90","DIESEL")),
         primax_no = if_else(tipo_bandera == "PROPIA PRIMAX", "PRIMAX", "RESTO"),
         semana_cont = if_else(año == 2017, semana, semana + 53)) %>% 
  filter(producto == "DIESEL") %>% 
  group_by(semana_cont, producto, primax_no) %>% 
  summarise(precio_promedio = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_line(aes(x = semana_cont, y = precio_promedio, linetype = primax_no),
            size = 1) +
  labs(x = "Fecha", 
       y = "Precio promedio", 
       linetype = "Venta Pecsa",
       title = "Efecto en los precios promedio de Primax para Gasohol 90") +
  scale_color_brewer(palette = "Set1") + 
  tema_grafica +
  formato_comun

data_total %>% filter(semana_cont>=40 & semana_cont <= 45 , tipo_bandera=="PROPIA PECSA") %>% 
  group_by(semana, distrito) %>% 
  summarize(precio_distrito = mean(precio_de_venta)) %>% 
  ggplot() +
  geom_point(aes(x = semana, y = precio_distrito, colour = distrito))
  