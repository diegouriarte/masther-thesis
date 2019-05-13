library(readxl)
library(tidyverse)
library(lubridate)

# Definición de funciones -------------------------------------------------


anadir_anio <- function(df, anio) {
  #para añadir el año al nombre de variables en df
  df %>% 
    rename_at(vars(-1), ~ str_c(., "_", anio))
}

full_join_multiple <- function(...){
  df1 = list(...)[[1]]
  df2 = list(...)[[2]]
  col1 = colnames(df1)[1]
  col2 = colnames(df2)[1]
  xxx = full_join(..., by = setNames(col2,col1))
  return(xxx)
}


# Cargamos los archivos de los excel en la carpeta ------------------------

raiz <- here::here("data","minem")
rutas_file <- str_c(raiz, "/", list.files(raiz))

#leemos los archivos de excel
t <- map(rutas_file, ~ read_excel(.x, col_names = TRUE, range = "C3:E26", na = c("", "-")))
nombres_anios <- map(rutas_file, ~ read_excel(.x, col_names = F, range = "C2:C2", na = c("", "-")))

#corregimos un par de excel mal importados
colnames(t[[21]]) <- t[[21]][1,]
t[[21]] <- t[[21]][-1:-2, ] %>% 
  mutate_at(vars(2, 3), as.double)
rutas_file[[22]]
colnames(t[[22]]) <- c("PRODUCTO", "FEBRERO 2017", "MARZO 2017") 

# obtenemos una lista de años
lista_anios <- map_chr(nombres_anios, pull) %>% 
  str_remove("VENTAS TOTALES  DE COMBUSTIBLES EN EL PAÍS - ") %>% 
  str_remove(fixed("\n(MBPD)")) %>% 
  str_remove("PRODUCTO") %>% 
  str_remove(fixed("VENTAS TOTALES  DE COMBUSTIBLES EN EL PAÍS"))



df_con_anios <- map2(t, lista_anios, anadir_anio)

df <- Reduce(full_join_multiple, df_con_anios)


# Creamos tidy dataframe para plot ----------------------------------------


df_clean_names <- df %>% 
  select(-ends_with(".y"), -ends_with("ENERO_2018")) %>% 
  select(-matches(fixed("ABRIL 2017_")), -contains("3_2018")) %>% 
  rename_at(vars(ends_with(".x")), str_remove, ".x") %>% 
  rename_all(str_replace, "_", " ")  %>% 
  rename_all(str_trim)

meses <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", 
           "AGOSTO", "SETIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

meses_number <- c("ENERO" = 1, "FEBRERO" = 2, "MARZO" = 3,
                  "ABRIL" = 4, "MAYO" = 5, "JUNIO" = 6, 
                  "JULIO" = 7, "AGOSTO" = 8, "SETIEMBRE" = 9, 
                  "OCTUBRE" = 10, "NOVIEMBRE" = 11, "DICIEMBRE" = 12)

tidy_df <- df_clean_names %>% 
  filter(str_detect(PRODUCTO, "Gasohol") |str_detect(PRODUCTO, "S-50")) %>% 
  gather(key = "Mes-Año", value = "Cantidad", -PRODUCTO) %>% 
  separate(`Mes-Año`, into = c("Mes", "Año"), sep = " ", convert = T) %>% 
  mutate(Mes = factor(Mes, meses),
         Mes_numero = recode(Mes, !!!meses_number),
         fecha = dmy(str_c("01", Mes_numero, `Año`, sep = "-")),
         tipo_comb = if_else(PRODUCTO == "Diesel B-5 (S-50)", "DIESEL", "GASOHOL"),
         PRODUCTO = factor(PRODUCTO) %>%  fct_relevel("Diesel B-5 (S-50)",
                                                      "Gasohol 90",
                                                      "Gasohol 95")) %>% 
  filter(fecha != dmy("1-12-2019")) 
#Gráficas==================

tema_tesis <- theme_bw() +
  theme(
    panel.spacing.x = unit(0.8, "cm"),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(colour = "black"),
    legend.key = element_rect(colour = "black", size = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

p1 <- tidy_df %>% 
  ggplot(aes(y = Cantidad, x = fecha, color = PRODUCTO)) + 
  geom_line() +
  geom_point(size = 1.3) + 
  scale_color_brewer(palette = "Accent") +
  tema_tesis

p1

p2 <- tidy_df %>% 
  ggplot(aes(y = Cantidad, x = fecha, fill = PRODUCTO)) + 
  geom_area(position = "fill", colour = "black", size = 0.2, alpha = .4) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Accent") +
  labs(y = "Venta total de combustible (%)",
         x = "Fecha",
         PRODUCTO = "Producto") +
  tema_tesis 

p2

ggsave("ventas_combustible.png", 
       path = here::here("plots"), device = "png", unit = "cm",
       height = 10,
       width = 10)
