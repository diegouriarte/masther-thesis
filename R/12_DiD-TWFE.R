
# Librerías =========
library(tidyverse)
library(lubridate)
source(here::here("R","funcion05_balancear-panel.R"), encoding = "UTF-8")
'%ni%' <- Negate('%in%')
# for robust standard error estimation
library(lmtest) 
# To calculate correct vcov matrix with 2WFE
library(multiwayvcov) 
# For a package way to do FE
library(lfe)
library(broom)
library(fixest)
# Cargamos datos ===========

data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))

data_total <- data_total %>% 
  mutate(
    distrito = factor(distrito)
  )

vecinos_pecsa_thiessen <- data_total %>% 
  filter(vecino_pecsa_thiessen == 1) %>%
  distinct(codigo_de_osinergmin) %>% 
  pull()

vecinos_primax_thiessen <- data_total %>% 
  filter(vecino_primax_thiessen == 1) %>%
  distinct(codigo_de_osinergmin) %>% 
  pull()

vecinos_primax_dist <- data_total %>% 
  filter(vecino_primax_dist == 1) %>%
  distinct(codigo_de_osinergmin) %>% 
  pull()

vecinos_pecsa_dist <- data_total %>% 
  filter(vecino_pecsa_dist == 1) %>%
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


# Event study  G90 PECSA ==============
event_study <- function(df, producto, cadena1, cadena2, leads, lags, controles, mes_trat) {
  #' INPUT
  #' df <- df con toda la data
  #' producto <- producto a evaluar G90 o DIESEL
  #' cadena1 <- la cadena a evaluar de pecsa o primax propia
  #' cadena2 <- la otra
  #' lead <- cuantos periodos antes del 0 (adquisición)
  #' lags <- cuanto periodos luego, en format de vector
  
  
  if (cadena1 == "PROPIA PECSA") {
    vecinos <- vecinos_pecsa_thiessen
    # mes_trat <- 14
  } else {
    vecinos <- vecinos_primax_thiessen
    # mes_trat <- 13
  }
  
  data_trans <- df %>% 
    filter(producto == !!producto) %>% 
    mutate(fecha_trat = if_else(tipo_bandera == !!cadena1, mes_trat, NA_real_)) %>% #considero febrero como mes de trat
    filter(tipo_bandera != !!cadena2,
           tipo_bandera == !!cadena1 | codigo_de_osinergmin %ni% vecinos,
           # codigo_de_osinergmin %ni% vecinos_pecsa_thiessen,
           # codigo_de_osinergmin %ni%  vecinos_primax_thiessen,
           # tipo_bandera == !!cadena1
           # tipo_bandera == !!cadena1 | codigo_de_osinergmin %ni% vecinos
           ) %>% 
  
    mutate(
      time_til = mes - fecha_trat,
      lead1 = case_when(time_til == -1 ~ 1, TRUE ~ 0),
      lead2 = case_when(time_til == -2 ~ 1, TRUE ~ 0),
      lead3 = case_when(time_til == -3 ~ 1, TRUE ~ 0),
      lead4 = case_when(time_til == -4 ~ 1, TRUE ~ 0),
      lead5 = case_when(time_til == -5 ~ 1, TRUE ~ 0),
      lead6 = case_when(time_til == -6 ~ 1, TRUE ~ 0),
      lead7 = case_when(time_til == -7 ~ 1, TRUE ~ 0),
      lead8 = case_when(time_til == -8 ~ 1, TRUE ~ 0),
      lead9 = case_when(time_til == -9 ~ 1, TRUE ~ 0),
      lead10 = case_when(time_til == -10 ~ 1, TRUE ~ 0),
      lead11 = case_when(time_til == -11 ~ 1, TRUE ~ 0),
      lead12 = case_when(time_til == -12 ~ 1, TRUE ~ 0),
      lead13 = case_when(time_til == -13 ~ 1, TRUE ~ 0),
      
      lag0 = case_when(time_til == 0 ~ 1, TRUE ~ 0),
      lag1 = case_when(time_til == 1 ~ 1, TRUE ~ 0),
      lag2 = case_when(time_til == 2 ~ 1, TRUE ~ 0),
      lag3 = case_when(time_til == 3 ~ 1, TRUE ~ 0),
      lag4 = case_when(time_til == 4 ~ 1, TRUE ~ 0),
      lag5 = case_when(time_til == 5 ~ 1, TRUE ~ 0),
      lag6 = case_when(time_til == 6 ~ 1, TRUE ~ 0),
      lag7 = case_when(time_til == 7 ~ 1, TRUE ~ 0),
      lag8 = case_when(time_til == 8 ~ 1, TRUE ~ 0),
      mes = relevel(factor(mes), ref = mes_trat),
    )
  event_study_formula <- as.formula(
    paste("precio_de_venta ~ + ",
          paste(
            paste(paste("lead", leads, sep = ""), collapse = " + "),
            paste(paste("lag", lags, sep = ""), collapse = " + "), sep = " + "),
            controles
              )
  )
  
  event_study_reg <- felm(event_study_formula, data = data_trans, exactDOF = TRUE)
  event_summary_reg <- summary(event_study_reg)
  print(eventstudy.plot(event_summary_reg, leads, lags))
  list("regresion" = event_summary_reg, "leads" = leads, "lags" = lags)
  
  
}

eventstudy.plot <- function(event_study_reg, leads, lags) {
  #' event_study_reg <- resultado de la func event_study
  #' 
  plot_order <- c(paste("lead", rev(leads), sep = ""), 
                  paste("lag", lags, sep = ""))
  leadslags_plot <- tibble(
    sd = c(event_study_reg$coefficients[plot_order,2], 0),
    mean = c(event_study_reg$coefficients[plot_order,1], 0),
    label = c(as.integer(c(paste(-rev(leads)), paste(lags))), 0)
  )
  
  leadslags_plot %>%
    ggplot(aes(x = label, y = mean,
               ymin = mean-1.96*sd, 
               ymax = mean+1.96*sd)) +
    # geom_hline(yintercept = 0.035169444, color = "red") +
    geom_pointrange() +
    theme_minimal() +
    xlab("Meses antes y después de la consolidación") +
    ylab("Precio") +
    geom_hline(yintercept = 0,
               linetype = "dashed") +
    geom_vline(xintercept = 0,
               linetype = "dashed")
}

controles <- "| codigo_de_osinergmin + mes | 0 | codigo_de_osinergmin"

g90_pecsa <- event_study(data_total,
  producto = "G90",
  cadena1 = "PROPIA PECSA",
  cadena2 = "PROPIA PRIMAX",
  leads = 1:5,
  lags = 1:8,
  controles,
  mes_trat = 14
)

data_total %>% 
  filter(producto == "G90") %>% 
  mutate(fecha_trat = if_else(tipo_bandera == "PROPIA PRIMAX", 13, NA_real_)) %>% #considero febrero como mes de trat
  filter(tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | codigo_de_osinergmin %ni% vecinos_primax_thiessen
  ) %>% 
  mutate(
    time_til = mes - fecha_trat,
    lead1 = case_when(time_til == -1 ~ 1, TRUE ~ 0),
    lead2 = case_when(time_til == -2 ~ 1, TRUE ~ 0),
    lead3 = case_when(time_til == -3 ~ 1, TRUE ~ 0),
    lead4 = case_when(time_til == -4 ~ 1, TRUE ~ 0),
    lead5 = case_when(time_til == -5 ~ 1, TRUE ~ 0),
    lead6 = case_when(time_til == -6 ~ 1, TRUE ~ 0),
    lead7 = case_when(time_til == -7 ~ 1, TRUE ~ 0),
    lead8 = case_when(time_til == -8 ~ 1, TRUE ~ 0),
    lead9 = case_when(time_til == -9 ~ 1, TRUE ~ 0),
    lead10 = case_when(time_til == -10 ~ 1, TRUE ~ 0),
    lead11 = case_when(time_til == -11 ~ 1, TRUE ~ 0),
    lead12 = case_when(time_til == -12 ~ 1, TRUE ~ 0),
    lead13 = case_when(time_til == -13 ~ 1, TRUE ~ 0),
    
    lag0 = case_when(time_til == 0 ~ 1, TRUE ~ 0),
    lag1 = case_when(time_til == 1 ~ 1, TRUE ~ 0),
    lag2 = case_when(time_til == 2 ~ 1, TRUE ~ 0),
    lag3 = case_when(time_til == 3 ~ 1, TRUE ~ 0),
    lag4 = case_when(time_til == 4 ~ 1, TRUE ~ 0),
    lag5 = case_when(time_til == 5 ~ 1, TRUE ~ 0),
    lag6 = case_when(time_til == 6 ~ 1, TRUE ~ 0),
    lag7 = case_when(time_til == 7 ~ 1, TRUE ~ 0),
    lag8 = case_when(time_til == 8 ~ 1, TRUE ~ 0),
    mes = relevel(factor(mes), ref = 14),
  ) %>% 
  pull(mes)

controles <- "| codigo_de_osinergmin + mes + num_grifos_cerc + distrito + bandera + tipo + distancia_prom_term| 0 | codigo_de_osinergmin"



g90_pecsa <- event_study(data_total,
                         producto = "G90",
                         cadena1 = "PROPIA PECSA",
                         cadena2 = "PROPIA PRIMAX",
                         leads = 1:5,
                         lags = 1:8,
                         controles
)

# Event study  G90 PRIMAX ==============
controles <- "| codigo_de_osinergmin + mes | 0 | codigo_de_osinergmin"

data_cocina <- data_total %>% 
  mutate(precio_de_venta = if_else(tipo_bandera == "PROPIA PRIMAX" & mes <= 14,
                                   precio_de_venta + 0,
                                   precio_de_venta))
data_cocina_2 <- data_total %>% 
  mutate(precio_de_venta = if_else(codigo_de_osinergmin %ni% vecinos_primax_thiessen & mes == 14 &
                                     tipo_bandera != "PROPIA PRIMAX",
                                   precio_de_venta + 0.8,
                                   precio_de_venta))
  
g90_primax <- event_study(data_cocina_2 %>% filter(mes >= 2), producto = "G90",
                         cadena1 = "PROPIA PRIMAX",
                         cadena2 = "PROPIA PECSA", 
                         leads = 1:5, 
                         lags = 1:8,
                         controles,
                         mes_trat = 14) 


# Metodo TWFE de acuerdo con libro====



#' Variable de tratamiento
#' 

#' PRIMAX G90
#' 
#' #thissen
data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PRIMAX") %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | codigo_de_osinergmin %ni% vecinos_primax_thiessen,
         producto == "G90") 

clfe <- feols(precio_de_venta ~ i(mes, est_trat, ref = 13) |codigo_de_osinergmin + mes,
              data = data_total_1)

coefplot(clfe)
summary(clfe)
# elijo como ref= 13 porque es el último periodo antes del tratamiento

#' #distancian
data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PRIMAX") %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | codigo_de_osinergmin %ni% vecinos_primax_dist,
         producto == "G90") 

clfe <- feols(precio_de_venta ~ i(mes, est_trat, ref = 13) | codigo_de_osinergmin + mes,
              data = data_total_1)

coefplot(clfe)
summary(clfe)

#' PECSA G90
#' 
#' thiessen
data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PECSA") %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PRIMAX",
         tipo_bandera == "PROPIA PECSA" | codigo_de_osinergmin %ni% vecinos_pecsa_thiessen,
         producto == "G90") 

clfe <- feols(precio_de_venta ~ i(mes, est_trat, ref = 13) | codigo_de_osinergmin + mes,
              data = data_total_1)


coefplot(clfe)

#' thiessen
data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PECSA") %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PRIMAX",
         tipo_bandera == "PROPIA PECSA" | codigo_de_osinergmin %ni% vecinos_pecsa_dist,
         producto == "G90") 

clfe <- feols(precio_de_venta ~ i(mes, est_trat, ref = 13) | codigo_de_osinergmin + mes,
              data = data_total_1)


coefplot(clfe)


#' PRIMAX DIESEL
data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PRIMAX") %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | codigo_de_osinergmin %ni% vecinos_primax_thiessen,
         producto == "DIESEL") 

clfe <- feols(precio_de_venta ~ i(mes, est_trat, ref = 13) | codigo_de_osinergmin + mes,
              data = data_total_1)

coefplot(clfe)


#' PECSA DIESEL
data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PECSA") %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PRIMAX",
         tipo_bandera == "PROPIA PECSA" | codigo_de_osinergmin %ni% vecinos_pecsa_thiessen,
         producto == "DIESEL") 

clfe <- feols(precio_de_venta ~ i(mes, est_trat, ref = 13) | codigo_de_osinergmin + mes,
              data = data_total_1)


coefplot(clfe)

# Ahora, hagamos lo mismo pero agrupando, un solo efecto

#Primax G90

data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PRIMAX" & mes >= 14) %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | codigo_de_osinergmin %ni% vecinos_primax_thiessen,
         producto == "G90") 

clfe <- feols(precio_de_venta ~ est_trat | codigo_de_osinergmin + mes,
              data = data_total_1)

summary(clfe)


# Ahora con time trends:

data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PRIMAX",
         at = mes>= 14, 
         at = factor(at),
         est_trat = factor(est_trat),
         mes_factor = factor(mes)) %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | codigo_de_osinergmin %ni% vecinos_primax_thiessen,
         producto == "G90") #%>% 
  # mutate(
  #   mes = relevel(factor(mes), ref = 13)
  # )

clfe <- feols(precio_de_venta ~ est_trat*mes  |codigo_de_osinergmin,
              data = data_total_1)

summary(clfe)

data_total_1 <- data_total %>% 
  mutate(est_trat = tipo_bandera == "PROPIA PRIMAX" & mes >= 14, 
         tau = if_else(tipo_bandera == "PROPIA PRIMAX", 1*mes, 0*mes)) %>% 
  filter(mes >= 8 & mes <= 22,
         tipo_bandera != "PROPIA PECSA",
         tipo_bandera == "PROPIA PRIMAX" | codigo_de_osinergmin %ni% vecinos_primax_thiessen,
         producto == "G90") 

clfe <- feols(precio_de_venta ~ est_trat + tau| codigo_de_osinergmin + mes,
              data = data_total_1)

summary(clfe)
