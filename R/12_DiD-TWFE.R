
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

'%ni%' <- Negate('%in%')

# Cargamos datos ===========

data_total_semanal <- readRDS(file = here::here("data", "processed", "data-final-regresiones_semanal.rds")) %>% 
  mutate(semana_cont = if_else(año == 2017, semana, semana + 53))

data_total_mensual <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))

data_total <- data_total_semanal

lista_vecinos <- data_total %>% 
  filter(vecino_pecsa_thiessen_did == 1) %>%
  distinct(codigo_de_osinergmin) %>% 
  pull()

lista_vecinos_km <- data_total %>% 
  filter(vecino_pecsa_dist_did == 1) %>%
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

# Two-way fix effects ===========
#' Agregando todo antes y después

data_g90 <- data_g90 %>% 
  mutate(codigo_de_osinergmin = factor(codigo_de_osinergmin),
         distrito = factor(distrito))

g90_panel <- pdata.frame(data_g90, c("codigo_de_osinergmin", "fecha"))
modelo <- precio_de_venta ~ COMPRADA_FE + factor(distrito)

fe <- plm(modelo, data= g90_panel, 
          index = c("codigo_de_osinergmin", "fecha"),
          model="within",
          effect = "twoways")

summary(fe)

fixef(fe, effect = "time")
fixef(fe, effect = 'individual') 
coeftest(fe, vcov = vcovHC(fe, type = "sss", cluster = "group"))

prueba <- plm(precio_de_venta ~ COMPRADA_FE + factor(bandera) + factor(fecha),
    data = g90_panel,
    model = "within",
    effect = "individual")

summary(prueba)

# Event study  G90 PECSA ==============
event_study <- function(df, producto, cadena1,cadena2, leads, lags) {
  #' INPUT
  #' df <- df con toda la data
  #' producto <- producto a evaluar G90 o DIESEL
  #' cadena1 <- la cadena a evaluar de pecsa o primax propia
  #' cadena2 <- la otra
  #' lead <- cuantos periodos antes del 0 (adquisición)
  #' lags <- cuanto periodos luego, en format de vector
  data_trans <- df %>% 
    filter(producto == !!producto) %>% 
    mutate(mes = (year(fecha)-2017)*12 + month(fecha),
           fecha_trat = if_else(tipo_bandera == !!cadena1, 14, NA_real_)) %>%
           # fecha_trat = if_else(tipo_bandera == !!cadena1, 13, NA_real_)) %>% 
  
    filter(tipo_bandera != !!cadena2,
           # tipo_bandera == !!cadena1 | codigo_de_osinergmin %ni% lista_vecinos) %>% 
             tipo_bandera == !!cadena1 | distrito %ni% distritos_con_primax) %>% 
  
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
      # lag9 = case_when(time_til == 8 ~ 1, TRUE ~ 0),
      mes = factor(mes),
      
      
    )
  event_study_formula <- as.formula(
    paste("precio_de_venta ~ + ",
          paste(
            paste(paste("lead", leads, sep = ""), collapse = " + "),
            paste(paste("lag", lags, sep = ""), collapse = " + "), sep = " + "),
          # "| mes + codigo_de_osinergmin + tipo_bandera + distrito| 0 | codigo_de_osinergmin"
           "| codigo_de_osinergmin + mes | 0 | codigo_de_osinergmin"
          
              ),
  )
  
  event_study_reg <- felm(event_study_formula, data = data_trans, exactDOF = TRUE)
  event_summary_reg <- summary(event_study_reg)
  print(eventstudy.plot(event_summary_reg, leads, lags))
  list("regresion" = event_summary_reg, "leads" = leads, "lags" = lags)
  
  
}

event_study_1 <- function(df, producto, cadena1,cadena2, leads, lags) {
  #' INPUT
  #' df <- df con toda la data
  #' producto <- producto a evaluar G90 o DIESEL
  #' cadena1 <- la cadena a evaluar de pecsa o primax propia
  #' cadena2 <- la otra
  #' lead <- cuantos periodos antes del 0 (adquisición)
  #' lags <- cuanto periodos luego, en format de vector
  data_trans <- df %>% 
    filter(producto == !!producto) %>% 
    mutate(mes = (year(fecha)-2017)*12 + month(fecha),
           fecha_trat = if_else(tipo_bandera == !!cadena1, 14, NA_real_)) %>%
    # fecha_trat = if_else(tipo_bandera == !!cadena1, 13, NA_real_)) %>% 
    
    filter(tipo_bandera != !!cadena2,
           # tipo_bandera == !!cadena1 | codigo_de_osinergmin %ni% lista_vecinos) %>% 
           tipo_bandera == !!cadena1 | distrito %ni% distritos_con_primax) %>% 
    
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
      # lag9 = case_when(time_til == 8 ~ 1, TRUE ~ 0),
      mes = factor(mes),
      
      
    )
  event_study_formula <- as.formula(
    paste("precio_de_venta ~ + ",
          paste(
            paste(paste("lead", leads, sep = ""), collapse = " + "),
            paste(paste("lag", lags, sep = ""), collapse = " + "), sep = " + "),
          # "| mes + codigo_de_osinergmin + tipo_bandera + distrito| 0 | codigo_de_osinergmin"
          "| codigo_de_osinergmin + mes | 0 | 0"
          
    ),
  )
  
  event_study_reg <- felm(event_study_formula, data = data_trans, exactDOF = TRUE)
  event_summary_reg <- summary(event_study_reg)
  print(eventstudy.plot_1(event_summary_reg, leads, lags))
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

eventstudy.plot_1 <- function(event_study_reg, leads, lags) {
  #' event_study_reg <- resultado de la func event_study
  #' 
  plot_order <- c(paste("lead", rev(leads), sep = ""), 
                  paste("lag", lags, sep = ""))
  leadslags_plot <- tibble(
    sd = c(event_study_reg$coefficients[plot_order,2]),
    mean = c(event_study_reg$coefficients[plot_order,1]),
    label = c(as.integer(c(paste(-rev(leads)), paste(lags))))
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

g90_pecsa <- event_study(data_total_mensual, producto = "G90",
                            cadena1 = "PROPIA PECSA",
                            cadena2 = "PROPIA PRIMAX", 
                            leads = 1:12, 
                            lags = 1:8) 

# Event study  G90 PRIMAX ==============

g90_primax <- event_study(data_total_mensual, producto = "G90",
                         cadena1 = "PROPIA PRIMAX",
                         cadena2 = "PROPIA PECSA", 
                         leads = 1:13, 
                         lags = 0:8) 

g90_primax <- event_study_1(data_total_mensual, producto = "G90",
                          cadena1 = "PROPIA PRIMAX",
                          cadena2 = "PROPIA PECSA", 
                          leads = 1:8, 
                          lags = 1:8) 
# Event study  Diesel PECSA ==============

diesel_pecsa <- event_study(data_total_mensual, producto = "DIESEL",
                      cadena1 = "PROPIA PECSA",
                      cadena2 = "PROPIA PRIMAX", 
                      leads = 2:13, 
                      lags = 0:8) 


# Event study  Diesel PRIMAX ==============


diesel_primax <- event_study(data_total_mensual, producto = "DIESEL",
                            cadena1 = "PROPIA PRIMAX",
                            cadena2 = "PROPIA PECSA", 
                            leads = 1:13, 
                            lags = 1:8) 
