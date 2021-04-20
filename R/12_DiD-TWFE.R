
# Librerías =========
library(tidyverse)
library(lubridate)
source(here::here("R","funcion05_balancear-panel.R"), encoding = "UTF-8")
'%ni%' <- Negate('%in%')
library(did)
# for robust standard error estimation
library(lmtest) 
# To calculate correct vcov matrix with 2WFE
library(multiwayvcov) 
# For a package way to do FE
library(plm)
library(bacondecomp) 

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

#Formatos comunes ==========
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
  ylim(10,13), 
  geom_vline(
    xintercept = dmy("01/02/2018"),
    linetype = "dashed",
    size = 0.5, color = "grey55"),
  geom_text(x = dmy("01/02/2018"), y = -Inf, vjust = -0.9, 
            hjust = -0.1, aes(label = label), data = f_labels,
            color = "grey55", fontface = "bold"),
  scale_color_brewer(palette = "Set1") )


# Paquete DID ========

data_g90 <- balancear_panel(data_total_mensual, prod = "G90", c("01-01-2017", "01-10-2018"))
data_g90 <- data_g90 %>% 
  mutate(mes = (year(fecha)-2017)*12 + month(fecha),
         fecha_trat = if_else(tipo_bandera == "PROPIA PECSA", 14, 0)) %>% 
  filter(tipo_bandera != "PROPIA PRIMAX",
         tipo_bandera == "PROPIA PECSA" | codigo_de_osinergmin %ni% lista_vecinos)

out_1 <- att_gt(yname = "precio_de_venta",
                gname = "fecha_trat",
                idname = "codigo_de_osinergmin",
                tname = "mes",
                xformla = ~codigo_de_osinergmin,
                data = data_g90,
                est_method = "dr",
                anticipation = 0,
                bstrap = F,
                cband = F,
)

ggdid(out_1, type = "attgt") 

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

#Event study ==============

data_g90_lm <- data_g90 %>% 
  mutate(mes_cont = if_else(año == 2017, mes, mes + 12),
         Di1 = 0,
         Di2 = 0,
         Di3 = 0,
         Di4 = 0,
         Di5 = 0,
         Di6 = 0,
         Di7 = 0,
         Di8 = 0,
         Di9 = 0,
         Di10 = 0,
         Di11 = 0,
         Di12 = 0,
         Di13 = 0,
         Di14 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 14, 1, 0),
         Di15 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 15, 1, 0),
         Di16 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 16, 1, 0),
         Di17 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 17, 1, 0),
         Di18 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 18, 1, 0),
         Di19 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 19, 1, 0),
         Di20 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 20, 1, 0),
         Di21 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 21, 1, 0),
         Di22 = if_else(tipo_bandera == "PROPIA PECSA" & mes_cont == 22, 1, 0))
         
  fastDummies::dummy_columns(., select_columns = "mes_cont") %>% 
  select(fecha, starts_with("mes"))

modelo <- precio_de_venta ~  
  Di2 + Di3 + Di4 + Di5 + Di6 + 
  Di7 + Di8 + Di9 + Di10 + Di11 + Di12 + Di13 +
  Di14 + Di15 + Di16 + Di17 + Di18 + Di19 + Di20 +
  Di21 + Di22 + factor(codigo_de_osinergmin) + factor(mes_cont)

fit_tw <- lm(modelo, 
             data = data_g90_lm)

summary(fit_tw)

#Nuevo intento estudio ============
library(lfe)

data_g90 <- balancear_panel(data_total_mensual, prod = "G90", c("01-01-2017", "01-10-2018"))
data_g90_reg <- data_g90 %>% 
  mutate(mes = (year(fecha)-2017)*12 + month(fecha),
         fecha_trat = if_else(tipo_bandera == "PROPIA PECSA", 14, NA_real_)) %>% 
  filter(tipo_bandera != "PROPIA PRIMAX",
         tipo_bandera == "PROPIA PECSA" | codigo_de_osinergmin %ni% lista_vecinos) %>% 
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
    
  )

event_study_formula <- as.formula(
  paste("precio_de_venta ~ + ",
        paste(
          paste(paste("lead", 1:5, sep = ""), collapse = " + "),
          paste(paste("lag", 1:8, sep = ""), collapse = " + "), sep = " + "),
        "| mes + codigo_de_osinergmin + tipo_bandera + distrito| 0 | codigo_de_osinergmin"
  ),
)

event_study_reg <- felm(event_study_formula, , data = data_g90_reg, exactDOF = TRUE)
summary(event_study_reg)
a