#' ---
#' title: Regresión panel
#' author: "Diego Uriarte"
#' date: `r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' 

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    error = TRUE
)
options(tidyverse.quiet = TRUE)

#+ body
# ----
library(conflicted)
library(tidyverse)
library(splm)
conflict_prefer("filter", "dplyr")
# ----


#' 
#' ### Cargamos datos
#' 
#+ cargar-datos
grifos_sc <-
    readRDS(here::here("data", "processed", "grifos_con_sc_razon_social.RDS"))

#' cargamos los precios para 2017 de DB5

#+ precios-cargar
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
        mutate(fecha = lubridate::dmy(fecha)) %>% 
        select(-dia) %>%
        filter(
            mes != 13,
            precio_de_venta > 6,
            fecha <= lubridate::dmy("2-10-2018")
        )
)


precios_db5 <- data_precios[[1]]

precios_g90 <- data_precios[[2]]

#' cargamos data de distritos

#+ data-distritos
data_distrital_raw <- read_csv(here::here("data", "demo-distrital", "data_pop_lima.csv")) %>%
    janitor::clean_names()

#' Limpiamos archivo distrital:
#' 
#+ clean-distrito
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

# numero de viajes en diciembre de 2017 x distrito
viajes_distrito <- readRDS(here::here("data",
                                      "processed",
                                      "data_viajes_distritos.rds"))

head(viajes_distrito)

#'Todo ok, así que hacemos el merge:
#+ join-big-data

data_distrital_clean <- left_join(data_distrital_clean, viajes_distrito, by = "distrito")

#' 
#' ## Creamos archivos con info
#' 
#+merge-data-distrito 

data_total <- map(
    list(precios_db5, precios_g90),
    ~ left_join(grifos_sc, .x, by = "codigo_de_osinergmin") %>%
        left_join(., data_distrital_clean, by = "distrito") %>% 
        mutate(sc = if_else(`año` == 2017, sc_pre, sc_post),
               ingresos_2012 = ingresos_2012 / 1000,
               densidad_2017 = densidad_2017/10000)
)

data_db5 <- data_total[[1]]
data_g90 <- data_total[[2]]

#' ### Creamos nuevas variables que cambian con la compra
#' 
#' Realizamos un modelo de efectos fijos a nivel de estación y en el tiempo
#' Primero debemos crear una variable que sea `COMPRADA` igual a 1 si si la estación la compró Primax, 0 lo contrario. Es decir, hasta Diciembre es 0 para todos, y luego 1 solo para las propias PECSA. Otra variable `vecino_comprado` si el grifo i tiene en su vecindad un grifo comprado por Primax. 


## Primero solo con variable comprada para estaciones PECSA

#filtramos la data mes octubre en adelante que no está bien calculada:

data_db5_comprada <- data_db5 %>%
    mutate(COMPRADA = case_when(`año` == 2017 ~ 0,
                                tipo_bandera == "PROPIA PECSA" ~ 1,
                                TRUE ~ 0)) %>% ## debe ir en otro archivo
    filter(fecha >= lubridate::dmy("30-01-2017"))


inicio <- lubridate::dmy("01-02-2017")
fin <- lubridate::dmy("01-10-2018")

# número de meses que debería tener una estación que ha estado abierta todo el
# perido
num_meses <- lubridate::month(fin) - lubridate::month(inicio) +12 + 1

grifos_creados_luego <- data_db5_comprada %>% 
    count(codigo_de_osinergmin) %>% 
    arrange(n) %>%
    filter(n < num_meses) %>% 
    pull(codigo_de_osinergmin)

'%ni%' <- Negate('%in%')

data_db5_comprada <- data_db5_comprada %>% 
    filter(codigo_de_osinergmin %ni% grifos_creados_luego)
```


#' ### Corremos regresión efectos fijos

Primero solo con 

#+ panel-two-ways
# library(multiwayvcov)
library(lmtest)
library(plm)
pdf <- pdata.frame(data_db5_comprada, index = c("codigo_de_osinergmin", "fecha"))

modelo <- precio_de_venta ~ COMPRADA

model <- plm(modelo, data = pdf, model = "within", effect = "twoways")

summary(model)

# Cluster by firm
coeftest(model, vcov = vcovHC(model, type = "sss", cluster = "group"))


#' Hacemos lo mismo, pero añadiendo los periodos como efectos fijos

#+ panel-fix-period 
pdf$fecha<-factor(pdf$fecha)
modelo <- precio_de_venta ~ COMPRADA + fecha
reg.fe <- plm(modelo, data=pdf, model="within")
t <- coeftest(reg.fe, vcov = vcovHC(reg.fe, type = "sss", cluster = "group"))
summary(reg.fe)

```


#' Intentamos con otro paquete:

#+ panel-con-lfe
library(lfe)
modelo <- precio_de_venta ~ COMPRADA | codigo_de_osinergmin + fecha | 0 | codigo_de_osinergmin

m2 <- felm(modelo, data = data_db5_comprada)

summary(m2)


#' ### Exportamos data a stata para hacer lo mismo

#+ export-stata
write.csv(data_db5_comprada, file = here::here("data", "processed", "data_diesel_reg_comprada.csv"))

#' ### Añadamos variables de competencia de estaciones cercanas:

#' Cargamos archivo con vecinos de Thiessen y generamos dataframe con datos e indicador si tiene vecino pecsa

grifos_vecinos <- readRDS(here::here("data", "processed", "grifo_con_vecinos_pre.RDS"))

#' esto debería ya venir con grifos_con_vecinos_pre.RDS
vecinos_pecsa_thissen <- grifos_vecinos %>%
    group_by(codigo_de_osinergmin.princ) %>%
    mutate(vecino_pecsa_thiessen = if_else(str_detect(razon_social.vec, "PERUANA DE ESTACIONES"),
                                           1,
                                           0
    )) %>%
    arrange(codigo_de_osinergmin.princ, desc(vecino_pecsa_thiessen)) %>%
    distinct(codigo_de_osinergmin.princ, .keep_all = TRUE) %>%
    select("codigo_de_osinergmin" = codigo_de_osinergmin.princ, vecino_pecsa_thiessen) 

#' La data ya tiene información sobre si la estación colindaba con una Pecsa (radio de 1.5 km)

#+ anadir-data-pecsa-thiessen
data_db5_comprada_1 <- data_db5_comprada %>% 
    left_join(vecinos_pecsa_thissen, by = "codigo_de_osinergmin") %>% 
    mutate(vecino_pecsa_dist = if_else(`año` == 2017, 0, vecino_pecsa_dist),
           vecino_pecsa_thiessen = if_else(`año` == 2017, 0, vecino_pecsa_thiessen),
           sc = if_else(`año` == 2017, sc_pre, sc_post)) 


data_db5_comprada_1
write.csv(data_db5_comprada_1, file = here::here("data", "processed", "data_diesel_reg_comprada_1.csv"))

#' ### Corremos regresión de efectos fijos:

#' Extraemos solo grifos operativos durante toda la ventana de tiempo:

#' Damos formato a los datos, agregando vecinos, indentificador de tipo de pecsa
library(lubridate)

df_panel_completo <- function(df) {
    
    df_con_compra <- df %>%
        mutate(
            COMPRADA_PROPIA = case_when(
                `año` == 2017 ~ 0,
                tipo_bandera == "PROPIA PECSA" ~ 1,
                TRUE ~ 0),
            SUMINISTRO = case_when(
                `año` == 2017 ~ 0,
                tipo_bandera == "ABANDERADA PECSA" ~ 1,
                TRUE ~ 0)
        ) 
    
    df_con_compra_vecinos <- df_con_compra %>% 
        left_join(vecinos_pecsa_thissen, by = "codigo_de_osinergmin") %>% 
        mutate(vecino_pecsa_dist = if_else(`año` == 2017, 0, vecino_pecsa_dist),
               vecino_pecsa_thiessen = if_else(`año` == 2017, 0, vecino_pecsa_thiessen),
               sc = if_else(`año` == 2017, sc_pre, sc_post)) 
    df_con_compra_vecinos
}

data_db5_completa <- df_panel_completo(data_db5)

#' Ahora hacemos la regresión por efectos fijos 10 meses y 3 meses antes y después:
    
inicio <- list("01-03-2017", "01-10-2017")
fin <- list("01-10-2018", "01-03-2018")

modelo2 <- precio_de_venta ~ COMPRADA_PROPIA  + SUMINISTRO + vecino_pecsa_thiessen + sc + fecha

calc_fe_fechas <- function(df, inicio, fin, modelo) {
    grifos_creados_luego <- df %>% 
        count(codigo_de_osinergmin) %>% 
        arrange(n) %>% 
        filter(n < max(n)) %>% 
        pull(codigo_de_osinergmin)
    
    '%ni%' <- Negate('%in%')
    
    df_panel_balanceado <- df %>% 
        filter(codigo_de_osinergmin %ni% grifos_creados_luego)
    
    panel_df <- df_panel_balanceado %>% 
        filter(fecha >= dmy(inicio), fecha <= dmy(fin)) %>% 
        pdata.frame(., index = c("codigo_de_osinergmin", "fecha"))
    
    fe <- plm(modelo, data= panel_df, model="within")
    fe
}


modelos_fe <- map2(inicio, fin, ~ calc_fe_fechas(data_db5_completa, .x, .y, modelo2))
names(modelos_fe) <- inicio

map(modelos_fe, ~summary(.x))


#' Calculos errores estándares clusterizados

map(modelos_fe, 
    ~coeftest(.x, vcov = vcovHC(.x, type = "sss", cluster = "group")))


#' ### Ahora un spatial fixed effects:

#damos formato a la data, primero codigo-fecha

data_db5_comprada_2 <- data_db5_comprada_1 %>% 
    select(codigo_de_osinergmin, fecha, everything())

coords_db5 <- data_db5_comprada_2 %>% 
    distinct(codigo_de_osinergmin, .keep_all = T) %>% 
    select(codigo_de_osinergmin, lon, lat) %>% 
    as.matrix()

head(coords_db5)

#creamos la matriz de distancias
grifos_nb <- tri2nb(coords_db5[,2:3], row.names = coords_db5[,1])

sp_grifos <- nb2listw(grifos_nb)



fm <- precio_de_venta ~ COMPRADA  + vecino_pecsa_thiessen + sc #+ fecha

#' #### Regresión de efectos fijos
#' 
#corremos regresión
sararfemod <- spml(formula = fm, data = data_db5_comprada_2, index = NULL,
                   listw = sp_grifos, lag = TRUE, spatial.error = "b", model = "within",
                   effect = "twoways", method = "eigen", na.action = na.fail,
                   quiet = TRUE, zero.policy = NULL,
                   tol.solve = 1e-13, control = list(), legacy = FALSE)
summary(sararfemod)

# W <- as(as_dgRMatrix_listw(sp_grifos), "CsparseMatrix")

# trMatc <- trW(W, type="mult")

# imp <- impacts(sararfemod, tr = trMatc, R = 200)
# summary(imp, zstats=TRUE, short=T)

imp1 <- impacts(sararfemod, listw = sp_grifos, time = 21, R = 200)
summary(imp1, zstats = TRUE, short = T)

#' #### Regresión de efectos aleatorios

sararremod <- spml(formula = fm, data = data_db5_comprada_2, index = NULL,
                   listw = sp_grifos, model = "random", lag = TRUE, spatial.error = "b")
summary(sararremod)

summary(verdoorn_SEM_FE<- spml(fm, data = data_db5_comprada_2,
                               listw = sp_grifos, lag=FALSE,model="within", effect="individual", spatial.error="b"))

summary(verdoorn_SEM_FE<- spml(fm, data = data_db5_comprada_2,
                               listw = sp_grifos, lag=FALSE,model="within", effect="individual", spatial.error="kkp"))


```

