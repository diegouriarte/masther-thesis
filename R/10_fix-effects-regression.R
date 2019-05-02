#' ---
#' title: Regresión panel
#' author: "Diego Uriarte"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_notebook:
#'     toc: true
#'     toc_float: true
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

#+ body, warning = FALSE, message = FALSE
# ----
library(conflicted)
library(tidyverse)
library(splm)
library(lmtest)
library(plm)
library(lubridate)
library(stargazer)
library(kableExtra)
'%ni%' <- Negate('%in%')
source(here::here("R","funcion04_formato-tablas-reproducibles.R"), encoding = "UTF-8")

conflict_prefer("filter", "dplyr")
# ----


#' 
#' ## Cargamos datos
#' 
#+ cargar-datos

data_total <- readRDS(file = here::here("data", "processed", "data-final-regresiones.rds"))

#' ## Corremos regresión de efectos fijos:


#' Ahora hacemos la regresión por efectos fijos 10 meses y 3 meses antes y después:
    
inicio <- list("01-05-2017", "01-11-2017")
fin <- list("01-10-2018", "01-04-2018")

#' El modelo es:
#' 
modelo2 <- precio_de_venta ~ COMPRADA  + SUMINISTRO + vecino_pecsa_thiessen + sc + fecha


calcular_reg_fe(df = data_total, modelo = modelo2, inicio, fin, 
                producto = "DIESEL", output = "fe-db5.htm")

calcular_reg_fe(df = data_total, modelo = modelo2, inicio, fin, 
                producto = "G90", output = "fe-g90.htm")

modelo3 <- precio_de_venta ~ COMPRADA  + SUMINISTRO + vecino_pecsa_dist + sc + fecha

calcular_reg_fe(df = data_total, modelo = modelo3, inicio, fin, 
                producto = "DIESEL", output = "fe-db5-dist.htm")

calcular_reg_fe(df = data_total, modelo = modelo3, inicio, fin, 
                producto = "G90", output = "fe-g90-dist.htm")
#'  Hausman test (plm)
#'  
#'  
wi <- plm(modelo3, data = balancear_panel(data_total, "DIESEL"), model = "within", effect = "twoways")
re <- plm(modelo3, data = balancear_panel(data_total, "DIESEL"), model = "random")
phtest(wi, re)
print(hausman_panel <- phtest(modelo3, 
                              data = balancear_panel(data_total, "DIESEL"),
                              index = c("codigo_de_osinergmin", "fecha"),
                              model = c("within", "random")))
#' ## Spatial fixed effects:


#' Creamos matriz de pesos para ambos casos:
#


spatial_w <- map(c("DIESEL", "G90"), ~ crear_spatial_w(data_total, .x))
names(spatial_w) <- c("DIESEL", "G90")


fm <- precio_de_venta ~ COMPRADA  + SUMINISTRO + vecino_pecsa_thiessen + sc + fecha

#' ### Regresión de efectos fijos
#'
#corremos regresión


#' Regresión por efectos fijos con el modelo completo
sararfemod <- spml(formula = fm, data = balancear_panel(data_total, "DIESEL"), index = NULL,
                   listw = spatial_w$DIESEL, lag = TRUE, spatial.error = "none", model = "within",
                   effect = "individual", method = "eigen", na.action = na.fail,
                   quiet = TRUE, zero.policy = NULL, hess = FALSE,
                   tol.solve = 1e-13, control = list(), legacy = FALSE)

summary(sararfemod)
str(summary(sararfemod))

nombres_variables <- c(lambda = "\u03bb",
                       COMPRADA = "COMPRADA",
                       vecino_pecsa_thiessen = "VECINO",
                       sc  = "sc" )


summary(sararfemod)$CoefTable %>% 
    as_tibble(rownames = "Variable") %>% 
    mutate_at(vars(starts_with("Pr")), ~case_when(
        . > 0.1 ~ '',
        . > 0.05 ~ "<sup>*</sub>",
        . > 0.01 ~ "<sup>**</sub>",
        TRUE ~ "<sup>***</sub>"
    )) %>% 
    mutate(Estimate = round(Estimate, 3),
           `Std. Error` = round(`Std. Error`, 4),
           Estimado = str_c("<span>", Estimate, `Pr(>|t|)`, "</span>", " (", `Std. Error`, ")"),
           Variable = recode(Variable, !!!nombres_variables)) %>% 
    select(-2:-5) %>% 
    filter(!str_detect(Variable, "fecha")) %>% 
    kable(escape = FALSE) %>% 
    kable_styling(bootstrap_options = "striped", full_width = F)


imp1 <- impacts(sararfemod, listw = spatial_w$DIESEL, time = 21, R = 200)
t <- summary(imp1, zstats = TRUE, short = T)
imp_measures <-  tibble(Variables = rownames(t$direct_sum$statistics),
                        "Directo" = t$direct_sum$statistics[,"Mean"],
                        "Indirecto" = t$indirect_sum$statistics[,"Mean"],
                        "Total" = t$total_sum$statistics[,"Mean"])

se_measures <- as_tibble(t$semat, rownames = "Variables") %>% 
    rename_if(is.numeric, ~ str_c(., "_SE"))

pvalues_measures <- as_tibble(t$pzmat, rownames = "Variables") %>% 
    rename_if(is.numeric, ~ str_c(., "_pvalue"))

inner_join(imp_measures, se_measures, by = "Variables") %>% 
    inner_join(pvalues_measures, by = "Variables") %>% 
    mutate_at(vars(2:4), round, 3) %>% 
    mutate_at(vars(5:7), round, 4) %>% 
    mutate_at(vars(ends_with("pvalue")), ~case_when(
        . > 0.1 ~ '',
        . > 0.05 ~ "<span><sup>*</sub></span>",
        . > 0.01 ~ "<span><sup>**</sub></span>",
        TRUE ~ "<span><sup>***</sub></span>"
    )) %>% 
    mutate(Directo = str_c(Directo, Direct_pvalue, " (", Direct_SE, ")"),
           Indirecto = str_c(Indirecto, Indirect_pvalue, " (", Indirect_SE, ")"),
           Total = str_c(Total, Total_pvalue, " (", Total_SE, ")"),
           Variables = recode(Variables, vecino_pecsa_thiessen = "VECINO")) %>% 
    select(1:4) %>% 
    filter(!str_detect(Variables, "fecha")) %>% 
    kable(escape = FALSE) %>% 
    kable_styling(bootstrap_options = "striped", full_width = F)

#' Solo spatial error tipo b 
summary(verdoorn_SEM_FE<- spml(fm, data = balancear_panel(data_total, "DIESEL"),
                               listw = spatial_w$DIESEL, lag=FALSE,model="within", effect="individual", spatial.error="b"))

#' Solo spatial error tipo kkp 

summary(verdoorn_SEM_FE<- spml(fm, data = balancear_panel(data_total, "DIESEL"),
                               listw = spatial_w$DIESEL, lag=FALSE,model="within", effect="individual", spatial.error="kkp"))

#' ### Regresión de efectos aleatorios

# no corre
# sararremod <- spml(formula = fm, data = balancear_panel(data_total, "DIESEL"), index = NULL,
#                    listw = spatial_w$DIESEL, model = "random", lag = TRUE, spatial.error = "b")


# no corre el random de errores
# summary(verdoorn_SEM_Ran<- spml(fm, data = balancear_panel(data_total, "DIESEL"),
                               # listw = spatial_w$DIESEL, lag=FALSE,model="random", 
                               # effect="individual", spatial.error="kkp"))

#' Hacemos test de Hausman espacial para modelo con lag
#'
test_lag <- sphtest(x = fm, data = balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL,
                  spatial.model = "lag", method = "ML")

test_lag

#' ### Tests para determinar modelo correcto
#'
#'
#'


# Hausman test robust to spatial autocorrelation (splm) for error model
print(
    spat_hausman_ML_SEM <- sphtest(
        fm,
        data = balancear_panel(data_total, "DIESEL"),
        listw = spatial_w$DIESEL,
        spatial.model = "error",
        method = "ML"
    )
)

# Hausman test robust to spatial autocorrelation (splm) for sarar model
print(
    spat_hausman_ML_SEM <- sphtest(
        fm,
        data = balancear_panel(data_total, "DIESEL"),
        listw = spatial_w$DIESEL,
        spatial.model = "sarar",
        method = "ML"
    )
)

# Fixed effects model
# Test 1
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lml",
        model="within")
# Test 2
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lme",
        model="within")
# Test 3
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlml",
        model="within")
# Test 4
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlme",
        model="within")

tests <- list("lml", "lme", "rlml", "rlme")
prods <- list("DIESEL", "G90")

slmtest_tes <- function(prod, tests, fecha_lim = c("01-05-2017", "01-10-2018")) {
    data_periodo <- balancear_panel(data_total, prod, fecha_lim)
    w_prod <- crear_spatial_w(data_periodo, prod)
    slmtest(fm, data = data_periodo, listw = w_prod, test=tests,
            model="within")   
}

test_panel_diesel <- map(tests, ~slmtest_tes("DIESEL", .x))
test_panel_g90 <- map(tests, ~slmtest_tes("G90", .x))


df_test_diesel <- map_df(test_panel_diesel, magrittr::extract, c("method", "alternative", "statistic", "p.value"))  

df_test_g90 <- map_df(test_panel_g90, magrittr::extract, c("method", "alternative", "statistic", "p.value")) 

inner_join(df_test_diesel, df_test_g90, by = c("method", "alternative"), suffix = c(".DB5", ".G90")) %>% 
    mutate_at(vars(starts_with("statis")), round, 2) %>% 
    mutate_at(vars(starts_with("statis")), format, c(nsmall = 2)) %>% 
    mutate_at(vars(starts_with("p.value")), round, 4) %>% 
    mutate_at(vars(starts_with("p.value")), format, c(nsmall = 4)) %>% 
    mutate(`Diésel` = str_c(statistic.DB5, " (", p.value.DB5, ")"),
           `Gasohol 90` = str_c(statistic.G90, " (", p.value.G90, ")")) %>% 
    select(-3:-6) %>% 
    
    kable(escape = FALSE)  %>%
    kable_styling(bootstrap_options = "striped", full_width = F)


test_panel_diesel_short <- map(tests, ~slmtest_tes("DIESEL", .x, fecha_lim = c("1-11-2017", "1-04-2018")))
test_panel_g90_short <- map(tests, ~slmtest_tes("G90", .x, fecha_lim = c("1-11-2017", "1-04-2018")))



df_test_diesel <- map_df(test_panel_diesel_short, magrittr::extract, c("method", "alternative", "statistic", "p.value"))  

df_test_g90 <- map_df(test_panel_g90_short, magrittr::extract, c("method", "alternative", "statistic", "p.value")) 

inner_join(df_test_diesel, df_test_g90, by = c("method", "alternative"), suffix = c(".DB5", ".G90")) %>% 
    mutate_at(vars(starts_with("statis")), round, 2) %>% 
    mutate_at(vars(starts_with("statis")), format, c(nsmall = 2)) %>% 
    mutate_at(vars(starts_with("p.value")), round, 4) %>% 
    mutate_at(vars(starts_with("p.value")), format, c(nsmall = 4)) %>% 
    mutate(`Diésel` = str_c(statistic.DB5, " (", p.value.DB5, ")"),
           `Gasohol 90` = str_c(statistic.G90, " (", p.value.G90, ")")) %>% 
    select(-3:-6) %>% 
    
    kable(escape = FALSE)  %>%
    kable_styling(bootstrap_options = "striped", full_width = F)










# Test 3

slmtest(fm, data=balancear_panel(data_total, "G90"), listw = spatial_w$G90, test="rlml",
        model="within")
# Test 4
slmtest(fm, data=balancear_panel(data_total, "G90"), listw = spatial_w$G90, test="rlme",
        model="within")


# Random effects model
# Test 1
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lml",
        model="random")
# Test 2
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="lme",
        model="random")
# Test 3
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlml",
        model="random")
# Test 4
slmtest(fm, data=balancear_panel(data_total, "DIESEL"), listw = spatial_w$DIESEL, test="rlme",
        model="random")
