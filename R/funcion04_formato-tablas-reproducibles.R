
mes_nombres <- c("01" = "Ene",
                 "02" = "Feb",
                 "03"="Mar",
                 "04"="Abr",
                 "05"="May",
                 "06"="Jun",
                 "07"="Jul",
                 "08"="Ago",
                 "09"="Set",
                 "10"="Oct",
                 "11"="Nov",
                 "12" = "Dic")


convertir_nombre_fecha <- function(str_fecha) {
    #formato 01-03-2018
    
    mes_nombres <- c("01" = "Ene",
                     "02" = "Feb",
                     "03"="Mar",
                     "04"="Abr",
                     "05"="May",
                     "06"="Jun",
                     "07"="Jul",
                     "08"="Ago",
                     "09"="Set",
                     "10"="Oct",
                     "11"="Nov",
                     "12" = "Dic")
    str_remove_all(str_fecha, "01-|20") %>% 
        str_replace_all(mes_nombres)
}

simp_impactos <- function(spa_reg_list, fecha, prod, rep = 100) {
    impacto <- impacts(spa_reg_list[[fecha]], listw = sp_grifos[[prod]][[fecha]], R = rep, useHESS = F)
    intervalos <- summary(impacto, zstats=TRUE, short = TRUE)
    t <- tibble(attr(impacto, "bnames"),
                "directo" = impacto$res$direct,
                "indirecto" = impacto$res$indirect,
                "total" = impacto$res$total) %>%
        bind_cols(intervalos$semat %>% as.data.frame()) %>%
        bind_cols(intervalos$pzmat %>% as.data.frame())
    
    cols_SE = c("Indirect", "Direct", "Total")
    to_app_SE = ".SE"
    cols_pvalue = c("Indirect1", "Direct1", "Total1")
    to_app_pvalue = ".pvalue"
    tabla_impacto <- rename_at(t, cols_SE, funs( paste0(., to_app_SE) ) ) %>%
        rename_at(cols_pvalue, funs( paste0(., to_app_pvalue) ) )
    nombre_producto <- ifelse(prod == "DB5", "Diésel", "Gasohol 90")
    metodo <- toString( spa_reg_list[[fecha]]$call) %>% str_split(",", simplify = T) %>% .[1,5] 
    nom_metodo <- ifelse(metodo == "durbin", "Modelo Espacial de Durbin (SDM)",
                         "Modelo Espacial Autoregresivo (SAR)")
    lst <- list( tabla_impacto,  nombre_producto,  rep, fecha, nom_metodo)
    names(lst) <- c(nom_metodo, "producto", "repeticiones", "fecha", "metodo" )
    lst
}



nombres_tabla <- c(
    `tipo_banderaABANDERADA PETROPERU` = "Abanderada Petroperu",
    `tipo_banderaABANDERADA PECSA` = "Abanderada Pecsa",
    `tipo_banderaABANDERADA PRIMAX` = "Abanderada Primax",
    `tipo_banderaABANDERADA REPSOL` = "Abanderada Repsol",
    `tipo_banderaPROPIA PECSA` = "Propia Pecsa",
    `tipo_banderaPROPIA PRIMAX`  = "Propia Primax",
    `tipo_banderaPROPIA REPSOL` = "Propia Repsol",
    sc = "SC", 
    distancia_avg = "DPROM",
    distancia_min = "DMIN", 
    num_grifos_cerc = "NCER",
    tiene_mecanico = "MECANICO",
    lavado = "LAVADO",
    cajero = "CAJERO",
    con_gnv = "GNV",
    con_glp = "GLP",
    ingresos_2012 = "INGRESO", 
    densidad_2017 = "DENPOB",
    `log(num_viajes)` = "LOGVIAJES"
)


imprimir_impacto <- function(lista_impacto, ols_lista) {
    modelo_ols <- ols_lista[[lista_impacto$fecha]] 
    header_top <- c(1, 1, 3)
    names(header_top) <- c(" ", "OLS", lista_impacto$metodo)
    lista_impacto[[1]] %>% 
        rename("Variable" = 1,) %>% 
        select(-ends_with("SE")) %>% #elimino SE para tabla
        bind_cols(tibble("OLS" =  modelo_ols$coefficients[-1],
                         "OLS_pvalue" = summary(modelo_ols)$coefficients[,4][-1])) %>%
        mutate_at(vars(-Variable), round, 3) %>% 
        mutate_at(vars(ends_with("pvalue")), ~case_when(
            . > 0.1 ~ '',
            . > 0.05 ~ "<sup>*</sub>",
            . > 0.01 ~ "<sup>**</sub>",
            TRUE ~ "<sup>***</sub>"
        )) %>% 
        mutate(Variable = recode(Variable, !!!nombres_tabla),
               Directo = str_c(directo, " ", Direct1.pvalue),
               Indirecto = str_c(indirecto, " ", Indirect1.pvalue),
               Total = str_c(total, " ", Total1.pvalue),
               OLS = str_c(OLS,  " ", OLS_pvalue)) %>% 
        select(-2:-7, -9, 10:12, 8) %>% 
        rename(" " = OLS) %>% 
        kable(., escape = FALSE, caption = str_c("Comparación para ", 
                                                 lista_impacto$producto,
                                                 " (",
                                                 convertir_nombre_fecha(lista_impacto$fecha),
                                                 ")"))  %>%
        kable_styling(bootstrap_options = "striped", full_width = F) %>% 
        add_header_above(header_top)
}



imprimir_modelo <- function(lista_spa, lista_ols, prod, out, durbin = F ) {
    
    rho <- map_dbl(lista_spa[c(2,3)], "rho") %>% round(3)
    
    lista_sar2 <- list(lista_spa[[2]], lista_ols[[2]],
                       lista_spa[[3]], lista_ols[[3]])
    
    LL <- c(lista_sar2[[1]]$LL, lista_sar2[[1]]$logLik_lm.model, 
            lista_sar2[[3]]$LL, lista_sar2[[3]]$logLik_lm.model) %>% 
        round(1)
    
    s2 <- c(lista_sar2[[1]]$s2, sigma(lista_sar2[[2]])^2, 
            lista_sar2[[3]]$s2, sigma(lista_sar2[[4]])^2) %>%  
        round(3)
    
    aic_vec <- c(AIC(lista_sar2[[1]]), lista_sar2[[1]]$AIC_lm.model, 
                 AIC(lista_sar2[[1]]), lista_sar2[[3]]$AIC_lm.model) %>% 
        round(1)
    
    notas_str <- ifelse(durbin == T, "Se omiten rezagos espaciales de variables dependientes.",
                        "")
    
    stargazer(lista_sar2,
              type = "html",
              covariate.labels = etiquetas_cov,
              dep.var.labels=str_c("Precio de venta - ", prod, " (soles/galón)"),
              dep.var.caption = "",
              model.numbers	= F,
              no.space = T,
              column.labels =  rep(fechas_formato[2:3], times = c(2,2)), 
              omit = c("lag"),
              omit.stat	= c("rsq", "adj.rsq", "f", "ll", "sigma2", "res.dev", "ser", "aic"),
              add.lines = list(append("rho", rho), append("Log.Lik", LL),
                               append("<p>&sigma;<sup>2</sub></p>", s2),
                               append("AIC", aic_vec)),
              notes = notas_str,
              notes.label = "Notas: ",
              single.row = T, out = here::here("doc", "tables", out))
}

# Panel data ==================

#' Función para hacer la regresión de efectos fijos filtrando solo las estaciones que
#' estuvieron activas durante todo el periodo
#' 
calc_fe_fechas <- function(df, inicio, fin, modelo, prod) {
    grifos_creados_luego <- df %>% 
        filter(producto == !!prod) %>% 
        count(codigo_de_osinergmin) %>% 
        arrange(n) %>% 
        filter(n < max(n)) %>% 
        pull(codigo_de_osinergmin)
    
    df_panel_balanceado <- df %>% 
        filter(codigo_de_osinergmin %ni% grifos_creados_luego)
    
    panel_df <- df_panel_balanceado %>% 
        filter(producto == !!prod) %>% 
        filter(fecha >= dmy(inicio), fecha <= dmy(fin)) %>% 
        pdata.frame(., index = c("codigo_de_osinergmin", "fecha"))
    
    fe <- plm(modelo, data= panel_df, model="within")
    fe
}

calcular_reg_fe <- function(df, modelo, inicio, fin, producto, output) {
    modelos_fe <-
        map2(inicio,
             fin,
             ~ calc_fe_fechas(df, .x, .y, modelo, producto))
    
    names(modelos_fe) <- inicio
    
    #' Calculos errores estándares clusterizados
    
    clust_error <- map(modelos_fe, 
                       ~coeftest(.x, vcov = vcovHC(.x, type = "sss", cluster = "group")))
    
    se <- map(clust_error, ~.x[,"Std. Error"])
    p.value <- map(clust_error, ~.x[,"Pr(>|t|)"])
    
    
    #' Imprimos tabla
    producto_label <- ifelse(producto == "DIESEL", "Diésel", "Gasohol 90")
    
    etiquetas_cov <- c("COMPRADA", "SUMINISTRO", "VECINO", "sc")
    
    t <- stargazer(modelos_fe,
                   type = "html",
                   covariate.labels = etiquetas_cov,
                   dep.var.labels=str_c("Precio de venta - ", producto_label, " (soles/galón)"),
                   dep.var.caption = "",
                   model.numbers	= F,
                   column.labels = c("18 meses", "6 meses"),
                   no.space = T,
                   se = se,
                   p = p.value,
                   omit.stat = "f",
                   omit = "fecha",
                   omit.labels = "¿Dummies por mes?",
                   omit.yes.no = c("Sí", "No"),
                   notes.label = "Notas: ",
                   single.row = T, out = here::here("doc", "tables", output))
    list(modelos_fe, producto)
}
