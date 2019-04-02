#' ---
#' title: Funciones para el cálculo de spatial clustering
#' author: "Diego Uriarte"
#' date: Fri Mar 29 19:23:25 2019
#' output: github_document
#' ---
#' 

#' Este R file contiene las funciones que permiten el cálculo de valor de spatial
#' clustering siguiendo el paper de Pennerstorfer (2013)

hallar_bandera <- function(cod1) {
    # se debe tener cargado el ouput de 2.5 estaciones thiessen
    #input: codigo del grifo
    #ouput: bandera del grifo
    grifos %>% 
        filter(codigo_de_osinergmin.princ == cod1) %>%
        distinct(bandera.princ) %>% 
        pull()
}

hallar_lista_vecinos <- function(cod1) {
    # se debe tener cargado el ouput de 2.5 estaciones thiessen
    #input: codigo del grifo
    #ouput: vector con codigos de vecinos    
    grifos[grifos$codigo_de_osinergmin.princ == cod1, ]$codigo_de_osinergmin.vec
}

hallar_cluster <- function(cod, tipo = "razon_social") {
    #halla todas las estaciones vecinas que tienen la misma bandera
    #input: codigo de la estación, tipo de cluster (razon_social o bandera)
    #output: dataframe que tiene todas las estacioens del cluster de cod,
    #        incluida cod, con razón social y bandera
    
    df1 <- filter(grifos, codigo_de_osinergmin.princ == cod) %>%
        distinct(codigo_de_osinergmin.princ,
                 razon_social.princ,
                 bandera.princ)
    lista_vecinos <- hallar_lista_vecinos(cod)
    
    bandera <- hallar_bandera(cod)
    if (tipo == "razon_social") {
        razon_social_cod <-
            unique(grifos[grifos$codigo_de_osinergmin.princ == cod,]$razon_social.princ)
        df2 <- filter(grifos,
                      codigo_de_osinergmin.princ %in% lista_vecinos,
                      razon_social.princ == razon_social_cod) %>%
            distinct(codigo_de_osinergmin.princ,
                     razon_social.princ,
                     bandera.princ)

    } else {
        bandera <-
            unique(grifos[grifos$codigo_de_osinergmin.princ == cod,]$bandera.princ)
        df2 <- filter(grifos,
                      codigo_de_osinergmin.princ %in% lista_vecinos,
                      bandera.princ == bandera) %>%
            distinct(codigo_de_osinergmin.princ,
                     razon_social.princ,
                     bandera.princ)
    }
    
    bind_rows(df1, df2)
}

hallar_todos_cluster <- function(cod, tipo = "razon_social") {
    #halla todas las que están en mismo cluster (a nivel de banderas) que cod,
    #sin importar el nivel
    #input: codigo de la estación
    #output: dataframe que tiene todas las estaciones del cluster de cod,
    #        incluida cod, con razón social y bandera
    
    if (tipo == "bandera" & hallar_bandera(cod) == "INDEPENDIENTE") {
        df1 <- filter(grifos, codigo_de_osinergmin.princ == cod) %>%
            distinct(codigo_de_osinergmin.princ,
                     razon_social.princ,
                     bandera.princ)
        return(df1)
    }
    
    i <- 1
    df <- hallar_cluster(cod, tipo)
    
    while (i < nrow(df)) {
        df <-
            bind_rows(df, hallar_cluster(df$codigo_de_osinergmin.princ[i + 1], tipo)) %>%
            distinct()
        i <- i + 1
    }
    
    arrange(df, codigo_de_osinergmin.princ)
}


comprobar_df_dup <- function(df1, df2) {
    #Funcionar para comprobar si dos dataframes son iguales. Es para elimitar los 
    #dataframes de vecinos del mismo cluster que se generan
    #input: dos df con codigo | razon_social | bandera
    #output: TRUE si son iguales, FALSE de lo contrario
    if (is.null(df1) | is.null(df2)) {
        return(FALSE)
    }
    if (nrow(df1) == nrow(df2)) {
        if (all(df1 == df2)) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}

hallar_clusteres_mercado <- function(cod, tipo = "razon_social") {
    #halla todos clusteres del mercado que genera el grifo cod
    #input: codigo de la estación
    #output: lista que contiene dataframes con cada cluster que rodea a la 
    #estación cod
    df1 <- grifos %>% filter(codigo_de_osinergmin.princ == cod) %>%
        select(codigo = codigo_de_osinergmin.princ,
               razon_social = razon_social.princ,
               bandera = bandera.princ) %>% distinct()
    df2 <- grifos %>% filter(codigo_de_osinergmin.princ == cod) %>%
        select(codigo = codigo_de_osinergmin.vec,
               razon_social = razon_social.vec,
               bandera = bandera.vec)
    
    grifo_con_vecinos <- bind_rows(df1, df2)
    
    #creamos lista con cluster de cada grifo (pueden haber repetidos)
    
    lista <- list()
    
    for (grifo in grifo_con_vecinos$codigo) {
        lista[[as.character(grifo)]] <- hallar_todos_cluster(grifo, tipo)
    }
    
    #removemos los df repetidos (que normalmente habrá si hay vecinos en el mercado
    #del mismo cluster)
    for (i in 1:(length(lista) - 1)) {
        for (j in (i + 1):length(lista)) {
            if (comprobar_df_dup(lista[[i]], lista[[j]])) {
                lista[j] <- list(NULL)
            }
        }
    }
    
    lista[sapply(lista, is.null)] <- NULL
    lista
}


calcular_sc <- function(cod, tipo = "razon_social") {
    #numero de grifos en el mercado local
    N <- length(hallar_lista_vecinos(cod)) + 1
    #número de clusteres
    M <- length(hallar_clusteres_mercado(cod, tipo))
    #suma número de estaciones en cada cluster
    suma_clusteres <- sum(sapply(hallar_clusteres_mercado(cod, tipo), nrow))
    suma_clusteres
    #medida de spatial cluster 
    sc <- suma_clusteres / M / N
    sc
}