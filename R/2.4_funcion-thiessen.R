library(spatstat)
suppressMessages(library(tidyverse))
library(deldir)
#' Funci?n para determinar los vecinos de thiessen de cada observaci?n del dataset.
#' Requiere los paquetes spatstat, tidyverse y deldir

#' Esta funci?n retorna una lista indexada de los vecinos pero sin identificarlo
#' 
#' X es un dataframe donde las dos primeras columnas son la 
sharededgemod <- function(X) {
    verifyclass(X, "ppp")
    Y <- X[as.rectangle(X)]
    dX <- deldir(Y)
    DS <- dX$dirsgs
    ans_prev <- data.frame(ind1=DS[,5], 
                           ind2=DS[,6])
    ans_reverse <- ans_prev %>% select(ind1 = ind2, ind2 = ind1)
    
    ans <- bind_rows(ans_prev, ans_reverse) %>% arrange(ind1)
    return(ans)
}

#Ac? los identificamos
vecinos_thiessen <- function(df) {
    #cargamos archivo con límites
    coordenadas_costa <-
        readr::read_csv(here::here("data", "limites_costa.csv"),
                        col_types = c("cc")) %>%
        separate(
            `lat, long`,
            into = c("y", "x"),
            sep = ",",
            convert = TRUE
        )
    #ventana respetando al mar
    W <- owin(poly=data.frame(x=(coordenadas_costa$x), y=(coordenadas_costa$y)))
    
    df <- rownames_to_column(df) %>%
        mutate(rowname = as.integer(rowname)) %>%
        select(x, y, rowname, everything())
    pp1 <- as.ppp(df, W = W)
    left_join(sharededgemod(pp1), df, by = c("ind1" = "rowname"))
}
