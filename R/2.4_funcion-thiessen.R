library(spatstat)
suppressMessages(library(tidyverse))
library(deldir)
#' Función para determinar los vecinos de thiessen de cada observación del dataset.
#' Requiere los paquetes spatstat, tidyverse y deldir

#' Esta función retorna una lista indexada de los vecinos pero sin identificarlo
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

#Acá los identificamos
vecinos_thiessen <- function(df) {
    df <- rownames_to_column(df) %>%
        mutate(rowname = as.integer(rowname)) %>%
        select(x, y, rowname, everything())
    W <- owin(c(min(df$x)-1, max(df$x)+1), c(min(df$y)-1, max(df$y)+1))
    pp1 <- as.ppp(df, W = W)
    left_join(sharededgemod(pp1), df, by = c("ind1" = "rowname"))
}
