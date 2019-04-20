Número de viajes por distrito
================
Diego Uriarte
Sat Apr 20 10:59:50 2019

Importaremos los archivos bajados de la aplicación del AATE extraeremos
el número de visitantes a cada distrito

``` r
library(conflicted)
library(tidyverse)
conflict_prefer("select", "dplyr")
```

``` r
lista_archivos <-
    list.files(here::here("data", "demo-distrital", "big-data-distritos"),
               full.names = TRUE)

big_data_aate <- map_dfr(lista_archivos, read_csv)
```

## Limpiamos data distritos

``` r

# numero de viajes en diciembre de 2017 x distrito
# numero de vaijes x millon de habitantes
viajes_distrito <- big_data_aate %>% 
    select("distrito" = nom_dist_d, tipo_dia, horario, motivo, edad, viajes) %>% 
    group_by(distrito) %>% 
    summarise(num_viajes = sum(viajes)) %>% 
    mutate(distrito = replace(distrito, distrito == "BRENA", "BREÑA"),
           distrito = replace(distrito, distrito == "ATE VITARTE", "ATE"),
           num_viajes_millon = num_viajes/1000000)

head(viajes_distrito)
#> # A tibble: 6 x 3
#>   distrito   num_viajes num_viajes_millon
#>   <chr>           <dbl>             <dbl>
#> 1 ATE           5076199             5.08 
#> 2 BARRANCO       332460             0.332
#> 3 BREÑA          713152             0.713
#> 4 CARABAYLLO    1695539             1.70 
#> 5 CHORRILLOS    2351771             2.35 
#> 6 COMAS         3366984             3.37

saveRDS(
    viajes_distrito,
    here::here(
        "data",
        "processed",
        "data_viajes_distritos.rds"
    )
)
```
