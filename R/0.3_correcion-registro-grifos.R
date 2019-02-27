#' Corrección de base de datos
#' He identificado que la base de datos cuenta con grifos repetidos, o con direcciones
#' incompletas. Resolveremos esos problemas
#' 

#Cargamos la data
prices <- readRDS(here::here("data","processed","data_2005_2018_clean.rds"))

#generamos la lista de estaciones de Lima
lista_estaciones_direccion <- prices %>% 
    filter(departamento == "LIMA", provincia == "LIMA") %>%
    count(codigo_de_osinergmin, direccion) %>%
    select(-n)

#extraemos todas las duplicadas
lista_duplicados <- duplicated(lista_estaciones_direccion %>% pull(codigo_de_osinergmin))

lista_codigos_duplicados <- unique(lista_estaciones_direccion[lista_duplicados,"codigo_de_osinergmin"])

#vemos las estaciones duplicadas
lista_estaciones_direccion %>%
    filter(codigo_de_osinergmin %in% pull(lista_codigos_duplicados))

#nos quedamos solo con el de mayor longitud de cada uno
direcciones_corregidas <- lista_estaciones_direccion %>%
    filter(codigo_de_osinergmin %in% pull(lista_codigos_duplicados)) %>%
    mutate(longitud = nchar(direccion)) %>%
    arrange(codigo_de_osinergmin, desc(longitud)) %>%
    distinct(codigo_de_osinergmin, .keep_all = TRUE) %>%
    select(-longitud)
    
direcciones_corregidas

prices %>% 
    filter(codigo_de_osinergmin %in% pull(lista_codigos_duplicados)) %>%
    arrange(codigo_de_osinergmin) %>%
    select(codigo_de_osinergmin, direccion)
    View()