library(rvest)
library(stringr)
library(tidyverse)


raiz <- "http://www.minem.gob.pe"

session <- polite::bow(raiz,
                       user_agent = "Diego U.")


url <- "http://www.minem.gob.pe/_estadisticaSector.php?idSector=5&idCategoria=9"

#scrapemaos tres primera páginas de minem de estadisticas
t <- map(1:3, ~ str_c("http://www.minem.gob.pe/_estadisticaSector.php?idSector=5&idCategoria=9&pagina=", .x) %>% 
      read_html(., encoding = "UTF-8"))


#obtenemos lista de urls
(lista_urls <- map(t, ~ html_nodes(.x, ".ver")) %>% 
  map(~ html_attr(.x, "href")) %>% 
  flatten_chr() %>% 
  map_chr(~ str_c(raiz, "/", .x )))

#intentamos descargar pero la ubicación de los links no es consistente, se requiere mejorar
descargar_excel <- function(url) {
  url_desc <- read_html(url, encoding = "UTF-8") %>% 
    html_node("tr:nth-child(15) td:nth-child(2) a") %>% 
    html_attr("href") %>% 
    str_c(raiz, .) 
  downloader::download(url_desc, mode = "wb", 
                       destfile = here::here("data", 
                                             "minem", 
                                             str_remove(url_desc, "http://www.minem.gob.pe/minem/archivos/")))
}
map(lista_urls, descargar_excel)

#al final solo abro los links en el navegador y descargo la info
lista_urls %>% map(browseURL)

#Limpiamos archivos##########################
#

