balancear_panel <- function(df, prod, fecha_lim = c("01-05-2017", "01-10-2018")) {
  #Creamos una función para balancear el panel (removemos grifos que no abrieron o 
  #cerraron en la ventana, son menos de 10)
  grifos_creados_luego <- df %>%
    filter(producto == !!prod,
           fecha >= dmy(fecha_lim[1]),
           fecha <= dmy(fecha_lim[2])) %>%
    count(codigo_de_osinergmin, sort = T) %>%
    filter(n < max(n)) %>%
    pull(codigo_de_osinergmin)
  
  df_balanceado <- df %>%
    filter(producto == !!prod) %>%
    filter(codigo_de_osinergmin %ni% grifos_creados_luego,
           fecha >= dmy(fecha_lim[1]),
           fecha <= dmy(fecha_lim[2]))
  
  df_balanceado
}