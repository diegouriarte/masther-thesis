use "E:\Dropbox\projects\maestria\masther-thesis\data\processed\data_diesel_mensual.dta", clear

gen fecha_stata = date(fecha, "DMY")

format fecha_stata %td 

xtset codigo_de_osinergmin fecha_stata

xtreg precio_de_venta i.fecha_stata, fe

import delimited E:\Dropbox\projects\maestria\masther-thesis\data\processed\data_diesel_reg_comprada.csv

xtset codigo_de_osinergmin fecha1
gen fecha1=date(fecha,"DMY")
format fecha1 %d
xtreg precio_de_venta comprada i.fecha1, fe
