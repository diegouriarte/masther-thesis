use "E:\Dropbox\projects\maestria\masther-thesis\data\processed\data_diesel_mensual.dta", clear

gen fecha_stata = date(fecha, "DMY")

format fecha_stata %td 

xtset codigo_de_osinergmin fecha_stata

xtreg precio_de_venta i.fecha_stata, fe
