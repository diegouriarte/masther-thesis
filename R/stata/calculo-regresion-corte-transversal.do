use "E:\Dropbox\projects\maestria\masther-thesis\data\processed\data_db5_cross.dta", clear

gen fecha_stata = date(fecha, "DMY")

format fecha_stata %td 


regress precio_de_venta sc_pre bandera, cluster(codigo_de_osinergmin) if fecha_stata == mdy(1,1, 2017)


list country continent urb if infmor>25	
xtset codigo_de_osinergmin fecha_stata

date<td("30may2004")
