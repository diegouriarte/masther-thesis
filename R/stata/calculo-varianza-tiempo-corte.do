use "E:\Dropbox\projects\maestria\masther-thesis\data\processed\data_diesel_mensual.dta", clear

gen fecha_stata = date(fecha, "DMY")

format fecha_stata %td 

xtset codigo_de_osinergmin fecha_stata

xtreg precio_de_venta i.fecha_stata, fe


clear
import delimited E:\Dropbox\projects\maestria\masther-thesis\data\processed\data_diesel_reg_comprada.csv
gen fecha1=date(fecha,"YMD")
format fecha1 %td
xtset codigo_de_osinergmin fecha1

xtreg precio_de_venta comprada i.fecha1, fe  cluster(codigo_de_osinergmin)

clear
import delimited E:\Dropbox\projects\maestria\masther-thesis\data\processed\data_diesel_reg_comprada_1.csv
gen fecha1=date(fecha,"YMD")
format fecha1 %td
xtset codigo_de_osinergmin fecha1

xtreg precio_de_venta comprada sc vecino_pecsa_thiessen vecino_pecsa_dist i.fecha1, fe  cluster(codigo_de_osinergmin)

********* ahora cre ***************

egen compradabar = mean(comprada), by(codigo_de_osinergmin)
egen scbar = mean(sc), by(codigo_de_osinergmin)
egen vecino_pecsa_thiessen_bar = mean(vecino_pecsa_thiessen), by(codigo_de_osinergmin)

xtreg precio_de_venta comprada sc vecino_pecsa_thiessen vecino_pecsa_dist ///
compradabar scbar vecino_pecsa_thiessen_bar i.fecha1, re  cluster(codigo_de_osinergmin)

test compradabar scbar vecino_pecsa_thiessen_bar 

egen unionbar = mean(union), by(nr)
egen marriedbar = mean(married), by(nr)

xtreg  lwage d81-d87 union married unionbar marriedbar educ black hisp , re 
estimates store rew_cre
xtreg  lwage d81-d87 union married unionbar marriedbar educ black hisp , re  cluster(nr)
estimates store rew_cre_ro
estimates table few few_ro rew_cre rew_cre_ro, se 
