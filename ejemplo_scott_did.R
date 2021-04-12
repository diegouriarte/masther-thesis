install.packages("tidyverse")
install.packages("cli")
install.packages("haven")
install.packages("stargazer")
install.packages("estimatr")

# read_data function
read_data <- function(df) {
  full_path <- paste0("https://raw.github.com/scunning1975/mixtape/master/", df)
  return(haven::read_dta(full_path))
}

#-- DD estimate of 15-19 year olds in repeal states vs Roe states
abortion <- read_data("abortion.dta") %>% 
  mutate(
    repeal = as_factor(repeal),
    year   = as_factor(year),
    fip    = as_factor(fip),
    fa     = as_factor(fa),
  )

reg <- abortion %>% 
  filter(bf15 == 1) %>% 
  lm_robust(lnr ~ repeal*year + fip + acc + ir + pi + alcohol+ crack + poverty+ income+ ur,
            data = ., weights = totpop, clusters = fip)

abortion_plot <- tibble(
  sd = reg$std.error[-1:-75],
  mean = reg$coefficients[-1:-75],
  year = c(1986:2000))

abortion_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_rect(aes(xmin=1985, xmax=1992, ymin=-Inf, ymax=Inf), fill = "cyan", alpha = 0.01)+
  geom_point()+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05)) +
  labs(title= "Estimated effect of abortion legalization on gonorrhea")


data_g90 <- data_g90 %>% 
  mutate(mes = (year(fecha)-2017)*12 + month(fecha),
         fecha_trat = if_else(tipo_bandera == "PROPIA PECSA", 14, 0)) %>% 
  filter(tipo_bandera != "PROPIA PRIMAX",
         tipo_bandera == "PROPIA PECSA" | codigo_de_osinergmin %ni% lista_vecinos_km)

out_1 <- att_gt(yname = "precio_de_venta",
                gname = "fecha_trat",
                idname = "codigo_de_osinergmin",
                tname = "mes",
                xformla = ~codigo_de_osinergmin,
                data = data_g90,
                est_method = "dr",
                anticipation = 0,
                bstrap = T,
                biters = 1000,
                cband = T,
                clustervars = "codigo_de_osinergmin"
)

# Ejemplo de:
#https://lost-stats.github.io/Model_Estimation/Research_Design/two_by_two_difference_in_difference.html
library(broom)
library(here)
library(readxl)

DiD <- read_excel("D:/OneDrive/6 Inbox/did_crime.xlsx")

DiD <- DiD %>%
  mutate(after = year >= 2014) %>%
  mutate(treatafter = after*treat)

mt <- ggplot(DiD,aes(x=year, y=murder, color = treat)) +
  geom_point(size=3)+geom_line() +
  geom_vline(xintercept=2014,lty=4) +
  labs(title="Murder and Time", x="Year", y="Murder Rate")
mt

reg<-lm(murder ~ treat+treatafter+after, data = DiD)
summary(reg)
