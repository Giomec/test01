# DIR<-"C:/Users/giord/OneDrive/Documentos/PROYECTOS"
DIR<-"C:/PROYECTOS"
setwd(DIR)  
# install.packages("installr")
# install.ImageMagick()

#install.packages("data.table") 
library(tidyverse)
library(dplyr)
library(ggplot2)
library("data.table")
library("lubridate")
install.packages("gganimate") 
library(gganimate)
install.packages("gifski")
library(gifski)
install.packages("viridis") 
library(viridis)

# theme_set(theme_bw())
# Abrir todas las variables en integer 64 a numeric.
options(datatable.integer64="character")
#ini=Sys.time()
#####  https://www.youtube.com/watch?v=Z5rMrI1e4kM
cartera<-"Informe_Precios_Tasas.csv"
# chunkSize<-2000000
# index<-0
# DIR<-"C:/D/Modelo_TC_CMR/InPut_sld"
# setwd(DIR)  
# inv<-file(description = cartera, open = "r")
rate <- read_csv2(cartera)
head(rate)#trim_ws = TRUE,
rate$Fecha=as.date(rate$Fecha)
# inv <- read_delim(cartera, delim = ";", escape_double = TRUE, col_names = TRUE,  skip = 0) #trim_ws = TRUE,
# rate$Fecha=as.Date(paste0(substr(rate$Fecha,7,8),"-",substr(rate$Fecha,5,6),"-",substr(rate$Fecha,1,4)),format = "%d-%m-%Y")
unique(rate$Fecha)
rate$Fecha=as.Date(rate$Fecha)
View(inv)
summary(rate)
head(rate)
rate %>% 
  filter(Instrumento=="BANCARIA CLF",
         Fecha=="2022-06-29") %>% 
  # mutate(Valor_Mercado=Valor_Mercado/1000000) %>% 
  ggplot(aes(x = Plazo_Ini, y = Tasa_Mid, fill=Precio_Mid)) +
  geom_point(alpha=0.99, shape=21, size=4, color="black") +
  # geom_point(aes(shape=Posicion))+
  # facet_grid(Instrumento  ~ Moneda, 
             # scales = "free", 
             # labeller = "label_both") +
  # scale_y_discrete(breaks = seq(0,100000,10000))+
  
  # scale_size(range = c(0.01, 3)) +
  scale_fill_viridis(discrete=FALSE, guide="none", option="viridis") +
  # scale_color_viridis_d(option = "viridis") +
  xlab("Plazo Días") +
  ylab("Tasa") +
  ggtitle("Análisis Cartera Renta Fija Banco Falabella") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_rect(colour = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./test Análisis Tasas Banco Falabella v2.png", 
       plot = last_plot(),
       units = "cm", width = 40, height = 25, dpi = 600) 

################################################################################
# [1] "BANCARIA CLF"      "BANCARIA CLP"      "CREDITICIA CLF"    "CREDITICIA CLP"   
# [5] "EUR ONSHORE 6M"    "GobCLF_Curva_Cero" "GobCLP_Curva_Cero" "INTERBANCARIA CLF"
# [9] "INTERBANCARIA CLP" "IPC"               "JPY ONSHORE 6M"    "MXN ONSHORE 6M"   
# [13] "OIS"               "Spot"              "TCRC"              "USD OFFSHORE 3M"  
# [17] "USD OFFSHORE 6M"   "USD ONSHORE 6M"   

head(p)
p<-rate %>% 
  mutate(Tipo=substr(Instrumento, start = 1, stop = 3)) %>% 
    filter(#Tipo=="Gob" |
             Tipo=="INT"|
             Tipo=="BAN") %>%   

  
  # filter(Instrumento=="BANCARIA CLF" | 
  #          Instrumento=="BANCARIA CLP" |
  #          Instrumento=="INTERBANCARIA CLF" | 
  #          Instrumento=="INTERBANCARIA CLP")  %>% 
  # mutate(Valor_Mercado=as.numeric(Valor_Mercado)/1000000) %>% 
  # filter(Plazo<3000) %>% 
  ggplot(aes(x = Plazo_Ini, y = Tasa_Mid, fill=Fecha)) +
  geom_point(alpha=0.99, shape=21, size=2, color="white") +
  
  # ggplot(aes(x = Plazo_Ini, y = Tasa_Mid, color=Precio_Mid )) +
  # geom_point(position = "jitter", size=3) +
  # ggplot(aes(Duration, Valor_Mercado, size=Tir, colour = Instrumento)) +
  # geom_point(alpha = 0.2, size=1, show.legend = TRUE) +
  # geom_point(position = "jitter")+
  # geom_jitter(position=position_jitter(width=1, seed=123))+
  #scale_size(range = c(0,8)) +
  facet_grid(Moneda ~ Tipo)+
  # facet_wrap(~Moneda,  ncol=1)+
  scale_fill_viridis(discrete=FALSE, guide="none", option="viridis") +
  # scale_color_viridis_c(option = "viridis", direction = -1) +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.05))+
  xlab("Plazo Días") +
  ylab("Tasa") +
  ggtitle("Variación Tasa de interés Banco Falabella")
  
p
## Here comes the gganimate specific bits

p + 
labs(title = 'Fecha: {frame_time}', x = 'Plazo (días)', y = 'Valor Tasa') +
transition_time(Fecha) +
shadow_wake(0.9)+
ease_aes('linear')

# last_animation()
# anim_save("Chick_Weights_over_time.gif", animation = last_animation(), path = )
anim_save(filename = "./test Animation Tasas Banco Falabella v4.gif")
getwd()
# animate(p, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("output.gif")

# inv$Valor_Mercado<-as.numeric(inv$Valor_Mercado)



###################################
# library(gapminder)
# head(gapminder)
# # library(gapminder)
# library(ggplot2)
# library(gganimate)
# data(gapminder)
# p <- ggplot(
#   gapminder,
#   aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
# ) +
#   geom_point(show.legend = FALSE, alpha = 0.7) +
#   scale_color_viridis_d() +
#   scale_size(range = c(2, 12)) +
#   scale_x_log10() +
#   labs(x = "GDP per capita", y = "Life expectancy")
# 
# p + transition_time(year)
#   animate(p) #, renderer = gifski_renderer())
