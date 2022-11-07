DIR<-"C:/PROYECTOS"
setwd(DIR)  
#install.packages("data.table") 
library(tidyverse)
library(dplyr)
library("data.table")
# Abrir todas las variables en integer 64 a numeric.
options(datatable.integer64="character")
#ini=Sys.time()
#####  https://www.youtube.com/watch?v=Z5rMrI1e4kM
cartera<-"Cartera_inversiones_20220831.csv"
# chunkSize<-2000000
# index<-0
# DIR<-"C:/D/Modelo_TC_CMR/InPut_sld"
# setwd(DIR)  
# inv<-file(description = cartera, open = "r")
inv <- read_csv2(cartera) #trim_ws = TRUE,
# inv <- read_delim(cartera, delim = ";", escape_double = TRUE, col_names = TRUE,  skip = 0) #trim_ws = TRUE,

View(inv)
summary(inv)
colnames(inv)
# [1] "Fecha_Proceso"            "Operacion"                "Documento"                "Correlativo"             
# [5] "Serie"                    "Seriado"                  "Emisor"                   "Fecha_Compra"            
# [9] "Fecha_Emision"            "Fecha_Vcto"               "Duration"                 "Plazo"                   
# [13] "Plazo_Residual"           "Nominal"                  "Moneda"                   "Tir"                     
# [17] "Tasa_Mercado"             "Tasa_Emision"             "Tera_Emision"             "Valor_Compra"            
# [21] "Interes"                  "Reajuste"                 "Valor_Presente"           "Valor_Mercado"           
# [25] "Ajuste_Mercado"           "Interes_Diario"           "Reajuste_Diario"          "Valor_Presente_Prox"     
# [29] "Valor_Compra_UM"          "Interes_UM"               "Valor_Presente_UM"        "Valor_Mercado_UM"        
# [33] "Ajuste_Mercado_UM"        "Interes_Diario_UM"        "Valor_Presente_Prox_UM"   "Valor_Compra_Par"        
# [37] "Interes_Par"              "Reajuste_Par"             "Valor_Presente_Par"       "Interes_Diario_Par"      
# [41] "Reajuste_Diario_Par"      "Valor_Presente_Prox_Par"  "Prima"                    "Descuento"               
# [45] "Prima_Diario"             "Descuento_Diario"         "Grupo"                    "Nivel_Libro"             
# [49] "Libro"                    "Posicion"                 "Evento"                   "Cartera_Normativa"       
# [53] "Instrumento"              "Sub_Instrumento"          "Tipo_Letra"               "Rut_Emisor"              
# [57] "Codigo_Emisor"            "Tipo_Emisor"              "Codigo_Instrumento"       "Codigo_Moneda"           
# [61] "Codigo_Libro"             "Codigo_Cartera_Normativa" "Periodo_Cupon"            "Ultimo_Cupon"            
# [65] "Fecha_Ultimo_Cupon"       "Nominal_Residual"         "Cupones_Cortados"         "Marca_VI"                
# [69] "Marca_GTA"                "Pago"                     "Fecha_Devengo"            "Fecha_Prx_Cupon"         
# [73] "Fecha_Valuta"             "Plaza_1"                  "Plaza_1_Glosa"            "Plaza_2"                 
# [77] "Plaza_2_Glosa"            "Plaza_3"                  "Plaza_3_Glosa"            "Forma_de_Pago"           
# [81] "Forma_de_Pago_Glosa"      "Dv01_Clp"                 "Dv01_Um"                  "fecha"                   
# [85] "codigo_instrumento"       "interes_Lir_UM"           "interes_Mes_Lir_UM"       "Retencion_Lir_UM"        
# [89] "Retencion_Lir"            "Retencion_Mes_Lir_UM"     "Retencion_Mes_Lir"        "Grupo_Especial"          
# [93] "Linea_1"                  "Linea_Glosa_1"            "Grupo_1"                  "Grupo_Factor_1"          
# [97] "Plazo_Pacto_1"            "Plazo_Instrumento_1"      "Monto_Linea_1"            "Monto_Linea_Clp_1"       
# [101] "Factor_1"                 "Rut_1"                    "Codigo_1"                 "Nombre_1"                
# [105] "Error_1"                  "Error_Glosa_1"            "Linea_2"                  "Linea_Glosa_2"           
# [109] "Grupo_2"                  "Grupo_Factor_2"           "Plazo_Pacto_2"            "Plazo_Instrumento_2"     
# [113] "Monto_Linea_2"            "Monto_Linea_Clp_2"        "Factor_2"                 "Rut_2"                   
# [117] "Codigo_2"                 "Nombre_2"                 "Error_2"                  "Error_Glosa_2"  


  

inv %>% 
  mutate(Valor_Mercado=Valor_Mercado/1000000) %>% 
  ggplot(aes(x = Plazo_Residual, y = Valor_Mercado, color=Tir )) +
  geom_point(position = "jitter") +
  geom_point(aes(shape=Posicion))+
  facet_grid(Instrumento  ~ Moneda, 
             scales = "free", 
             labeller = "label_both") +
  # scale_y_discrete(breaks = seq(0,100000,10000))+
  
  # scale_size(range = c(0.01, 3)) +
   scale_color_viridis_c(option = "viridis") +
  xlab("Plazo Días") +
  ylab("Valor de Mercado") +
  ggtitle("Análisis Cartera Renta Fija Banco Falabella") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

ggsave(filename = "./test Análisis Cartera Renta Fija Banco Falabella.png", 
       plot = last_plot(),
       units = "cm", width = 40, height = 25, dpi = 600) 

################################################################################

inv %>% 
   mutate(Valor_Mercado=as.numeric(Valor_Mercado)/1000000) %>% 
  # filter(Plazo<3000) %>% 
  ggplot(aes(Duration, Valor_Mercado, size=Tir, colour = Instrumento)) +
  geom_point(alpha = 0.2, size=1, show.legend = TRUE) +
  # geom_point(position = "jitter")+
   geom_jitter(position=position_jitter(width=1, seed=123))+
  #scale_size(range = c(0,8)) +
  facet_wrap(~Moneda,  ncol=1)+
  scale_color_viridis_d(option = "viridis") 
  ## Here comes the gganimate specific bits
  # labs(title = 'Year: {frame_time}', x = 'GDP per capita in USD', y = 'Life Expectancy') +
   # transition_time(year) +
  # shadow_wake(.3)+
  # ease_aes('linear')

# inv$Valor_Mercado<-as.numeric(inv$Valor_Mercado)
