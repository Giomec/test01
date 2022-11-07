library(lubridate)
ini<-Sys.time()   
DIR<-"D:/TC_CMR_INGRESO"
setwd(DIR)  
FG=0.913  
INPUT_TC_CMR <- read.table(file = "INPUT_TC-CMR_FAC_ANT.csv", header = TRUE,fill = TRUE, sep = ";", dec = ",")
INPUT_PERFILES <- read.table(file = "Perfil_Factor.csv", header = TRUE, sep = ";", dec = ",")
#INPUT_PERFILES <- read.table(file = "Perfil_Factor - original.csv", header = TRUE, sep = ";", dec = ",")
# colnames(INPUT_PERFILES)
INPUT_PERFILES<-INPUT_PERFILES[,c("Dia_Mes", "Perfil", "Factor_puro_MM" )] # Factor Factor_puro_MM** Factor_Puro
# tail(INPUT_PERFILES)
colnames(INPUT_PERFILES)=c("DIAMES","PP","FACTOR")

# head(INPUT_PERFILES)
INPUT_TC_CMR		<-	INPUT_TC_CMR[INPUT_TC_CMR$CODIGO_PRODUCTO=="TC",]
# N Normal
# V Vencido
# R Reemplazo
# A Agregado
INPUT_TC_CMR		<-	INPUT_TC_CMR[INPUT_TC_CMR$DESTINOCREDITO!="V",] 
INPUT_TC_CMR		<-	INPUT_TC_CMR[INPUT_TC_CMR$DESTINOCREDITO!="R",]
PROCESO2<-INPUT_TC_CMR[1,1]
PROCESO<-as.Date(paste0(substr(PROCESO2,7,8),"-",substr(PROCESO2,5,6),"-",substr(PROCESO2,1,4)),format = "%d-%m-%Y")
# PROCESO
INPUT_TC_CMR$SALDO=INPUT_TC_CMR$AMORTIZACION+INPUT_TC_CMR$INTERES
INPUT_TC_CMR2<-aggregate(SALDO~FECHA_VENCIMIENTO_CUOTA+PP+FF,data=INPUT_TC_CMR, FUN=sum, na.rm=T)

INPUT_TC_CMR2$FECHA_VENCIMIENTO_CUOTA=as.Date(paste0(substr(INPUT_TC_CMR2$FECHA_VENCIMIENTO_CUOTA,7,8),"-",substr(INPUT_TC_CMR2$FECHA_VENCIMIENTO_CUOTA,5,6),"-",substr(INPUT_TC_CMR2$FECHA_VENCIMIENTO_CUOTA,1,4)),format = "%d-%m-%Y")

#####################################################################################################
#INPUT_TC_CMR_5<-INPUT_TC_CMR2[INPUT_TC_CMR2$FF==5,] #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
INPUT_TC_CMR_5<-INPUT_TC_CMR2                        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
INPUT_TC_CMR_5<-INPUT_TC_CMR_5[order(INPUT_TC_CMR_5$FECHA_VENCIMIENTO_CUOTA,INPUT_TC_CMR_5$PP),] 
INPUT_TC_CMR_5$SALDO<-INPUT_TC_CMR_5$SALDO*FG

colnames(INPUT_TC_CMR_5)=c("FVC","PP","FF","SALDO")
nn<-nrow(INPUT_TC_CMR_5)
 # i=150
for (i in 1:nrow(INPUT_TC_CMR_5)){
  # print(i)
  #i=1
  
  FVC<-INPUT_TC_CMR_5[i,1]
  PP<-INPUT_TC_CMR_5[i,2]
  FF<-INPUT_TC_CMR_5[i,3]
  ####################################################### TEST FECHAS #################################################################
  ####################################################### TEST FECHAS #################################################################
  FF<-INPUT_TC_CMR_5[i,3]
  FF
  MES<-month(INPUT_TC_CMR_5[i,1])
  if(FF<15) {
    FINI<-INPUT_TC_CMR_5[i,1]-months(1)+15
    FIFIN<-INPUT_TC_CMR_5[i,1]+15-1
  } else if(FF==20|FF==25){
    FINI<-INPUT_TC_CMR_5[i,1]-15
    FIFIN<-INPUT_TC_CMR_5[i,1]-15+months(1)-1
  } else if(FF==28){
    FINI<-INPUT_TC_CMR_5[i,1]-15+2
    FIFIN<-INPUT_TC_CMR_5[i,1]-15+2+months(1)-1
  } else if(FF==30){
    FINI<-INPUT_TC_CMR_5[i,1]-15
    FIFIN<-INPUT_TC_CMR_5[i,1]-15+months(1)-1
  } else if(FF==15){
    FINI<-INPUT_TC_CMR_5[i,1]-months(1)+15
    FINIXX<-INPUT_TC_CMR_5[i,1]-months(1)+25
    FIFIN<-INPUT_TC_CMR_5[i,1]+15-1
    FIFINXX<-INPUT_TC_CMR_5[i,1]+25-1
    if(MES==3){
      FINI<-INPUT_TC_CMR_5[i,1]-months(1)+15-1
    }else if(MES==2){
      FIFIN<-INPUT_TC_CMR_5[i,1]+15-2
    }
  }
  ####################################################### TEST FECHAS #################################################################
  ####################################################### TEST FECHAS #################################################################
  
  MONTO<-INPUT_TC_CMR_5[i,4]
  FLUXMES<-seq.Date(FINI,to=FIFIN,by="day")
  FLUXMES<-as.data.frame(FLUXMES)
  colnames(FLUXMES)<-c("FECHIX")
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # ultimo_del_mes <- function(dt){
  #   dt <- (as.character(dt))
  #   dt <- as.character(as.Date(dt) %m+% months(1))
  #   dt <- as.Date(ISOdate(as.numeric(substr(dt, 1, 4)),
  #                         as.numeric(substr(dt, 6, 7)),
  #                         1) - days(1))
  #   return(dt)
  # }
  # 
  # 
  # FINI+months(1)
  # FIFIN+months(1)
  # 
  # FINI+months(2)
  # FIFIN+months(2)
  # 
  # FINI+months(3)
  # FIFIN+months(3)
  # 
  # FINI+months(5)
  # FIFIN+months(5)
  # 
  # FIFIN<-FIFIN+1 
  # 
  # (ifelse(is.na(FIFIN+months(5)),ultimo_del_mes(FIFINXX+months(5)),FIFIN+months(5)))
  
  # add_with_rollback(FIFIN,months(5))
  
  # ultimo_del_mes(FINI+1)
  # FINI+1
  # as.Date("2021-01-30")-1
  # ifelse(is.na(FINI+months(1)),as.Date("2021-02-29"),FINI+months(1))
  # class(FINI)
  # 
  # FLUXMES1<-seq.Date(FINI+months(1),to=FIFIN+months(1),by="day")
  # FLUXMES1<-as.data.frame(FLUXMES1)
  # colnames(FLUXMES1)<-c("FECHIX")
  # 
  # FLUXMES2<-seq.Date(FINI+months(2),to=FIFIN+months(2),by="day")
  # FLUXMES2<-as.data.frame(FLUXMES2)
  # colnames(FLUXMES2)<-c("FECHIX")
  # 
  # FLUXMES3<-seq.Date(FINI+months(3),to=FIFIN+months(3),by="day")
  # FLUXMES3<-as.data.frame(FLUXMES3)
  # colnames(FLUXMES3)<-c("FECHIX")
  
  
  # ultimo_del_mes(FINI+1)
  # FINI+1
  # as.Date("2021-01-30")-1
  # ifelse(is.na(FINI+months(1)),as.Date("2021-02-29"),FINI+months(1))
  # class(FINI)
 
  FLUXMES1<-seq.Date( add_with_rollback(FINI,months(1)),to= add_with_rollback(FIFIN,months(1)),by="day")
  FLUXMES1<-as.data.frame(FLUXMES1)
  colnames(FLUXMES1)<-c("FECHIX")

  FLUXMES2<-seq.Date( add_with_rollback(FINI,months(2)),to= add_with_rollback(FIFIN,months(2)),by="day")
  FLUXMES2<-as.data.frame(FLUXMES2)
  colnames(FLUXMES2)<-c("FECHIX")

  FLUXMES3<-seq.Date( add_with_rollback(FINI,months(3)),to= add_with_rollback(FIFIN,months(3)),by="day")
  FLUXMES3<-as.data.frame(FLUXMES3)
  colnames(FLUXMES3)<-c("FECHIX")
  head(FLUXMES)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  FLUXMES$FVC<-FVC
  FLUXMES$PP<-PP
  FLUXMES$FF<-FF
  FLUXMES$MONTO<-MONTO
  FLUXMES$DIAMES=paste0(day(FLUXMES$FECHIX),"_",month(FLUXMES$FECHIX))
  FLUXMES	<-	merge(FLUXMES,INPUT_PERFILES,
                   by=c("DIAMES","PP"),all.x = TRUE)
  FLUXMES<-FLUXMES[order(FLUXMES$FECHIX),]
  SUM_FAC_PAG<-colSums(FLUXMES[7])
  SUM_FAC_PAGS100<-SUM_FAC_PAG
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # head(FLUXMES1)
  FLUXMES1$FVC<-FVC
  FLUXMES1$PP<-PP
  FLUXMES1$FF<-FF
  FLUXMES1$MONTO<-MONTO
  FLUXMES1$DIAMES=paste0(day(FLUXMES1$FECHIX),"_",month(FLUXMES1$FECHIX))
  FLUXMES1	<-	merge(FLUXMES1,INPUT_PERFILES,
                   by=c("DIAMES","PP"),all.x = TRUE)
  FLUXMES1<-FLUXMES1[order(FLUXMES1$FECHIX),]
  SUM_FAC_PAG1<-colSums(FLUXMES1[7])
  head(FLUXMES2)
  FLUXMES2$FVC<-FVC
  FLUXMES2$PP<-PP
  FLUXMES2$FF<-FF
  FLUXMES2$MONTO<-MONTO
  FLUXMES2$DIAMES=paste0(day(FLUXMES2$FECHIX),"_",month(FLUXMES2$FECHIX))
  FLUXMES2	<-	merge(FLUXMES2,INPUT_PERFILES,
                    by=c("DIAMES","PP"),all.x = TRUE)
  FLUXMES2<-FLUXMES2[order(FLUXMES2$FECHIX),]
  SUM_FAC_PAG2<-colSums(FLUXMES2[7])
  
  FLUXMES3$FVC<-FVC
  FLUXMES3$PP<-PP
  FLUXMES3$FF<-FF
  FLUXMES3$MONTO<-MONTO
  FLUXMES3$DIAMES=paste0(day(FLUXMES3$FECHIX),"_",month(FLUXMES3$FECHIX))
  FLUXMES3	<-	merge(FLUXMES3,INPUT_PERFILES,
                    by=c("DIAMES","PP"),all.x = TRUE)
  FLUXMES3<-FLUXMES3[order(FLUXMES3$FECHIX),]
  SUM_FAC_PAG3<-colSums(FLUXMES3[7])
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ###########################################################################################################################

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (SUM_FAC_PAG1>1){
    FREVOL11=0
    SUM_FAC_PAG1=1
  }else{
    FREVOL11=1-SUM_FAC_PAG1
  }
  
  if (SUM_FAC_PAG2>1){
    FREVOL12=0
    SUM_FAC_PAG2=1
  }else{
    FREVOL12=1-SUM_FAC_PAG2
  }
  
  if (SUM_FAC_PAG3>1){
    FREVOL13=0
    SUM_FAC_PAG3=1
  }else{
    FREVOL13=1-SUM_FAC_PAG3
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # SUM_FAC_PAG
  if (SUM_FAC_PAG>1){
    FREVOL1=0
    SUM_FAC_PAG=1
  }else{
    FREVOL1=1-SUM_FAC_PAG
  }
  
  ###########################################################################################################################
  FLUXMES$SUMAFACT<-SUM_FAC_PAGS100 #SUM_FAC_PAG 
  
  if (SUM_FAC_PAGS100>1){
    FLUXMES$FACTOR<-FLUXMES$FACTOR/SUM_FAC_PAGS100 
  }else{
    2+2
  }  
  
  # head(FLUXMES)
  FREVOL2=FREVOL1^2
  FREVOL3=FREVOL1^3
  # REVOL1<-MONTO*FREVOL1 #  MONTO*FREVOL1*SUM_FAC_PAG1                         (MONTO*FREVOL1-MONTO*FREVOL2-REVOL3<-MONTO*FREVOL3)
  # REVOL2<-MONTO*FREVOL2 # REVOL1/SUM_FAC_PAG1*FREVOL11*SUM_FAC_PAG2
  # REVOL3<-MONTO*FREVOL3 # REVOL2/SUM_FAC_PAG2*FREVOL12*SUM_FAC_PAG3
  # # REVOL1<-REVOL1-REVOL2-REVOL3
  # REVOL1<-MONTO*FREVOL1 *SUM_FAC_PAG1          #  (MONTO*FREVOL1-MONTO*FREVOL2-REVOL3<-MONTO*FREVOL3)
  # REVOL2<-REVOL1/SUM_FAC_PAG1*FREVOL11*SUM_FAC_PAG2
  # REVOL3<-REVOL2/SUM_FAC_PAG2*FREVOL12*SUM_FAC_PAG3
  
  REVOL1<-MONTO*FREVOL1 #*SUM_FAC_PAG1          #  (MONTO*FREVOL1-MONTO*FREVOL2-REVOL3<-MONTO*FREVOL3)
  REVOL2<-REVOL1*FREVOL11 #*SUM_FAC_PAG2
  REVOL3<-REVOL2*FREVOL12 #*SUM_FAC_PAG3
  

  REVOL4<-REVOL1-(REVOL1*SUM_FAC_PAG1+REVOL2*SUM_FAC_PAG2+REVOL3*SUM_FAC_PAG3)
  if (REVOL4<0){
    REVOL4=0 
  } 
  FVC_REV<-add_with_rollback(FVC,months(12))
  DAT_R4<-data.frame(FVC_REV, PP, FF, REVOL4)
  colnames(DAT_R4)<-c("FVC","PP","FF","PAGO_EST")
  # head(INPUT_TC_CMR_5)
  ######################################### HASTA ACÁ OK #############################################
  INPUT_TC_CMR_X		<-	INPUT_TC_CMR_5[INPUT_TC_CMR_5$PP=="P00",]
  # head(INPUT_TC_CMR_X)
  INPUT_TC_CMR_X<-INPUT_TC_CMR_X[,c("FVC","PP","FF")]
  # colnames(INPUT_TC_CMR_X)
  INPUT_TC_CMR_X[1,1]<-INPUT_TC_CMR_5[i,1]+months(1)
  INPUT_TC_CMR_X[2,1]<-INPUT_TC_CMR_5[i,1]+months(2)
  INPUT_TC_CMR_X[3,1]<-INPUT_TC_CMR_5[i,1]+months(3)
  INPUT_TC_CMR_X$REV1<-0
  INPUT_TC_CMR_X$REV2<-0
  INPUT_TC_CMR_X$REV3<-0
  INPUT_TC_CMR_X[1,4]<-REVOL1
  INPUT_TC_CMR_X[2,5]<-REVOL2
  INPUT_TC_CMR_X[3,6]<-REVOL3
  INPUT_TC_CMR_X$FVC_ORIG<-FVC
  INPUT_TC_CMR_X$PP<-PP
  INPUT_TC_CMR_X$FF<-FF
  # head(INPUT_TC_CMR_X)
  colnames(INPUT_TC_CMR_X)<-c("FVC","PP","FF","REV1","REV2","REV3","FVC_ORIG")  
  INPUT_TC_CMR_X<-INPUT_TC_CMR_X[,c("FVC","PP","FF","FVC_ORIG","REV1","REV2","REV3")]
  if (i==1){
    ACUM_REV_CMR_X<-INPUT_TC_CMR_X
    ACUM_DAT_R4<-DAT_R4
    
  }else{
    ACUM_REV_CMR_X			<-	rbind(ACUM_REV_CMR_X,INPUT_TC_CMR_X)
    ACUM_DAT_R4 <- rbind(ACUM_DAT_R4,DAT_R4)
  }  
  
  if (i==1){
    ESTIMAFLUXMES<-FLUXMES
  }else{
    ESTIMAFLUXMES			<-	rbind(ESTIMAFLUXMES,FLUXMES)
  }  
   print(paste(round(i/nn*100, 2), "%", sep=""))  
}  

#############################################################  FIN FOR   #########################################

TOTAL_REV_CMR_X<-aggregate(cbind(REV1,REV2,REV3)~FVC+PP+FF,data=ACUM_REV_CMR_X, FUN=sum, na.rm=T)

TOTAL_CMR_FF<-merge(INPUT_TC_CMR_5,TOTAL_REV_CMR_X,
                    by=c("FVC","PP","FF"),all.x = TRUE)
TOTAL_CMR_FF$REV1[is.na(TOTAL_CMR_FF$REV1)] <- 0
TOTAL_CMR_FF$REV2[is.na(TOTAL_CMR_FF$REV2)] <- 0
TOTAL_CMR_FF$REV3[is.na(TOTAL_CMR_FF$REV3)] <- 0

TOTAL_ESTIMA_FLUX<-merge(ESTIMAFLUXMES,TOTAL_CMR_FF,
                         by=c("FVC","PP","FF"),all.x = TRUE)

TOTAL_ESTIMA_FLUX$FLUJO_MES<-TOTAL_ESTIMA_FLUX$SALDO+TOTAL_ESTIMA_FLUX$REV1+TOTAL_ESTIMA_FLUX$REV2+TOTAL_ESTIMA_FLUX$REV3


# if (SUM_FAC_PAGS100>1){
#   TOTAL_ESTIMA_FLUX$PAGO_EST<-TOTAL_ESTIMA_FLUX$FLUJO_MES*TOTAL_ESTIMA_FLUX$FACTOR/SUM_FAC_PAGS100
# }else{
  TOTAL_ESTIMA_FLUX$PAGO_EST<-TOTAL_ESTIMA_FLUX$FLUJO_MES*TOTAL_ESTIMA_FLUX$FACTOR
# }  

TOTAL_ESTIMA_FLUX$FPROCES<-PROCESO
# print("perfil_MMMM 2097318 Perfil MM 2097426  Perfil puro 2096999")
# total1<-colSums(TOTAL_ESTIMA_FLUX[14])/1000000
TOTAL_ESTIMA_FLUX		<-	TOTAL_ESTIMA_FLUX[TOTAL_ESTIMA_FLUX$FECHIX>=PROCESO,]
# print("perfil_MMMM 1856220 Perfil MM  1852639   Perfil puro 1853347")
# total2<-colSums(TOTAL_ESTIMA_FLUX[14])/1000000
############################# se debe eliminar fechas anteriores a la fecha de proceso ################################
TOTAL_ESTIMA_FLUX<-TOTAL_ESTIMA_FLUX[,c("FPROCES", "FVC", "FECHIX","PP", "FF", "DIAMES", "FACTOR", "SUMAFACT", "MONTO", "SALDO",  "REV1",  "REV2", "REV3", "FLUJO_MES", "PAGO_EST")]
TOTAL_ESTIMA_FLUX$RESIDUAL<-TOTAL_ESTIMA_FLUX$FECHIX-TOTAL_ESTIMA_FLUX$FPROCES
TOTAL_ESTIMA_FLUX$DIAMES= as.character(TOTAL_ESTIMA_FLUX$DIAMES)
head(ACUM_DAT_R4)
ACUM_DAT_R4$FPROCES<-PROCESO
ACUM_DAT_R4$FECHIX<-ACUM_DAT_R4$FVC
ACUM_DAT_R4$DIAMES<-""
ACUM_DAT_R4$FACTOR<-0
ACUM_DAT_R4$SUMAFACT<-0
ACUM_DAT_R4$MONTO<-0
ACUM_DAT_R4$SALDO<-0
ACUM_DAT_R4$REV1<-0
ACUM_DAT_R4$REV2<-0
ACUM_DAT_R4$REV3<-0
ACUM_DAT_R4$FLUJO_MES<-0
ACUM_DAT_R4$RESIDUAL<-999
ACUM_DAT_R4<-ACUM_DAT_R4[,c("FPROCES", "FVC", "FECHIX","PP", "FF", "DIAMES", "FACTOR", "SUMAFACT", "MONTO", "SALDO",  "REV1",  "REV2", "REV3", "FLUJO_MES", "PAGO_EST", "RESIDUAL")]
TOTAL_ESTIMA_FLUX$CLASIFIC<-"Modelado"
ACUM_DAT_R4$CLASIFIC<-"NoRealocado"
TOTAL_ESTIMA_FLUX<-rbind(TOTAL_ESTIMA_FLUX,ACUM_DAT_R4)

DIR<-"D:/TC_CMR_INGRESO/OUTPUT_ESTIMADO"
setwd(DIR)  
write.table(TOTAL_ESTIMA_FLUX, "OUTPUT_TC_CMR_INGRESO.txt",row.names = F , sep="\t", dec = ",")
write.table(TOTAL_ESTIMA_FLUX, paste("OUTPUT_TC_CMR_INGRESO_",PROCESO,".txt"),row.names = F , sep="\t", dec = ",")
# colnames(TOTAL_ESTIMA_FLUX)
DIR<-"D:/TC_CMR_INGRESO"
setwd(DIR) 
ini
Sys.time()
# head(TOTAL_ESTIMA_FLUX)
# tail(TOTAL_ESTIMA_FLUX)
# total1
# total2
# dim(TOTAL_ESTIMA_FLUX)
