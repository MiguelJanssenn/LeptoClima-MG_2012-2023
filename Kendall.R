#========================================= ATRIBUIÇÕES ==============================================
library(ggplot2)
library(dplyr)
library(readr)
library(gtsummary)

library(gridExtra)
library(lubridate)
library(tidyverse)
library(nortest)
library(tidyr)
library(ggbump)


#========================================= BAIXANDO OS BANCOS =======================================
setwd("C:/Users/migue/Documents/Projeto IC Leptospirose/RStudioLEP/Diretório")

#Meses
Meses <-data.frame (Num = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                    Mes= c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                           "Jul", "Ago", "Set", "Out", "Nov", "Dez"))

#Código do Município
Municipio <- read_csv("Municipio.csv", col_types = cols(IBGE6 = col_character(), 
                                                        IBGE7 = col_character(), 
                                                        latitude = col_character(), 
                                                        longitude = col_character()))
#População do Município
População <- read_csv("População.csv", 
                      col_types = cols(`2012` = col_number(), 
                                       `2013` = col_number(), `2014` = col_number(), 
                                       `2015` = col_number(), `2016` = col_number(), 
                                       `2017` = col_number(), `2018` = col_number(), 
                                       `2019` = col_number(), `2020` = col_number(), 
                                       `2021` = col_number(), `2022` = col_number(),
                                       `2023` = col_number()))

#Datasus
LEPTMG <- read_csv("LEPTMG.csv", col_types = cols(DT_NOTIFIC = col_date(format = "%Y-%m-%d"), 
                                                  NU_ANO = col_number(), ID_MUNICIP = col_character(), 
                                                  NU_IDADE_N = col_number(), ID_MN_RESI = col_character()))
#INMET
INMET <- read_csv("Precipitação.csv", 
                         col_types = cols(Código = col_character(), 
                                          Municipio = col_character(), Mesorregião = col_character(), 
                                          Ano = col_number(), Data = col_date(format = "%Y-%m-%d"), 
                                          `Precipitação mensal (mm)` = col_number()))


#========================================= TRATANDO OS BANCOS =============================================

#SINAN E IGBE:
LEPTMG <- left_join(LEPTMG, Municipio %>% select(Município, Mesorregiao, latitude, longitude, IBGE6), by = c("ID_MN_RESI" = "IBGE6")) #processamento de municípios
LEPTMG["Dataref"]<- LEPTMG$DT_NOTIFIC - ddays(14)#  PERÍODO DE INCUBAÇÃO
LEPTMG["Anoref"]<- as.character( format(LEPTMG$Dataref,"%Y"))
LEPTMG["M"] <- as.character( format(LEPTMG$Dataref,"%m"))
LEPTMG["N"] <- 1
LEPTMG <- filter(LEPTMG, Anoref != 2011)

#INMET
INMET$`Precipitação mensal (mm)`[INMET$`Precipitação mensal (mm)` == "NA"] <- NA
INMET <- left_join(INMET, Meses, by = c("M" = "Num"))
INMET <- na.omit(INMET)

#========================================= INCIDÊNCIAS ==============================================

for (a in 2012:2023){
for (b in 1:12) {
  TEMP <- LEPTMG %>% filter((Mesorregiao == População$Mesorregiao[b])& (Anoref == a))
  TEMP <- TEMP %>% group_by(M)%>% summarise(A = sum(N)/População[b,(a-2010), drop = TRUE]*100000)#incidência
  TEMP2 <- INMET %>% filter((Mesorregião == População$Mesorregiao[b])& (Ano == a))
  TEMP2 <- TEMP2 %>% group_by(M)%>% summarise(B = median(`Precipitação mensal (mm)`))#mediana
  if(b == 1) { TEMPP <- left_join(Meses, TEMP, by = c("Num" = "M"))
  TEMPP <- left_join(TEMPP, TEMP2, by = c("Num" = "M"))
  } else {
  TEMPP <- left_join(TEMPP, TEMP, by = c("Num" = "M"))
  TEMPP <- left_join(TEMPP, TEMP2, by = c("Num" = "M")) }
  if(b == 12) {TEMPP["Ano"] <- a}}
  if(a == 2012) { INC <- TEMPP} else {INC <- rbind(INC,TEMPP) }
  if(a == 2023) {
  INC <- INC %>% mutate_all(replace_na, 0)
  INC <- rename(INC,c(IncCV = "A.x", IncCM = "A.y", IncJeq = "A.x.x", IncMBH = "A.y.y", 
  IncNoMG = "A.x.x.x", IncNMG = "A.y.y.y", IncOMG = "A.x.x.x.x", IncSSeMG = "A.y.y.y.y",
  IncTrMG = "A.x.x.x.x.x", IncVM = "A.y.y.y.y.y", IncVRD = "A.x.x.x.x.x.x", IncZM = "A.y.y.y.y.y.y",
  PluvCV = "B.x", PluvCM = "B.y", PluvJeq = "B.x.x", PluvMBH = "B.y.y",PluvNoMG = "B.x.x.x",
  PluvNMG = "B.y.y.y", PluvOMG = "B.x.x.x.x", PluvSSeMG = "B.y.y.y.y", PluvTrMG = "B.x.x.x.x.x",
  PluvVM = "B.y.y.y.y.y", PluvVRD = "B.x.x.x.x.x.x", PluvZM = "B.y.y.y.y.y.y"))}}

#=========================================== CORRELAÇÕES ==============================================
Kendall <- data.frame(População$Mesorregiao)
Kendall["Qgis"] <- c("3111", "3106", "3103", "3107", "3101", "3102",
                     "3109", "3110", "3105", "3104", "3108", "3112")

a=1
for (a in 1:12) {
  b<- (a*2)+1
  Ken <- cor.test(INC[,b],INC[,(b+1)], method = "k", data = NULL)
  Spe <-cor.test(INC[,b],INC[,(b+1)], method = "s", data = NULL)
  
  Tau <- as.numeric(substr(as.character(Ken["estimate"]), start = 09, stop = 19))
  Rho <- as.numeric(substr(as.character(Spe["estimate"]), start = 09, stop = 19))
  
  Kendall[a,"Tau (K)"] <- round(Tau, 2)

  
  Y <- as.numeric(as.character(Ken["p.value"]))
  if(Y <= 0.05){Kendall[a, "p (K)"] <- "*"}
  else{
  Kendall[a, "p (K)"] <- round(Y, 2) }
  
  Kendall[a,"Rho (S)"] <- round(Rho, 2)
  Z <- as.numeric(as.character(Spe["p.value"]))
  if(Y <= 0.05){Kendall[a, "p (S)"] <- "*"}
  else{
    Kendall[a, "p (S)"] <- round(Z, 2) }
  
}




 
