#config ----------------------------

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}
if (require (srvyr) == FALSE) {install.packages("srvyr"); require (srvyr)}
if (require (svrep) == FALSE) {install.packages("svrep"); require (svrep)}
if (require (PNADcIBGE) == FALSE) {install.packages("PNADcIBGE"); require (PNADcIBGE)}
if (require (survey) == FALSE) {install.packages("survey"); require (survey)}
if (require (ggrepel) == FALSE) {install.packages("ggrepel"); require (ggrepel)}
if (require (zoo) == FALSE) {install.packages("zoo"); require (zoo)}
if (require (openxlsx) == FALSE) {install.packages("openxlsx"); require (openxlsx)}
if (require (gtools) == FALSE) {install.packages("gtools"); require (gtools)}


rm(list = ls())
gc()

# calculando a composição da renda média por ano e por capital --------------------


rendasportipo <- function (ano, entrevista) {
  
  arquivo <- paste0 ('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}}, '.rds')
  pnadc_anual_visita <- import (here (arquivo))
  
  #renda de todos os trabalhos de cada decil e cada capital
  pnadc_anual_visita <- transform(pnadc_anual_visita, VD4020_real=VD4020*CO1e)
  db1 <- svyby (formula=~VD4020_real, 
                by=~Capital, 
                design=pnadc_anual_visita, 
                FUN=svymean, na.rm=T)
  db1$tipo <- 'Trabalhos'
 
  #renda bpc
  pnadc_anual_visita <- transform(pnadc_anual_visita, BPCreal=ifelse(is.na(V5001A2),0,V5001A2*CO1e))
  db2a <- svyby (formula=~BPCreal,
                 by=~Capital, 
                 design=pnadc_anual_visita,
                 FUN=svymean, 
                 na.rm=T)
  db2a$tipo <- 'BPC'
  colnames (db2a) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  
  #renda pbf
  pnadc_anual_visita <- transform(pnadc_anual_visita, PBFreal=ifelse(is.na(V5002A2),0,V5002A2*CO1e))
  db2b <- svyby (formula=~PBFreal, 
                 by=~Capital, 
                 design=pnadc_anual_visita, 
                 FUN=svymean, na.rm=T)
  db2b$tipo <- 'PBF'
  colnames (db2b) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  # renda outros programas
  pnadc_anual_visita <- transform(pnadc_anual_visita, Outros_Prog_real=ifelse(is.na(V5003A2),0,V5003A2*CO1e))
  db2c <- svyby (formula=~Outros_Prog_real, 
                 by=~Capital, 
                 design=pnadc_anual_visita, 
                 FUN=svymean, na.rm=T)
  db2c$tipo <- 'Outros programas sociais'
  colnames (db2c) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  #renda aposentadoria
  pnadc_anual_visita <- transform(pnadc_anual_visita, Apos_real=ifelse(is.na(V5004A2),0,V5004A2*CO1e))
  db3 <- svyby (formula=~Apos_real, 
                by=~Capital, 
                design=pnadc_anual_visita, 
                FUN=svymean, na.rm=T)
  db3$tipo <- 'Aposentadoria'
  colnames (db3) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  #seguro desemp e defeso
  pnadc_anual_visita <- transform(pnadc_anual_visita, Seg_real=ifelse(is.na(V5005A2),0,V5005A2*CO1e))
  db4 <- svyby (formula=~Seg_real, 
                by=~Capital, 
                design=pnadc_anual_visita, 
                FUN=svymean, na.rm=T)
  db4$tipo <- 'Seguros'
  colnames (db4) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  #pensões e doações
  pnadc_anual_visita <- transform(pnadc_anual_visita, Pens_real=ifelse(is.na(V5006A2),0,V5006A2*CO1e))
  db5 <- svyby (formula=~Pens_real, 
                by=~Capital, 
                design=pnadc_anual_visita, 
                FUN=svymean, na.rm=T)
  db5$tipo <- 'Pensões e Doações'
  colnames (db5) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  #rendimentos de aluguel ou arrendamento
  pnadc_anual_visita <- transform(pnadc_anual_visita, Alug_real=ifelse(is.na(V5007A2),0,V5007A2*CO1e))
  db6 <- svyby (formula=~Alug_real, 
                by=~Capital, 
                design=pnadc_anual_visita, 
                FUN=svymean, na.rm=T)
  db6$tipo <- 'Imóveis'
  colnames (db6) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  #outros
  pnadc_anual_visita <- transform(pnadc_anual_visita, Outros_real=ifelse(is.na(V5008A2),0,V5008A2*CO1e))
  db7 <- svyby (formula=~Outros_real, 
                by=~Capital, 
                design=pnadc_anual_visita, 
                FUN=svymean, na.rm=T)
  db7$tipo <- 'Outros'
  colnames (db7) <- c('capital.decil', 'valor', 'se', 'tipo')
  
  db <- bind_rows(db1,db2a,db2b,db2c,db3,db4,db5,db6,db7)
  db$ano <- {{ano}}
  colnames (db) <- c('capital', 'valor', 'se', 'tipo', 'ano')
  
  return (db)
}

db23 <- rendasportipo(2023,1)
db22 <- rendasportipo(2022,5)
db21 <- rendasportipo(2021,5)
db20 <- rendasportipo(2020,5)
db19 <- rendasportipo(2019,5)
db18 <- rendasportipo(2018,5)
db17 <- rendasportipo(2017,5)
db16 <- rendasportipo(2016,5)

db <- bind_rows (db23,db22,db21,db20,db19,db18,db17,db16)


write.xlsx(db, 'rendamediapercapita_capital_ano_componente.xlsx')
