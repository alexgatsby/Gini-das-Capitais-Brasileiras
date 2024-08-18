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

# FORA - calculando a composição da renda média por ano e por capital --------------------

interesse <- c('Município de Porto Velho (RO)',
               'Município de Rio Branco (AC)',
               'Município de Manaus (AM)',
               'Município de Boa Vista (RR)',
               'Município de Belém (PA)',
               'Município de Macapá (AP)',
               'Município de Palmas (TO)',
               'Município de São Luís (MA)',
               'Município de Teresina (PI)',
               'Município de Fortaleza (CE)',
               'Município de Natal (RN)',
               'Município de João Pessoa (PB)',
               'Município de Recife (PE)',
               'Município de Maceió (AL)',
               'Município de Aracaju (SE)',
               'Município de Salvador (BA)',
               'Município de Belo Horizonte (MG)',
               'Município de Vitória (ES)',
               'Município de Rio de Janeiro (RJ)',
               'Município de São Paulo (SP)',
               'Município de Curitiba (PR)',
               'Município de Florianópolis (SC)',
               'Município de Porto Alegre (RS)',
               'Município de Campo Grande (MS)',
               'Município de Cuiabá (MT)',
               'Município de Goiânia (GO)',
               'Município de Brasília (DF)')

rendasportipo <- function (ano, entrevista,capitais) {
  
  interesse <- {{capitais}}
  
  arquivo <- paste0 ('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}}, '.rds')
  data <- import (here (arquivo))
  data$variables <- transform(data$variables, VD4020_real=VD4020*CO1e)
  data$variables <- transform(data$variables, VD4047_reala=V5001A2*CO1e)
  data$variables <- transform(data$variables, VD4047_realb=V5002A2*CO1e)
  data$variables <- transform(data$variables, VD4047_realc=V5003A2*CO1e)
  data$variables <- transform(data$variables, V5004A2_real=V5004A2*CO1e)
  data$variables <- transform(data$variables, V5005A2_real=V5005A2*CO1e)
  data$variables <- transform(data$variables, V5006A2_real=V5006A2*CO1e)
  data$variables <- transform(data$variables, V5007A2_real=V5007A2*CO1e)
  data$variables <- transform(data$variables, V5008A2_real=V5008A2*CO1e)
  
  db1 <- svyby (formula=~VD4020_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svymean, na.rm=T)
  db1$tipo <- 'Trabalhos'
  colnames (db1) <- c('capital', 'valor', 'se', 'tipo')
  
  db2a <- svyby (formula=~VD4047_reala, by=~Capital, design=subset(data, Capital%in%interesse), 
                 FUN=svymean, na.rm=T)
  db2a$tipo <- 'BPC'
  colnames (db2a) <- c('capital', 'valor', 'se', 'tipo')
  
  db2b <- svyby (formula=~VD4047_realb, by=~Capital, design=subset(data, Capital%in%interesse), 
                 FUN=svymean, na.rm=T)
  db2b$tipo <- 'PBF'
  colnames (db2b) <- c('capital', 'valor', 'se', 'tipo')
  
  db2c <- svyby (formula=~VD4047_realc, by=~Capital, design=subset(data, Capital%in%interesse), 
                 FUN=svymean, na.rm=T)
  db2c$tipo <- 'Outros programas sociais'
  colnames (db2c) <- c('capital', 'valor', 'se', 'tipo')
  
  db3 <- svyby (formula=~V5004A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svymean, na.rm=T)
  db3$tipo <- 'Aposentadoria'
  colnames (db3) <- c('capital', 'valor', 'se', 'tipo')
  
  db4 <- svyby (formula=~V5005A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svymean, na.rm=T)
  db4$tipo <- 'Seguros'
  colnames (db4) <- c('capital', 'valor', 'se', 'tipo')
  
  db5 <- svyby (formula=~V5006A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svymean, na.rm=T)
  db5$tipo <- 'Pensões e Doações'
  colnames (db5) <- c('capital', 'valor', 'se', 'tipo')
  
  db6 <- svyby (formula=~V5007A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svymean, na.rm=T)
  db6$tipo <- 'Imóveis'
  colnames (db6) <- c('capital', 'valor', 'se', 'tipo')
  
  db7<- svyby (formula=~V5008A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
               FUN=svymean, na.rm=T)
  db7$tipo <- 'Outros'
  colnames (db7) <- c('capital', 'valor', 'se', 'tipo')
  
  db <- bind_rows(db1,db2a,db2b,db3,db4,db5,db6,db7)
  db$ano <- {{ano}}
  colnames (db) <- c('capital', 'valor', 'se', 'tipo', 'ano')
  
  return (db)
}

db23 <- rendasportipo(2023,1,interesse)
db22 <- rendasportipo(2022,5,interesse)
db21 <- rendasportipo(2021,5,interesse)
db20 <- rendasportipo(2020,5,interesse)
db19 <- rendasportipo(2019,5,interesse)
db18 <- rendasportipo(2018,5,interesse)
db17 <- rendasportipo(2017,5,interesse)
db16 <- rendasportipo(2016,5,interesse)

db <- bind_rows (db23,db22,db21,db20,db19,db18,db17,db16)


write.xlsx(db, 'rendamediapercapita_capital_ano_componente.xlsx')
