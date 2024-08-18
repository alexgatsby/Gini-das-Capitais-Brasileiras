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

# baixando dados sem formato survey -----------
#pnadc_anual_visita <- get_pnadc(year=2023, interview=1, labels=TRUE, deflator=TRUE, design=FALSE)
#saveRDS(pnadc_anual_visita, 'data/pnad_anual_2023_interview1_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2016_interview5_nodesign.rds'))

# limpeza e transformações ----------

#id domicílio
pnadc_anual_visita <- transform(pnadc_anual_visita, ID_DOMICILIO=paste0(UPA,V1008,V1014))

#preparando desagregação por país e grande região
pnadc_anual_visita <- transform(pnadc_anual_visita, Pais=as.factor("Brasil"))
pnadc_anual_visita$Pais <- factor(x=pnadc_anual_visita$Pais, levels=c("Brasil"))
pnadc_anual_visita <- transform(pnadc_anual_visita, GR=as.factor(ifelse(substr(UPA, start=1, stop=1)=="1","Norte",ifelse(substr(UPA, start=1, stop=1)=="2","Nordeste",ifelse(substr(UPA, start=1, stop=1)=="3","Sudeste",ifelse(substr(UPA, start=1, stop=1)=="4","Sul",ifelse(substr(UPA, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
pnadc_anual_visita$GR <- factor(x=pnadc_anual_visita$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))

#retirando não moradores e atribuindo 1 aos moradores
pnadc_anual_visita <- transform(pnadc_anual_visita, 
                                V2001_rendimento=ifelse(V2005=="Pensionista" | 
                                                          V2005=="Empregado(a) doméstico(a)" | 
                                                          V2005=="Parente do(a) empregado(a) doméstico(a)",
                                                        NA,1))

#rendimento efetivo de todos os trabalhos
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4020real_proprioano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*CO1e))

#rendimento efetivo de outras fontes
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))

#agrupando por domicílio
pnadc_anual_visita_rendimento <- pnadc_anual_visita %>% 
  dplyr::group_by(ID_DOMICILIO) %>% 
  dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                   rendimento_todos_trabalhos_proprioano=sum(VD4020real_proprioano, na.rm=TRUE),
                   rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE))

#renda total do domicílio
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)

#renda média per capita do domicílio
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)

#reunificando bases de dados
pnadc_anual_visita <- pnadc_anual_visita[,!(names(pnadc_anual_visita) %in% c("V2001_rendimento","VD4020real_proprioano","VD4048real_proprioano"))]
pnadc_anual_visita_rendimento <- pnadc_anual_visita_rendimento[,!(names(pnadc_anual_visita_rendimento) %in% c("moradores_rendimento",
                                                                                                              "rendimento_todos_trabalhos_proprioano",
                                                                                                              "rendimento_outras_fontes_proprioano"))]
pnadc_anual_visita <- merge(x=pnadc_anual_visita, y=pnadc_anual_visita_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
rm(pnadc_anual_visita_rendimento)
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))

#voltando ao formato survey
pnadc_anual_visita <- tibble::as_tibble(x=pnadc_anual_visita)
pnadc_anual_visita <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_anual_visita)


# rendimento mensal real domiciliar per capita por local ----------
rendamediapercapita_all_levels <- survey::svybys(formula=~VD5008real_proprioano,
                                                 bys=~Pais+GR+UF+Capital,
                                                 design=pnadc_anual_visita,
                                                 FUN=svymean,
                                                 na.rm=TRUE)

rendamediapercapita_all_levels <- bind_rows(
  rendamediapercapita_all_levels[[1]] %>% rename('local'=Pais),
  rendamediapercapita_all_levels[[2]] %>% rename('local'=GR),
  rendamediapercapita_all_levels[[3]] %>% rename('local'=UF),
  rendamediapercapita_all_levels[[4]] %>% rename('local'=Capital),
)

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2016 - E5'


# rendimento mensal médio por decil e local -------------

decis <- seq(0.1,1,by=0.1)

decilnolocal <- svybys (formula=~VD5008real_proprioano,
                        bys=~Pais+GR+UF+Capital,
                        design=pnadc_anual_visita,
                        FUN=svyquantile,
                        quantiles=decis,
                        na.rm=TRUE)

decilnolocalok <- bind_rows(
  decilnolocal[[1]] %>% rename('local'=Pais),
  decilnolocal[[2]] %>% rename('local'=GR),
  decilnolocal[[3]] %>% rename('local'=UF),
  decilnolocal[[4]] %>% rename('local'=Capital),
)

decilnolocalok$ed <- 'PNAD Anual - 2016 - E5'



#esses dados são cuting points, agora quero criar uma variável com eles

# atribuindo um decil para cada pessoa na capital --------------------------

decilcapitais <- decilnolocal[[4]] %>%
  select (Capital:VD5008real_proprioano.1)

pnadc_anual_visita <- transform (pnadc_anual_visita, decilnacapital='oi')

for (i in decilcapitais$Capital){
  
  capital_vez <- decilcapitais %>% filter(Capital==i)
  
  pnadc_anual_visita <- transform(pnadc_anual_visita,
                                  decilnacapital=ifelse (Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.1, 'VD5008real_proprioano.0.1',
                                                         ifelse (Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.2, 'VD5008real_proprioano.0.2',
                                                                 ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.3, 'VD5008real_proprioano.0.3',
                                                                        ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.4, 'VD5008real_proprioano.0.4',
                                                                               ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.5, 'VD5008real_proprioano.0.5',
                                                                                      ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.6, 'VD5008real_proprioano.0.6',
                                                                                             ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.7, 'VD5008real_proprioano.0.7',
                                                                                                    ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.8, 'VD5008real_proprioano.0.8',
                                                                                                           ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.0.9, 'VD5008real_proprioano.0.9',
                                                                                                                  ifelse(Capital==i&VD5008real_proprioano<=capital_vez$VD5008real_proprioano.1, 'VD5008real_proprioano.1',
                                                                                                                         decilnacapital)))))))))))
  
  
}

pnadc_anual_visita$variables %>% count (Capital, decilnacapital)

# vendo a composição da renda de cada decil --------------------

#renda de todos os trabalhos de cada decil e cada capital
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4020_real=VD4020*CO1e)
db1 <- svyby (formula=~VD4020_real, 
              by=~interaction(Capital, decilnacapital), 
              design=pnadc_anual_visita, 
              FUN=svymean, na.rm=T)
db1$tipo <- 'Trabalhos'
colnames (db1) <- c('capital.decil', 'valor', 'se', 'tipo')

#renda bpc
pnadc_anual_visita <- transform(pnadc_anual_visita, BPCreal=ifelse(is.na(V5001A2),0,V5001A2*CO1e))
db2a <- svyby (formula=~BPCreal,
               by=~interaction(Capital, decilnacapital), 
               design=pnadc_anual_visita,
               FUN=svymean, 
               na.rm=T)
db2a$tipo <- 'BPC'
colnames (db2a) <- c('capital.decil', 'valor', 'se', 'tipo')


#renda pbf
pnadc_anual_visita <- transform(pnadc_anual_visita, PBFreal=ifelse(is.na(V5002A2),0,V5002A2*CO1e))
db2b <- svyby (formula=~PBFreal, 
               by=~interaction(Capital, decilnacapital), 
               design=pnadc_anual_visita, 
               FUN=svymean, na.rm=T)
db2b$tipo <- 'PBF'
colnames (db2b) <- c('capital.decil', 'valor', 'se', 'tipo')

# renda outros programas
pnadc_anual_visita <- transform(pnadc_anual_visita, Outros_Prog_real=ifelse(is.na(V5003A2),0,V5003A2*CO1e))
db2c <- svyby (formula=~Outros_Prog_real, 
               by=~interaction(Capital, decilnacapital), 
               design=pnadc_anual_visita, 
               FUN=svymean, na.rm=T)
db2c$tipo <- 'Outros programas sociais'
colnames (db2c) <- c('capital.decil', 'valor', 'se', 'tipo')

#renda aposentadoria
pnadc_anual_visita <- transform(pnadc_anual_visita, Apos_real=ifelse(is.na(V5004A2),0,V5004A2*CO1e))
db3 <- svyby (formula=~Apos_real, 
              by=~interaction(Capital, decilnacapital), 
              design=pnadc_anual_visita, 
              FUN=svymean, na.rm=T)
db3$tipo <- 'Aposentadoria'
colnames (db3) <- c('capital.decil', 'valor', 'se', 'tipo')

#seguro desemp e defeso
pnadc_anual_visita <- transform(pnadc_anual_visita, Seg_real=ifelse(is.na(V5005A2),0,V5005A2*CO1e))
db4 <- svyby (formula=~Seg_real, 
              by=~interaction(Capital, decilnacapital), 
              design=pnadc_anual_visita, 
              FUN=svymean, na.rm=T)
db4$tipo <- 'Seguros'
colnames (db4) <- c('capital.decil', 'valor', 'se', 'tipo')

#pensões e doações
pnadc_anual_visita <- transform(pnadc_anual_visita, Pens_real=ifelse(is.na(V5006A2),0,V5006A2*CO1e))
db5 <- svyby (formula=~Pens_real, 
              by=~interaction(Capital, decilnacapital), 
              design=pnadc_anual_visita, 
              FUN=svymean, na.rm=T)
db5$tipo <- 'Pensões e Doações'
colnames (db5) <- c('capital.decil', 'valor', 'se', 'tipo')

#rendimentos de aluguel ou arrendamento
pnadc_anual_visita <- transform(pnadc_anual_visita, Alug_real=ifelse(is.na(V5007A2),0,V5007A2*CO1e))
db6 <- svyby (formula=~Alug_real, 
              by=~interaction(Capital, decilnacapital), 
              design=pnadc_anual_visita, 
              FUN=svymean, na.rm=T)
db6$tipo <- 'Imóveis'
colnames (db6) <- c('capital.decil', 'valor', 'se', 'tipo')

#outros
pnadc_anual_visita <- transform(pnadc_anual_visita, Outros_real=ifelse(is.na(V5008A2),0,V5008A2*CO1e))
db7 <- svyby (formula=~Outros_real, 
              by=~interaction(Capital, decilnacapital), 
              design=pnadc_anual_visita, 
              FUN=svymean, na.rm=T)
db7$tipo <- 'Outros'
colnames (db7) <- c('capital.decil', 'valor', 'se', 'tipo')


#juntando
db <- bind_rows(db1,db2a,db2b,db2c,db3,db4,db5,db6,db7)
db$ano <- 2016
glimpse (db)

db %>% count (tipo)

write.xlsx(db, 'rendamediapercapita_capital_2016_componente_decil.xlsx')

# juntando resultados -------

db <- bind_rows(import('results_estudo_gini/rendamediapercapita_capital_2016_componente_decil.xlsx'),
                import('results_estudo_gini/rendamediapercapita_capital_2017_componente_decil.xlsx'),
                import('results_estudo_gini/rendamediapercapita_capital_2018_componente_decil.xlsx'),
                import('results_estudo_gini/rendamediapercapita_capital_2019_componente_decil.xlsx'),
                import('results_estudo_gini/rendamediapercapita_capital_2020_componente_decil.xlsx'),
                import('results_estudo_gini/rendamediapercapita_capital_2021_componente_decil.xlsx'),
                import('results_estudo_gini/rendamediapercapita_capital_2022_componente_decil.xlsx'),
                import('results_estudo_gini/rendamediapercapita_capital_2023_componente_decil.xlsx'))

glimpse (db)
write.xlsx(db, 'results_estudo_gini/rendamediapercapita_capital_ano_componente_decil.xlsx')
