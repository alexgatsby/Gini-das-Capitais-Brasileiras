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

#2023 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2023, interview=1, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2023_interview1_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2023_interview1_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2023 - E1'

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2023.xlsx')

# rendimento mensal médio por decil e local

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

decilnolocalok$ed <- 'PNAD Anual - 2023 - E1'

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2023.xlsx')

#2022 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2022, interview=5, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2022_interview5_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2022_interview5_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2022 - E5'

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2022.xlsx')

# rendimento mensal médio por decil e local

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

decilnolocalok$ed <- 'PNAD Anual - 2022 - E5'

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2022.xlsx')

#2021 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2021, interview=5, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2021_interview5_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2021_interview5_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2021 - E5'

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2021.xlsx')

# rendimento mensal médio por decil e local

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

decilnolocalok$ed <- 'PNAD Anual - 2021 - E5'

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2021.xlsx')

#2020 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2020, interview=5, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2020_interview5_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2020_interview5_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2020 - E5'

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2020.xlsx')

# rendimento mensal médio por decil e local

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

decilnolocalok$ed <- 'PNAD Anual - 2020 - E5'

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2020.xlsx')

#2019 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2019, interview=5, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2019_interview5_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2019_interview5_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2019 - E5'

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2019.xlsx')

# rendimento mensal médio por decil e local

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

decilnolocalok$ed <- 'PNAD Anual - 2019 - E5'

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2019.xlsx')

#2018 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2018, interview=5, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2018_interview5_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2018_interview5_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2018 - E5'

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2018.xlsx')

# rendimento mensal médio por decil e local

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

decilnolocalok$ed <- 'PNAD Anual - 2018 - E5'

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2018.xlsx')

#2017 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2017, interview=5, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2017_interview5_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2017_interview5_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

rendamediapercapita_all_levels$ed <- 'PNAD Anual - 2017 - E5'

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2017.xlsx')

# rendimento mensal médio por decil e local

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

decilnolocalok$ed <- 'PNAD Anual - 2017 - E5'

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2017.xlsx')

#2016 ------------

# baixando dados sem formato survey
pnadc_anual_visita <- get_pnadc(year=2016, interview=5, labels=TRUE, deflator=TRUE, design=FALSE)
saveRDS(pnadc_anual_visita, 'data/pnad_anual_2016_interview5_nodesign.rds')
pnadc_anual_visita <- import (here ('data/pnad_anual_2016_interview5_nodesign.rds'))

# limpeza e transformações

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


# rendimento mensal real domiciliar per capita por local
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

write.xlsx(rendamediapercapita_all_levels, 'extracted_data/rendamediapercapita_all_levels_2016.xlsx')

# rendimento mensal médio por decil e local

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

write.xlsx(decilnolocalok, 'extracted_data/rendamediapercapita_decilnolocal_2016.xlsx')

# juntando -----


rendamedia_local_ano <- bind_rows(import('extracted_data/rendamediapercapita_all_levels_2016.xlsx'),
                                  import('extracted_data/rendamediapercapita_all_levels_2017.xlsx'),
                                  import('extracted_data/rendamediapercapita_all_levels_2018.xlsx'),
                                  import('extracted_data/rendamediapercapita_all_levels_2019.xlsx'),
                                  import('extracted_data/rendamediapercapita_all_levels_2020.xlsx'),
                                  import('extracted_data/rendamediapercapita_all_levels_2021.xlsx'),
                                  import('extracted_data/rendamediapercapita_all_levels_2022.xlsx'),
                                  import('extracted_data/rendamediapercapita_all_levels_2023.xlsx')
                                  )

write.xlsx(rendamedia_local_ano, 'rendapercapitamedia_capital_ano.xlsx')


rendamedia_local_ano_decil <- bind_rows(import('extracted_data/rendamediapercapita_decilnolocal_2016.xlsx'),
                                        import('extracted_data/rendamediapercapita_decilnolocal_2017.xlsx'),
                                        import('extracted_data/rendamediapercapita_decilnolocal_2018.xlsx'),
                                        import('extracted_data/rendamediapercapita_decilnolocal_2019.xlsx'),
                                        import('extracted_data/rendamediapercapita_decilnolocal_2020.xlsx'),
                                        import('extracted_data/rendamediapercapita_decilnolocal_2021.xlsx'),
                                        import('extracted_data/rendamediapercapita_decilnolocal_2022.xlsx'),
                                        import('extracted_data/rendamediapercapita_decilnolocal_2023.xlsx')
                                        )

write.xlsx(rendamedia_local_ano_decil, 'rendapercapitamedia_capital_ano_decil.xlsx')



