# pacotes

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (srvyr) == FALSE) {install.packages("srvyr"); require (srvyr)}
if (require (svrep) == FALSE) {install.packages("svrep"); require (svrep)}
if (require (PNADcIBGE) == FALSE) {install.packages("PNADcIBGE"); require (PNADcIBGE)}
if (require (survey) == FALSE) {install.packages("survey"); require (survey)}
if (require (convey) == FALSE) {install.packages("convey"); require (convey)}
if (require (zoo) == FALSE) {install.packages("zoo"); require (zoo)}
if (require (openxlsx) == FALSE) {install.packages("openxlsx"); require (openxlsx)}

# Extraindo dados

rm(list = ls())

# dados trimestre - extraindo -------------

db <- data.frame(trimestre=rep(1:4, 11),
                 ano=c(rep(2012,4),
                       rep(2013,4),
                       rep(2014,4),
                       rep(2015,4),
                       rep(2016,4),
                       rep(2017,4),
                       rep(2018,4),
                       rep(2019,4),
                       rep(2020,4),
                       rep(2021,4),
                       rep(2022,4)))



for (i in rev(2:43)) {
  
  nome <- paste0('data/pnad', db$ano[i], '.', db$trimestre[i], '.rds')
  
  db <- get_pnadc(year=db$ano[i], quarter=db$trimestre[i])
  
  saveRDS(db, nome)
  
}

db <- get_pnadc(year=2021, quarter=1)
nome <- paste0('data/pnad', 2021, '.', 1, '.rds')
saveRDS(db, nome)


# function - trimestral


ranking_gini_capitais_trimestral_vd4020 <- function(i, ano){
  
  data <- import (i)
  
  prep <- convey_prep(data)
  
  gini <- svyby(formula=~VD4020, by=~Capital, design=prep, FUN=svygini, na.rm=TRUE)
  
  gini <- as_tibble(gini)
  
  gini$ano <- ano
  
  gini$ranking_menos_desigual <- rank(gini$VD4020)
  
  gini$ranking_mais_desigual <- dense_rank(desc(gini$VD4020))
  
  return (gini)
  
}

lista <- c(list.files('data')[14:26], list.files('data')[1])

db0 <- data.frame()

for (i in lista){
  
  data <- paste0('data/', i)
  
  ano <- str_sub (i, start=5, end=11)
  
  temp <- ranking_gini_capitais_trimestral_vd4020 (data, ano)
  
  db0 <- bind_rows(db0, temp)
  
}

glimpse (db0)
saveRDS(db0, 'extracted_data/ginis_trimestrais_2021a2024.rds')

# gráfico -----------

db0 %>%
  filter (ano!='2012.1.') %>%
  filter (Capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=ano)) +
  geom_line(aes(y=VD4020, group=1)) +
  geom_point(aes(y=VD4020)) +
  geom_text (aes(y=VD4020,
                 label=paste0(round(VD4020,3),
                              '\n', ranking_menos_desigual,
                              'ª capital mais\nigualitária')),
             vjust=2,
             size=3) +
  scale_y_continuous(limits=c(0.25,0.75), n.breaks=10) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (title='Índice de Gini para o Recife baseado na Renda de Todos os Trabalhos',
        x=NULL,
        y=NULL,
        color='Entrevista\nmáxima')

ggsave ('results/gini_linha_trimestral_vd4020.png', width=12, height = 5)
