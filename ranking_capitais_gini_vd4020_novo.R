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

# function


ranking_gini_capitais <- function(i, ano){
  
  data <- import (i)
  
  prep <- convey_prep(data)
  
  gini <- svyby(formula=~VD4020, by=~Capital, design=prep, FUN=svygini, na.rm=TRUE)
  
  gini <- as_tibble(gini)
  
  gini$ano <- ano
  
  gini$ranking_menos_desigual <- rank(gini$VD4020)
  
  gini$ranking_mais_desigual <- dense_rank(desc(gini$VD4020))
  
  return (gini)
  
}

db<- ranking_gini_capitais('data/pnad_anual_2023_interview1.rds', 2023)
glimpse (db)


lista <- data.frame(file=paste0('data/',list.files('data')[6:12]),
                    ano=2016:2022)

for (i in 1:7){
  
  temp <- ranking_gini_capitais(lista$file[i], lista$ano[i])
  
  db <- bind_rows(db, temp)
  
}

glimpse (db)
db %>% count (ano)


# -----------------

db$me90 <- db$se.VD4020*1.65
db$me95 <- db$se.VD4020*1.96
db$me99 <- db$se.VD4020*2.58
db$ed <- ifelse (db$ano==2023, "1", '5')
db %>%
  filter (Capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=ano)) +
  geom_ribbon(aes(ymin=VD4020-me99, ymax=VD4020+me99), fill='darkgrey') +
  geom_ribbon(aes(ymin=VD4020-me95, ymax=VD4020+me95), fill='lightgreen', alpha=1) +
  geom_ribbon(aes(ymin=VD4020-me90, ymax=VD4020+me90), fill='lightblue', alpha=1) +
  geom_line(aes(y=VD4020)) +
  geom_point(aes(y=VD4020, color=ed)) +
  geom_text (aes(y=VD4020,
                 label=paste0(round(VD4020,3),
                              '\n', ranking_menos_desigual,
                              'ª capital mais\nigualitária')),
             vjust=2,
             size=3) +
  scale_y_continuous(limits=c(0.25,0.75), n.breaks=10) +
  scale_x_continuous(n.breaks=10) +
  scale_color_manual (values=c('black', 'orange')) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (title='Índice de Gini para o Recife baseado na Renda de Todos os Trabalhoscom Margens de Erro',
        x=NULL,
        y=NULL,
        color='Entrevista\nmáxima',
        #caption='Índice de Gini para o Recife calculado a partir da variável VD4020 da PNAD Contínua (IBGE). Em azul, a margem de erro para 90% de confiança; em verde, a margem de erro para 95% de confiança; em cinza, a margem de erro para 99% de confiança. O IBGE ainda não disponibilizou os dados de todas as entrevistas para 2023, de forma que foram utilizados os dados acumulados da 1ª entrevista para este ano e da 5ª entrevista para os demais.'
  )

ggsave ('results/gini_me_vd4020.png', width=10, height = 5)

db %>%
  filter (Capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=ano)) +
  geom_line(aes(y=VD4020)) +
  geom_point(aes(y=VD4020, color=ed)) +
  geom_text (aes(y=VD4020,
                 label=paste0(round(VD4020,3),
                              '\n', ranking_menos_desigual,
                              'ª capital mais\nigualitária')),
             vjust=2,
             size=3) +
  scale_y_continuous(limits=c(0.25,0.75), n.breaks=10) +
  scale_x_continuous(n.breaks=10) +
  scale_color_manual (values=c('black', 'orange')) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs (title='Índice de Gini para o Recife baseado na Renda de Todos os Trabalhos',
        x=NULL,
        y=NULL,
        color='Entrevista\nmáxima')

ggsave ('results/gini_linha_vd4020.png', width=10, height = 5)
