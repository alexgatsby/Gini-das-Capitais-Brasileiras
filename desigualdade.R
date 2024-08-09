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


rm(list = ls())
gc()

# testando -------------------------------

#Arenda domiciliar per capita no Brasil cresceu 11,5% em 2023 em comparação a 2022, 
#atingindo o recorde de R$ 1.848. 

decis <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

pnad23 <- import(here('data/pnad_anual_2023_interview1.rds'))

pnad23$variables <- transform(pnad23$variables, VD5002_real=VD5002*CO3)
db23 <- svymean (x=~VD5002_real, design=pnad23, na.rm=T)

pnad23$variables <- transform(pnad23$variables, VD5005_real=VD5005*CO3)
db23a <- svymean (x=~VD5005_real, design=pnad23, na.rm=T)

pnad23$variables <- transform(pnad23$variables, VD5008_real=VD5008*CO3)
db23b <- svymean (x=~VD5008_real, design=pnad23, na.rm=T)

pnad23$variables <- transform(pnad23$variables, VD5011_real=VD5011*CO3)
db23c <- svymean (x=~VD5011_real, design=pnad23, na.rm=T)

db23
db23a
db23b
db23c

# 4020 por decil - MAYRES
# 5008 por decil - EU ----------------------------------

rendperc_VD5008 <- function(ano, entrevista) {
  arquivo <- paste0 ('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}}, '.rds')
  data <- import (here (arquivo))
  data$variables <- transform(data$variables, VD5008_real=VD5008*CO1e)
  db <- svyby (formula=~VD5008_real, design=data, by=~Capital, FUN=svyquantile, quantiles=decis, na.rm=T)
  return (db)
}

db23 <- rendperc_VD5008(2023,1)
db22 <- rendperc_VD5008(2022,5)
db21 <- rendperc_VD5008(2021,5)
db20 <- rendperc_VD5008(2020,5)
db19 <- rendperc_VD5008(2019,5)
db18 <- rendperc_VD5008(2018,5)
db17 <- rendperc_VD5008(2017,5)
db16 <- rendperc_VD5008(2016,5)
db15 <- rendperc_VD5008(2015,1)
db14 <- rendperc_VD5008(2014,1)
db13 <- rendperc_VD5008(2013,1)
db12 <- rendperc_VD5008(2012,1)

db23$ano <- 2023
db22$ano <- 2022
db21$ano <- 2021
db20$ano <- 2020
db19$ano <- 2019
db18$ano <- 2018
db17$ano <- 2017
db16$ano <- 2016
db15$ano <- 2015
db14$ano <- 2014
db13$ano <- 2013
db12$ano <- 2012

db <- bind_rows(db23,db22,db21,db20,db19,db18,db17,db16,db15,db14,db13,db12) %>%
  pivot_longer(VD5008_real.0.1:se.VD5008_real.1)

db$decil <- str_remove (db$name, 'VD5008_real.')
db$is_se <- ifelse (db$name%>%str_detect('se.'),1,0)

dbse <- db %>%
  filter (is_se==1) %>%
  mutate (decil=str_remove(decil,'se.')) %>%
  rename ('se_value'=value) %>%
  select (-c(is_se, name))

db <- db %>%
  filter (is_se==0) %>%
  select (-is_se) %>%
  full_join(dbse, by=c('Capital', 'ano', 'decil'))

decis_importantes <- c(0.1, 0.9, 1)
db$decil_imp <- ifelse (db$decil%in%decis_importantes, 1, 0)

db %>%
  filter (Capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=as.character(ano), y=value)) +
  geom_line (aes(group=decil, color=as.character(decil_imp))) +
  geom_point (aes(color=as.character(decil_imp))) +
  geom_text (aes(label=paste('R$',prettyNum(round(value,2), decimal.mark = ',', big.mark = '.')),
                 alpha=as.character(decil_imp)),
             angle=45,
             size=3,
             vjust=-.5,
             hjust=0) +
  scale_alpha_manual(values=c(0,1)) +
  scale_color_manual(values=c('#ffdfa5','#1f305f')) +
  scale_y_sqrt(labels=comma, limits=c(0,40000)) +
  theme_classic() +
  theme (legend.position = 'none') +
  labs (y=NULL,
        x=NULL,
        title='Renda Per Capita por Decil e Ano em Recife',
        subtitle='Fonte: PNAD Contínua.',
        caption='Variável utilizada: VD5008 deflacionada, correspondente ao "Rendimento domiciliar per capita (habitual de todos os trabalhos e efetivo de outras fontes)\n(exclusive o rendimento das pessoas cuja condição na unidade domiciliar era pensionista, empregado doméstico ou parente do empregado doméstico)".')

ggsave ('results/recife_decil_VD5008.png', height = 8, width = 12)

# 4022 por decil -----------------------------

rendperc_VD4022 <- function(ano, entrevista) {
  arquivo <- paste0 ('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}}, '.rds')
  data <- import (here (arquivo))
  data$variables <- transform(data$variables, VD4022_real=VD4022*CO1e)
  db <- svyby (formula=~VD4022_real, design=data, by=~Capital, FUN=svyquantile, quantiles=decis, na.rm=T)
  return (db)
}

db23 <- rendperc_VD4022(2023,1)
db22 <- rendperc_VD4022(2022,5)
db21 <- rendperc_VD4022(2021,5)
db20 <- rendperc_VD4022(2020,5)
db19 <- rendperc_VD4022(2019,5)
db18 <- rendperc_VD4022(2018,5)
db17 <- rendperc_VD4022(2017,5)
db16 <- rendperc_VD4022(2016,5)
db15 <- rendperc_VD4022(2015,1)
db14 <- rendperc_VD4022(2014,1)
db13 <- rendperc_VD4022(2013,1)
db12 <- rendperc_VD4022(2012,1)

db23$ano <- 2023
db22$ano <- 2022
db21$ano <- 2021
db20$ano <- 2020
db19$ano <- 2019
db18$ano <- 2018
db17$ano <- 2017
db16$ano <- 2016
db15$ano <- 2015
db14$ano <- 2014
db13$ano <- 2013
db12$ano <- 2012

db <- bind_rows(db23,db22,db21,db20,db19,db18,db17,db16,db15,db14,db13,db12) %>%
  pivot_longer(VD4022_real.0.1:se.VD4022_real.1)

db$decil <- str_remove (db$name, 'VD4022_real.')
db$is_se <- ifelse (db$name%>%str_detect('se.'),1,0)

dbse <- db %>%
  filter (is_se==1) %>%
  mutate (decil=str_remove(decil,'se.')) %>%
  rename ('se_value'=value) %>%
  select (-c(is_se, name))

db <- db %>%
  filter (is_se==0) %>%
  select (-is_se) %>%
  full_join(dbse, by=c('Capital', 'ano', 'decil'))

decis_importantes <- c(0.1, 0.9, 1)
db$decil_imp <- ifelse (db$decil%in%decis_importantes, 1, 0)

db %>%
  filter (Capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=as.character(ano), y=value)) +
  geom_line (aes(group=decil, color=as.character(decil_imp))) +
  geom_point (aes(color=as.character(decil_imp))) +
  geom_text (aes(label=paste('R$',prettyNum(round(value,2), decimal.mark = ',', big.mark = '.')),
                 alpha=as.character(decil_imp)),
             angle=45,
             size=3,
             vjust=-.5,
             hjust=0) +
  scale_alpha_manual(values=c(0,1)) +
  scale_color_manual(values=c('#ffdfa5','#1f305f')) +
  scale_y_sqrt(labels=comma, limits=c(0,100000)) +
  theme_classic() +
  theme (legend.position = 'none') +
  labs (y=NULL,
        x=NULL,
        title='Renda de Todos os Trabalhos por Decil e Ano em Recife',
        subtitle='Fonte: PNAD Contínua.',
        caption='Variável utilizada: VD4022 deflacionada, correspondente ao "Rendimento mensal efetivo de todas as fontes (apenas para pessoas\nque receberam em dinheiro, produtos ou mercadorias em qualquer trabalho ou que receberam rendimentos em dinheiro de outras fontes)".')

ggsave ('results/recife_decil_VD4022.png', height = 8, width = 12)


# rendimentos por tipo de renda ----------------------------------------------------
## todos os trabalhos - VD4020
## programas sociais - bpc - V5001A2
## programas sociais - pbf - V5002A2
## programas socias - outros - V5003A2
## aposentadoria - V5004A2
## seguro-desempregro e seguro-defeso - V5005A2
## pensão, doação, mesada e não morador do domicílio - V5006A2
## aluguel e arrendamento - V5007A2
## outros rendimentos (bolsa de estudos, rendimento de caderneta de poupança, aplicações financeiras, etc.) - V5008A2

interesse <- c("Município de Recife (PE)",
               "Município de São Paulo (SP)",
               "Município de Fortaleza (CE)",
               "Município de João Pessoa (PB)",
               "Município de Salvador (BA)")

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
db15 <- rendasportipo(2015,1,interesse)

db <- bind_rows (db23,db22,db21,db20,db19,db18,db17,db16,db15)
glimpse (db)
db %>% count (capital)


write.csv(db, 'extracted_data/data_renda_componentes_cap_selecionadas.csv')
pal_napcd <- c("#95a7a7ff","#f4d0b2ff", "#f8f2e8","#b6d7a8ff", '#9AB5C1', '#F9D99A', '#bf9780')

db <- import (here ('extracted_data/data_renda_componentes_cap_selecionadas.csv'))

db %>%
  filter (capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_stack(vjust=.5)) +
  geom_text (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo),
             position = position_stack(vjust=.5)) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Médias por Tipo no Recife',
       subtitle='Fonte: PNAD Contínua.',
       caption='Variáveis de Análise: VD4020, VD4047, VD5004A2, V5005A2, V5006A2, V5007A2 e V5008A2.')

ggsave ('results/rendastipo_media_recife.png', width = 12, height=8)


# extração de tipo de renda para quantis selecionados ---------------------

decis <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

interesse <- c("Município de Recife (PE)",
               "Município de São Paulo (SP)")

rendasportipo_decis <- function (ano, entrevista,capitais,decis) {
  
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
  
  colunas <- c('capital', 'decil_tipo', 'valor')
  
  db1 <- svyby (formula=~VD4020_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db1 <- db1 %>% pivot_longer(2:21)
  db1$tipo <- 'Trabalhos'
  colnames (db1) <- colunas
  
  db2a <- svyby (formula=~VD4047_reala, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db2a <- db2a %>% pivot_longer(2:21)
  db2a$tipo <- 'BPC'
  colnames (db2a) <- colunas
  
  db2b <- svyby (formula=~VD4047_realb, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db2b <- db2b %>% pivot_longer(2:21)
  db2b$tipo <- 'PBF'
  colnames (db2b) <- colunas
  
  db2c <- svyby (formula=~VD4047_realc, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db2c <- db2c %>% pivot_longer(2:21)
  db2c$tipo <- 'Outros programas sociais'
  colnames (db2c) <- colunas
  
  db3 <- svyby (formula=~V5004A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db3 <- db3 %>% pivot_longer(2:21)
  db3$tipo <- 'Aposentadoria'
  colnames (db3) <- colunas
  
  db4 <- svyby (formula=~V5005A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db4 <- db4 %>% pivot_longer(2:21)
  db4$tipo <- 'Seguros'
  colnames (db4) <- colunas
  
  db5 <- svyby (formula=~V5006A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db5 <- db5 %>% pivot_longer(2:21)
  db5$tipo <- 'Pensões e Doações'
  colnames (db5) <- colunas
  db6 <- svyby (formula=~V5007A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db6 <- db6 %>% pivot_longer(2:21)
  db6$tipo <- 'Imóveis'
  colnames (db6) <- colunas
  db7<- svyby (formula=~V5008A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
               FUN=svyquantile, quantiles=decis, na.rm=T)
  db7 <- db7 %>% pivot_longer(2:21)
  db7$tipo <- 'Outros'
  colnames (db7) <- colunas
  
  db <- bind_rows(db1,db2a,db2b,db2c,db3,db4,db5,db6,db7)
  db$ano <- {{ano}}
  colnames (db) <- c('capital', 'decil_tipo', 'valor', 'tipo', 'ano')
  
  return (db)
}

db23 <- rendasportipo_decis (2023,1,interesse,decis)
db22 <- rendasportipo_decis (2022,5,interesse,decis)
db21 <- rendasportipo_decis (2021,5,interesse,decis)
db20 <- rendasportipo_decis (2020,5,interesse,decis)
db19 <- rendasportipo_decis (2019,5,interesse,decis)
db18 <- rendasportipo_decis (2018,5,interesse,decis)
db17 <- rendasportipo_decis (2017,5,interesse,decis)
db16 <- rendasportipo_decis (2016,5,interesse,decis)
db15 <- rendasportipo_decis (2015,1,interesse,decis)

db <- bind_rows (db23,db22,db21,db20,db19,db18,db17,db16,db15)
glimpse (db)
db %>% count (decil_tipo)

db$is_decil_maisbaixo <- ifelse (str_detect(db$decil_tipo,'.0.1'),1,0)
db$is_decil_maisalto <- ifelse (db$is_decil_maisbaixo==1, 0, 
                                ifelse(str_detect(db$decil_tipo,'.1'),1,0))
db$is_se <- ifelse (str_detect(db$decil_tipo, 'se.'),1,0)

db %>% count (is_decil_maisalto)
db %>% count (is_decil_maisbaixo)
db %>% count (tipo)

write.csv (db, 'extracted_data/rendasportipo_cap_selec_decisaltoebaixo.csv')


# gráficos ----------------------


db <- import (here ('extracted_data/rendasportipo_cap_selec_decisaltoebaixo.csv'))

db %>%
  filter (is_decil_maisbaixo==1) %>%
  filter (is_se==0) %>%
  filter (capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_stack(vjust=.5)) +
  geom_label_repel (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo),
             position = position_stack(vjust=.5),
             size=3) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo no Recife - Decil Mais Baixo',
       subtitle='Fonte: PNAD Contínua.',
       caption='Variáveis de Análise: VD4020, VD4047, VD5004A2, V5005A2, V5006A2, V5007A2 e V5008A2.')

ggsave ('results/recife_rendasportipo_decil_mais_baixo.png', width = 12, height=8)

db %>%
  filter (is_decil_maisalto==1) %>%
  filter (is_se==0) %>%
  filter (capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_stack(vjust=.5)) +
  geom_label_repel (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo),
             position = position_stack(vjust=.5),
             size=3) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo no Recife - Decil Mais Alto',
       subtitle='Fonte: PNAD Contínua.',
       caption='Variáveis de Análise: VD4020, VD4047, VD5004A2, V5005A2, V5006A2, V5007A2 e V5008A2.')

ggsave ('results/recife_rendasportipo_decil_mais_alto.png', width = 12, height=8)



db %>%
  filter (is_decil_maisbaixo==1) %>%
  filter (is_se==0) %>%
  filter (capital=="Município de São Paulo (SP)") %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_stack(vjust=.5)) +
  geom_label_repel(aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo),
             position = position_stack(vjust=.5),
             size=3) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo em São Paulo-Capital - Decil Mais Baixo',
       subtitle='Fonte: PNAD Contínua.',
       caption='Variáveis de Análise: VD4020, VD4047, VD5004A2, V5005A2, V5006A2, V5007A2 e V5008A2.')

ggsave ('results/saopaulo_rendasportipo_decil_mais_baixo.png', width = 12, height=8)

db %>%
  filter (is_decil_maisalto==1) %>%
  filter (is_se==0) %>%
  filter (capital=="Município de São Paulo (SP)") %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_stack(vjust=.5)) +
  geom_label_repel (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo),
             position = position_stack(vjust=.5),
             size=3) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo em São Paulo-Capital - Decil Mais Alto',
       subtitle='Fonte: PNAD Contínua.',
       caption='Variáveis de Análise: VD4020, VD4047, VD5004A2, V5005A2, V5006A2, V5007A2 e V5008A2.')

ggsave ('results/saopaulo_rendasportipo_decil_mais_alto.png', width = 12, height=8)

glimpse (db)

db %>%
  filter (is_decil_maisbaixo==1) %>%
  filter (is_se==0) %>%
  filter (capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_dodge2(width = .9)) +
  geom_text (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                        group=tipo,
                 y=1),
                    position=position_dodge2(width = .9),
                    size=3,
                    angle=90,
             hjust=0) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo no Recife - Decil Mais Baixo',
       subtitle='Fonte: PNAD Contínua.')

ggsave ('results/recife_rendasportipo_decil_mais_baixo_dodge.png', width = 12, height=8)


db %>%
  filter (is_decil_maisalto==1) %>%
  filter (is_se==0) %>%
  filter (capital=='Município de Recife (PE)') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_dodge2(width = .9)) +
  geom_text (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo,
                 y=1),
             position=position_dodge2(width = .9),
             size=3,
             angle=90,
             hjust=0) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo no Recife - Decil Mais Alto',
       subtitle='Fonte: PNAD Contínua.')

ggsave ('results/recife_rendasportipo_decil_mais_alto_dodge.png', width = 12, height=8)


db %>%
  filter (is_decil_maisbaixo==1) %>%
  filter (is_se==0) %>%
  filter (capital=="Município de São Paulo (SP)") %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_dodge2(width = .9)) +
  geom_text (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo,
                 y=1),
             position=position_dodge2(width = .9),
             size=3,
             angle=90,
             hjust=0) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo em São Paulo - Decil Mais Baixo',
       subtitle='Fonte: PNAD Contínua.')

ggsave ('results/saopaulo_rendasportipo_decil_mais_baixo_dodge.png', width = 12, height=8)


db %>%
  filter (is_decil_maisalto==1) %>%
  filter (is_se==0) %>%
  filter (capital=="Município de São Paulo (SP)") %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(fill=tipo), position=position_dodge2(width = .9)) +
  geom_text (aes(label=paste('R$', prettyNum(round(valor,2),decimal.mark = ',', big.mark = '.')),
                 group=tipo,
                 y=1),
             position=position_dodge2(width = .9),
             size=3,
             angle=90,
             hjust=0) +
  scale_fill_brewer (palette='Pastel1') +
  scale_y_continuous(labels=comma) +
  theme_classic() +
  theme (legend.position = 'bottom') +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title='Rendas Média por Tipo em São Paulo - Decil Mais Alto',
       subtitle='Fonte: PNAD Contínua.')

ggsave ('results/saopaulo_rendasportipo_decil_mais_alto_dodge.png', width = 12, height=8)

# renda decil mais alto -----------------------------

rendasportipo_decis <- function (ano, entrevista,capitais,decis) {
  
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
  
  colunas <- c('capital', 'decil_tipo', 'valor')
  
  db1 <- svyby (formula=~VD4020_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db1 <- db1 %>% pivot_longer(2:21)
  db1$tipo <- 'Trabalhos'
  colnames (db1) <- colunas
  
  db2a <- svyby (formula=~VD4047_reala, by=~Capital, design=subset(data, Capital%in%interesse), 
                 FUN=svyquantile, quantiles=decis, na.rm=T)
  db2a <- db2a %>% pivot_longer(2:21)
  db2a$tipo <- 'BPC'
  colnames (db2a) <- colunas
  
  db2b <- svyby (formula=~VD4047_realb, by=~Capital, design=subset(data, Capital%in%interesse), 
                 FUN=svyquantile, quantiles=decis, na.rm=T)
  db2b <- db2b %>% pivot_longer(2:21)
  db2b$tipo <- 'PBF'
  colnames (db2b) <- colunas
  
  db2c <- svyby (formula=~VD4047_realc, by=~Capital, design=subset(data, Capital%in%interesse), 
                 FUN=svyquantile, quantiles=decis, na.rm=T)
  db2c <- db2c %>% pivot_longer(2:21)
  db2c$tipo <- 'Outros programas sociais'
  colnames (db2c) <- colunas
  
  db3 <- svyby (formula=~V5004A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db3 <- db3 %>% pivot_longer(2:21)
  db3$tipo <- 'Aposentadoria'
  colnames (db3) <- colunas
  
  db4 <- svyby (formula=~V5005A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db4 <- db4 %>% pivot_longer(2:21)
  db4$tipo <- 'Seguros'
  colnames (db4) <- colunas
  
  db5 <- svyby (formula=~V5006A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db5 <- db5 %>% pivot_longer(2:21)
  db5$tipo <- 'Pensões e Doações'
  colnames (db5) <- colunas
  db6 <- svyby (formula=~V5007A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
                FUN=svyquantile, quantiles=decis, na.rm=T)
  db6 <- db6 %>% pivot_longer(2:21)
  db6$tipo <- 'Imóveis'
  colnames (db6) <- colunas
  db7<- svyby (formula=~V5008A2_real, by=~Capital, design=subset(data, Capital%in%interesse), 
               FUN=svyquantile, quantiles=decis, na.rm=T)
  db7 <- db7 %>% pivot_longer(2:21)
  db7$tipo <- 'Outros'
  colnames (db7) <- colunas
  
  db <- bind_rows(db1,db2a,db2b,db2c,db3,db4,db5,db6,db7)
  db$ano <- {{ano}}
  colnames (db) <- c('capital', 'decil_tipo', 'valor', 'tipo', 'ano')
  
  return (db)
}
