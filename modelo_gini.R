#config ------------

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}
if (require (cowplot) == FALSE) {install.packages("cowplot"); require (cowplot)}


remove.packages("lme4")
install.packages("lme4", type = "source")

if (require (lmerTest) == FALSE) {install.packages("lmerTest"); require (lmerTest)}
if (require (corrgram) == FALSE) {install.packages("corrgram"); require (corrgram)}
if (require (GGally) == FALSE) {install.packages("GGally"); require (GGally)}
if (require (jtools) == FALSE) {install.packages("jtools"); require (jtools)}
if (require (broom) == FALSE) {install.packages("broom"); require (broom)}
if (require (broom.mixed) == FALSE) {install.packages("broom.mixed"); require (broom.mixed)}
if (require (sjPlot) == FALSE) {install.packages("sjPlot"); require (sjPlot)}

rm(list = ls())
gc()

`%ni%` <- Negate(`%in%`)
numviz <- function(data, n=2){
  
  temp <- {data} %>%
    round ({n}) %>%
    prettyNum(decimal.mark=',', big.mark='.')
  
  return (temp)
  
}

# data -----------

ginia <- import (here ('extracted_data/gini_vd4022_capitais_anualate2023.xlsx')) %>%
  rename ('giniVD4022'=VD4022,
          'capital'=Capital) %>%
  mutate (ano=as.character(ano)) %>%
  select (-c(ranking_menos_desigual, ranking_mais_desigual))

ginib <- import (here ('extracted_data/gini_vd4020_capitais_anualate2023_trim2024.xlsx')) %>%
  rename ('giniVD4020'=VD4020,
          'capital'=Capital) %>%
  mutate (ano=as.character(ano)) %>%
  filter (ano%ni%c('2024.1', '2024.2')) %>%
  select (-c(ranking_menos_desigual, ranking_mais_desigual, is_20242))


comp <- import (here ('results_estudo_gini/rendamediapercapita_capital_ano_componente.xlsx')) %>%
  mutate (ano=as.character(ano),
          capital=str_remove(capital, 'Município de ')) %>%
  rename ('rmpc_comp'=valor,
          'se_rmpc_comp'=se) %>%
  pivot_wider(names_from = tipo, values_from = c(rmpc_comp, se_rmpc_comp))


cap <- ginia %>% count (capital)
dec <- import (here ('results_estudo_gini/rendapercapitamedia_capital_ano_decil.xlsx')) %>%
  mutate (ano=str_sub(ed, start=14, end=17),
          local=str_remove(local, 'Município de ')) %>%
  filter (local %in% cap$capital) %>%
  rename ('capital'=local) %>%
  select (-ed)

cols <- c('capital',
          paste0('rmpc_decil_',seq(0.1,1, by=.1)),
          paste0('se.rmpc_decil_',seq(0.1,1, by=.1)),
          'ano')
colnames (dec) <- cols

db <- full_join(ginia, ginib)
glimpse (db)
db <- full_join(db, comp)
glimpse (db)
db <- full_join(db, dec)
glimpse (db)


# o rico ficou mais rico ou o pobre ficou mais pobre?

# correlações ---------------


dbcor <- db %>%
  select (capital,giniVD4022, rmpc_decil_0.1:rmpc_decil_1) %>%
  pivot_longer(rmpc_decil_0.1:rmpc_decil_1)


dbcor %>%
  group_by(capital, name) %>%
  summarise (quant=n(),
             corginirenda=cor(giniVD4022, value)) %>%
  mutate (posneg=ifelse(corginirenda>0, 'pos', 'neg'),
          corpuro=abs(corginirenda)) %>%
  ggplot (aes(x=name, y=capital)) +
  geom_count (aes(size=corpuro, color=posneg),alpha=.7) +
  geom_text(aes(label=numviz(corginirenda,3)),
            size=3) +
  theme_classic() +
  theme (axis.text.x = element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        color='Sinal da\nCorrelação',
        size='Valor da\nCorrelação',
        title='Correlação entre índice de Gini (VD4022) e renda média per capita por decil e capital')

ggsave ('painelf.png', height = 8, width = 12)


dbcor %>%
  group_by(capital, name) %>%
  summarise (quant=n(),
             corginirenda=cor(giniVD4022, value)) %>%
  mutate (posneg=ifelse(corginirenda>0, 'pos', 'neg'),
          corpuro=abs(corginirenda)) %>%
  as_tibble() %>%
  filter (corpuro>.5) %>%
  count (name, sort=T)

dbcor %>%
  group_by(capital, name) %>%
  summarise (quant=n(),
             corginirenda=cor(giniVD4022, value)) %>%
  mutate (posneg=ifelse(corginirenda>0, 'pos', 'neg'),
          corpuro=abs(corginirenda)) %>%
  as_tibble() %>%
  group_by(name) %>%
  summarise (corsum=sum(corpuro)) %>%
  arrange (desc(corsum))

# mixed effects model -----

m1 <- lm (giniVD4022 ~ rmpc_decil_0.1+rmpc_decil_0.2+rmpc_decil_0.3+
            rmpc_decil_0.4+rmpc_decil_0.5+rmpc_decil_0.6+rmpc_decil_0.7+
            rmpc_decil_0.8+rmpc_decil_0.9+rmpc_decil_1,
          data=db)

summary (m1)

m2 <- lmer (giniVD4022 ~ rmpc_decil_0.1+rmpc_decil_0.2+rmpc_decil_0.3+
              rmpc_decil_0.4+rmpc_decil_0.5+rmpc_decil_0.6+rmpc_decil_0.7+
              rmpc_decil_0.8+rmpc_decil_0.9+rmpc_decil_1 + (1|capital),
            data=db)

summary(m2)

plot_summs(m1,m2)

tab_model(m2)

# gini vs transferências ------------

dbcor <- db %>%
  select (giniVD4022, capital, rmpc_comp_BPC:rmpc_comp_Seguros) %>%
  pivot_longer(rmpc_comp_BPC:rmpc_comp_Seguros)

grupo1 <- c('Rio Branco (AC)', 'Recife (PE)', 'Porto Velho (RO)', 
            'Brasília (DF)', 'Boa Vista (RR)', 'Belo Horizonte (MG)',
            'Porto Alegre (RS)')

dbcor$grupo <- ifelse (dbcor$capital%in%grupo1, "Grupo 1", "Grupo 2")

labs <- c('Aposentadorias', 'BPC', 'Outros\nprogramas\nsociais', 'PBF', 'Seguros')

dbcor %>%
  mutate (capital=paste0(capital, ' - ', grupo)) %>%
  group_by(capital, name) %>%
  summarise (quant=n(),
             corginirenda=cor(giniVD4022, value)) %>%
  mutate (posneg=ifelse(corginirenda>0, 'pos', 'neg'),
          corpuro=abs(corginirenda)) %>%
  ggplot (aes(x=name, y=(capital))) +
  geom_count (aes(size=corpuro, color=posneg),alpha=.7) +
  geom_text(aes(label=numviz(corginirenda,3)),
            size=3) +
  scale_x_discrete(labels=labs) +
  theme_classic() +
  theme (axis.text.x = element_text(angle=0, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        color='Sinal da\nCorrelação',
        size='Valor da\nCorrelação',
        title='Correlação entre índice de Gini (VD4022) e o ganho per capita de\ntransferências de renda por tipo de transferência e capital')



ggsave ('results_estudo_gini/painelh.png', height = 8, width = 12)
