#config ------------

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}
if (require (cowplot) == FALSE) {install.packages("cowplot"); require (cowplot)}

rm(list = ls())
gc()

`%ni%` <- Negate(`%in%`)

pal <- c('#a6cee3',
         '#1f78b4',
         '#b2df8a',
         '#33a02c',
         '#fb9a99',
         '#e31a1c',
         '#fdbf6f',
         '#ff7f00',
         '#cab2d6')

pal <- c('#fee0d2', 
         '#fcbba1', 
         '#fc9272', 
         '#fb6a4a',
         '#f7fbff', 
         '#deebf7', 
         '#c6dbef', 
         '#9ecae1', 
         '#6baed6',
         '#4292c6')

# data ------------

db <- import (here ('results_estudo_gini/rendamediapercapita_capital_ano_componente_decil.xlsx'))


db$capital <- strsplit(db$capital.decil, '\\.')
db$capital <- sapply(db$capital, `[`, 1)
db$capital <- db$capital %>% str_remove ('Município de ')
db$decil <- str_split(db$capital.decil, '\\.',n=2)
db$decil <- sapply(db$decil, `[`, 2)
db$decil <- str_replace(db$decil, 'VD5008real_proprioano.', 'decil_')


capitais_regioes <- data.frame(
  capital = c("Rio Branco (AC)", "Maceió (AL)", "Macapá (AP)", "Manaus (AM)", 
              "Salvador (BA)", "Fortaleza (CE)", "Brasília (DF)", "Vitória (ES)", 
              "Goiânia (GO)", "São Luís (MA)", "Cuiabá (MT)", "Campo Grande (MS)", 
              "Belo Horizonte (MG)", "Belém (PA)", "João Pessoa (PB)", "Curitiba (PR)", 
              "Recife (PE)", "Teresina (PI)", "Rio de Janeiro (RJ)", "Natal (RN)", 
              "Porto Alegre (RS)", "Porto Velho (RO)", "Boa Vista (RR)", "Florianópolis (SC)", 
              "São Paulo (SP)", "Aracaju (SE)", "Palmas (TO)"),
  regiao = c("Norte", "Nordeste", "Norte", "Norte", 
             "Nordeste", "Nordeste", "Centro-Oeste", "Sudeste", 
             "Centro-Oeste", "Nordeste", "Centro-Oeste", "Centro-Oeste", 
             "Sudeste", "Norte", "Nordeste", "Sul", 
             "Nordeste", "Nordeste", "Sudeste", "Nordeste", 
             "Sul", "Norte", "Norte", "Sul", 
             "Sudeste", "Nordeste", "Norte")
)


db <- left_join(db, capitais_regioes, by='capital')

db$capital <- ifelse (db$capital=='Belo Horizonte (MG)', 'Belo\nHorizonte\n(MG)',
              ifelse (db$capital=='Rio de Janeiro (RJ)', 'Rio de\nJaneiro\n(RJ)', db$capital))


# decil mais baixo -------

ne <- db %>%
  filter (regiao=='Nordeste') %>%
  filter (decil=='decil_0.1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

no <- db %>%
  filter (regiao=='Norte') %>%
  filter (decil=='decil_0.1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'right',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

co <- db %>%
  filter (regiao=='Centro-Oeste') %>%
  filter (decil=='decil_0.1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

sud <- db %>%
  filter (regiao=='Sudeste') %>%
  filter (decil=='decil_0.1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

sul <- db %>%
  filter (regiao=='Sul') %>%
  filter (decil=='decil_0.1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

bottom <- plot_grid(co, sul, sud, nrow=1)
plot_grid(ne, no, bottom, ncol=1, nrow=3)

# decil mais alto -------

ne <- db %>%
  filter (regiao=='Nordeste') %>%
  filter (decil=='decil_1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

no <- db %>%
  filter (regiao=='Norte') %>%
  filter (decil=='decil_1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'right',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

co <- db %>%
  filter (regiao=='Centro-Oeste') %>%
  filter (decil=='decil_1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

sud <- db %>%
  filter (regiao=='Sudeste') %>%
  filter (decil=='decil_1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

sul <- db %>%
  filter (regiao=='Sul') %>%
  filter (decil=='decil_1') %>%
  ggplot (aes(x=as.character(ano), y=valor)) +
  geom_col (aes(group=tipo, fill=tipo)) +
  scale_fill_manual(values=pal) +
  facet_grid (cols=vars(capital)) +
  theme_classic() +
  theme (legend.position = 'none',
         axis.text.x=element_text(angle=90, vjust=.5)) +
  labs (x=NULL,
        y=NULL,
        fill=NULL)

bottom <- plot_grid(co, sul, sud, nrow=1)
plot_grid(ne, no, bottom, ncol=1, nrow=3)
