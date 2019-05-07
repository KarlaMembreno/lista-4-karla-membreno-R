## UNIVERSIDADE FEDERAL DE PERNAMBUCO###
## DEPARTAMENTO DE CIÊNCIA POLITICA ###
##KARLA JANIXIA MEMBRENO####
##LISTA No 3#

##Exercício 1####
# link:https://github.com/KarlaMembreno/lista-4-karla-membreno-R #

##Exercício 2####

#Carregando os pacotes#

library(tidyverse)
library(ffbase)
library(readr)
library(readxl)
library(rlang)
install.packages("GGally")
library(Ggally)

#Processando os dados#

setwd("C:\\Users\\sc\\Desktop\\AD_Lista_4_Karla_Membreno")

#Verificando se está correto#

getwd()

#Accesando a o banco de dados censo 2016#

load ("matricula_pe_censo_escolar_2016.RData")
load ("docentes_pe_censo_escolar_2016.RData")
load ("turmas_pe_censo_escolar_2016.RData")
load ("escolas_pe_censo_escolar_2016.RData")

#Inciso 2a.. Abrindo dados PNUD ANO 2010####

getwd()

#Transformando e conhecendo a base de dados, usando a função read_xlsx do pacote readxl para carregar os dados do PNUD#

install.packages("readxl")

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(readxl) == F) install.packages('readxl'); require(readxl)

pnud <- read_excel ("atlas2013_dadosbrutos_pt.xlsx", sheet= 2)

head(pnud)
unique (pud)
unique(pnud$ANO)

#Selecionando os dados referentes ao ano 2010, Estado de Pernambuco#

pnud_pe_2010 <- pnud %>% filter(ANO == 2010 & UF == 26)

rm(pnud)  # removendo base pnud#

# processamento da base de dados#

# base TURMAS#

require(tidyverse)

turmas_pe_sel <- turmas_pe %>% group_by(CO_MUNICIPIO) %>%
summarise (n_turmas = n(), turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T),
            turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T),
            turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T),
            turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T),
            turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))

# verificação#

dim("turmas_pe_sel")[1] == length(unique(turmas_pe$CO_MUNICIPIO))
summary("turmas_pe_sel")


# base de Escolas#

escolas_pe_sel <- escolas_pe %>% group_by(CO_MUNICIPIO) %>% 
summarise(n_escolas = n(), n_escolas_priv = sum(TP_DEPENDENCIA == 4, na.rm = T),
escolas_func = sum(TP_SITUACAO_FUNCIONAMENTO == 1, na.rm = T),
escolas_agua_inex = sum(IN_AGUA_INEXISTENTE, na.rm = T),#
escolas_energia_inex = sum(IN_ENERGIA_INEXISTENTE, na.rm = T),
escolas_esgoto_inex = sum(IN_ESGOTO_INEXISTENTE, na.rm = T),
escolas_internet = sum(IN_INTERNET, na.rm = T),
escolas_alimentacao = sum(IN_ALIMENTACAO, na.rm = T))
            

# verificação#

dim(escolas_pe_sel)[1] == length(unique(escolas_pe$CO_MUNICIPIO))

summary(escolas_pe_sel)

library(tidyverse)


# Docentes#

install.packages(summ)


# Matriculas

censo_pnud_pe_sel <- pnud_pe_2010 %>% full_join(matriculas_pe_sel, 
                                                by = c("Codmun7" = "CO_MUNICIPIO"))

dim(pnud_pe_2010)

dim(matriculas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# Escolas#

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(escolas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))

dim(escolas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# Turmas#

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(turmas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))

dim(turmas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# Docentes#

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(docentes_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))

dim(docentes_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# salvando a nova base de dados#

setwd()

getwd()

dir()

save(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.RData")

write.csv2(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.csv",
           row.names = F)

# limpando area de trabalho#
rm(list = ls())  

# Abrindo o banco de dados novo#

# observando  o caminho da base de dados:

getwd()

# abrindo a base de dados:

load("2016_censo_pnud_pe_sel.RData")

#verificando algumas características da base de dados:#

dim(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel)

head(censo_pnud_pe_sel)


## Inciso 2b. Não deve haver docente com mais de 70 anos ou com menos de 18 anos ####

summary(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel$alunos_media_idade)

load("docentes_pe_censo_escolar_2016.RData")

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE => 18, NU_IDADE <= 70)

dim(docentes_pe_selecao)


## Inciso 2c. Não deve haver aluno com mais de 25 anos ou com menos de 1 ano ####

load("matricula_pe_censo_escolar_2016.RData")

matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE => 1, NU_IDADE <= 25)

dim(matricula_pe_selecao)

summary(matricula_pe_selecao$NU_IDADE)

## Inciso 2d. Apresente estatísticas descritivas do número de alunos por docente nos municípios do Estado####

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

DocentesAlunos <- censo_pnud_pe_sel$n_matriculas/censo_pnud_pe_sel$n_docentes

#Estatística Descritiva de Docentes por alunos#

DocentesAlunos

summary(DocentesAlunos)

plot(DocentesAlunos)

ggplot(censo_pnud_pe_sel, aes(DocentesAlunos))+geom_histogram()


## Inciso 2e. Apresente o município com maior número de alunos por docente e seu IDHM####

names(censo_pnud_pe_sel)

summary(DocentesAlunos)

## juntando variáveis

censo_pnud_pe_sel_docentesalunos <- censo_pnud_pe_sel %>%  mutate(DocentesAlunos)

View(censo_pnud_pe_sel_docentesalunos)

censo_pnud_pe_sel_docentesalunos["177", ]

# A cidade é Tupanatinga # 

plot(censo_pnud_pe_sel_docentesalunos$DocentesAlunos)

ggplot(censo_pnud_pe_sel_docentesalunos, aes(DocentesAlunos))+geom_histogram()


## Inciso 2f.Faça o teste do coeficiente de correlação linear de pearson e apresente sua resposta ####

cor(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

cor.test(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

#RESPOSTA: Comforme os dados obtidos, a correlação é de "-0.5057435"# 
#então  como é negativa indica que não há correlação entre o número de discentes por aluno e o IDH dos municipios#


## Exercício No. 3 ###

# gráfico de R de dispersão no ggplot#

install.packages(ggplot2)
library(ggplot2)
data("mpg", package = "ggplot2")

ggplot(censo_pnud_pe_sel_docentesalunos, aes(DocentesAlunos, IDHM, color = IDHM))+geom_point


