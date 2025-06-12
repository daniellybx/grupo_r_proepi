# ======================================================
# SEÇÃO 0: Criar Pasta para Dados
# ======================================================

# Cria uma pasta chamada "dados" no diretório de trabalho atual, se ainda não existir.
# Isso garante que tenhamos um local padrão e organizado para salvar ou importar arquivos usados nas análises.
dir.create("dados", showWarnings = FALSE)

# ======================================================
# SEÇÃO 1: Instalação e Carregamento de Pacotes
# ======================================================

# Instala e carrega pacotes essenciais para análise de dados em saúde pública.
# Usamos o pacote "pacman" pela conveniência de instalar e carregar pacotes em uma única função.
pacman::p_load(rio, tidyverse, here)  # rio = importação/exportação; tidyverse = ciência de dados; here = caminhos relativos

# Instala pacotes diretamente do GitHub
# Esses pacotes são úteis para análises de redes de transmissão e rastreamento de contatos em epidemiologia.
pacman::p_load_gh("reconhub/epicontacts")                   # versão principal do pacote
pacman::p_load_gh("reconhub/epicontacts@timeline")          # versão com funcionalidades extras de linha do tempo

# Alternativas usando funções base do R para instalação e carregamento dos pacotes:
install.packages("tidyverse")                                # instala tidyverse (coleção de pacotes para análise de dados)
install.packages(c("tidyverse", "rio", "here"))              # instala múltiplos pacotes de uma vez

library(tidyverse)                                           # carrega o tidyverse
library(rio)                                                 # carrega o rio
library(here)                                                # carrega o here

# Garante que o pacote pacman esteja instalado antes de seu uso
# Isso evita erros se o usuário estiver executando o script pela primeira vez
if (!require("pacman")) install.packages("pacman")

# Carrega uma lista extensa de pacotes úteis em projetos de epidemiologia aplicada.
# Os pacotes estão organizados por categoria para facilitar a compreensão do seu propósito:
pacman::p_load(
  
  # Ensino e tutoriais interativos
  learnr, swirl,
  
  # Caminhos e arquivos
  here, rio, openxlsx,
  
  # Gestão de pacotes
  pacman, renv, remotes,
  
  # Manipulação e visualização de dados
  apyramid, tidyverse, linelist, naniar, magrittr,
  
  # Estatísticas e sumários
  janitor, gtsummary, rstatix, broom, lmtest, easystats,
  
  # Epidemiologia aplicada
  epirhandbook, epicontacts, EpiNow2, EpiEstim, projections, incidence2, i2extras, epitrix, distcrete,
  
  # Visualização de dados
  cowplot, RColorBrewer, ggnewscale, DiagrammeR, gghighlight, ggrepel, plotly, gganimate,
  
  # Dados espaciais e mapas
  sf, tmap, OpenStreetMap, spdep,
  
  # Relatórios e apresentações
  rmarkdown, reportfactory, officer,
  
  # Dashboards e aplicações web
  flexdashboard, shiny,
  
  # Tabelas de apresentação
  knitr, flextable,
  
  # Filogenia e genômica
  ggtree, ape, treeio
) 

# Instala pacotes específicos diretamente do GitHub, se ainda não instalados
# Isso é necessário porque essas versões não estão disponíveis no CRAN
pacman::p_install_gh("reconhub/epicontacts@timeline")      # versão especial do epicontacts com linha do tempo
pacman::p_install_gh("appliedepi/epirhandbook")            # pacote com funções e dados do Epi R Handbook

# ======================================================
# SEÇÃO 2: Importação e Exploração de Dados
# ======================================================

# Importa planilha Excel com rio::import() da pasta "dados"
linelist <- rio::import("dados/linelist_raw.xlsx")

# Importa planilha Excel com rio::import() da pasta "dados"
linelist <- read_rds("dados/linelist_cleaned.rds")

# Alternativa com pipe: importa e conta linhas
linelist_count <- import("dados/linelist_raw.xlsx") %>% count()

# Importa arquivo CSV da pasta "dados"
likert_data <- import("dados/likert_data.csv")

# Visualização de estatísticas descritivas
summary(linelist$age)               # resumo completo da idade
summary(linelist$age)[2]            # segundo valor do resumo (1º quartil)
summary(linelist$age)[["Median"]]   # calcula a mediana ignorando valores ausentes

# Visualização de conteúdo específico da base
table(linelist$gender, linelist$outcome)  # tabela cruzada de gênero e desfecho
linelist[2, ]                              # exibe a linha 2 inteira
table(linelist$gender, linelist$outcome)
linelist[, "temp"]                 # exibe apenas a coluna date_onset
linelist[2, 5:10]                         # linha 2, colunas 5 a 10
linelist[2, c(5:10, 18)]                  # linha 2, colunas 5-10 e 18
linelist[2:20, c("date_onset", "outcome", "age")]   # linhas 2 a 20, colunas específicas
linelist[linelist$age > 25 , c("date_onset", "outcome", "age")] # filtro por critério lógico

# Exibe subconjunto visualmente no Viewer do RStudio
View(linelist[2:20, "date_onset"])

# Salva subconjunto como novo objeto
new_table <- linelist[2:20, c("date_onset")]

# Visualizações com dplyr
linelist %>% head(100)  # primeiras 100 linhas
dplyr::filter(linelist, row_number() == 5)  # linha 5
linelist %>% filter(row_number() %in% 2:20) %>% select(date_onset, outcome, age)

# Criar variável categórica com lógica condicional baseada em idade e resposta afirmativa
linelist <- linelist %>% 
  mutate(child_hospitaled = case_when(
   age < 18 ~ "Hospitalized Child",
    TRUE ~ "Not"))

# ======================================================
# SEÇÃO 3: Manipulação de Dados
# ======================================================

# Cria e imprime tabela de frequência cruzada
gen_out_table <- table(linelist$gender, linelist$outcome)
gen_out_table

# Criação da variável BMI (IMC)
linelist$bmi <- linelist$wt_kg / (linelist$ht_cm / 100)^2        # usando R base
linelist <- linelist %>% mutate(bmi = wt_kg / (ht_cm / 100)^2)   # usando dplyr

# Verificação da classe dos objetos
class(linelist)         # deve ser data.frame ou tibble
class(linelist$age)     # numérica
class(linelist$gender)  # character

# Vetores e coerção de tipos
num_vector <- c(1,2,3,4,5) # cria vetor numérico
class(num_vector)          # verifica classe
num_vector[3] <- "three"   # insere string em vetor numérico
class(num_vector)          # vetor vira character

# Vetor de variáveis explicativas (nomes de colunas)
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")
explanatory_vars

# Tamanho de vetor (número de valores)
length(linelist$age)

# Acesso a elementos específicos
demo_vector <- c("a", "b", "c", "d", "e", "f")
demo_vector[5]

# Agrupamentos e contagens
linelist %>% count(age_cat)                   # tabela de frequência
linelist_summary <- linelist %>% count(age_cat)  # salva como objeto

# Filtragem de linhas
linelist <- linelist %>% filter(age > 50)     # apenas maiores de 50 anos

# Criar nova variável derivada (idade em meses)
linelist <- linelist %>% mutate(age_months = age_years * 12)

# Classificação condicional de casos com base no desfecho ('outcome')
linelist_cleaned <- linelist %>%
  mutate(case_def = case_when(
    is.na(outcome)         ~ NA_character_,   # sem desfecho → indefinido
    outcome == "Death"     ~ "Confirmed",     # morte → confirmado
    outcome == "Recover"   ~ "Probable",      # recuperação → provável
    TRUE                   ~ "Suspected"      # qualquer outro → suspeito
  ))

# Verifica presença de valores ausentes (NA)
rdt_result <- c("Positive", "Suspected", "Positive", NA)
is.na(rdt_result)

# ======================================================
# SEÇÃO 4: Visualização de Dados
# ======================================================
library(apyramid)

# Gera pirâmides etárias a partir de dados agrupados
age_pyramid(data = linelist, age_group = "age_cat5", split_by = "gender")
age_pyramid(linelist, "age_cat5", "gender", proportional = TRUE, pal = c("orange", "purple"))

# ======================================================
# SEÇÃO 5: Funções e Objetos Complexos
# ======================================================

# Cria uma lista com dois elementos: um vetor e um data.frame
my_list <- list(
  hospitals = c("Central", "Empire", "Santa Anna"),
  addresses = data.frame(
    street = c("145 Medical Way", "1048 Brown Ave", "999 El Camino"),
    city = c("Andover", "Hamilton", "El Paso")
  )
)

my_list[1]               # exibe o primeiro elemento (como lista)
my_list[[1]]             # exibe apenas o vetor
my_list[["hospitals"]]  # indexação por nome
my_list[[1]][3]          # terceiro hospital
my_list[[2]][1]          # primeira coluna (street) do data.frame

# Remove objetos do ambiente
rm(list = ls(all = TRUE))           # limpa todos os objetos do ambiente

# ======================================================
# SEÇÃO 6: Exemplos Ilustrativos com Pipe
# ======================================================

# Criando os ingredientes como objetos simples
flour <- "farinha"
eggs <- "ovos"
oil <- "óleo"
water <- "água"

# Funções fictícias nativas
add <- function(base, ingrediente) {
  paste(base, "+", ingrediente)
}

mix_together <- function(object, utensil = "colher", minutes = 2) {
  paste("Mistura de", object, "com", utensil, "por", minutes, "minutos")
}

bake <- function(massa, degrees = 350, system = "Fahrenheit", minutes = 35) {
  paste("Assado:", massa, "-", degrees, system, "-", minutes, "minutos")
}

let_cool <- function(bolo) {
  paste("Bolo pronto (resfriado):", bolo)
}

# Adicionando ingredientes um a um
batter_1 <- add(flour, eggs)
batter_2 <- add(batter_1, oil)
batter_3 <- add(batter_2, water)

# Mistura
batter_4 <- mix_together(batter_3, utensil = "colher", minutes = 2)

# Assa
baked <- bake(batter_4, degrees = 350, system = "Fahrenheit", minutes = 35)

# Resfriar
cake <- let_cool(baked)

# Ver resultado
print(cake)

# ======================================================
# SEÇÃO 7: Operações Lógicas e String Matching
# ======================================================

# Desativa notação científica
options(scipen = 999)

# Arredondamento padrão vs arredondamento estilo "janitor"
round(c(2.5, 3.5))
janitor::round_half_up(c(2.5, 3.5))

# Cálculo de média com uso correto de c()
mean(c(1, 6, 12, 10, 5, 0))

# Uso do operador %in% para verificar presença de valores
meu_vetor <- c("a", "b", "c", "d")
"a" %in% meu_vetor
"h" %in% meu_vetor
!"a" %in% meu_vetor
!"h" %in% meu_vetor

# Criar vetor de respostas afirmativas em diferentes formatos
affirmative <- c("1", "Yes", "YES", "yes", "y", "Y", "oui", "Oui", "Si")
affirmative_str_search <- paste0(affirmative, collapse = "|")
affirmative_str_search <- stringr::str_c(affirmative, collapse = "|")