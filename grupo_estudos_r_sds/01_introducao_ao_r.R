##############################################################
# AULA 1 - INTRODUÇÃO AO R E AO RSTUDIO
##############################################################

# OBJETIVOS DA AULA:
# 1. Apresentar o ambiente R e o RStudio
# 2. Ensinar a instalação e carregamento de pacotes
# 3. Explicar os principais tipos de dados no R
# 4. Demonstrar os tipos de objetos usados em R
# 5. Apresentar estruturas básicas de comparação
# 6. Aplicar estruturas de repetição em dataframes
# 7. Utilizar estruturas condicionais no R

##############################################################
# 1. INSTALAÇÃO E CARREGAMENTO DE PACOTES
##############################################################

# Instalar pacote auxiliar para instalar pacotes do GitHub
install.packages("remotes")

# Instalar o pacote microdatasus a partir do GitHub
remotes::install_github("rfsaldanha/microdatasus")

# Instalar pacotes essenciais para manipulação de dados e gráficos
install.packages(c("tidyverse", "lubridate", "janitor"))

# Carregar todos os pacotes necessários
library(microdatasus)
library(tidyverse)
library(lubridate)
library(janitor)

##############################################################
# 2. TIPOS DE DADOS BÁSICOS EM R
##############################################################

# Caractere (texto)
nome <- "Maria"
class(nome)  # Resultado: "character"

# Fator (usado para variáveis categóricas)
sexo <- factor(c("Feminino", "Masculino", "Feminino"))
class(sexo)  # Resultado: "factor"

# Fator ordinal (com hierarquia entre categorias)
classe_social <- factor(
  c("Alta", "Baixa", "Média", "Média", "Alta"),
  levels = c("Baixa", "Média", "Alta"),
  ordered = TRUE
)
class(classe_social)  # Resultado: "ordered"

# Inteiro (número sem casa decimal)
idade <- 32L
class(idade)  # Resultado: "integer"

# Número decimal (float / double)
peso <- 65.7
class(peso)  # Resultado: "numeric"

# Booleano (lógico: TRUE ou FALSE)
vacinado <- TRUE
class(vacinado)  # Resultado: "logical"

# Criando um dataframe com diferentes tipos
dados <- data.frame(
  nome = c("Ana", "Bruno", "Carlos"),
  idade = c(25, 32, 28),
  peso = c(62.5, 80.2, 74.0),
  sexo = c("Feminino", "Masculino", "Masculino"),
  vacinado = c(TRUE, FALSE, TRUE),
  classe_social = c("Média", "Alta", "Baixa"),
  stringsAsFactors = FALSE  # Evita conversão automática para fator
)

# Visualizando o dataframe original
print(dados)
str(dados)

# Transformações de tipo com funções do R base

# Convertendo 'nome' para fator
dados$nome <- as.factor(dados$nome)

# Convertendo 'idade' para caractere (exemplo didático, não é usual)
dados$idade <- as.character(dados$idade)

# Convertendo 'peso' para inteiro (pode perder precisão!)
dados$peso <- as.integer(dados$peso)

# Convertendo 'sexo' para fator ordinal com levels definidos
dados$sexo <- factor(dados$sexo, levels = c("Feminino", "Masculino"), ordered = FALSE)

# Convertendo 'vacinado' para caractere
dados$vacinado <- as.character(dados$vacinado)

# Convertendo 'classe_social' para fator ordinal
dados$classe_social <- factor(
  dados$classe_social,
  levels = c("Baixa", "Média", "Alta"),
  ordered = TRUE
)

# Visualizando a estrutura final do dataframe
str(dados)

##############################################################
# 3. TIPOS DE OBJETOS EM R
##############################################################

# Vetor (conjunto de elementos do mesmo tipo)
idades <- c(25, 30, 40)

# Lista (conjunto de elementos de tipos diferentes)
pessoa <- list(nome = "João", idade = 45, vacinado = TRUE)

# Data frame (estrutura tabular, como uma planilha)
df <- data.frame(
  nome = c("Ana", "Carlos", "Fernanda"),
  idade = c(28, 34, 26),
  sexo = factor(c("F", "M", "F"))
)

# Matriz (estrutura bidimensional, todos elementos do mesmo tipo)
matriz_exemplo <- matrix(1:6, nrow = 2, ncol = 3)

##############################################################
# 4. ESTRUTURAS DE COMPARAÇÃO (CONDICIONAIS SIMPLES)
##############################################################

# Exemplo: filtrar pessoas com idade maior que 30
df[df$idade > 30, ]

# Comparações básicas
5 == 5         # igual (TRUE)
5 != 3         # diferente (TRUE)
5 > 2          # maior que
5 < 10         # menor que
5 >= 5         # maior ou igual
5 <= 6         # menor ou igual

# Uso de %in% (verifica se um valor está em um vetor)
"F" %in% df$sexo

##############################################################
# 5. ESTRUTURAS DE REPETIÇÃO 
##############################################################

# Loop FOR: imprime todos os nomes do dataframe
for (i in 1:nrow(df)) {
  print(df$nome[i])
}

# Loop WHILE: imprime todas as idades
i <- 1
while (i <= nrow(df)) {
  print(df$idade[i])
  i <- i + 1
}

##############################################################
# 6. ESTRUTURAS CONDICIONAIS
##############################################################

# Estrutura IF: verifica se a primeira pessoa tem mais de 30 anos
if (df$idade[1] > 30) {
  print("Maior de 30 anos")
} else {
  print("30 anos ou menos")
}

# Função IFELSE aplicada ao vetor de idades
# Cria nova variável classificando como "Adulto" ou "Jovem"
df$status <- ifelse(df$idade > 30, "Adulto", "Jovem")

# Visualizar o dataframe atualizado
print(df)