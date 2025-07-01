# Aula 05 - Eliminação de duplicidades | Interações, Loops e Listas

# Objetivo: Ensinar os fundamentos clássicos da eliminação de duplicidades,
# estruturas de repetição (for, while, repeat), controle condicional (if, ifelse),
# e manipulação de listas no R, com foco em aplicações epidemiológicas.
# Esta aula é baseada no capítulo 5 do "Manual de R para Epidemiologistas"

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(dplyr)       # Manipulação de dados
library(lubridate)   # Manipulação de datas
library(stringr)     # Texto
library(purrr)       # Listas e programação funcional

# =============================================================
# 2. Simulando base de dados
# =============================================================

set.seed(123)
n <- 1000

dados <- data.frame(
  id_notificacao = sample(100000:199999, n, replace = TRUE),
  nome_paciente = sample(c("ANA CARLA", "JOSE LUIZ", "MARIA EDUARDA", "PEDRO PAULO"), n, replace = TRUE),
  sexo = sample(c("Feminino", "Masculino"), n, replace = TRUE),
  idade = sample(0:100, n, replace = TRUE),
  municipio = sample(c("São Paulo", "Belo Horizonte", "Recife"), n, replace = TRUE),
  doenca = sample(c("Dengue", "Zika", "Chikungunya"), n, replace = TRUE),
  data_notificacao = sample(seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"), n, replace = TRUE),
  resultado_exame = sample(c("Positivo", "Negativo", NA), n, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
  stringsAsFactors = FALSE
)

# Introduzindo duplicatas
base <- bind_rows(dados, dados[sample(1:n, 50), ])

# =============================================================
# 3. Conteúdo prático
# =============================================================

# ------------------------------
# 3.1. Eliminação de duplicidades
# ------------------------------

# a) Todas as colunas idênticas
duplicadas_total <- duplicated(base)
sum(duplicadas_total)
base_sem_total <- base[!duplicadas_total, ]

# b) Por uma coluna (id_notificacao)
duplicadas_id <- duplicated(base$id_notificacao)
sum(duplicadas_id)
base_sem_id <- base[!duplicadas_id, ]

# c) Por múltiplas colunas (nome_paciente + doenca)
duplicadas_parcial <- duplicated(base[, c("nome_paciente", "doenca")])
sum(duplicadas_parcial)
base_sem_parcial <- base[!duplicadas_parcial, ]

# ------------------------------
# 3.2. Estruturas de controle e repetição
# ------------------------------

# IF simples: criar coluna indicando se o paciente é idoso
base$eh_idoso <- NA
base$eh_idoso[base$idade >= 60] <- "Sim"
base$eh_idoso[base$idade < 60] <- "Não"

# IF-ELSE: classificar resultado do exame
base$classificacao_exame <- NA
for (i in 1:nrow(base)) {
  if (is.na(base$resultado_exame[i])) {
    base$classificacao_exame[i] <- NA
  } else if (base$resultado_exame[i] == "Positivo") {
    base$classificacao_exame[i] <- "Infectado"
  } else {
    base$classificacao_exame[i] <- "Não infectado"
  }
}

# IFELSE vetorizado: criar variável de risco
base$grupo_risco <- ifelse(base$idade >= 60 & base$resultado_exame == "Positivo", 
                           "Alto risco", "Outro")

# FOR: criar vetor com contagem de positivos por município
positivos_por_mun <- numeric(length(unique(base$municipio)))
nomes_mun <- unique(base$municipio)

for (i in seq_along(nomes_mun)) {
  mun <- nomes_mun[i]
  positivos_por_mun[i] <- sum(base$resultado_exame == "Positivo" & base$municipio == mun, na.rm = TRUE)
}

base_resumo <- data.frame(municipio = nomes_mun, total_positivos = positivos_por_mun)

# WHILE: criar coluna indicando se paciente com menos de 1 ano já foi identificado entre os 10 primeiros
base$menor_1_ano_flag <- "Não marcado"
contador <- 0
index <- 1
while (contador < 10 & index <= nrow(base)) {
  if (base$idade[index] < 1) {
    base$menor_1_ano_flag[index] <- "Sim"
    contador <- contador + 1
  }
  index <- index + 1
}

# REPEAT: criar nova coluna indicando se é o primeiro paciente positivo acima de 80 anos
base$primeiro_idoso_pos <- "Não"
linha <- 1
repeat {
  if (!is.na(base$idade[linha]) &&
      !is.na(base$resultado_exame[linha]) &&
      base$idade[linha] > 80 &&
      base$resultado_exame[linha] == "Positivo") {
    
    base$primeiro_idoso_pos[linha] <- "Sim"
    break
  }
  
  linha <- linha + 1
  if (linha > nrow(base)) break  # Garante que não ultrapasse os limites
}

# NEXT e BREAK: nova coluna com marcador até encontrar paciente com 100 anos
base$cem_anos_encontrado <- "Antes de 100"
for (i in 1:nrow(base)) {
  if (is.na(base$idade[i])) next
  if (base$idade[i] == 100) {
    base$cem_anos_encontrado[i] <- "Encontrado"
    break
  }
  base$cem_anos_encontrado[i] <- "Verificado"
}

# ------------------------------
# 3.3. Trabalhando com listas e funções do pacote purrr
# ------------------------------

# Criando uma lista com subconjuntos da base por município
lista_municipios <- base %>% group_split(municipio)
nomes_municipios <- base %>% group_keys(municipio) %>% pull()
names(lista_municipios) <- nomes_municipios

# Verificando o número de registros por item da lista com map
map(lista_municipios, nrow)

# Criando nova lista apenas com pacientes positivos
lista_positivos <- map(lista_municipios, ~ filter(.x, resultado_exame == "Positivo"))

# Calculando a média de idade dos positivos por município com map_dfr
media_idade_positivos <- map_dfr(lista_positivos, ~ summarise(.x,
                                                              municipio = unique(municipio),
                                                              media_idade = mean(idade, na.rm = TRUE)
), .id = "lista")

# Usando map2 para comparar totais entre listas
total_geral <- map_int(lista_municipios, nrow)
total_positivos <- map_int(lista_positivos, nrow)

comparativo <- map2_df(total_geral, total_positivos, ~ data.frame(total = .x, positivos = .y))
comparativo$municipio <- names(lista_municipios)
comparativo$proporcao <- round(comparativo$positivos / comparativo$total, 2)

# ------------------------------
# Aula concluída
# ------------------------------

# Você aprendeu:
# - A remover duplicidades (total, por coluna, por múltiplas colunas)
# - A usar estruturas clássicas: if, else, ifelse, for, while, repeat, next, break
# - A manipular e iterar sobre listas
# - A aplicar lapply e map para percorrer elementos
