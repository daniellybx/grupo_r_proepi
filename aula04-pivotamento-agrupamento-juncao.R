# Aula 04 - Fatores e pivotamento de dados | Agrupando e juntando dados

# Objetivo: Nesta aula, aprenderemos a trabalhar com variáveis do tipo fator, fazer pivotamento de dados,
# realizar agrupamentos e aplicar diferentes tipos de junção entre dataframes (joins/merges).
# Esta aula é baseada no capítulo 4 do "Manual de R para Epidemiologistas"

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(dplyr)       # Manipulação de dados
library(tidyr)       # Pivotamento de dados
library(forcats)     # Manipulação de fatores
library(stringr)     # Strings auxiliares (se necessário)

# =============================================================
# 2. Simulando base de dados
# =============================================================

set.seed(123)
n <- 1000

# Base principal com informações de pacientes
dados_pacientes <- data.frame(
  id_paciente = 1:n,
  sexo = sample(c("Masculino", "Feminino", "Outro"), n, replace = TRUE),
  faixa_etaria = sample(c("0-17", "18-34", "35-49", "50-64", "65+"), n, replace = TRUE),
  municipio = sample(c("São Paulo", "Rio de Janeiro", "Belo Horizonte"), n, replace = TRUE),
  ano = sample(c(2021, 2022, 2023), n, replace = TRUE),
  casos = sample(0:10, n, replace = TRUE),
  stringsAsFactors = FALSE
)

# Segunda base para demonstrar joins
dados_cnes <- data.frame(
  municipio = c("São Paulo", "Rio de Janeiro", "Belo Horizonte", "Curitiba"),
  cod_ibge = c(3550308, 3304557, 3106200, 4106902),
  regiao_saude = c("Região Sudeste", "Região Sudeste", "Região Sudeste", "Região Sul"),
  stringsAsFactors = FALSE
)

# =============================================================
# 3. Conteúdo prático
# =============================================================

# ------------------------------
# 3.1. Trabalhando com fatores
# ------------------------------

# Converter faixa etária em fator com ordem definida
dados_pacientes <- dados_pacientes %>%
  mutate(faixa_etaria = factor(faixa_etaria,
                               levels = c("0-17", "18-34", "35-49", "50-64", "65+"),
                               ordered = TRUE))

# Usando forcats para reordenar categorias com base na frequência
library(forcats)
dados_pacientes <- dados_pacientes %>%
  mutate(sexo = fct_infreq(sexo))

# Visualizar frequência
summary(dados_pacientes$faixa_etaria)
table(dados_pacientes$sexo)

# ------------------------------
# 3.2. Pivotamento de dados
# ------------------------------

# Pivotar para formato largo: total de casos por sexo e ano
casos_largo <- dados_pacientes %>%
  group_by(ano, sexo) %>%
  summarise(total_casos = sum(casos), .groups = "drop") %>%
  pivot_wider(names_from = sexo, values_from = total_casos, values_fill = 0)

# Pivotar para formato longo novamente
casos_longo <- casos_largo %>%
  pivot_longer(cols = -ano, names_to = "sexo", values_to = "total_casos")

# ------------------------------
# 3.3. Agrupamento e sumarização
# ------------------------------

# Casos médios por faixa etária e sexo
media_casos <- dados_pacientes %>%
  group_by(faixa_etaria, sexo) %>%
  summarise(media = mean(casos), .groups = "drop")

# Casos totais por ano e município
total_por_municipio <- dados_pacientes %>%
  group_by(ano, municipio) %>%
  summarise(total = sum(casos), .groups = "drop")

# ------------------------------
# 3.4. Joins e merges
# ------------------------------

# Join interno: apenas os registros com municipio em ambas as tabelas
inner <- inner_join(dados_pacientes, dados_cnes, by = "municipio")

# Join à esquerda: mantém todos os pacientes e junta os dados do CNES se houver
left <- left_join(dados_pacientes, dados_cnes, by = "municipio")

# Join à direita: mantém todos os dados do CNES e junta pacientes se houver
right <- right_join(dados_pacientes, dados_cnes, by = "municipio")

# Join completo: une todos os registros das duas tabelas
full <- full_join(dados_pacientes, dados_cnes, by = "municipio")

# Verificar quantos NA aparecem após cada join
colSums(is.na(full))

# ------------------------------
# Aula concluída
# ------------------------------

# Nessa aula, você aprendeu:
# - A criar e manipular variáveis do tipo fator com ordem
# - A reordenar categorias com forcats
# - A usar pivot_wider() e pivot_longer() para transformar dados
# - A agrupar e sumarizar usando group_by() e summarise()
# - A combinar dados com inner_join, left_join, right_join e full_join