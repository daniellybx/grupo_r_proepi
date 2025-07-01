# Aula 03 - Limpeza de dados e principais funções | Trabalhando com datas, caracteres e strings

# Objetivo: Ensinar as principais tarefas de limpeza de dados, manipulação de variáveis de texto e datas,
# utilizando as funções fundamentais do R.
# Esta aula é baseada no capítulo 3 do "Manual de R para Epidemiologistas"

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(dplyr)       # Manipulação de dados
library(stringr)     # Manipulação de strings
library(lubridate)   # Manipulação de datas
library(tidyr)       # Limpeza e transformação de dados
library(forcats)     # Manipulação de variáveis categóricas

# =============================================================
# 2. Simulando base de dados
# =============================================================

set.seed(123)

n <- 1000

dados <- data.frame(
  id = 1:n,
  nome = sample(c("ana silva", "joão santos", "MARIA OLIVEIRA", "Carlos Souza", "lucas lima", NA), n, replace = TRUE),
  data_nascimento = sample(seq(as.Date("1950-01-01"), as.Date("2005-12-31"), by = "day"), n, replace = TRUE),
  idade_texto = sample(c("35 anos", "40", "27 anos", "vinte", NA), n, replace = TRUE),
  sexo = sample(c("F", "M", "feminino", "masculino", "Outro", "Não informado", NA), n, replace = TRUE),
  renda_mensal = sample(c("R$ 2.000", "3000", "Quatro mil", "R$3500,00", NA), n, replace = TRUE),
  data_entrevista = sample(c("01/01/2022", "15/02/2023", "2023-03-10", "20-04-2022", "31/12/2021", NA), n, replace = TRUE),
  municipio = sample(c("São Paulo", "Rio de Janeiro", "Belo Horizonte", NA), n, replace = TRUE),
  telefone = sample(c("(11) 91234-5678", "(21) 99876-5432", NA, "sem telefone"), n, replace = TRUE),
  observacao = sample(c("Nenhuma", "Precisa ligar", "Atualizar cadastro", NA), n, replace = TRUE),
  stringsAsFactors = FALSE
)

# =============================================================
# 3. Conteúdo prático
# =============================================================

# ------------------------------
# 3.1. Conhecendo os dados
# ------------------------------

str(dados)
summary(dados)
colSums(is.na(dados))

# ------------------------------
# 3.2. Padronização de textos (strings)
# ------------------------------

dados <- dados %>%
  mutate(
    nome = str_to_lower(nome),                      # tudo minúsculo
    nome = str_squish(nome),                        # remover espaços extras
    nome = str_to_title(nome)                       # capitalizar nomes
  )

# ------------------------------
# 3.3. Tratamento de variáveis categóricas (sexo)
# ------------------------------

dados <- dados %>%
  mutate(sexo = case_when(
    sexo %in% c("F", "feminino", "Feminino") ~ "Feminino",
    sexo %in% c("M", "masculino", "Masculino") ~ "Masculino",
    sexo == "Outro" ~ "Outro",
    is.na(sexo) ~ "Ignorado",
    TRUE ~ "Ignorado"
  )) %>%
  mutate(sexo = factor(sexo))

table(dados$sexo)

# ------------------------------
# 3.4. Limpando variáveis de idade
# ------------------------------

dados <- dados %>%
  mutate(idade_extraida = str_extract(idade_texto, "\\d+"),
         idade_extraida = as.numeric(idade_extraida))

summary(dados$idade_extraida)

# ------------------------------
# 3.5. Tratando datas
# ------------------------------

# data_entrevista pode ter vários formatos

dados <- dados %>%
  mutate(data_entrevista_formatada = dmy(data_entrevista))

# para valores que ainda estão NA, tentar outro formato

dados <- dados %>%
  mutate(data_entrevista_formatada = if_else(
    is.na(data_entrevista_formatada),
    ymd(data_entrevista),
    data_entrevista_formatada
  ))

# calcular idade com base na data de nascimento

dados <- dados %>%
  mutate(idade_calculada = floor(interval(data_nascimento, Sys.Date()) / years(1)))

# ------------------------------
# 3.6. Limpando renda mensal
# ------------------------------

dados <- dados %>%
  mutate(renda_limpa = str_replace_all(renda_mensal, "[^0-9]", ""),
         renda_limpa = as.numeric(renda_limpa))

summary(dados$renda_limpa)

# ------------------------------
# 3.7. Manipulando colunas com texto livre
# ------------------------------

# Detectar palavras-chave na observação

dados <- dados %>%
  mutate(precisa_ligar = str_detect(str_to_lower(observacao), "ligar"))

table(dados$precisa_ligar)

# ------------------------------
# 3.8. Separando e unindo colunas
# ------------------------------

# Separar DDD do telefone

dados <- dados %>%
  separate(telefone, into = c("ddd", "numero"), sep = " ", fill = "right", remove = FALSE)

# Unir nome e município em uma nova coluna

dados <- dados %>%
  mutate(nome_completo_local = str_c(nome, " - ", municipio))

# =============================================================
# Aula concluída!
# =============================================================

# Nessa aula, você aprendeu:
# - A explorar e entender dados brutos
# - A padronizar textos, datas e valores categóricos
# - A limpar campos com números em formato textual
# - A lidar com datas inconsistentes
# - A extrair informações de colunas textuais livres
# - A separar e unir colunas

# Essas são ferramentas fundamentais para qualquer análise epidemiológica.
