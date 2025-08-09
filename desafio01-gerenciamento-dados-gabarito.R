# Gabarito - Desafio Aula 04: Análise de casos de dengue com microdatasus

# Instalando microdatasus (para quem não o tem instalado)
# Tutorial de uso do pacote 'microdatasus':https://medium.com/@danielly.bx/realizando-download-de-dados-p%C3%BAblicos-do-datasus-com-o-pacote-microdatasus-do-r-218cf4181e47
# Dicionário de dados SINAN: https://portalsinan.saude.gov.br/images/documentos/Agravos/via/DIC_DADOS_NET_Violencias_v5.pdf
install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")

# Carregar pacotes
pacman::p_load(
  microdatasus,
  here,
  tidyverse,
  lubridate,
  fs
)

# Baixar os dados de dengue (2020)
dados_sinan <- fetch_datasus(
  year_start = 2020,
  year_end = 2020,
  information_system = "SINAN-DENGUE"
)

# Explorar estrutura inicial
glimpse(dados_sinan)

# 1. Filtrar apenas os casos confirmados de dengue no município de Campinas
#    - Use a coluna ID_MN_RESI com o código "350950"
#    - Use a coluna CLASSI_FIN para filtrar os casos "5" (Dengue Clássico)
dengue_campinas <- dados_sinan %>%
  filter(
    ID_MN_RESI == "350950",     # Mantém apenas residentes de Campinas (código IBGE)
    CLASSI_FIN == "5",          # Casos confirmados de dengue clássico
    !is.na(DT_NOTIFIC),         # Remove registros sem data de notificação
    !is.na(NU_IDADE_N),         # Remove registros sem idade
    !is.na(CS_SEXO)             # Remove registros sem sexo
  )

# 2. Selecionar as colunas: CS_SEXO, NU_IDADE_N, ID_MN_RESI, DT_NOTIFIC
dengue_campinas <- dengue_campinas %>%
  select(
    CS_SEXO,                    # Sexo do paciente
    NU_IDADE_N,                 # Idade numérica
    ID_MN_RESI,                 # Código do município de residência
    DT_NOTIFIC                  # Data de notificação do caso
  )

# 3. Criar uma nova coluna chamada faixa_etaria com categorias:
#    "0-9", "10-19", ..., "60+" usando case_when()
dengue_campinas <- dengue_campinas %>%
  mutate(
    unidade = as.integer(substr(NU_IDADE_N, 1, 1)),     # extrai o 1º dígito
    valor   = as.integer(substr(NU_IDADE_N, 2, 4)),     # extrai os 3 últimos
    idade_anos = case_when(
      unidade == 1 ~ valor / (24 * 365.25),  # horas → anos
      unidade == 2 ~ valor / 365.25,         # dias  → anos
      unidade == 3 ~ valor / 12,             # meses → anos
      unidade == 4 ~ valor,                  # anos  → anos
      TRUE ~ NA_real_
    ),
    faixa_etaria = case_when(
      idade_anos < 10 ~ "0-9",
      idade_anos < 20 ~ "10-19",
      idade_anos < 30 ~ "20-29",
      idade_anos < 40 ~ "30-39",
      idade_anos < 50 ~ "40-49",
      idade_anos < 60 ~ "50-59",
      idade_anos >= 60 ~ "60+",
      TRUE ~ NA_character_
    )
  )

# 4. Criar uma nova coluna chamada semana_epi com a semana epidemiológica de DT_NOTIFIC usando epiweek()
dengue_campinas <- dengue_campinas %>%
  mutate(
    semana_epi = epiweek(DT_NOTIFIC)  # Extrai a semana epidemiológica da data de notificação
  )

# Realizando tudo em um único código
dengue_campinas_unico <- dados_sinan %>%
  filter(
    ID_MN_RESI == "350950",
    CLASSI_FIN == "5",
    !is.na(DT_NOTIFIC),
    !is.na(NU_IDADE_N),
    !is.na(CS_SEXO)
  ) %>%
  select(CS_SEXO, NU_IDADE_N, ID_MN_RESI, DT_NOTIFIC) %>%
  mutate(
    unidade = as.integer(substr(NU_IDADE_N, 1, 1)),     # extrai o 1º dígito
    valor   = as.integer(substr(NU_IDADE_N, 2, 4)),     # extrai os 3 últimos
    idade_anos = case_when(
      unidade == 1 ~ valor / (24 * 365.25),  # horas → anos
      unidade == 2 ~ valor / 365.25,         # dias  → anos
      unidade == 3 ~ valor / 12,             # meses → anos
      unidade == 4 ~ valor,                  # anos  → anos
      TRUE ~ NA_real_
    ),
    faixa_etaria = case_when(
      idade_anos < 10 ~ "0-9",
      idade_anos < 20 ~ "10-19",
      idade_anos < 30 ~ "20-29",
      idade_anos < 40 ~ "30-39",
      idade_anos < 50 ~ "40-49",
      idade_anos < 60 ~ "50-59",
      idade_anos >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    semana_epi = epiweek(DT_NOTIFIC)  # linha movida para dentro do mutate
  )


# 5. Exportar o resultado para a pasta "dados/limpos" como um .csv
fs::dir_create(here("dados", "limpos"))
write_csv(dengue_campinas, here("dados", "limpos", "dengue_campinas_2020.csv"))

## Questões do formulário

# 1. Qual o número total de casos de dengue confirmados em Campinas em 2020 (classificação final igual a 5)?
cat("\nTotal de casos confirmados em Campinas (2020):\n")
dengue_campinas %>% count()

# 2. Qual faixa etária teve o maior número de casos confirmados?
cat("\nCasos por faixa etária:\n")
dengue_campinas %>% count(faixa_etaria, sort = TRUE)

# 3. Qual foi a semana epidemiológica com o maior número de notificações?
cat("\nCasos por semana epidemiológica:\n")
dengue_campinas %>% count(semana_epi, sort = TRUE)

# 4. Quantos casos do sexo feminino foram confirmados?
cat("\nDistribuição por sexo:\n")
dengue_campinas %>% count(CS_SEXO)

# 5. Quantos registros foram identificados com sexo informado como “I”?
cat("\nDistribuição por sexo:\n")
dengue_campinas %>% count(CS_SEXO)