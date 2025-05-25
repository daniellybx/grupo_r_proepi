# Aula 03 - Limpeza e Gerenciamento de Dados com R

# Objetivo: Demonstrar, de forma progressiva e didática, o processo de limpeza de dados epidemiológicos,
# com base no cap. 8 do Manual de R para Epidemiologistas, usando como exemplo o arquivo linelist_raw.xlsx.

# =============================================================
# 1. Carregar pacotes necessários
# =============================================================

# Usamos pacman::p_load() pois ele instala automaticamente os pacotes que não estão disponíveis ainda,
# e carrega os que já estão instalados.
pacman::p_load(
  rio,        # Para importar e exportar dados em múltiplos formatos
  here,       # Para usar caminhos relativos no projeto R
  janitor,    # Para limpeza automática de nomes de colunas
  lubridate,  # Para lidar com datas
  epikit,     # Para categorização de idade
  tidyverse,  # Coleção de pacotes para manipulação e visualização de dados
  skimr       # Para gerar resumos descritivos das variáveis
)

# =============================================================
# 2. Importar os dados brutos
# =============================================================

# Importamos o arquivo Excel com linelist de casos. Esse arquivo deve estar dentro da pasta "dados" do projeto.
linelist_raw <- import(here("dados", "linelist_raw.xlsx"))

# Visualizar estrutura geral das colunas, classes e valores ausentes.
skim(linelist_raw)

# =============================================================
# 3. Início da limpeza de dados com pipeline (%>%)
# =============================================================

# Vamos iniciar uma sequência de limpeza de dados utilizando o operador pipe (%>%) do pacote magrittr.
# Isso permite aplicar diversas transformações de forma encadeada e legível.
linelist <- linelist_raw %>%
  
  # Padronizar os nomes das colunas (sem espaços, letras minúsculas, nomes únicos)
  janitor::clean_names() %>%
  
  # Renomear manualmente colunas específicas para facilitar uso padronizado
  rename(
    date_infection       = infection_date,
    date_hospitalisation = hosp_date,
    date_outcome         = date_of_outcome
  ) %>%
  
  # Remover colunas irrelevantes ou redundantes
  select(-c(row_num, merged_header, x28)) %>%
  
  # Remover linhas completamente duplicadas
  distinct() %>%
  
  # Criar nova coluna de IMC (Índice de Massa Corporal) a partir de peso e altura
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>%
  
  # Converter variáveis para tipos apropriados (datas e numéricos)
  mutate(
    across(contains("date"), as.Date),
    generation = as.numeric(generation),
    age        = as.numeric(age)
  ) %>%
  
  # Criar nova coluna com a diferença entre data de início e hospitalização
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>%
  
  # Corrigir grafias inconsistentes na variável 'hospital' usando recode()
  mutate(hospital = recode(hospital,
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  )) %>%
  
  # Preencher valores ausentes em hospital com o texto "Ausente"
  mutate(hospital = replace_na(hospital, "Ausente")) %>%
  
  # Criar coluna de idade em anos considerando diferentes unidades (anos ou meses)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age / 12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_
  )) %>%
  
  # Criar colunas com categorias de idade (largura personalizada e de 5 em 5 anos)
  mutate(
    age_cat = epikit::age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 50, 70)),
    age_cat5 = epikit::age_categories(age_years, breakers = seq(0, 85, 5))
  ) %>%
  
  # Filtrar para manter apenas casos da segunda epidemia e registros válidos
  filter(
    !is.na(case_id),
    date_onset > as.Date("2013-06-01") |
      (is.na(date_onset) & !hospital %in% c("Hospital A", "Hospital B"))
  )

# =============================================================
# 4. Resumo da estrutura final
# =============================================================

# Verificar estrutura geral do objeto limpo
str(linelist)

# Visualizar nomes finais das colunas
names(linelist)

# =============================================================
# 5. Exportar dados limpos para nova pasta
# =============================================================

dir.create(here("dados", "limpos"), showWarnings = FALSE)
export(linelist, here("dados", "limpos", "linelist_limpo_aula3.csv"))

# =============================================================
# 6. Visualização: histograma da idade dos casos
# =============================================================

# Criar histograma da variável de idade para visualizar a distribuição etária
linelist %>%
  ggplot(aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribuição de Idade dos Casos", x = "Idade (anos)", y = "Número de Casos") +
  theme_minimal()
