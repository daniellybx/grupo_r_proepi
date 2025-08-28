##############################################################
# AULA 3 - ANÁLISE DESCRITIVA EM EPIDEMIOLOGIA
##############################################################

# OBJETIVOS DA AULA:
# 1. Importar e preparar dados do SIH 2024 (DF)
# 2. Tratar variáveis (transformações e recategorização)
# 3. Calcular medidas de frequência (absoluta e relativa)
# 4. Calcular medidas de tendência central e dispersão
# 5. Tratar valores ausentes (missing) e outliers
# 6. Construir gráficos básicos para variáveis categóricas e numéricas

##############################################################
# 1. IMPORTAÇÃO DO ARQUIVO .RDS SALVO NA AULA ANTERIOR
##############################################################
library(tidyverse)

# Importa os dados do arquivo salvo anteriormente (dados do SIH 2024 - DF)
sih_df_2024 <- readRDS("dados/sih_df_2024.rds")

# Verifica a estrutura dos dados
glimpse(sih_df_2024)

##############################################################
# 2. TRANSFORMAÇÃO DE VARIÁVEIS (Tidyverse)
##############################################################

# Transformando a variável "SEXO"
# Substituímos os códigos por rótulos significativos
# 1 = Masculino, 3 = Feminino, outros valores e NA = Ignorado
sih_df_2024 <- sih_df_2024 %>%
  mutate(
    SEXO = case_when(
      SEXO == '1' ~ "Masculino",
      SEXO == '3' ~ "Feminino",
      TRUE ~ "Ignorado"
    )
  )

# Verificando a nova variável SEXO
table(sih_df_2024$SEXO)

# Transformando todas as colunas que começam com "DT_" para o formato de data
# O formato original é "YYYYMMDD" (ex: "20241003"), por isso usamos ymd() do lubridate
sih_df_2024 <- sih_df_2024 %>%
  mutate(across(starts_with("DT_"), ~ ymd(.)))

# Verificando resultado
sih_df_2024 %>%
  select(starts_with("DT_")) %>%
  glimpse()

# Transformando a variável "NASC" (data de nascimento) individualmente
# Também está no formato "YYYYMMDD"
sih_df_2024 <- sih_df_2024 %>%
  mutate(NASC = ymd(NASC))

# Criando a variável IDADE_CALCULADA com base na diferença entre a data de internação e a data de nascimento
# Supondo que DT_INTER seja a data de internação hospitalar
sih_df_2024 <- sih_df_2024 %>%
  mutate(IDADE_CALCULADA = as.integer((DT_INTER - NASC) / 365.25))

# Verificando as idades
summary(sih_df_2024$IDADE_CALCULADA)

# Criando a variável de faixa etária com base em IDADE_CALCULADA
sih_df_2024 <- sih_df_2024 %>%
  mutate(
    FAIXA_ETARIA = case_when(
      IDADE_CALCULADA < 1 ~ "<1 ano",
      IDADE_CALCULADA >= 1 & IDADE_CALCULADA <= 4 ~ "1–4 anos",
      IDADE_CALCULADA >= 5 & IDADE_CALCULADA <= 9 ~ "5–9 anos",
      IDADE_CALCULADA >= 10 & IDADE_CALCULADA <= 19 ~ "10–19 anos",
      IDADE_CALCULADA >= 20 & IDADE_CALCULADA <= 39 ~ "20–39 anos",
      IDADE_CALCULADA >= 40 & IDADE_CALCULADA <= 59 ~ "40–59 anos",
      IDADE_CALCULADA >= 60 & IDADE_CALCULADA <= 79 ~ "60–79 anos",
      IDADE_CALCULADA >= 80 ~ "80+ anos",
      TRUE ~ "Ignorado"
    )
  )

# Transformando a faixa etária em um fator ordenado
# Isso garante que a ordem lógica seja preservada em tabelas e gráficos
sih_df_2024 <- sih_df_2024 %>%
  mutate(
    FAIXA_ETARIA = factor(
      FAIXA_ETARIA,
      levels = c("<1 ano", "1–4 anos", "5–9 anos", "10–19 anos",
                 "20–39 anos", "40–59 anos", "60–79 anos", "80+ anos", "Ignorado"),
      ordered = TRUE
    )
  )

# Verificando resultado final
sih_df_2024 %>%
  select(SEXO, NASC, IDADE_CALCULADA, FAIXA_ETARIA, starts_with("DT_")) %>%
  head()

##############################################################
# 3. MEDIDAS DE FREQUÊNCIA (ABSOLUTA E RELATIVA)
##############################################################

# Tabela de frequência absoluta de SEXO
tabela_sexo <- sih_df_2024 %>%
  count(SEXO, name = "frequencia")

# Visualizar tabela
print(tabela_sexo)

# Gráfico de barras de SEXO com cores personalizadas
# Vermelho para mulheres, azul para homens
ggplot(tabela_sexo, aes(x = SEXO, y = frequencia, fill = SEXO)) +
  geom_col() +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue", "Ignorado" = "gray")) +
  labs(title = "Frequência por Sexo", x = "Sexo", y = "Frequência") +
  theme_minimal()

# Incluindo labels
ggplot(tabela_sexo, aes(x = SEXO, y = frequencia, fill = SEXO)) +
  geom_col() +
  geom_text(aes(label = frequencia), vjust = 1.5, color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue", "Ignorado" = "gray")) +
  labs(title = "Frequência por Sexo", x = "Sexo", y = "Frequência") +
  theme_minimal()

# Tabela com frequência e percentual de FAIXA_ETARIA
tabela_faixa <- sih_df_2024 %>%
  count(FAIXA_ETARIA, name = "frequencia") %>%
  mutate(percentual = round(100 * frequencia / sum(frequencia), 1))

# Visualizar tabela
print(tabela_faixa)

# Gráfico de barras horizontais com percentual por FAIXA_ETARIA
ggplot(tabela_faixa, aes(x = FAIXA_ETARIA, y = percentual)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Distribuição percentual por faixa etária",
       x = "Faixa etária",
       y = "Percentual (%)") +
  theme_minimal()

# Reordenando o fator FAIXA_ETARIA para começar por "<1 ano" no topo do gráfico
tabela_faixa <- tabela_faixa %>%
  mutate(FAIXA_ETARIA = fct_rev(FAIXA_ETARIA))

# Gráfico com barras horizontais e labels de percentual
ggplot(tabela_faixa, aes(x = FAIXA_ETARIA, y = percentual)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(percentual, "%")),
            hjust = 1.1, color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  labs(title = "Distribuição percentual por faixa etária",
       x = "Faixa etária",
       y = "Percentual (%)") +
  theme_minimal()

# Criando uma nova variável: mês da data de internação
# Usamos lubridate::month com label = TRUE para obter o nome do mês
sih_df_2024 <- sih_df_2024 %>%
  mutate(MES_INTER = month(DT_INTER, label = TRUE, abbr = FALSE))

# Tabela de frequência por mês e sexo
tabela_mes_sexo <- sih_df_2024 %>%
  count(MES_INTER, SEXO)

# Gráfico de barras empilhadas
ggplot(tabela_mes_sexo, aes(x = MES_INTER, y = n, fill = SEXO)) +
  geom_col() +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue", "Ignorado" = "gray")) +
  labs(
    title = "Internações por mês e sexo (barras empilhadas)",
    x = "Mês de internação",
    y = "Frequência"
  ) +
  theme_minimal()

# Gráfico de barras agrupadas (lado a lado)
ggplot(tabela_mes_sexo, aes(x = MES_INTER, y = n, fill = SEXO)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue", "Ignorado" = "gray")) +
  labs(
    title = "Internações por mês e sexo (barras agrupadas)",
    x = "Mês de internação",
    y = "Frequência"
  ) +
  theme_minimal()

##############################################################
# 4. MEDIDAS DE TENDÊNCIA CENTRAL E DISPERSÃO
##############################################################

# Calculando medidas descritivas de IDADE_CALCULADA com arredondamento (1 casa decimal)
idade_stats <- sih_df_2024 %>%
  summarise(
    media = round(mean(IDADE_CALCULADA, na.rm = TRUE), 1),
    mediana = round(median(IDADE_CALCULADA, na.rm = TRUE), 1),
    moda = as.numeric(names(sort(table(IDADE_CALCULADA), decreasing = TRUE)[1])),
    variancia = round(var(IDADE_CALCULADA, na.rm = TRUE), 1),
    desvio_padrao = round(sd(IDADE_CALCULADA, na.rm = TRUE), 1)
  )

print(idade_stats)

# Calculando medidas descritivas de QT_DIARIAS com arredondamento (1 casa decimal)
qt_stats <- sih_df_2024 %>%
  summarise(
    media = round(mean(QT_DIARIAS, na.rm = TRUE), 1),
    mediana = round(median(QT_DIARIAS, na.rm = TRUE), 1),
    moda = as.numeric(names(sort(table(QT_DIARIAS), decreasing = TRUE)[1])),
    variancia = round(var(QT_DIARIAS, na.rm = TRUE), 1),
    desvio_padrao = round(sd(QT_DIARIAS, na.rm = TRUE), 1)
  )

print(qt_stats)

##############################################################
# 4.1 HISTOGRAMA SIMPLES DE IDADE
##############################################################

ggplot(sih_df_2024, aes(x = IDADE_CALCULADA)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Histograma simples da idade calculada",
    x = "Idade (anos)",
    y = "Frequência"
  ) +
  theme_minimal()

##############################################################
# 4.2 HISTOGRAMA COM MÉDIA, MEDIANA E DESVIO PADRÃO
##############################################################

media_idade <- idade_stats$media
mediana_idade <- idade_stats$mediana
dp_idade <- idade_stats$desvio_padrao

ggplot(sih_df_2024, aes(x = IDADE_CALCULADA)) +
  geom_histogram(binwidth = 5, fill = "gray70", color = "white") +
  geom_vline(xintercept = media_idade, color = "blue", linetype = "solid", size = 1.2) +
  geom_vline(xintercept = mediana_idade, color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = c(media_idade - dp_idade, media_idade + dp_idade),
             color = "darkgreen", linetype = "dotted", size = 1) +
  labs(
    title = "Histograma com média, mediana e desvio padrão",
    x = "Idade (anos)",
    y = "Frequência"
  ) +
  theme_minimal()

##############################################################
# 4.3 BOXPLOT DE QT_DIARIAS
##############################################################

ggplot(sih_df_2024, aes(y = QT_DIARIAS)) + coord_flip() +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(
    title = "Boxplot do número de diárias",
    y = "Número de diárias"
  ) +
  theme_minimal()

##############################################################
# 4.4 DIAGRAMA DE DISPERSÃO: IDADE vs. QT_DIARIAS
##############################################################

ggplot(sih_df_2024, aes(x = IDADE_CALCULADA, y = QT_DIARIAS)) +
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Dispersão entre idade e número de diárias",
    x = "Idade (anos)",
    y = "Número de diárias"
  ) +
  theme_minimal()

##############################################################
# 5. TRATAMENTO DE MISSING VALUES (VALORES AUSENTES)
##############################################################

# 5.1 Criando um novo dataframe excluindo colunas que só possuem NA
# Isso é útil quando importamos bases muito amplas com campos vazios
sih_sem_colunas_na <- sih_df_2024 %>%
  select(where(~ !all(is.na(.))))

# Verificando dimensões antes e depois
dim(sih_df_2024)           # Original
dim(sih_sem_colunas_na)    # Sem colunas 100% NA

# 5.2 Criando um novo dataframe excluindo linhas com qualquer NA
# Atenção: essa prática pode reduzir muito a amostra, então deve ser usada com cuidado
sih_sem_linhas_na <- sih_sem_colunas_na %>%
  drop_na()

# Verificando número de linhas antes e depois
nrow(sih_sem_colunas_na)   # Com linhas com NA
nrow(sih_sem_linhas_na)    # Apenas linhas completas

# 5.3 Substituindo os NA da coluna DIAGSEC1 por "IGNORADO"
# Quando uma variável é do tipo categórica, podemos criar uma nova categoria chamada "IGNORADO"
sih_df_2024 <- sih_df_2024 %>%
  mutate(DIAGSEC1 = if_else(is.na(DIAGSEC1), "IGNORADO", DIAGSEC1))

# Conferindo resultado
table(sih_df_2024$DIAGSEC1, useNA = "ifany")

# 5.4 Tratando valores extremos como NA e preenchendo com média ou interpolação
# Vamos considerar que valores de IDADE_CALCULADA acima de 100 anos são outliers ou codificações erradas

# Substituindo valores extremos por NA
sih_df_2024 <- sih_df_2024 %>%
  mutate(IDADE_LIMPA = if_else(IDADE_CALCULADA > 100, NA_integer_, IDADE_CALCULADA))

# Visualizando quantidade de NA
sum(is.na(sih_df_2024$IDADE_LIMPA))

# 5.4.1 Substituindo os NA por média (técnica simples, mas comum em bases epidemiológicas)
media_idade <- mean(sih_df_2024$IDADE_LIMPA, na.rm = TRUE)

sih_df_2024 <- sih_df_2024 %>%
  mutate(IDADE_MEDIA = if_else(is.na(IDADE_LIMPA), round(media_idade), IDADE_LIMPA))

# 5.4.2 Substituindo os NA por interpolação linear
# É necessário ordenar a base por data ou por algum identificador lógico
# Neste exemplo, usaremos mutate + na.approx (do pacote zoo)

# install.packages("zoo")
library(zoo)

sih_df_2024 <- sih_df_2024 %>%
  arrange(DT_INTER) %>%
  mutate(IDADE_INTERPOLADA = na.approx(IDADE_LIMPA, na.rm = FALSE))

# Comparando as 3 versões
sih_df_2024 %>%
  select(IDADE_CALCULADA, IDADE_LIMPA, IDADE_MEDIA, IDADE_INTERPOLADA) %>%
  head(10)

##############################################################
# 6. TRATAMENTO DE OUTLIERS (VALORES EXTREMOS)
##############################################################

# Identificando outliers com boxplot e quantis

# Usando boxplot para identificar visualmente os outliers
ggplot(sih_df_2024, aes(y = IDADE_CALCULADA)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot da idade calculada",
       y = "Idade (anos)") +
  theme_minimal()

# Calculando o quantil de 95% (limite superior dos valores esperados)
q95 <- quantile(sih_df_2024$IDADE_CALCULADA, 0.95, na.rm = TRUE)
q95

# Quando temos certeza de que os dados são corretos
# Podemos analisar separadamente os valores extremos sem excluí-los

# Criando dois dataframes: um com "extremos" e outro com "valores regulares"
sih_idade_extremos <- sih_df_2024 %>%
  filter(IDADE_CALCULADA > q95)

sih_idade_regulares <- sih_df_2024 %>%
  filter(IDADE_CALCULADA <= q95)

# Verificando tamanho dos subconjuntos
nrow(sih_idade_extremos)
nrow(sih_idade_regulares)

# Quando temos certeza de que o valor é um erro
# Podemos aplicar diferentes estratégias para lidar com outliers incorretos

## Excluindo as observações com idade acima do percentil 95
sih_idade_excluida <- sih_df_2024 %>%
  filter(IDADE_CALCULADA <= q95)

## Substituindo valores extremos pela mediana
mediana_idade <- median(sih_df_2024$IDADE_CALCULADA, na.rm = TRUE)

sih_idade_mediana <- sih_df_2024 %>%
  mutate(IDADE_TRATADA = if_else(IDADE_CALCULADA > q95, mediana_idade, IDADE_CALCULADA))

## Substituindo por regressão linear baseada em quantidade de diárias
# Estimamos a idade com base em uma regressão linear simples: idade ~ qt_diarias

modelo_regressao <- lm(IDADE_CALCULADA ~ QT_DIARIAS, data = sih_df_2024)

# Gerando valores previstos para idade
sih_idade_regressao <- sih_df_2024 %>%
  mutate(
    IDADE_PREDITA = predict(modelo_regressao),
    IDADE_TRATADA = if_else(IDADE_CALCULADA > q95, round(IDADE_PREDITA), IDADE_CALCULADA)
  )

# Visualizando a diferença entre valores reais e ajustados
sih_idade_regressao %>%
  select(QT_DIARIAS, IDADE_CALCULADA, IDADE_PREDITA, IDADE_TRATADA) %>%
  head(10)