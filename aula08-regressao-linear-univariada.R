# Aula 08 - Regressão Linear Simples

# Problema proposto:
# Suponha que uma equipe de vigilância epidemiológica esteja avaliando o estado nutricional
# de crianças menores de 5 anos em um município. As crianças são acompanhadas em unidades de saúde
# com registro de peso e idade. A equipe quer investigar a relação entre idade (em meses) e peso,
# e verificar se existe diferença significativa no peso médio entre crianças mais novas (até 30 meses)
# e mais velhas (acima de 30 meses). A análise será feita com base em uma base simulada representativa,
# incluindo tratamento de dados faltantes e outliers, e avaliação da normalidade dos dados para aplicação
# de testes estatísticos apropriados como o teste t e regressão linear.

# Objetivo: Nesta aula, aprenderemos a construir e avaliar um modelo de regressão linear simples aplicado à epidemiologia infantil.
# Iremos utilizar uma base simulada com variáveis de idade (em meses) e peso infantil, com introdução de valores faltantes e outliers.
# Os tópicos cobertos incluem: pré-processamento, visualizações, teste de normalidade, modelagem, diagnóstico e pressupostos do modelo.

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(tidyverse)      # Manipulação de dados e gráficos
library(naniar)         # Visualização de valores ausentes
library(nortest)        # Teste de normalidade para grandes amostras

# =============================================================
# 2. Simulando base com missing values e outliers
# =============================================================

set.seed(123)
n <- 1000

# Simulando idade em meses (0 a 60 meses)
idade_meses <- round(runif(n, 0, 60), 1)

# Simulando peso infantil com distribuição normal verdadeira
# Fórmula do peso baseada em taxa de crescimento média: 2.5kg + 0.25kg/mês
peso <- round(rnorm(n, mean = 2.5 + 0.25 * idade_meses, sd = 1), 2)

# Criando base inicial
base <- tibble(
  idade_meses = idade_meses,
  peso = peso
)

# Inserindo valores faltantes (5% por variável)
set.seed(456)
base[sample(1:n, 50), "peso"] <- NA
base[sample(1:n, 50), "idade_meses"] <- NA

# Inserindo outliers em peso (valores anormalmente altos)
set.seed(789)
outlier_idx <- sample(which(!is.na(base$peso)), 10)
base$peso[outlier_idx] <- base$peso[outlier_idx] + rnorm(10, mean = 8, sd = 2)

# =============================================================
# 3. Tratamento de missing values e outliers
# =============================================================

# Visualizando dados faltantes
vis_miss(base)

# Remoção completa de linhas com NA (tratamento simples para regressão)
base_clean <- base %>% drop_na()

# Detectando e removendo outliers com base no IQR (Boxplot)
Q1 <- quantile(base_clean$peso, 0.25)
Q3 <- quantile(base_clean$peso, 0.75)
IQR_val <- Q3 - Q1

base_clean <- base_clean %>%
  filter(peso >= (Q1 - 1.5 * IQR_val),
         peso <= (Q3 + 1.5 * IQR_val))

# =============================================================
# 4. Avaliação da distribuição da variável resposta
# =============================================================

# Histograma com curva de densidade + média e desvio padrão
base_clean %>%
  ggplot(aes(x = peso)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", bins = 30, color = "white") +
  geom_density(color = "darkblue", size = 1) +
  geom_vline(aes(xintercept = mean(peso)), linetype = "dashed", color = "red", size = 1) +
  geom_vline(aes(xintercept = mean(peso) + sd(peso)), linetype = "dotted", color = "purple") +
  geom_vline(aes(xintercept = mean(peso) - sd(peso)), linetype = "dotted", color = "purple") +
  labs(title = "Distribuição do Peso Infantil com Curva de Densidade",
       x = "Peso (kg)", y = "Densidade") +
  theme_minimal()

# Teste de normalidade para a variável peso usando Anderson-Darling
ad.test(base_clean$peso)

# =============================================================
# 5. Teste t de comparação de médias
# =============================================================

# Criando uma variável categórica: grupo_idade (0-29 meses vs 30-60 meses)
base_clean <- base_clean %>%
  mutate(grupo_idade = if_else(idade_meses <= 30, "0-30m", "31-60m"))

# Comparando médias de peso entre os dois grupos com teste t
# O objetivo é verificar se crianças mais novas pesam menos que as mais velhas, em média

t.test(peso ~ grupo_idade, data = base_clean)

# =============================================================
# 6. Visualização da relação entre idade e peso
# =============================================================

# Gráfico de dispersão simples
base_clean %>%
  ggplot(aes(x = idade_meses, y = peso)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(title = "Dispersão entre Idade e Peso",
       x = "Idade (meses)", y = "Peso (kg)") +
  theme_minimal()

# Gráfico com linha de tendência linear
base_clean %>%
  ggplot(aes(x = idade_meses, y = peso)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Tendência Linear entre Idade e Peso",
       x = "Idade (meses)", y = "Peso (kg)") +
  theme_minimal()

# =============================================================
# 7. Modelagem de regressão linear simples
# =============================================================

modelo <- lm(peso ~ idade_meses, data = base_clean)
summary(modelo)

# Coeficientes do modelo
coeficientes <- coef(modelo)
intercepto <- coeficientes[1]
inclinacao <- coeficientes[2]

# Exibindo equação da reta ajustada
cat("\nEquação estimada do modelo:")
cat(paste0("peso = ", round(intercepto, 2), " + ", round(inclinacao, 2), " * idade_meses\n"))

# Interpretação:
# - Intercepto: peso médio estimado para idade 0 meses.
# - Inclinação: ganho de peso estimado por mês de idade.

# Visualização da regressão
base_clean %>%
  ggplot(aes(x = idade_meses, y = peso)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Regressão Linear Simples: Peso vs Idade",
       x = "Idade (meses)", y = "Peso (kg)") +
  theme_minimal()

# =============================================================
# 8. Avaliação do modelo e diagnóstico dos resíduos
# =============================================================

par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))

# Explicando as métricas:
# - R²: proporção da variância explicada pelo modelo
# - p-valor: testa a hipótese de que os coeficientes são diferentes de zero
# - Resíduos: devem apresentar distribuição normal e variância constante
# - Gráficos: avaliam linearidade, homocedasticidade e normalidade dos resíduos

# =============================================================
# 9. Pressupostos da Regressão Linear
# =============================================================

# Os principais pressupostos são:
# - Linearidade: a relação entre as variáveis é linear
# - Normalidade dos resíduos
# - Homocedasticidade (variância constante dos erros)
# - Independência dos erros

# Verificar os gráficos de diagnóstico é essencial para validar o modelo.

# Aula concluída!
