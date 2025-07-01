# Aula 10 - Médias Móveis, Séries Temporais e Detecção de Surtos

# Problema proposto:
# Uma equipe de vigilância epidemiológica monitora semanalmente o número de casos de diarreia infecciosa em um município.
# O objetivo é identificar padrões sazonais, tendências e detectar possíveis surtos ao longo do tempo.
# A equipe usará técnicas de séries temporais como médias móveis e modelos de suavização exponencial (Holt-Winters)
# para apoiar decisões de intervenção.

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

library(tidyverse)     # Manipulação de dados e gráficos
library(lubridate)     # Manipulação de datas
library(zoo)           # Médias móveis
library(forecast)      # Modelos de séries temporais (Holt-Winters)

# =============================================================
# 2. Simulando base semanal de casos de diarreia (3 anos)
# =============================================================

set.seed(123)
data_semanal <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2022-12-31"), by = "week")
n <- length(data_semanal)

# Criando padrão com tendência leve de crescimento + sazonalidade + ruído
casos <- 20 + 
  0.05 * seq_len(n) +                                   # tendência
  10 * sin(2 * pi * seq_len(n)/52) +                    # sazonalidade anual
  rnorm(n, mean = 0, sd = 3)                            # ruído aleatório

# Adicionando surtos pontuais em algumas datas
casos[c(40, 90, 125)] <- casos[c(40, 90, 125)] + 30

serie <- tibble(
  semana = data_semanal,
  casos = round(casos)
)

# =============================================================
# 3. Visualização inicial da série temporal
# =============================================================

serie %>%
  ggplot(aes(x = semana, y = casos)) +
  geom_line(color = "darkblue") +
  labs(title = "Casos Semanais de Diarreia Infecciosa (2020–2022)",
       x = "Semana", y = "Número de casos") +
  theme_minimal()

# =============================================================
# 4. Média móvel simples (janela de 3 semanas)
# =============================================================

serie <- serie %>%
  mutate(media_movel_3s = rollmean(casos, k = 3, fill = NA, align = "right"))

serie %>%
  ggplot(aes(x = semana)) +
  geom_line(aes(y = casos), color = "gray", alpha = 0.5) +
  geom_line(aes(y = media_movel_3s), color = "red", size = 1) +
  labs(title = "Média Móvel de 3 Semanas sobre Casos Semanais",
       x = "Semana", y = "Casos") +
  theme_minimal()

# Comentário:
# A média móvel suaviza as flutuações semanais e ajuda a destacar a tendência geral.
# No entanto, ela pode atrasar a detecção de surtos rápidos por suavizar os picos.

# =============================================================
# 5. Suavização exponencial simples (Holt-Winters sem sazonalidade)
# =============================================================

serie_ts <- ts(serie$casos, frequency = 52)  # frequência semanal
modelo_hw_simples <- HoltWinters(serie_ts, beta = TRUE, gamma = FALSE)

# Gerando previsões a partir do modelo
previsao_simples <- forecast(modelo_hw_simples, h = 12)

# Plotando as previsões
forecast::autoplot(previsao_simples) +
  labs(title = "Previsão com Holt-Winters sem Sazonalidade",
       x = "Tempo", y = "Casos Estimados") +
  theme_minimal()
# =============================================================

serie_ts <- ts(serie$casos, frequency = 52)  # frequência semanal
modelo_hw_simples <- HoltWinters(serie_ts, beta = TRUE, gamma = FALSE)

forecast::autoplot(previsao_simples) +
  labs(title = "Previsão com Holt-Winters sem Sazonalidade",
       x = "Tempo", y = "Casos Estimados") +
  theme_minimal()

# Comentário:
# Esse modelo captura tendência mas não a sazonalidade. Útil para séries com variações suaves e sem padrão cíclico.

# =============================================================
# 6. Holt-Winters com sazonalidade
# =============================================================

modelo_hw_sazonal <- HoltWinters(serie_ts)

# Gerando previsões a partir do modelo com sazonalidade
previsao_sazonal <- forecast(modelo_hw_sazonal, h = 12)

# Plotando as previsões
forecast::autoplot(previsao_sazonal) +
  labs(title = "Previsão com Holt-Winters com Sazonalidade",
       x = "Tempo", y = "Casos Estimados") +
  theme_minimal()
# =============================================================

modelo_hw_sazonal <- HoltWinters(serie_ts)

forecast::autoplot(previsao_sazonal) +
  labs(title = "Previsão com Holt-Winters com Sazonalidade",
       x = "Tempo", y = "Casos Estimados") +
  theme_minimal()

# Comentário:
# Este modelo considera tendência e sazonalidade (via componente gamma).
# Ideal quando há variações cíclicas previsíveis, como em doenças sensíveis ao clima.

# =============================================================
# 7. Avaliação dos modelos Holt-Winters
# =============================================================

# Gerando previsões a partir dos modelos
previsao_simples <- forecast(modelo_hw_simples, h = 12)
previsao_sazonal <- forecast(modelo_hw_sazonal, h = 12)

# Calculando métricas de erro (MAE, RMSE, MAPE) com base nos dados ajustados
library(Metrics)

# Erros do modelo simples
erros_simples <- modelo_hw_simples$x - modelo_hw_simples$fitted[,"xhat"]
mae_simples <- mae(modelo_hw_simples$x, modelo_hw_simples$fitted[,"xhat"])
rmse_simples <- rmse(modelo_hw_simples$x, modelo_hw_simples$fitted[,"xhat"])
mape_simples <- mape(modelo_hw_simples$x, modelo_hw_simples$fitted[,"xhat"])

# Erros do modelo sazonal
erros_sazonal <- modelo_hw_sazonal$x - modelo_hw_sazonal$fitted[,"xhat"]
mae_sazonal <- mae(modelo_hw_sazonal$x, modelo_hw_sazonal$fitted[,"xhat"])
rmse_sazonal <- rmse(modelo_hw_sazonal$x, modelo_hw_sazonal$fitted[,"xhat"])
mape_sazonal <- mape(modelo_hw_sazonal$x, modelo_hw_sazonal$fitted[,"xhat"])

# Exibindo os resultados
cat("
Métricas do modelo Holt-Winters simples:
")
cat(paste("MAE:", round(mae_simples, 2), "| RMSE:", round(rmse_simples, 2), "| MAPE:", round(mape_simples * 100, 2), "%
"))

cat("
Métricas do modelo Holt-Winters com sazonalidade:
")
cat(paste("MAE:", round(mae_sazonal, 2), "| RMSE:", round(rmse_sazonal, 2), "| MAPE:", round(mape_sazonal * 100, 2), "%
"))

# Comentário:
# - MAE: erro absoluto médio
# - RMSE: raiz do erro quadrático médio (penaliza mais grandes erros)
# - MAPE: erro percentual médio absoluto (em %)
# Esses indicadores ajudam a escolher o modelo mais adequado para previsão e vigilância.


# =============================================================
# 8. Detecção de surtos e anomalias
# =============================================================

# Para comparar os resíduos corretamente, precisamos garantir que o vetor de resíduos
# tenha o mesmo comprimento da série original. Vamos extrair apenas os resíduos ajustados:

residuos_ajustados <- modelo_hw_sazonal$x - modelo_hw_sazonal$fitted[,"xhat"]

# Como a série original é um ts com atributos, vamos remover os atributos para concatenar
residuos_completos <- rep(NA, n)
residuos_completos[(length(residuos_completos) - length(residuos_ajustados) + 1):length(residuos_completos)] <- residuos_ajustados

serie <- serie %>%
  mutate(
    residuos = residuos_completos,
    limite_sup = mean(residuos, na.rm = TRUE) + 2 * sd(residuos, na.rm = TRUE),
    limite_inf = mean(residuos, na.rm = TRUE) - 2 * sd(residuos, na.rm = TRUE),
    anomalia = if_else(residuos > limite_sup, "surto", "normal")
  )

serie %>%
  ggplot(aes(x = semana, y = casos)) +
  geom_line(color = "gray") +
  geom_point(aes(color = anomalia), size = 2) +
  scale_color_manual(values = c("surto" = "red", "normal" = "black")) +
  labs(title = "Detecção de Surtos com Base em Resíduos de Holt-Winters",
       color = "Classificação", y = "Casos", x = "Semana") +
  theme_minimal()

# =============================================================
# 8. Outros modelos de séries temporais (comentário)
# =============================================================

# Além de médias móveis e Holt-Winters, outros modelos importantes incluem:
# - ARIMA: modela componentes autorregressivos, de média móvel e diferenciação
# - SARIMA: inclui componentes sazonais no ARIMA
# - Prophet: modelo do Facebook que lida bem com sazonalidades e feriados
# - Modelos Bayesianos e redes neurais para séries temporais mais complexas

# Aula concluída!
