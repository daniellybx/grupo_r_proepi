# Aula 11 - Modelagem de Epidemias

# Objetivo: Nesta aula, vamos aprender a modelar epidemias usando ferramentas para:
# - Estimar o número efetivo de reprodução (Rt)
# - Realizar projeções de incidência de casos
# - Interpretar os resultados e aplicar essas informações para detectar mudanças na dinâmica da transmissão
# São recomendados os pacotes: EpiEstim, EpiNow2 e projections.
# Também simularemos uma base de dados "linelist" com datas de infecção, início dos sintomas e informações de transmissão.

# =============================================================
# Diferença entre Modelagem de Epidemias e Séries Temporais
# =============================================================

# Na aula anterior, abordamos séries temporais com foco em técnicas descritivas, como médias móveis e suavizações de tendências.
# Essas técnicas são úteis para descrever padrões de incidência ao longo do tempo, mas não capturam diretamente a dinâmica da transmissão.

# A modelagem de epidemias vai além: utiliza conhecimentos teóricos sobre a transmissão da doença para inferir parâmetros como o Rt (número de reprodução),
# tempo de duplicação e potencial de crescimento. Esses modelos permitem projeções com base em pressupostos epidemiológicos e ajudam a prever
# o impacto de intervenções e mudanças de comportamento.

# Em resumo:
# - Séries temporais: descritivas, focadas em tendência, sazonalidade e anomalias
# - Modelagem de epidemias: inferenciais, focadas em Rt, transmissibilidade, previsão baseada em mecanismos epidemiológicos

# =============================================================
# O que é o Rt (número efetivo de reprodução)?
# =============================================================

# O Rt representa o número médio de novas infecções causadas por uma pessoa infectada
# em um determinado momento (t) da epidemia.

# Por exemplo:
# - Se Rt = 2, significa que cada pessoa infectada transmite a doença, em média, para duas outras pessoas.
# - Se Rt = 1, a epidemia está estável (cada caso gera apenas um novo caso).
# - Se Rt < 1, a epidemia tende a diminuir, pois os casos não estão substituindo a si mesmos.

# O Rt muda ao longo do tempo, pois depende de fatores como:
# - Intervenções (ex.: quarentenas, uso de máscara)
# - Imunidade da população (natural ou vacinal)
# - Comportamento da população

# Estimar o Rt é essencial para entender a velocidade da transmissão e avaliar se as medidas
# de controle estão funcionando.

# =============================================================
# 1. Carregar pacotes
# =============================================================

library(tidyverse)      # Manipulação de dados e gráficos
library(lubridate)      # Manipulação de datas
library(zoo)            # Cálculo de médias móveis
library(EpiEstim)       # Estimativa formal de Rt
library(projections)    # Projeções com Rt e intervalos seriais
library(epitrix)        # Utilitário para distribuição de Rt
library(incidence)      # Criação de objetos de incidência

# =============================================================
# 2. Simular dados de uma epidemia
# =============================================================

set.seed(123)  # Garante reprodutibilidade da simulação
total_casos <- 500  # Número de casos simulados

# Simulando datas de infecção entre 01/01/2023 e 30/04/2023
data_infeccao <- sample(seq(as.Date("2023-01-01"), as.Date("2023-04-30"), by = "day"), total_casos, replace = TRUE)

# Simulando período de incubação com distribuição log-normal
periodo_incubacao <- round(rlnorm(total_casos, log(9.1), log(7.3)))

# Data de início dos sintomas = infecção + incubação
data_sintomas <- data_infeccao + periodo_incubacao

# Data de notificação com atraso entre 1 e 3 dias após sintomas
data_notificacao <- data_sintomas + sample(1:3, total_casos, replace = TRUE)

# Gerando identificadores para os casos e transmissores
id_casos <- paste0("c", 1:total_casos)
id_transmissor <- sample(c(NA, id_casos), total_casos, replace = TRUE)

# Criando base linelist simulada com dados por caso
linelist <- tibble(
  id_caso = id_casos,
  transmissor = id_transmissor,
  data_infeccao = data_infeccao,
  data_sintomas = data_sintomas,
  data_notificacao = data_notificacao
)

# =============================================================
# 3. Incidência diária e estimativa simplificada de Rt
# =============================================================

# Criando tabela de casos por dia de início de sintomas
tabela_casos <- linelist %>%
  count(data_sintomas, name = "casos") %>%           # Conta casos por dia
  arrange(data_sintomas) %>%                         # Organiza por data
  mutate(
    media_movel = zoo::rollmean(casos, k = 7, fill = NA, align = "right"),  # Média móvel de 7 dias
    casos_passado = lag(casos, 7),                                          # Casos 7 dias antes
    rt_estimado = casos / casos_passado                                     # Estimativa simplificada de Rt
  )

# Visualização da estimativa simplificada de Rt
tabela_casos %>%
  ggplot(aes(x = data_sintomas, y = rt_estimado)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Estimativa simplificada de Rt", x = "Data", y = "Rt") +
  theme_minimal()

# =============================================================
# 4. Estimativa formal de Rt com EpiEstim
# =============================================================

# Criando base de incidência para o pacote EpiEstim com preenchimento de datas ausentes
inc <- linelist %>%
  count(data_sintomas, name = "I") %>%               # 'I' é o nome padrão exigido pelo pacote
  rename(dates = data_sintomas) %>%                  # Renomeia coluna para 'dates'
  complete(dates = seq(min(dates), max(dates), by = "day"), fill = list(I = 0))  # Preenche dias sem casos

# Definindo parâmetros para intervalo serial (ex: SARS-CoV-2)
config <- make_config(list(mean_si = 12, std_si = 5.2))

# Estimando Rt com o método paramétrico
res_epiestim <- estimate_R(incid = inc, method = "parametric_si", config = config)

# Plotando estimativas de Rt
plot(res_epiestim)

# =============================================================
# 5. Projeções com o pacote projections
# =============================================================

# Extraindo valores finais de média e desvio do Rt estimado
media_r <- tail(res_epiestim$R$`Mean(R)`, 1)
desvio_r <- tail(res_epiestim$R$`Std(R)`, 1)

# Convertendo média e CV para shape e scale de distribuição gamma
parametros <- gamma_mucv2shapescale(mu = media_r, cv = desvio_r / media_r)

# Simulando valores plausíveis de Rt
rt_simulado <- rgamma(1000, shape = parametros$shape, scale = parametros$scale)

# Criando distribuição discreta do intervalo serial
dist_si <- distcrete("gamma", interval = 1, shape = 3, scale = 4, w = 0)

# Criando objeto de incidência com a série histórica
inc_obj <- incidence(linelist$data_sintomas)

# Realizando projeção de casos para os próximos 14 dias
max_intervalo <- 20  # define até quantos dias considerar para o intervalo serial
vetor_si <- dist_si$d(1:max_intervalo)  # gera vetor de probabilidades

# Realizando projeção de casos para os próximos 14 dias
proj <- project(
  x = inc_obj,
  R = rt_simulado,
  si = vetor_si,
  n_days = 14,
  n_sim = 1000
)


# Plotando apenas os dados recentes + projeção
grafico <- plot(inc_obj[inc_obj$dates > as.Date("2023-03-01")])
add_projections(grafico, proj)

# =============================================================
# 6. Conclusão
# =============================================================

# Nesta aula, utilizamos três componentes da modelagem de epidemias:
# - Estimativa simplificada de Rt com dados brutos
# - Estimativa formal de Rt com o pacote EpiEstim
# - Projeção de novos casos com o pacote projections
# Isso permite apoiar decisões durante epidemias, prever cenários futuros e avaliar políticas públicas.
