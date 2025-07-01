# Aula 14 - Introdução ao GIS em Epidemiologia

# Objetivo: Demonstrar como utilizar dados espaciais em epidemiologia para identificar e visualizar padrões geográficos de doenças.
# Nesta aula, exploraremos um recorte dos casos de dengue de 2020 (SINAN via microdatasus)
# e faremos mapas com localização por município, usando o pacote geobr para manipulação espacial.

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

# Instale o devtools se ainda não estiver instalado
# install.packages("devtools")

# Carregue o devtools
# library(devtools)

# Instale a versão mais recente do microdatasus a partir do GitHub
# devtools::install_github("rfsaldanha/microdatasus")

# Instale o geobr se necessário
# install.packages("geobr")

# Instale o sf se necessário
# install.packages("sf")

# Carregando bibliotecas
library(microdatasus)   # Acesso a dados do DATASUS
library(tidyverse)      # Manipulação e visualização de dados
library(lubridate)      # Manipulação de datas
library(geobr)          # Dados geográficos do Brasil
library(sf)             # Manipulação de dados espaciais (shapefiles, geometria)

# =============================================================
# 2. Cenário epidemiológico simulado
# =============================================================

# A dengue é uma arbovirose endêmica em várias regiões do Brasil. Em 2020, diversas regiões do país registraram aumento de casos.
# O objetivo desta análise é identificar a distribuição espacial dos casos notificados no estado da Bahia.
# A partir de dados do SINAN, analisaremos a frequência por município e aplicaremos diferentes representações espaciais.

# =============================================================
# 3. Baixando dados de dengue - SINAN 2020 (amostra completa)
# =============================================================

# O SINAN-DENGUE não permite baixar apenas um estado. Baixaremos o banco completo e filtraremos depois.
dados_dengue <- fetch_datasus(
  year_start = 2020,
  year_end = 2020,
  information_system = "SINAN-DENGUE"
)

# Visualizando a estrutura dos dados
glimpse(dados_dengue)

# =============================================================
# 4. Limpando e resumindo os dados por município
# =============================================================

# Mantendo apenas colunas de interesse (município de residência)
dengue_limpo <- dados_dengue %>%
  select(ID_MN_RESI, DT_NOTIFIC) %>%
  filter(!is.na(ID_MN_RESI)) %>%
  mutate(
    municipio = as.numeric(substr(ID_MN_RESI, 1, 6)),
    data = DT_NOTIFIC
  )

# Resumindo número de casos por município
dengue_mun <- dengue_limpo %>%
  group_by(municipio) %>%
  summarise(casos = n()) %>%
  ungroup()

# =============================================================
# 5. Carregando mapa dos municípios da Bahia
# =============================================================

# Carregando geometria de municípios da Bahia
mapa_ba <- read_municipality(code_muni = "BA", year = 2020)

# Conferindo estrutura do shapefile
head(mapa_ba)

# =============================================================
# 6. Juntando dados de dengue com o mapa
# =============================================================

# Convertendo code_muni para numérico com 6 dígitos
mapa_ba <- mapa_ba %>%
  mutate(code_muni = as.numeric(substr(as.character(code_muni), 1, 6)))

# Juntando com base de casos
mapa_dengue <- mapa_ba %>%
  left_join(dengue_mun, by = c("code_muni" = "municipio")) %>%
  mutate(casos = replace_na(casos, 0))  # Substitui NA por 0

# =============================================================
# 7. Gráfico de pontos (sem tamanho proporcional)
# =============================================================

ggplot(mapa_dengue) +
  geom_sf() +
  geom_sf(data = mapa_dengue %>% filter(casos > 0),
          color = "red", size = 0.5) +
  labs(title = "Casos de Dengue - Bahia (2020)",
       subtitle = "Presença de casos por município") +
  theme_minimal()

# =============================================================
# 8. Gráfico de pontos com tamanho proporcional ao número de casos
# =============================================================

# Convertendo centroide de cada município
centroides <- st_centroid(mapa_dengue)

# Plotando pontos proporcionais
# =============================================================
# 7. Gráfico de pontos (sem tamanho proporcional)
# =============================================================

# Gerar os centroides dos municípios
centroides <- st_centroid(mapa_dengue)

# Plotar o mapa com os polígonos + pontos onde há casos
ggplot(mapa_dengue) +
  geom_sf(fill = "gray90") +  # Mapa base
  geom_sf(data = centroides %>% filter(casos > 0), 
          color = "red", size = 1) +  # Pontos vermelhos
  labs(title = "Casos de Dengue - Bahia (2020)",
       subtitle = "Presença de casos por município") +
  theme_minimal()

# =============================================================
# 9. Mapa temático (choropleth map)
# =============================================================

ggplot(mapa_dengue) +
  geom_sf(aes(fill = casos)) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(title = "Mapa Temático de Casos de Dengue - Bahia (2020)",
       fill = "Nº de casos") +
  theme_minimal()

# =============================================================
# 10. Conclusão
# =============================================================

# Nesta aula aprendemos:
# - Como importar dados do SINAN com o pacote microdatasus
# - Como utilizar o pacote geobr para mapas de municípios
# - Como juntar bases com geometrias e fazer mapas
# - Criamos gráficos simples de localização, pontos proporcionais e mapa temático

# Esses métodos são extremamente úteis em vigilância epidemiológica territorial.
# A próxima aula pode explorar como criar mapas interativos e dashboards.