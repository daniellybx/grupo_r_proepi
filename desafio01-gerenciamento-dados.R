# Desafio Aula 04 - Análise de casos de dengue com microdatasus

# Objetivo: Aplicar os conceitos aprendidos (importação, limpeza, manipulação e exportação)
# utilizando dados reais do SINAN-DENGUE para o ano de 2020.

# =============================================================
# 1. Carregar pacotes necessários
# =============================================================
pacman::p_load(
  microdatasus,  # Para baixar dados do SUS
  here,          # Caminhos relativos
  tidyverse,     # Manipulação e visualização
  lubridate,     # Datas e semana epidemiológica
  fs             # Criar diretórios
)

# =============================================================
# 2. Baixar os dados do SINAN-DENGUE (2020)
# =============================================================
dados_sinan <- fetch_datasus(
  year_start = 2020,
  year_end = 2020,
  information_system = "SINAN-DENGUE"
)

# =============================================================
# 3. DESAFIO — Complete os passos abaixo com base nas aulas 1 a 3
# =============================================================

# 1. Filtrar apenas os casos confirmados de dengue no município de Campinas
#    - Use a coluna ID_MN_RESI com o código "350950"
#    - Use a coluna CLASSI_FIN para filtrar os casos "5" (Dengue Clássico)

# 2. Selecionar as colunas: CS_SEXO, NU_IDADE_N, ID_MN_RESI, DT_NOTIFIC

# 3. Criar uma nova coluna chamada faixa_etaria com categorias:
#    "0-9", "10-19", ..., "60+" usando case_when()

# 4. Criar uma nova coluna chamada semana_epi com a semana epidemiológica de DT_NOTIFIC usando epiweek()

# 5. Exportar o resultado para a pasta "dados/limpos" como um .csv

# =============================================================
# DICAS:
# - Use funções: filter(), select(), mutate(), case_when(), epiweek(), write_csv()
# - Use glimpse(), View(), names() para explorar a base
# - ID_MN_RESI == "350950" é Campinas/SP

# Boa sorte!
