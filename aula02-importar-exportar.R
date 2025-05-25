# Aula 02 - Importar e Exportar Dados com R

# Objetivo: Ensinar como importar e exportar dados de forma prática, organizada e reproduzível, utilizando os pacotes 'rio' e 'here'.
# Esta aula é baseada nos capítulos 6 e 7 do Manual de R para Epidemiologistas.

# =============================================================
# 1. Carregando pacotes necessários
# =============================================================

# Carregamos os pacotes que serão utilizados:
# 'here' ajuda a definir caminhos relativos dentro do projeto R.
# 'rio' permite importar e exportar dados em diversos formatos.
pacman::p_load(here, rio, dplyr, readr, ggplot2)

# =============================================================
# 2. Estrutura de um Projeto R
# =============================================================

# Em projetos R, o diretório raiz deve conter subpastas organizadas, como:
# - "dados": para arquivos brutos
# - "scripts": para códigos em R
# - "outputs": para gráficos e arquivos exportados
# O pacote 'here' identifica automaticamente o diretório raiz do projeto.
here()  # Mostra o caminho raiz do projeto

# =============================================================
# 3. Listando arquivos de uma pasta
# =============================================================

# Podemos verificar os arquivos dentro da pasta "dados"
list.files(here("dados"))

# =============================================================
# 4. Importando dados com o pacote 'rio'
# =============================================================

# Importar um arquivo Excel que está na pasta "dados"
linelist_raw <- import(here("dados", "linelist_raw.xlsx"))

# Verificar as primeiras linhas do dataset
head(linelist_raw)

# Visualizar estrutura e tipos de dados
str(linelist_raw)
glimpse(linelist_raw)

# =============================================================
# 5. Importar planilhas específicas e manipular cabeçalhos
# =============================================================

# Caso queira pular linhas iniciais (ex: cabeçalhos duplicados)
linelist_raw_nomes <- import(here("dados", "linelist_raw.xlsx")) %>% names()
linelist_corrigido <- import(here("dados", "linelist_raw.xlsx"),
                             skip = 2,
                             col_names = linelist_raw_nomes)

# =============================================================
# 6. Designando valores ausentes (NA)
# =============================================================

# Converter valores como "Missing", "99", "" para NA
linelist_na <- import(here("dados", "linelist_raw.xlsx"),
                      na = c("Missing", "99", ""))

# =============================================================
# 7. Exportando dados com 'rio'
# =============================================================

# Criar pasta "limpos" se não existir
dir.create(here("dados", "limpos"), showWarnings = FALSE)

# Exportar para CSV
export(linelist_raw, here("dados", "limpos", "linelist_limpo.csv"))

# Exportar para Excel
export(linelist_raw, here("dados", "limpos", "linelist_limpo.xlsx"))

# Exportar para RDS
export(linelist_raw, here("dados", "limpos", "linelist_limpo.rds"))

# =============================================================
# 8. Exportando para a área de transferência (Clipboard)
# =============================================================

# Exportar para copiar e colar em planilhas externas
# pacman::p_load(clipr)
# clipr::write_clip(linelist_raw)

# =============================================================
# 9. Exportando gráficos com ggsave()
# =============================================================

# Criar gráfico de exemplo com variável de idade (se existir)
if("age" %in% names(linelist_raw)) {
  # Verifica se a variável 'age' é numérica, se não for, tenta converter
  if(!is.numeric(linelist_raw$age)) {
    linelist_raw <- linelist_raw %>% mutate(age = as.numeric(age))
  }
  
  # Remover valores NA para evitar erro no gráfico
  linelist_plot <- linelist_raw %>% filter(!is.na(age))
  
  # Criar gráfico
  p <- ggplot(linelist_plot, aes(x = age)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(title = "Distribuição de idade") +
    theme_minimal()
  
  # Criar pasta de saída se necessário
  dir.create(here("outputs", "epicurves"), recursive = TRUE, showWarnings = FALSE)
  
  # Salvar o gráfico
  ggsave(here("outputs", "epicurves", "epicurve_idade.png"), plot = p, width = 8, height = 5, dpi = 300)
}

# =============================================================
# 10. Importando o arquivo mais recente automaticamente
# =============================================================

# Listar arquivos na pasta "dados" com "linelist" no nome
arquivos <- list.files(here("dados"), pattern = "linelist", full.names = TRUE)
nomes <- basename(arquivos)
datas <- stringr::str_extract(nomes, "[0-9]{8}")
datas <- lubridate::ymd(datas)
mais_recente <- arquivos[which.max(datas)]
linelist_mais_recente <- import(mais_recente)

# =============================================================
# 11. Considerações finais
# =============================================================

# - Use 'here()' para garantir reprodutibilidade e evitar erros de caminho
# - Prefira import()/export() do pacote 'rio' por sua versatilidade
# - Organize seus dados e saídas em pastas lógicas dentro do projeto
# - Evite caminhos absolutos (como "C:/...") que quebram em outros computadores
# - Sempre valide os dados importados (head(), str(), glimpse())

# Fim da Aula 02
