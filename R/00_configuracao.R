# ------------------------------------------------------------------------------
# 00_configuracao.R
# Pacotes, pastas, anos do estudo e funções de uso geral.
# Todos os scripts começam com source("R/00_configuracao.R").
# O diretório de trabalho deve ser a pasta principal do projeto.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readxl)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

options(stringsAsFactors = FALSE, scipen = 999, timeout = 600)

# Pastas
pasta_brutos <- "dados_brutos"           # arquivos baixados manualmente (SINAN, PNI, Censo, ESF)
pasta_tabnet <- "dados/tabnet"           # consultas ao TabNet salvas
pasta_proc   <- "dados/processados"      # bases intermediárias (.rds)
pasta_saidas <- "saidas"                 # tabelas de resultados (.csv)
pasta_fig    <- "figuras"                # figuras (.png e .tiff)

dir.create(pasta_tabnet, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_proc, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_saidas, showWarnings = FALSE)
dir.create(pasta_fig, showWarnings = FALSE)

# Anos
anos_casos      <- 2010:2025   # casos de coqueluche (2025: dados preliminares)
anos_nascimento <- 2005:2025   # crianças de 1 a 4 anos em 2010 nasceram a partir de 2005
anos_referencia <- 2016:2019   # período de referência, antes da pandemia
anos_alvo       <- c(2024, 2025)

# Janelas comparadas com 2016–2019 (mês inicial e final).
# Principal: julho de 2024 a dezembro de 2025. A razão entre casos observados e
# esperados pelas taxas de 2016–2019 passou a superar 1 em julho de 2024; de janeiro
# a junho de 2024 os casos ficaram abaixo do esperado, e incluí-los diluiria a onda.
# As demais janelas são análises suplementares.
janelas <- tibble::tribble(
  ~nome,          ~inicio,      ~fim,         ~rotulo,
  "principal",    "2024-07-01", "2025-12-01", "jul/2024–dez/2025",
  "ano_2024",     "2024-01-01", "2024-12-01", "2024",
  "ano_2025",     "2025-01-01", "2025-12-01", "2025",
  "jul_dez_2024", "2024-07-01", "2024-12-01", "jul–dez/2024")
janela_principal <- "principal"

# Mantém 2016–2019 e os meses da janela; a janela vira um único período ("ano" = 9999),
# de modo que os modelos tratam a janela como uma unidade de tempo.
recorte <- function(painel, janela) {
  ini <- as.Date(janelas$inicio[janelas$nome == janela])
  fim <- as.Date(janelas$fim[janelas$nome == janela])
  painel |>
    dplyr::mutate(alvo = as.integer(data >= ini & data <= fim)) |>
    dplyr::filter(ano %in% anos_referencia | alvo == 1) |>
    dplyr::mutate(ano = ifelse(alvo == 1, 9999, ano))
}
anos_esf        <- 2017:2020   # cobertura média da Estratégia Saúde da Família

faixas       <- c("lt_6m", "6_11m", "1_4a")
nomes_faixas <- c(lt_6m = "< 6 meses", `6_11m` = "6–11 meses", `1_4a` = "1–4 anos")

siglas_uf <- c(`11` = "RO", `12` = "AC", `13` = "AM", `14` = "RR", `15` = "PA", `16` = "AP",
               `17` = "TO", `21` = "MA", `22` = "PI", `23` = "CE", `24` = "RN", `25` = "PB",
               `26` = "PE", `27` = "AL", `28` = "SE", `29` = "BA", `31` = "MG", `32` = "ES",
               `33` = "RJ", `35` = "SP", `41` = "PR", `42` = "SC", `43` = "RS", `50` = "MS",
               `51` = "MT", `52` = "GO", `53` = "DF")

# Região a partir do código do município (o primeiro dígito indica a região)
regiao_do_municipio <- function(codigo) {
  primeiro <- substr(codigo, 1, 1)
  case_when(primeiro == "1" ~ "Norte",
            primeiro == "2" ~ "Nordeste",
            primeiro == "3" ~ "Sudeste",
            primeiro == "4" ~ "Sul",
            primeiro == "5" ~ "Centro-Oeste")
}

# Salvar tabela de resultados
salvar_csv <- function(tabela, nome) {
  write.csv(tabela, file.path(pasta_saidas, nome), row.names = FALSE, fileEncoding = "UTF-8")
}

# Ler tabela de resultados
ler_csv <- function(nome) {
  read.csv(file.path(pasta_saidas, nome), encoding = "UTF-8")
}

# Tema e gravação das figuras
tema <- theme_classic(base_size = 10) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 10))

salvar_figura <- function(grafico, nome, largura, altura) {
  ggsave(file.path(pasta_fig, paste0(nome, ".png")), grafico,
         width = largura, height = altura, dpi = 400, bg = "white")
  ggsave(file.path(pasta_fig, paste0(nome, ".tiff")), grafico,
         width = largura, height = altura, dpi = 400, compression = "lzw", bg = "white")
}
