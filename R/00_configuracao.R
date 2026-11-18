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

# Fonte das figuras: Times New Roman ----------------------------------------------
# Usa a fonte instalada no sistema; se não houver, procura os arquivos times*.ttf na
# pasta fontes/ (copiados de C:/Windows/Fonts) e, em último caso, usa a Liberation
# Serif, que tem as mesmas métricas da Times New Roman.
FONTE <- "Times New Roman"
local({
  if (!FONTE %in% systemfonts::system_fonts()$family) {
    arq <- list.files("fontes", pattern = "(?i)^times.*\\.ttf$", full.names = TRUE)
    achar <- function(padrao, alternativa) {
      f <- arq[grepl(padrao, basename(arq), ignore.case = TRUE)]
      if (length(f)) f[1] else alternativa
    }
    lib <- function(negrito, italico) systemfonts::match_font("Liberation Serif", bold = negrito, italic = italico)$path
    if (length(arq)) {
      systemfonts::register_font(FONTE, plain = achar("^times\\.ttf$", arq[1]), bold = achar("^timesbd", arq[1]),
                                 italic = achar("^timesi\\.ttf$", arq[1]), bolditalic = achar("^timesbi", arq[1]))
    } else {
      systemfonts::register_font(FONTE, plain = lib(FALSE, FALSE), bold = lib(TRUE, FALSE),
                                 italic = lib(FALSE, TRUE), bolditalic = lib(TRUE, TRUE))
      message("Times New Roman não encontrada: figuras com Liberation Serif (mesmas métricas).")
    }
  }
})

# Tema e gravação das figuras
tema <- theme_classic(base_size = 10, base_family = FONTE) +
  theme(text = element_text(colour = "black"),
        axis.text = element_text(colour = "black", size = 9),
        axis.title = element_text(size = 10),
        axis.line = element_line(colour = "grey25", linewidth = .35),
        axis.ticks = element_line(colour = "grey25", linewidth = .35),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10, hjust = 0),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9.5),
        legend.key.height = unit(4, "mm"),
        panel.spacing = unit(4, "mm"),
        plot.title = element_text(face = "bold", size = 10.5, hjust = 0),
        plot.title.position = "plot",
        plot.tag = element_text(face = "bold", size = 11),
        plot.margin = margin(6, 8, 6, 6))
ggplot2::update_geom_defaults("text", list(family = FONTE))
ggplot2::update_geom_defaults("label", list(family = FONTE))

salvar_figura <- function(grafico, nome, largura, altura, resolucao = 600, escala = 0.8) {
  # escala < 1 reduz a área útil mantendo o tamanho da fonte: o texto fica legível quando a
  # figura é reduzida à largura da página (cerca de 16 cm)
  ggsave(file.path(pasta_fig, paste0(nome, ".png")), grafico, device = ragg::agg_png,
         width = largura, height = altura, units = "in", dpi = resolucao, scale = escala, bg = "white")
  ggsave(file.path(pasta_fig, paste0(nome, ".tiff")), grafico, device = ragg::agg_tiff,
         width = largura, height = altura, units = "in", dpi = resolucao, scale = escala, compression = "lzw", bg = "white")
}

# Rótulos das categorias de exposição com pontos de corte e número de municípios -----
# (tercis ponderados pelos nascidos vivos de 2019–2023; tabela gerada pelo script 04)
int_br <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
dec_br <- function(x, d = 1) format(round(x, d), decimal.mark = ",", nsmall = d, trim = TRUE)
rotulo_categoria <- function(exp, sep = "\n", n_texto = "municípios") {
  t <- read.csv(file.path(pasta_saidas, "tercis_resumo.csv"), encoding = "UTF-8")
  t <- t[t$exposicao == exp, ]
  n <- paste0(int_br(t$municipios), " ", n_texto)
  if (exp == "delta_dtp") return(setNames(paste0(t$categoria, sep, n), t$categoria))
  c1 <- dec_br(t$corte_1[1]); c2 <- dec_br(t$corte_2[1])
  pos <- ifelse(grepl("T1", t$categoria), 1, ifelse(grepl("T2", t$categoria), 2, 3))
  faixa <- c(paste0("< ", c1, "%"), paste0(c1, "–", c2, "%"), paste0("≥ ", c2, "%"))[pos]
  setNames(paste0(t$categoria, sep, faixa, "; ", n), t$categoria)
}
rotulo_esf <- function(sep = " ", n_texto = "municípios") {
  a <- readRDS(file.path(pasta_proc, "aps_mun.rds"))
  n <- table(as.character(a$cat_aps))
  setNames(paste0(names(n), sep, "(", int_br(as.integer(n)), " ", n_texto, ")"), names(n))
}
