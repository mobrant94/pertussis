# ------------------------------------------------------------------------------
# rodar_tudo.R
# Reproduz o estudo completo: baixa e organiza os dados, roda as análises,
# gera as figuras e o manuscrito (artigo.docx).
#
# Antes de rodar:
#   1. Coloque os arquivos descritos em dados_brutos/LEIA-ME.md na pasta dados_brutos/
#   2. Ajuste a pasta do projeto abaixo
# ------------------------------------------------------------------------------

# pasta do projeto (altere se estiver em outro computador)
setwd("C:/Users/MARCOS.ANTUNES/OneDrive - PUCRS - BR/Documentações - Marcos/Área de Trabalho/Meus artigos/Coqueluche/coqueluche-brasil-2024")

scripts <- c(
  "R/01_nascidos_e_obitos.R",          # TabNet: nascidos vivos e óbitos (demorado)
  "R/02_casos_sinan.R",                # SINAN: casos de coqueluche
  "R/03_populacao_em_risco.R",         # população mensal por município e faixa etária
  "R/04_coberturas_vacinais.R",        # DTP e dTpa por município
  "R/05_cobertura_esf.R",              # cobertura da Estratégia Saúde da Família
  "R/06_painel_e_descritivo.R",        # painel, fluxograma e taxas
  "R/07_comparacao_com_referencia.R",  # 2024 e 2025 vs 2016–2019
  "R/08_analise_por_uf.R",             # comparação entre estados
  "R/09_modelos_vacinacao.R",          # vacinação e aumento de casos
  "R/10_modelos_esf.R",                # ESF como modificadora de efeito
  "R/11_laboratorio_deteccao_onda.R",  # casos laboratoriais, detecção, momento da onda
  "R/12_esf_vacinacao_mediacao.R",     # ESF, coberturas vacinais e mediação
  "R/13_casos_evitaveis.R",            # casos evitáveis com maior vacinação
  "R/14_subdeteccao.R",                # sensibilidade à menor detecção no Norte e Nordeste
  "R/15_figuras_artigo.R",             # figuras do artigo
  "R/16_figuras_complementares.R")     # figuras complementares

for (script in scripts) {
  message("\n===== ", script, " =====")
  source(script, encoding = "UTF-8", local = new.env())
}

rmarkdown::render("artigo.Rmd", output_file = "artigo.docx", encoding = "UTF-8")

# Remove os indicadores (marcadores) que o pandoc cria em cada título do Word
tmp <- tempfile(); dir.create(tmp)
unzip("artigo.docx", exdir = tmp)
doc <- file.path(tmp, "word", "document.xml")
xml <- readLines(doc, encoding = "UTF-8", warn = FALSE)
xml <- gsub("<w:bookmarkStart [^>]*/>|<w:bookmarkEnd [^>]*/>", "", xml)
writeLines(xml, doc, useBytes = TRUE)
arquivos <- list.files(tmp, recursive = TRUE, all.files = TRUE)
old_wd <- setwd(tmp); zip::zip(file.path(old_wd, "artigo.docx"), arquivos); setwd(old_wd)
message("\nManuscrito gerado: artigo.docx")
