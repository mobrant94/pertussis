# ------------------------------------------------------------------------------
# 00_en.R
# English settings for the figures of the English manuscript (Vaccine submission).
# Sourced after 00_configuracao.R by 15_figuras_artigo_en.R and 16_figuras_complementares_en.R.
# Figures are saved in figuras_en/ as PNG (600 dpi, for the Word file), TIFF (1000 dpi,
# line-art requirement of the journal) and PDF (vector, embedded fonts).
# ------------------------------------------------------------------------------
pasta_fig <- "figuras_en"
dir.create(pasta_fig, showWarnings = FALSE)

nomes_faixas <- c(lt_6m = "< 6 months", `6_11m` = "6–11 months", `1_4a` = "1–4 years")
REG_EN <- c(Sul = "South", Sudeste = "Southeast", `Centro-Oeste` = "Center-West", Nordeste = "Northeast",
            Norte = "North", Brasil = "Brazil")
reg_en <- function(x) unname(ifelse(x %in% names(REG_EN), REG_EN[x], x))

fmt <- function(x, d = 0) sub("-", "−", format(round(x, d), big.mark = ",", decimal.mark = ".", nsmall = d, trim = TRUE), fixed = TRUE)
int_br <- function(x) format(x, big.mark = ",", decimal.mark = ".", trim = TRUE)
dec_br <- function(x, d = 1) format(round(x, d), decimal.mark = ".", nsmall = d, trim = TRUE)

# Category names used in the data (Portuguese) and their English labels
CAT_EN <- c("T1 (menor)" = "T1 (lowest)", "T2" = "T2", "T3 (maior)" = "T3 (highest)",
            "Menor cobertura (T1)" = "Lowest coverage (T1)", "Intermediária (T2)" = "Intermediate (T2)",
            "Maior cobertura (T3)" = "Highest coverage (T3)",
            "Estável ou aumento" = "Stable or increase", "Queda < 10 p.p." = "Decline < 10 pp",
            "Queda ≥ 10 p.p." = "Decline ≥ 10 pp",
            "< 50%" = "< 50%", "50–79,9%" = "50–79.9%", "≥ 80%" = "≥ 80%")
cat_en <- function(x) unname(ifelse(x %in% names(CAT_EN), CAT_EN[x], x))

rotulo_categoria <- function(exp, sep = "\n", n_texto = "municipalities") {
  t <- read.csv(file.path(pasta_saidas, "tercis_resumo.csv"), encoding = "UTF-8")
  t <- t[t$exposicao == exp, ]
  n <- paste0(int_br(t$municipios), " ", n_texto)
  if (exp == "delta_dtp") return(setNames(paste0(cat_en(t$categoria), sep, n), t$categoria))
  c1 <- dec_br(t$corte_1[1]); c2 <- dec_br(t$corte_2[1])
  pos <- ifelse(grepl("T1", t$categoria), 1, ifelse(grepl("T2", t$categoria), 2, 3))
  faixa <- c(paste0("< ", c1, "%"), paste0(c1, "–", c2, "%"), paste0("≥ ", c2, "%"))[pos]
  setNames(paste0(cat_en(t$categoria), sep, faixa, "; ", n), t$categoria)
}
rotulo_esf <- function(sep = " ", n_texto = "municipalities") {
  a <- readRDS(file.path(pasta_proc, "aps_mun.rds"))
  n <- table(as.character(a$cat_aps))
  setNames(paste0(cat_en(names(n)), sep, "(", int_br(as.integer(n)), " ", n_texto, ")"), names(n))
}

salvar_figura <- function(grafico, nome, largura, altura, resolucao = 600, escala = 0.8) {
  ggsave(file.path(pasta_fig, paste0(nome, ".png")), grafico, device = ragg::agg_png,
         width = largura, height = altura, units = "in", dpi = resolucao, scale = escala, bg = "white")
  ggsave(file.path(pasta_fig, paste0(nome, ".tiff")), grafico, device = ragg::agg_tiff,
         width = largura, height = altura, units = "in", dpi = 1000, scale = escala, compression = "lzw", bg = "white")
  ggsave(file.path(pasta_fig, paste0(nome, ".pdf")), grafico, device = cairo_pdf,
         width = largura, height = altura, units = "in", scale = escala, bg = "white")
}
