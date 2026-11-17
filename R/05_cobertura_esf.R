# ------------------------------------------------------------------------------
# 05_cobertura_esf.R
# Cobertura da Estratégia Saúde da Família (ESF) por município.
# Arquivo: dados_brutos/aps/cobertura_esf_harmonizada_municipio_ano_2007_atual.csv
#   (cobertura anual da ESF do e-Gestor Atenção Básica, por município)
# Exposição: cobertura média da ESF em 2017–2020, antes da mudança de método de 2021.
# Categorias: < 50%, 50–79,9% e ≥ 80% (referência).
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")

arquivo_esf <- file.path(pasta_brutos, "aps", "cobertura_esf_harmonizada_municipio_ano_2007_atual.csv")

esf_ano <- read.csv(arquivo_esf, encoding = "UTF-8") |>
  transmute(code_muni = substr(sprintf("%06d", as.integer(code)), 1, 6),
            ano = as.integer(ano),
            cobertura = as.numeric(cobertura_esf_harmonizada_pct)) |>
  filter(!is.na(code_muni), !is.na(ano), !is.na(cobertura)) |>
  mutate(cobertura = pmin(cobertura, 100)) |>
  group_by(code_muni, ano) |>
  summarise(cobertura = mean(cobertura), .groups = "drop")
saveRDS(esf_ano, file.path(pasta_proc, "aps_mun_ano.rds"))

esf <- esf_ano |>
  filter(ano %in% anos_esf) |>
  group_by(code_muni) |>
  summarise(aps = mean(cobertura), n_anos_aps = n())

esf$cat_aps <- cut(esf$aps, c(-Inf, 50, 80, Inf), right = FALSE,
                   labels = c("< 50%", "50–79,9%", "≥ 80%"))
esf$cat_aps <- relevel(esf$cat_aps, ref = "≥ 80%")
saveRDS(esf, file.path(pasta_proc, "aps_mun.rds"))

resumo <- esf |>
  summarise(n = n(),
            anos = paste(range(anos_esf), collapse = "–"),
            tipo = "ESF",
            mediana = median(aps),
            q1 = quantile(aps, 0.25),
            q3 = quantile(aps, 0.75),
            pct_lt50  = 100 * mean(cat_aps == "< 50%"),
            pct_50_79 = 100 * mean(cat_aps == "50–79,9%"),
            pct_ge80  = 100 * mean(cat_aps == "≥ 80%"))
salvar_csv(resumo, "aps_resumo.csv")
print(resumo)
