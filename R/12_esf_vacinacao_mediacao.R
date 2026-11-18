# ------------------------------------------------------------------------------
# 12_esf_vacinacao_mediacao.R
#   A) Cobertura da ESF e coberturas vacinais: regressão linear ponderada pelos
#      nascidos vivos de 2019–2023, ajustada por região (por 10 p.p. de ESF)
#   B) Coberturas vacinais médias por categoria de ESF, por região e no Brasil
#   C) Mediação (produto dos coeficientes), crianças de 6 meses a 4 anos, janela principal:
#        a = efeito da ESF sobre a proporção de não vacinados
#        b = efeito dos não vacinados sobre o aumento de casos
#        indireto = a × b; direto = efeito da ESF ajustado pelos não vacinados
#      Efeitos expressos para +50 p.p. de cobertura da ESF.
#      IC95% por bootstrap, reamostrando municípios (200 repetições).
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(fixest)
set.seed(20241)
n_bootstrap <- 200

painel    <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cobertura <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))
esf       <- readRDS(file.path(pasta_proc, "aps_mun.rds"))

municipios <- cobertura |>
  select(code_muni, nv_1923, nao_vac, delta_dtp, dtpa_2324, dtp_pre, dtp_pan, dtp_2024) |>
  inner_join(esf |> select(code_muni, aps, cat_aps), by = "code_muni") |>
  mutate(regiao = regiao_do_municipio(code_muni),
         aps10 = aps / 10,
         w = nv_1923) |>
  filter(w > 0)

# A) ESF e coberturas vacinais ------------------------------------------------------
desfechos <- c("dtp_pre", "dtp_pan", "dtp_2024", "nao_vac", "delta_dtp", "dtpa_2324")
esf_vacinas <- list()
for (y in desfechos) {
  modelo <- lm(as.formula(paste(y, "~ aps10 + regiao")), data = municipios, weights = w)
  ic <- confint(modelo)["aps10", ]
  esf_vacinas[[y]] <- tibble(desfecho = y, ajuste = "regiao",
                             b = coef(modelo)[["aps10"]], lo = ic[[1]], hi = ic[[2]],
                             p = summary(modelo)$coefficients["aps10", 4],
                             n = nobs(modelo))
}
salvar_csv(bind_rows(esf_vacinas), "esf_cobertura_vacinal.csv")

# B) Coberturas por categoria de ESF -----------------------------------------------------
por_regiao <- municipios |>
  group_by(regiao, cat_aps) |>
  summarise(municipios = n(),
            dtp_pre = weighted.mean(pmin(dtp_pre, 100), w, na.rm = TRUE),
            dtp_pan = weighted.mean(pmin(dtp_pan, 100), w, na.rm = TRUE),
            nao_vac = weighted.mean(nao_vac, w, na.rm = TRUE),
            dtpa    = weighted.mean(pmin(dtpa_2324, 100), w, na.rm = TRUE), .groups = "drop")
brasil <- municipios |>
  group_by(cat_aps) |>
  summarise(municipios = n(),
            dtp_pre = weighted.mean(pmin(dtp_pre, 100), w, na.rm = TRUE),
            dtp_pan = weighted.mean(pmin(dtp_pan, 100), w, na.rm = TRUE),
            nao_vac = weighted.mean(nao_vac, w, na.rm = TRUE),
            dtpa    = weighted.mean(pmin(dtpa_2324, 100), w, na.rm = TRUE)) |>
  mutate(regiao = "Brasil")
salvar_csv(bind_rows(por_regiao, brasil), "esf_cobertura_por_categoria.csv")

# C) Mediação ------------------------------------------------------------------------------
painel_2024 <- recorte(painel, janela_principal) |>
  filter(faixa != "lt_6m") |>
  group_by(code_muni, regiao, ano) |>
  summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
  filter(pa > 0) |>
  inner_join(municipios |> select(code_muni, nao_vac, aps), by = "code_muni") |>
  filter(!is.na(nao_vac)) |>
  mutate(alvo = as.integer(ano == 9999),
         cont = nao_vac / 10 * alvo,      # não vacinados, por 10 p.p., no ano-alvo
         e10  = aps / 10 * alvo)          # ESF, por 10 p.p., no ano-alvo

estimar_mediacao <- function(dados_mun, dados_painel) {
  a <- coef(lm(nao_vac ~ aps10 + regiao, data = dados_mun, weights = w))[["aps10"]]
  m_total <- fepois(casos ~ e10 | code_muni + regiao^ano, offset = ~ log(pa), data = dados_painel)
  m_ajust <- fepois(casos ~ e10 + cont | code_muni + regiao^ano, offset = ~ log(pa), data = dados_painel)
  b <- coef(m_ajust)[["cont"]]
  tibble(a = a, b = b,
         total    = 5 * coef(m_total)[["e10"]],
         direto   = 5 * coef(m_ajust)[["e10"]],
         indireto = 5 * a * b / 10)
}

estimativa <- estimar_mediacao(municipios, painel_2024)

# Bootstrap: sorteia municípios com reposição; cada município sorteado recebe um
# identificador próprio, para que repetições sejam tratadas como municípios distintos.
lista_munis <- unique(painel_2024$code_muni)
boot <- list()
for (k in 1:n_bootstrap) {
  sorteio <- tibble(code_muni = sample(lista_munis, replace = TRUE)) |>
    mutate(id = paste0(code_muni, "_", row_number()))
  mun_k <- sorteio |> inner_join(municipios, by = "code_muni", relationship = "many-to-many")
  painel_k <- sorteio |> inner_join(painel_2024, by = "code_muni", relationship = "many-to-many") |>
    mutate(code_muni = id)
  resultado <- try(suppressMessages(estimar_mediacao(mun_k, painel_k)), silent = TRUE)
  if (!inherits(resultado, "try-error")) boot[[k]] <- resultado
}
boot <- bind_rows(boot)

mediacao <- tibble(efeito = c("total", "direto", "indireto"),
                   est = c(estimativa$total, estimativa$direto, estimativa$indireto),
                   lo  = c(quantile(boot$total, 0.025), quantile(boot$direto, 0.025), quantile(boot$indireto, 0.025)),
                   hi  = c(quantile(boot$total, 0.975), quantile(boot$direto, 0.975), quantile(boot$indireto, 0.975))) |>
  mutate(irr = exp(est), irr_lo = exp(lo), irr_hi = exp(hi),
         a = estimativa$a, b_irr10 = exp(estimativa$b), n_boot = nrow(boot))
salvar_csv(mediacao, "mediacao_esf.csv")
print(mediacao)
