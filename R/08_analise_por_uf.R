# ------------------------------------------------------------------------------
# 08_analise_por_uf.R
# Comparação entre unidades da federação: RR da janela principal vs 2016–2019 e variação da
# cobertura vacinal, com correlação de Spearman (Figura 5A).
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")

painel <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cobertura_uf <- read.csv(file.path(pasta_saidas, "cobertura_uf_ponderada.csv"),
                         encoding = "UTF-8", colClasses = c(uf = "character"))

# 1. RR por UF (0,5 somado aos casos para evitar zero) ---------------------------
rr_uf <- recorte(painel, janela_principal) |>
  mutate(grupo = ifelse(faixa == "lt_6m", "lt_6m", "6m_4a"),
         periodo = ifelse(alvo == 1, "p24", "ref")) |>
  group_by(uf, grupo, periodo) |>
  summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
  pivot_wider(names_from = periodo, values_from = c(casos, pa)) |>
  mutate(taxa_ref = 1e5 * casos_ref / pa_ref,
         taxa_24  = 1e5 * casos_p24 / pa_p24,
         rr = ((casos_p24 + 0.5) / pa_p24) / ((casos_ref + 0.5) / pa_ref))

# 2. Queda de casos durante a pandemia (2020–2023 vs 2016–2019) -----------------
deficit <- painel |>
  filter(ano %in% c(anos_referencia, 2020:2023)) |>
  mutate(periodo = ifelse(ano >= 2020, "pan", "ref")) |>
  group_by(uf, periodo) |>
  summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
  pivot_wider(names_from = periodo, values_from = c(casos, pa)) |>
  mutate(deficit_pct = 100 * (1 - (casos_pan / pa_pan) / (casos_ref / pa_ref))) |>
  select(uf, deficit_pct)

# 3. Cobertura por UF ---------------------------------------------------------------
cobertura <- cobertura_uf |>
  group_by(uf) |>
  summarise(dtp_pre = mean(dtp[ano %in% 2010:2014]),
            dtp_pan = mean(dtp[ano %in% 2019:2023]),
            dtpa_2324 = mean(dtpa[ano %in% 2023:2024])) |>
  mutate(delta_dtp = dtp_pan - dtp_pre)

tabela_uf <- rr_uf |>
  left_join(cobertura, by = "uf") |>
  left_join(deficit, by = "uf") |>
  mutate(sigla = siglas_uf[uf], regiao = regiao_do_municipio(uf))
salvar_csv(tabela_uf, "uf_rr_cobertura.csv")

# 4. Correlações de Spearman ---------------------------------------------------------
correlacoes <- list()
for (g in c("6m_4a", "lt_6m")) {
  d <- filter(tabela_uf, grupo == g)
  t1 <- cor.test(log(d$rr), d$delta_dtp, method = "spearman", exact = FALSE)
  t2 <- cor.test(log(d$rr), d$dtpa_2324, method = "spearman", exact = FALSE)
  t3 <- cor.test(log(d$rr), d$deficit_pct, method = "spearman", exact = FALSE)
  correlacoes[[g]] <- tibble(grupo = g,
                             rho_delta = t1$estimate, p_delta = t1$p.value,
                             rho_dtpa = t2$estimate, p_dtpa = t2$p.value,
                             rho_deficit = t3$estimate, p_deficit = t3$p.value)
}
correlacoes <- bind_rows(correlacoes) |> arrange(grupo)
salvar_csv(correlacoes, "uf_correlacoes.csv")
print(correlacoes)
