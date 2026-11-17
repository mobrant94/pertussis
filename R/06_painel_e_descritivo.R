# ------------------------------------------------------------------------------
# 06_painel_e_descritivo.R
# Junta casos e população em um painel município x faixa etária x mês e calcula:
#   - fluxograma de seleção dos casos
#   - taxas anuais (total, por faixa etária e por região)
#   - proporção de confirmação laboratorial por ano
#   - taxas mensais nacionais por faixa etária
# Taxas por 100.000 pessoas-ano (pessoas-ano = soma da população mensal / 12).
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")

populacao <- readRDS(file.path(pasta_proc, "pop_mensal_nv.rds"))
casos     <- readRDS(file.path(pasta_proc, "casos_mun_mes.rds"))
sinan     <- readRDS(file.path(pasta_proc, "sinan_lt5.rds"))
fluxo     <- readRDS(file.path(pasta_proc, "fluxo_parcial.rds"))

# 1. Painel ---------------------------------------------------------------------
painel <- populacao |>
  left_join(casos, by = c("code_muni", "faixa", "data")) |>
  mutate(casos      = coalesce(casos, 0L),
         casos_lab  = coalesce(casos_lab, 0L),
         casos_nlab = coalesce(casos_nlab, 0L),
         notif      = coalesce(notif, 0L),
         obitos     = coalesce(obitos, 0L),
         ano = year(data),
         mes = month(data),
         regiao = regiao_do_municipio(code_muni),
         uf = substr(code_muni, 1, 2),
         faixa = factor(faixa, levels = faixas))
saveRDS(painel, file.path(pasta_proc, "painel_mensal.rds"))

# 2. Fluxograma -------------------------------------------------------------------
# Notificações de municípios sem população estimada (ignorados ou criados após 2005)
municipios <- unique(populacao$code_muni)
sinan_final <- sinan |> filter(code_muni %in% municipios)

confirmados_faixa <- sinan_final |>
  filter(CLASSI_FIN %in% "1") |>
  count(faixa) |>
  transmute(etapa = paste("Confirmados", faixa), n)

fluxo <- bind_rows(
  fluxo,
  tibble(etapa = c("Município de residência ignorado ou sem denominador",
                   "Notificações elegíveis", "Confirmados", "Descartados",
                   "Inconclusivos", "Sem classificação final"),
         n = c(nrow(sinan) - nrow(sinan_final),
               nrow(sinan_final),
               sum(sinan_final$CLASSI_FIN %in% "1"),
               sum(sinan_final$CLASSI_FIN %in% "2"),
               sum(sinan_final$CLASSI_FIN %in% "8"),
               sum(!sinan_final$CLASSI_FIN %in% c("1", "2", "8")))),
  confirmados_faixa)
salvar_csv(fluxo, "fluxograma.csv")

# conferência: os casos do painel são os mesmos do fluxograma
stopifnot(sum(painel$casos) == sum(sinan_final$CLASSI_FIN %in% "1"))

# 3. Taxas anuais -----------------------------------------------------------------
taxa_total <- painel |>
  group_by(ano) |>
  summarise(casos = sum(casos), casos_lab = sum(casos_lab), notif = sum(notif),
            obitos = sum(obitos), pa = sum(pop) / 12) |>
  mutate(taxa = 1e5 * casos / pa, taxa_lab = 1e5 * casos_lab / pa, taxa_notif = 1e5 * notif / pa)

taxa_faixa <- painel |>
  group_by(ano, faixa) |>
  summarise(casos = sum(casos), casos_lab = sum(casos_lab), notif = sum(notif),
            obitos = sum(obitos), pa = sum(pop) / 12, .groups = "drop") |>
  mutate(taxa = 1e5 * casos / pa, taxa_lab = 1e5 * casos_lab / pa, taxa_notif = 1e5 * notif / pa)

taxa_regiao <- painel |>
  group_by(ano, regiao) |>
  summarise(casos = sum(casos), casos_lab = sum(casos_lab), notif = sum(notif),
            obitos = sum(obitos), pa = sum(pop) / 12, .groups = "drop") |>
  mutate(taxa = 1e5 * casos / pa, taxa_lab = 1e5 * casos_lab / pa, taxa_notif = 1e5 * notif / pa)

salvar_csv(taxa_total, "taxa_anual_total.csv")
salvar_csv(taxa_faixa, "taxa_anual_faixa.csv")
salvar_csv(taxa_regiao, "taxa_anual_regiao.csv")

# 4. Confirmação por ano ------------------------------------------------------------
confirmacao <- sinan_final |>
  mutate(ano = year(dt_sin)) |>
  group_by(ano) |>
  summarise(notif = n(),
            conf = sum(CLASSI_FIN %in% "1"),
            lab = sum(CLASSI_FIN %in% "1" & CRITERIO %in% "1")) |>
  mutate(pct_conf = 100 * conf / notif, pct_lab = 100 * lab / conf)
salvar_csv(confirmacao, "confirmacao_por_ano.csv")

# 5. Taxa mensal nacional por faixa etária (com média móvel de 3 meses) ----------
mensal <- painel |>
  group_by(data, faixa) |>
  summarise(casos = sum(casos), pop = sum(pop), .groups = "drop") |>
  arrange(faixa, data) |>
  group_by(faixa) |>
  mutate(taxa = 1e5 * casos / pop,
         taxa_mm3 = 1e5 * (lag(casos) + casos + lead(casos)) / (lag(pop) + pop + lead(pop))) |>
  ungroup()
salvar_csv(mensal, "taxa_mensal_nacional_faixa.csv")

print(taxa_total |> select(ano, casos, taxa))
