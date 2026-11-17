# ------------------------------------------------------------------------------
# 14_subdeteccao.R
# Análise de sensibilidade para a menor detecção no Norte e no Nordeste.
# Em 2024, a notificação de suspeitos caiu no Norte e no Nordeste e subiu no Sul e
# no Sudeste. Os casos do Norte e do Nordeste no ano-alvo são multiplicados por um
# fator de correção k, e a magnitude e os modelos são recalculados.
#
# Cenários (k aplicado só ao Norte e ao Nordeste, só no ano-alvo):
#   sem correção (k = 1); fatores fixos 1,5, 2 e 3;
#   "Investigação no nível de 2016–2019": k = notificação 2016–2019 / notificação no ano-alvo
#   "Mesma variação do Sul e do Sudeste": k = variação da notificação no Sul+Sudeste /
#      variação na região (limite superior)
# Nos modelos, multiplicar os casos por k equivale a usar offset log(pa) − log(k).
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(fixest)

painel    <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cobertura <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))
deteccao  <- ler_csv("deteccao_por_regiao.csv")
regioes_corrigidas <- c("Norte", "Nordeste")

# 1. Fatores de correção --------------------------------------------------------------
# variação da taxa de notificação em relação a 2016–2019
variacao <- deteccao |>
  group_by(regiao) |>
  mutate(var = tx_notif / tx_notif[periodo == "2016–2019"]) |>
  ungroup() |>
  filter(periodo != "2016–2019") |>
  select(regiao, periodo, var)

variacao_sul_sudeste <- deteccao |>
  filter(regiao %in% c("Sul", "Sudeste")) |>
  group_by(periodo) |>
  summarise(tx = 1e5 * sum(notif) / sum(pa)) |>
  mutate(var_sse = tx / tx[periodo == "2016–2019"]) |>
  filter(periodo != "2016–2019") |>
  select(periodo, var_sse)

fatores_empiricos <- variacao |>
  filter(regiao %in% regioes_corrigidas) |>
  left_join(variacao_sul_sudeste, by = "periodo") |>
  transmute(regiao, ano = as.integer(periodo),
            `Investigação no nível de 2016–2019` = pmax(1, 1 / var),
            `Mesma variação do Sul e do Sudeste` = pmax(1, var_sse / var)) |>
  pivot_longer(-c(regiao, ano), names_to = "cenario", values_to = "k")

fatores_fixos <- expand_grid(regiao = regioes_corrigidas, ano = anos_alvo, k = c(1, 1.5, 2, 3)) |>
  mutate(cenario = ifelse(k == 1, "Sem correção",
                          paste0("Fator fixo ", format(k, decimal.mark = ","), "×")))

fatores <- bind_rows(fatores_fixos, fatores_empiricos) |> filter(ano %in% anos_alvo)
salvar_csv(fatores, "subdeteccao_fatores.csv")
cenarios <- unique(fatores$cenario)

# 2. Observados / esperados com os casos corrigidos ------------------------------------
taxa_referencia <- painel |>
  filter(ano %in% anos_referencia) |>
  group_by(faixa, regiao, mes) |>
  summarise(tx = sum(casos) / sum(pop), .groups = "drop")

obs_esp <- painel |>
  filter(ano %in% anos_alvo) |>
  left_join(taxa_referencia, by = c("faixa", "regiao", "mes")) |>
  group_by(ano, regiao) |>
  summarise(obs = sum(casos), esp = sum(tx * pop, na.rm = TRUE), .groups = "drop")

magnitude <- list()
for (cen in cenarios) {
  for (a in anos_alvo) {
    k_cen <- fatores |> filter(cenario == cen, ano == a) |> select(regiao, k)
    regioes <- obs_esp |>
      filter(ano == a) |>
      left_join(k_cen, by = "regiao") |>
      mutate(k = coalesce(k, 1),
             obs_c = k * obs,
             v = k^2 * obs)               # variância de Poisson dos casos corrigidos
    brasil <- regioes |>
      summarise(ano = a, regiao = "Brasil", obs = sum(obs), esp = sum(esp),
                obs_c = sum(obs_c), v = sum(v), k = NA_real_)
    magnitude[[length(magnitude) + 1]] <- bind_rows(regioes, brasil) |>
      mutate(cenario = cen,
             excesso = obs_c - esp,
             oe = obs_c / esp,
             se = sqrt(v) / obs_c,
             lo = oe * exp(-1.96 * se),
             hi = oe * exp(1.96 * se))
  }
}
magnitude <- bind_rows(magnitude) |>
  select(cenario, ano, regiao, k, obs, obs_c, esp, excesso, oe, lo, hi)
salvar_csv(magnitude, "subdeteccao_magnitude.csv")

# 3. Modelos de associação com os casos corrigidos --------------------------------------
exposicoes <- tibble(grupo    = c("6m_4a", "6m_4a", "lt_6m"),
                     variavel = c("nao_vac", "delta_dtp", "dtpa_2324"),
                     sinal    = c(1, -1, -1),
                     rotulo   = c("Não vacinados 2019–2023", "Variação da DTP", "dTpa 2023–2024"))

painel_ano <- painel |>
  filter(ano %in% c(anos_referencia, anos_alvo)) |>
  mutate(grupo = ifelse(faixa == "lt_6m", "lt_6m", "6m_4a")) |>
  group_by(code_muni, regiao, grupo, ano) |>
  summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
  filter(pa > 0) |>
  left_join(cobertura |> select(code_muni, nao_vac, delta_dtp, dtpa_2324), by = "code_muni")

modelos <- list()
for (cen in cenarios) {
  for (a in anos_alvo) {
    k_cen <- fatores |> filter(cenario == cen, ano == a) |> select(regiao, k)
    for (i in 1:nrow(exposicoes)) {
      dados <- painel_ano |> filter(grupo == exposicoes$grupo[i], ano %in% c(anos_referencia, a))
      dados$exposicao <- dados[[exposicoes$variavel[i]]]
      dados <- dados |>
        filter(!is.na(exposicao)) |>
        left_join(k_cen, by = "regiao") |>
        mutate(alvo = as.integer(ano == a),
               k = ifelse(alvo == 1, coalesce(k, 1), 1),
               cont = exposicoes$sinal[i] * exposicao / 10 * alvo)

      for (comparacao in c("Todos os municípios", "Mesma região")) {
        if (comparacao == "Todos os municípios") {
          modelo <- fepois(casos ~ cont | code_muni + ano, offset = ~ log(pa) - log(k),
                           data = dados, cluster = ~ code_muni)
        } else {
          modelo <- fepois(casos ~ cont | code_muni + regiao^ano, offset = ~ log(pa) - log(k),
                           data = dados, cluster = ~ code_muni)
        }
        ct <- coeftable(modelo)
        modelos[[length(modelos) + 1]] <- tibble(
          irr = exp(ct["cont", 1]),
          lo = exp(ct["cont", 1] - 1.96 * ct["cont", 2]),
          hi = exp(ct["cont", 1] + 1.96 * ct["cont", 2]),
          p = ct["cont", 4],
          cenario = cen, ano_alvo = a, grupo = exposicoes$grupo[i],
          exposicao = exposicoes$rotulo[i], comparacao = comparacao)
      }
    }
  }
}
salvar_csv(bind_rows(modelos), "subdeteccao_modelos.csv")

magnitude |> filter(regiao == "Brasil", ano == 2024) |> select(cenario, oe, excesso) |> print()
