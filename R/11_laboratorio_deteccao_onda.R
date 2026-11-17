# ------------------------------------------------------------------------------
# 11_laboratorio_deteccao_onda.R
#   A) Modelo principal (contínuo, mesma região) com todos os casos e só com os
#      confirmados por laboratório, em 2024
#   B) Notificação, confirmação e confirmação laboratorial por região e período
#   C) Momento da onda em cada região: mês em que os casos passaram a superar o
#      dobro do esperado e mês do pico
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(fixest)

painel    <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cobertura <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))
sinan     <- readRDS(file.path(pasta_proc, "sinan_lt5.rds"))

# A) Sensibilidade laboratorial ------------------------------------------------------
painel_2024 <- painel |>
  filter(ano %in% c(anos_referencia, 2024)) |>
  mutate(grupo = ifelse(faixa == "lt_6m", "lt_6m", "6m_4a")) |>
  group_by(code_muni, regiao, grupo, ano) |>
  summarise(casos = sum(casos), casos_lab = sum(casos_lab), pa = sum(pop) / 12, .groups = "drop") |>
  filter(pa > 0) |>
  left_join(cobertura |> select(code_muni, nao_vac, delta_dtp, dtpa_2324), by = "code_muni")

exposicoes <- tibble(grupo    = c("6m_4a", "6m_4a", "lt_6m"),
                     variavel = c("nao_vac", "delta_dtp", "dtpa_2324"),
                     sinal    = c(1, -1, -1),
                     rotulo   = c("Não vacinados 2019–2023", "Variação da DTP", "dTpa 2023–2024"))

sensibilidade <- list()
for (i in 1:nrow(exposicoes)) {
  for (desfecho in c("casos", "casos_lab")) {
    dados <- painel_2024 |> filter(grupo == exposicoes$grupo[i])
    dados$exposicao <- dados[[exposicoes$variavel[i]]]
    dados$casos <- dados[[desfecho]]
    dados <- dados |>
      filter(!is.na(exposicao)) |>
      mutate(alvo = as.integer(ano == 2024),
             cont = exposicoes$sinal[i] * exposicao / 10 * alvo)

    modelo <- fepois(casos ~ cont | code_muni + regiao^ano, offset = ~ log(pa),
                     data = dados, cluster = ~ code_muni)
    ct <- coeftable(modelo)
    sensibilidade[[length(sensibilidade) + 1]] <- tibble(
      b = ct["cont", 1], se = ct["cont", 2],
      irr = exp(ct["cont", 1]),
      lo = exp(ct["cont", 1] - 1.96 * ct["cont", 2]),
      hi = exp(ct["cont", 1] + 1.96 * ct["cont", 2]),
      p = ct["cont", 4],
      grupo = exposicoes$grupo[i], exposicao = exposicoes$rotulo[i],
      desfecho = ifelse(desfecho == "casos", "Todos os confirmados", "Confirmados por laboratório"),
      n_muni = length(unique(dados$code_muni[obs(modelo)])))
  }
}
salvar_csv(bind_rows(sensibilidade), "sensibilidade_laboratorio.csv")

# B) Detecção por região ---------------------------------------------------------------
pessoas_ano <- painel |>
  filter(ano %in% c(anos_referencia, anos_alvo)) |>
  group_by(regiao, ano) |>
  summarise(pa = sum(pop) / 12, .groups = "drop")

deteccao <- sinan |>
  filter(code_muni %in% unique(painel$code_muni)) |>
  mutate(ano = year(dt_sin),
         regiao = regiao_do_municipio(code_muni),
         confirmado = CLASSI_FIN %in% "1",
         laboratorio = confirmado & CRITERIO %in% "1") |>
  filter(ano %in% c(anos_referencia, anos_alvo)) |>
  group_by(regiao, ano) |>
  summarise(notif = n(), conf = sum(confirmado), lab = sum(laboratorio), .groups = "drop") |>
  left_join(pessoas_ano, by = c("regiao", "ano")) |>
  mutate(periodo = ifelse(ano %in% anos_referencia, "2016–2019", as.character(ano))) |>
  group_by(regiao, periodo) |>
  summarise(notif = sum(notif), conf = sum(conf), lab = sum(lab), pa = sum(pa), .groups = "drop") |>
  mutate(tx_notif = 1e5 * notif / pa,
         tx_conf  = 1e5 * conf / pa,
         tx_lab   = 1e5 * lab / pa,
         tx_clin  = 1e5 * (conf - lab) / pa,
         pct_conf = 100 * conf / notif,
         pct_lab  = 100 * lab / conf)
salvar_csv(deteccao, "deteccao_por_regiao.csv")

# C) Momento da onda -------------------------------------------------------------------
taxa_referencia <- painel |>
  filter(ano %in% anos_referencia) |>
  group_by(regiao, mes) |>
  summarise(tx = sum(casos) / sum(pop), .groups = "drop")

serie <- painel |>
  filter(ano %in% anos_alvo) |>
  group_by(regiao, data, ano, mes) |>
  summarise(casos = sum(casos), pop = sum(pop), .groups = "drop") |>
  left_join(taxa_referencia, by = c("regiao", "mes")) |>
  mutate(esperado = tx * pop,
         acima = casos > 2 * esperado & casos >= 5) |>
  arrange(regiao, data)

onda <- serie |>
  group_by(regiao) |>
  summarise(pico = data[which.max(casos)],
            casos_pico = max(casos),
            inicio = if (any(acima)) min(data[acima]) else as.Date(NA),
            casos_2024 = sum(casos[ano == 2024]),
            casos_2025 = sum(casos[ano == 2025]))
salvar_csv(onda, "momento_onda_regiao.csv")

print(bind_rows(sensibilidade) |> select(exposicao, desfecho, irr, lo, hi))
print(onda)
