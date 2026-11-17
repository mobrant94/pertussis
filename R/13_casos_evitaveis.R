# ------------------------------------------------------------------------------
# 13_casos_evitaveis.R
# Casos de 2024 que não teriam ocorrido com maior vacinação, usando os
# coeficientes do modelo principal (11_laboratorio_deteccao_onda.R, todos os casos):
#   - < 6 meses: municípios com dTpa em gestantes < 80% elevados a 80%
#   - 6 meses a 4 anos: não vacinados reduzidos ao nível anterior à queda das
#     coberturas (100 − DTP média de 2010–2014)
# Em cada município: evitáveis = casos × (1 − exp(−b × diferença/10)).
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")

painel        <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cobertura     <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))
sensibilidade <- ler_csv("sensibilidade_laboratorio.csv")
decomposicao  <- ler_csv("decomposicao_excesso_2024.csv")

# Casos observados em 2024 por município e grupo etário (municípios com a exposição)
casos_2024 <- painel |>
  filter(ano == 2024) |>
  mutate(grupo = ifelse(faixa == "lt_6m", "lt_6m", "6m_4a")) |>
  group_by(code_muni, grupo) |>
  summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
  filter(pa > 0) |>
  left_join(cobertura |> select(code_muni, nao_vac, dtpa_2324, dtp_pre), by = "code_muni")

# Coeficiente (b) e limites do IC95% na escala log
coeficientes <- function(rotulo) {
  linha <- sensibilidade |> filter(exposicao == rotulo, desfecho == "Todos os confirmados")
  c(linha$b, log(linha$lo), log(linha$hi))
}

# Soma dos casos evitáveis para cada valor de b
evitaveis <- function(casos, diferenca10, valores_b) {
  sapply(valores_b, function(b) sum(casos * (1 - exp(-b * diferenca10))))
}

# Lactentes: dTpa
lactentes <- casos_2024 |>
  filter(grupo == "lt_6m", !is.na(dtpa_2324)) |>
  mutate(diferenca10 = pmax(0, 80 - dtpa_2324) / 10)
ev_lactentes <- evitaveis(lactentes$casos, lactentes$diferenca10, coeficientes("dTpa 2023–2024"))

# 6 meses a 4 anos: não vacinados
criancas <- casos_2024 |>
  filter(grupo == "6m_4a", !is.na(nao_vac)) |>
  mutate(diferenca10 = coalesce(pmax(0, nao_vac - (100 - pmin(dtp_pre, 100))) / 10, 0))
ev_criancas <- evitaveis(criancas$casos, criancas$diferenca10, coeficientes("Não vacinados 2019–2023"))

# Casos a mais em cada grupo (de 07_comparacao_com_referencia.R)
excesso <- decomposicao |> filter(estrato == "Faixa")
excesso_lactentes <- sum(excesso$excesso[excesso$nivel == "lt_6m"])
excesso_criancas  <- sum(excesso$excesso[excesso$nivel %in% c("6_11m", "1_4a")])

resultado <- tibble(
  cenario = c("dTpa ≥ 80% em todos os municípios (< 6 meses)",
              "Não vacinados no nível de 2010–2014 (6 m–4 anos)"),
  grupo = c("lt_6m", "6m_4a"),
  casos_2024 = c(sum(lactentes$casos), sum(criancas$casos)),
  excesso_2024 = c(excesso_lactentes, excesso_criancas),
  evitaveis = c(ev_lactentes[1], ev_criancas[1]),
  evit_lo = c(min(ev_lactentes[2:3]), min(ev_criancas[2:3])),
  evit_hi = c(max(ev_lactentes[2:3]), max(ev_criancas[2:3]))) |>
  mutate(pct_casos = 100 * evitaveis / casos_2024)
salvar_csv(resultado, "casos_atribuiveis_2024.csv")
print(resultado)
