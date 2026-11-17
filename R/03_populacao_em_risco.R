# ------------------------------------------------------------------------------
# 03_populacao_em_risco.R
# População mensal em risco por município e faixa etária, pelo método de
# componentes de coorte (sem migração):
#
#   pop_G(t) = soma, nas idades a (em meses) da faixa G, de NV(t − a) × [1 − F(a)]
#
#   NV(b): nascidos vivos residentes no município no mês b
#   F(a):  proporção acumulada de óbitos da coorte até a idade a, calculada com as
#          razões óbitos/nascidos vivos da UF no ano de nascimento:
#            a = 0:      0,5 × q_neo
#            a = 1–11:   q_neo + q_pn × (a − 0,5)/11
#            a = 12–59:  q_neo + q_pn + q_14 × (a − 11,5)/48
#   q_neo = óbitos 0–27 dias / NV;  q_pn = óbitos 28–364 dias / NV;
#   q_14  = óbitos 1–4 anos / NV
# Faixas: < 6 meses (a = 0–5), 6–11 meses (a = 6–11), 1–4 anos (a = 12–59).
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")

nascidos <- readRDS(file.path(pasta_proc, "nascidos_mun_mes.rds"))
obitos   <- readRDS(file.path(pasta_proc, "obitos_uf_ano.rds"))

# 1. Municípios ------------------------------------------------------------------
# Exclui "município ignorado" (código terminado em 0000) e municípios instalados
# depois de 2005, que não têm nascimentos no início da série.
nascidos <- nascidos |>
  filter(!grepl("0000$", code_muni)) |>
  mutate(data = make_date(ano, mes, 1), uf = substr(code_muni, 1, 2))

inicio <- nascidos |>
  filter(nv > 0) |>
  group_by(code_muni) |>
  summarise(primeiro_ano = min(ano))
municipios <- inicio$code_muni[inicio$primeiro_ano <= min(anos_nascimento)]
excluidos  <- setdiff(unique(nascidos$code_muni), municipios)
salvar_csv(tibble(code_muni = excluidos), "municipios_excluidos_denominador.csv")

# 2. Razões de óbitos por nascidos vivos, por UF e ano --------------------------
razoes <- nascidos |>
  group_by(uf, ano) |>
  summarise(nv = sum(nv), .groups = "drop") |>
  left_join(obitos, by = c("uf", "ano")) |>
  mutate(q_neo = d_neo / nv,
         q_pn  = d_pn / nv,
         q_14  = coalesce(d_14, 0) / nv)
salvar_csv(razoes, "razoes_obitos_nascidos_uf_ano.csv")

# 3. Grade completa município x mês de nascimento -------------------------------
meses <- seq(make_date(min(anos_nascimento), 1, 1), make_date(max(anos_nascimento), 12, 1), by = "month")

grade <- expand_grid(code_muni = municipios, data = meses) |>
  left_join(nascidos |> select(code_muni, data, nv), by = c("code_muni", "data")) |>
  mutate(nv = coalesce(nv, 0), uf = substr(code_muni, 1, 2), ano = year(data)) |>
  left_join(razoes |> select(uf, ano, q_neo, q_pn, q_14), by = c("uf", "ano")) |>
  arrange(code_muni, data)

# 4. Sobreviventes de cada coorte em cada idade (0 a 59 meses) ------------------
# Para cada idade a, os sobreviventes no mês t são os nascidos no mês t − a
# multiplicados por 1 − F(a). A função lag() desloca a série a meses para trás.
grade$pop_lt_6m  <- 0
grade$pop_6_11m  <- 0
grade$pop_1_4a   <- 0

for (a in 0:59) {
  f_neo <- ifelse(a == 0, 0.5, 1)
  f_pn  <- ifelse(a == 0, 0, ifelse(a <= 11, (a - 0.5) / 11, 1))
  f_14  <- ifelse(a <= 11, 0, (a - 11.5) / 48)

  sobreviventes <- grade |>
    mutate(s = nv * (1 - f_neo * q_neo - f_pn * q_pn - f_14 * q_14)) |>
    group_by(code_muni) |>
    mutate(s = lag(s, a, default = 0)) |>
    pull(s)

  if (a <= 5)            grade$pop_lt_6m <- grade$pop_lt_6m + sobreviventes
  if (a >= 6 & a <= 11)  grade$pop_6_11m <- grade$pop_6_11m + sobreviventes
  if (a >= 12)           grade$pop_1_4a  <- grade$pop_1_4a + sobreviventes
}

populacao <- grade |>
  filter(year(data) %in% anos_casos) |>
  select(code_muni, data, lt_6m = pop_lt_6m, `6_11m` = pop_6_11m, `1_4a` = pop_1_4a) |>
  pivot_longer(c(lt_6m, `6_11m`, `1_4a`), names_to = "faixa", values_to = "pop") |>
  mutate(pop = pmax(pop, 0))

saveRDS(populacao, file.path(pasta_proc, "pop_mensal_nv.rds"))

# 5. Comparação com os Censos de 2010 e 2022 (julho de cada ano) ----------------
comparacao <- populacao |>
  filter(year(data) %in% c(2010, 2022), month(data) == 7) |>
  group_by(ano = year(data), faixa) |>
  summarise(pop_nv = sum(pop), .groups = "drop")

arquivos_censo <- file.path(pasta_brutos, c("pop10.xlsx", "pop22.xlsx"))
if (all(file.exists(arquivos_censo))) {
  faixa_do_censo <- function(x) {
    x <- str_squish(str_replace_all(x, "_", " "))
    case_when(str_detect(x, "^Menos de 1") ~ "lt_6m",
              str_detect(x, "^[1-5] m") ~ "lt_6m",
              str_detect(x, "^([6-9]|1[01]) m") ~ "6_11m",
              str_detect(x, "^[1-4] ano") ~ "1_4a")
  }
  censo_2010 <- read_excel(arquivos_censo[1]) |>
    fill(code_muni) |>
    filter(faixa_etaria != "Total") |>
    mutate(faixa = faixa_do_censo(faixa_etaria), ano = 2010)
  censo_2022 <- read_excel(arquivos_censo[2]) |>
    pivot_longer(-code_muni, names_to = "faixa_etaria", values_to = "valor") |>
    mutate(faixa = faixa_do_censo(faixa_etaria), ano = 2022)
  censo <- bind_rows(censo_2010, censo_2022) |>
    filter(!is.na(faixa)) |>
    group_by(ano, faixa) |>
    summarise(pop_censo = sum(as.numeric(valor), na.rm = TRUE), .groups = "drop")
  comparacao <- comparacao |>
    left_join(censo, by = c("ano", "faixa")) |>
    mutate(razao_nv_censo = pop_nv / pop_censo)
}
salvar_csv(comparacao, "denominador_vs_censo.csv")
print(comparacao)
