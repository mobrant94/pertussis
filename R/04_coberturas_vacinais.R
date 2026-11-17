# ------------------------------------------------------------------------------
# 04_coberturas_vacinais.R
# Coberturas vacinais municipais (SI-PNI), arquivo dados_brutos/pni_completo.xlsx
#   aba DTP:  esquema primário da vacina DTP em crianças
#   aba DTPA: dTpa em gestantes
# Regras: valores < 1% = ausentes; valores > 120% = 120%.
# Exposições do estudo, por município:
#   delta_dtp: cobertura média de DTP em 2019–2023 menos a de 2010–2014 (p.p.)
#   nao_vac:   proporção estimada de crianças nascidas em 2019–2023 sem o esquema
#              primário = soma NV_ano × (1 − DTP_ano/100) / soma NV_ano
#   dtpa_2324: cobertura média de dTpa em gestantes em 2023–2024
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")

arquivo_pni <- file.path(pasta_brutos, "pni_completo.xlsx")

# 1. Leitura das duas abas ------------------------------------------------------
ler_aba <- function(aba) {
  read_excel(arquivo_pni, sheet = aba) |>
    rename(code_muni = Municipio) |>
    pivot_longer(-code_muni, names_to = "ano", values_to = "cobertura",
                 names_pattern = "^dtp_(\\d{4})$") |>
    mutate(code_muni = sprintf("%06d", as.integer(code_muni)),
           ano = as.integer(ano),
           cobertura = as.numeric(cobertura),
           cobertura = ifelse(cobertura < 1, NA, pmin(cobertura, 120)))
}

dtp  <- ler_aba("DTP")  |> rename(dtp = cobertura)
dtpa <- ler_aba("DTPA") |> rename(dtpa = cobertura) |>
  mutate(dtpa = ifelse(ano < 2015, NA, dtpa))     # dTpa em gestantes começou em 2014

nascidos_ano <- readRDS(file.path(pasta_proc, "nascidos_mun_mes.rds")) |>
  group_by(code_muni, ano) |>
  summarise(nv = sum(nv), .groups = "drop")

cobertura_ano <- dtp |>
  full_join(dtpa, by = c("code_muni", "ano")) |>
  left_join(nascidos_ano, by = c("code_muni", "ano"))
saveRDS(cobertura_ano, file.path(pasta_proc, "cobertura_mun_ano.rds"))

# 2. Exposições por município -----------------------------------------------------
media <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

cobertura <- cobertura_ano |>
  group_by(code_muni) |>
  summarise(dtp_pre   = media(dtp[ano %in% 2010:2014]),
            dtp_pan   = media(dtp[ano %in% 2019:2023]),
            dtp_2024  = media(dtp[ano == 2024]),
            dtpa_pre  = media(dtpa[ano %in% 2015:2019]),
            dtpa_pan  = media(dtpa[ano %in% 2020:2023]),
            dtpa_2324 = media(dtpa[ano %in% 2023:2024]),
            nv_1923   = sum(nv[ano %in% 2019:2023], na.rm = TRUE),
            .groups = "drop")

# Não vacinados nas coortes de 2019–2023 (exige pelo menos 3 anos com dados)
nao_vacinados <- cobertura_ano |>
  filter(ano %in% 2019:2023, !is.na(dtp), !is.na(nv), nv > 0) |>
  group_by(code_muni) |>
  summarise(anos_com_dado = n(),
            nao_vac = 100 * sum(nv * (1 - pmin(dtp, 100) / 100)) / sum(nv)) |>
  mutate(nao_vac = ifelse(anos_com_dado < 3, NA, nao_vac)) |>
  select(code_muni, nao_vac)

cobertura <- cobertura |>
  left_join(nao_vacinados, by = "code_muni") |>
  mutate(delta_dtp  = dtp_pan - dtp_pre,
         delta_dtpa = dtpa_pan - dtpa_pre,
         regiao = regiao_do_municipio(code_muni),
         uf = substr(code_muni, 1, 2))

# 3. Categorias -------------------------------------------------------------------
# Variação da DTP
cobertura$cat_delta <- cut(cobertura$delta_dtp, c(-Inf, -10, 0, Inf), right = FALSE,
                           labels = c("Queda ≥ 10 p.p.", "Queda < 10 p.p.", "Estável ou aumento"))
cobertura$cat_delta <- relevel(cobertura$cat_delta, ref = "Estável ou aumento")

# dTpa por faixas fixas (usada como análise de sensibilidade)
cobertura$cat_dtpa <- cut(cobertura$dtpa_2324, c(-Inf, 50, 80, Inf), right = FALSE,
                          labels = c("< 50%", "50–79,9%", "≥ 80%"))
cobertura$cat_dtpa <- relevel(cobertura$cat_dtpa, ref = "≥ 80%")

# Tercis ponderados pelos nascidos vivos de 2019–2023: os pontos de corte são
# escolhidos para que cada tercil reúna cerca de um terço das crianças do país.
cortes_ponderados <- function(x, peso) {
  ok <- !is.na(x) & !is.na(peso) & peso > 0
  x <- x[ok]; peso <- peso[ok]
  ordem <- order(x)
  acumulado <- cumsum(peso[ordem]) / sum(peso)
  c(x[ordem][which(acumulado >= 1/3)[1]], x[ordem][which(acumulado >= 2/3)[1]])
}

cortes_nv <- cortes_ponderados(cobertura$nao_vac, cobertura$nv_1923)
cobertura$ter_naovac <- cut(cobertura$nao_vac, c(-Inf, cortes_nv, Inf),
                            labels = c("T1 (menor)", "T2", "T3 (maior)"))

cortes_tpa <- cortes_ponderados(cobertura$dtpa_2324, cobertura$nv_1923)
cobertura$ter_dtpa <- cut(cobertura$dtpa_2324, c(-Inf, cortes_tpa, Inf),
                          labels = c("Menor cobertura (T1)", "Intermediária (T2)", "Maior cobertura (T3)"))
cobertura$ter_dtpa <- relevel(cobertura$ter_dtpa, ref = "Maior cobertura (T3)")

saveRDS(cobertura, file.path(pasta_proc, "cobertura_mun.rds"))

# 4. Coberturas nacionais e por UF, ponderadas pelos nascidos vivos -------------
cobertura_nacional <- cobertura_ano |>
  filter(!is.na(nv), nv > 0) |>
  group_by(ano) |>
  summarise(dtp  = weighted.mean(dtp, nv, na.rm = TRUE),
            dtpa = weighted.mean(dtpa, nv, na.rm = TRUE))
salvar_csv(cobertura_nacional, "cobertura_nacional_ponderada.csv")

cobertura_uf <- cobertura_ano |>
  filter(!is.na(nv), nv > 0) |>
  mutate(uf = substr(code_muni, 1, 2)) |>
  group_by(uf, ano) |>
  summarise(dtp  = weighted.mean(dtp, nv, na.rm = TRUE),
            dtpa = weighted.mean(dtpa, nv, na.rm = TRUE), .groups = "drop")
salvar_csv(cobertura_uf, "cobertura_uf_ponderada.csv")

# 5. Resumo -----------------------------------------------------------------------
resumo <- cobertura |>
  summarise(n = n(),
            n_delta = sum(!is.na(delta_dtp)),
            mediana_delta = median(delta_dtp, na.rm = TRUE),
            q1 = quantile(delta_dtp, 0.25, na.rm = TRUE),
            q3 = quantile(delta_dtp, 0.75, na.rm = TRUE),
            pct_queda10 = 100 * mean(delta_dtp <= -10, na.rm = TRUE),
            mediana_naovac = median(nao_vac, na.rm = TRUE))
salvar_csv(resumo, "cobertura_resumo_municipal.csv")
salvar_csv(count(cobertura, cat_delta), "cobertura_categorias.csv")
print(resumo)
