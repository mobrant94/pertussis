# ------------------------------------------------------------------------------
# 09_modelos_vacinacao.R
# O aumento de casos foi maior onde menos crianças e gestantes foram vacinadas?
# Painel município-ano (2016–2019 e ano-alvo), regressão de Poisson com efeitos
# fixos (pacote fixest), offset log(pessoas-ano) e erros-padrão agrupados por município.
#
# Exposições:
#   6 meses a 4 anos: proporção de não vacinados (coortes 2019–2023) e variação da DTP
#   < 6 meses:        cobertura de dTpa em gestantes em 2023–2024
# Modelos (alvo = 1 no ano-alvo, 0 em 2016–2019):
#   a) RR do ano-alvo em cada categoria          casos ~ i(cat, alvo) | município
#   b) comparação entre categorias, país todo    casos ~ i(cat, alvo, ref) | município + ano
#   c) comparação entre categorias, mesma região casos ~ i(cat, alvo, ref) | município + região^ano
#   d) exposição contínua, país todo             casos ~ cont:alvo | município + ano
#   e) exposição contínua, mesma região          casos ~ cont:alvo | município + região^ano  (principal)
# "cont" = exposição por 10 pontos percentuais, no sentido de menor vacinação.
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(fixest)

painel    <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cobertura <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))

# Exposições analisadas
exposicoes <- tibble(
  grupo     = c("6m_4a", "6m_4a", "lt_6m"),
  categoria = c("cat_delta", "ter_naovac", "ter_dtpa"),
  referencia = c("Estável ou aumento", "T1 (menor)", "Maior cobertura (T3)"),
  continua  = c("delta_dtp", "nao_vac", "dtpa_2324"),
  sinal     = c(-1, 1, -1),     # -1: queda da cobertura = aumento da exposição
  rotulo    = c("Variação da DTP", "Não vacinados 2019–2023", "dTpa 2023–2024"))

# Coeficientes de um modelo em formato de tabela
tabela_coef <- function(ajuste, dados, nome_modelo, exp_i) {
  ct <- coeftable(ajuste)
  usados <- obs(ajuste)
  tibble(grupo = exposicoes$grupo[exp_i], exposicao = exposicoes$rotulo[exp_i],
         modelo = nome_modelo, termo = rownames(ct),
         b = ct[, 1], se = ct[, 2], p = ct[, 4],
         n_obs = nobs(ajuste),
         n_muni = length(unique(dados$code_muni[usados])),
         casos = sum(dados$casos[usados]))
}

resultados <- list()

for (ano_alvo in janelas$nome) {

  # Painel município-período (cada ano de 2016–2019 e a janela)
  painel_ano <- recorte(painel, ano_alvo) |>
    mutate(grupo = ifelse(faixa == "lt_6m", "lt_6m", "6m_4a")) |>
    group_by(code_muni, regiao, grupo, ano) |>
    summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
    filter(pa > 0) |>
    mutate(alvo = as.integer(ano == 9999)) |>
    left_join(cobertura, by = c("code_muni", "regiao"))

  for (i in 1:nrow(exposicoes)) {
    dados <- painel_ano |> filter(grupo == exposicoes$grupo[i])
    dados$cat  <- dados[[exposicoes$categoria[i]]]
    dados$cont <- exposicoes$sinal[i] * dados[[exposicoes$continua[i]]] / 10
    dados <- dados |> filter(!is.na(cat))

    # A categoria de referência entra como texto na fórmula
    formula_b <- as.formula(paste0('casos ~ i(cat, alvo, ref = "', exposicoes$referencia[i], '") | code_muni + ano'))
    formula_c <- as.formula(paste0('casos ~ i(cat, alvo, ref = "', exposicoes$referencia[i], '") | code_muni + regiao^ano'))

    m_a <- fepois(casos ~ i(cat, alvo) | code_muni, offset = ~ log(pa), data = dados, cluster = ~ code_muni)
    m_b <- fepois(formula_b, offset = ~ log(pa), data = dados, cluster = ~ code_muni)
    m_c <- fepois(formula_c, offset = ~ log(pa), data = dados, cluster = ~ code_muni)
    m_d <- fepois(casos ~ cont:alvo | code_muni + ano, offset = ~ log(pa), data = dados, cluster = ~ code_muni)
    m_e <- fepois(casos ~ cont:alvo | code_muni + regiao^ano, offset = ~ log(pa), data = dados, cluster = ~ code_muni)

    teste_wald <- wald(m_b, keep = "cat::", print = FALSE)

    resultados[[length(resultados) + 1]] <- bind_rows(
      tabela_coef(m_a, dados, "a_rr_categoria", i),
      tabela_coef(m_b, dados, "b_razao_excessos", i) |> mutate(wald_p = teste_wald$p),
      tabela_coef(m_c, dados, "c_razao_excessos_regiao_ano", i),
      tabela_coef(m_d, dados, "d_continua", i),
      tabela_coef(m_e, dados, "e_continua_regiao_ano", i)) |>
      mutate(ano_alvo = ano_alvo)
  }
}

resultados <- bind_rows(resultados) |>
  mutate(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
         termo = str_remove_all(termo, "^cat::|:alvo$|^alvo:"))
salvar_csv(resultados, "modelos_cobertura_excesso.csv")

resultados |>
  filter(modelo %in% c("d_continua", "e_continua_regiao_ano")) |>
  select(ano_alvo, exposicao, modelo, irr, lo, hi) |>
  print()
