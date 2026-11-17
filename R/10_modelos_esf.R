# ------------------------------------------------------------------------------
# 10_modelos_esf.R
# A cobertura da ESF mudou a relação entre a falta de vacinação e o aumento de casos?
# Mesmo painel de 09_modelos_vacinacao.R, com efeitos fixos de município e de
# região x ano. Modelos:
#   f) estratificado: efeito da exposição em cada categoria de ESF
#   g) interação contínua: cont:alvo + cont:alvo:esf10 + alvo:esf10
#      (esf10 = cobertura da ESF em dezenas de p.p., centrada em 80%)
#      e efeitos condicionais com ESF de 40%, 60%, 80% e 100%
#   g_lab) modelo g restrito aos casos confirmados por laboratório
#   h) grade: cada combinação categoria de exposição x categoria de ESF
#   i) ESF isolada: aumento de casos por categoria de ESF (referência ≥ 80%)
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(fixest)

painel    <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cobertura <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))
esf       <- readRDS(file.path(pasta_proc, "aps_mun.rds"))

exposicoes <- tibble(
  grupo      = c("6m_4a", "6m_4a", "lt_6m"),
  categoria  = c("cat_delta", "ter_naovac", "ter_dtpa"),
  referencia = c("Estável ou aumento", "T1 (menor)", "Maior cobertura (T3)"),
  continua   = c("delta_dtp", "nao_vac", "dtpa_2324"),
  sinal      = c(-1, 1, -1),
  rotulo     = c("Variação da DTP", "Não vacinados 2019–2023", "dTpa 2023–2024"))

# Uma linha de resultado
linha <- function(ajuste, dados, nome_modelo, exp_i, termo, b, se) {
  tibble(grupo = exposicoes$grupo[exp_i], exposicao = exposicoes$rotulo[exp_i],
         modelo = nome_modelo, termo = termo, b = b, se = se,
         p = 2 * pnorm(-abs(b / se)),
         n_obs = nobs(ajuste),
         n_muni = length(unique(dados$code_muni[obs(ajuste)])),
         casos = sum(dados$casos[obs(ajuste)]))
}

# Todos os coeficientes de um modelo
todos_coef <- function(ajuste, dados, nome_modelo, exp_i) {
  ct <- coeftable(ajuste)
  linha(ajuste, dados, nome_modelo, exp_i, rownames(ct), ct[, 1], ct[, 2])
}

# Nomes curtos para os termos do modelo de interação
nomeia <- function(termo) {
  case_when(grepl("esf10", termo) & grepl("cont", termo) ~ "interacao",
            grepl("esf10", termo) ~ "aps_p24",
            TRUE ~ "exposicao_aps80")
}

resultados <- list()
descritivo <- list()

for (ano_alvo in anos_alvo) {

  painel_ano <- painel |>
    filter(ano %in% c(anos_referencia, ano_alvo)) |>
    mutate(grupo = ifelse(faixa == "lt_6m", "lt_6m", "6m_4a")) |>
    group_by(code_muni, regiao, grupo, ano) |>
    summarise(casos = sum(casos), casos_lab = sum(casos_lab), pa = sum(pop) / 12, .groups = "drop") |>
    filter(pa > 0) |>
    mutate(alvo = as.integer(ano == ano_alvo)) |>
    left_join(cobertura, by = c("code_muni", "regiao")) |>
    left_join(esf, by = "code_muni") |>
    mutate(esf10 = (aps - 80) / 10)

  # Municípios, casos e taxas por categoria de ESF
  descritivo[[as.character(ano_alvo)]] <- painel_ano |>
    filter(!is.na(cat_aps)) |>
    mutate(periodo = ifelse(alvo == 1, as.character(ano_alvo), "2016–2019")) |>
    group_by(grupo, cat_aps, periodo) |>
    summarise(municipios = n_distinct(code_muni), casos = sum(casos), pa = sum(pa), .groups = "drop") |>
    mutate(taxa = 1e5 * casos / pa, ano_alvo = ano_alvo)

  for (i in 1:nrow(exposicoes)) {
    dados <- painel_ano |> filter(grupo == exposicoes$grupo[i])
    dados$cont <- exposicoes$sinal[i] * dados[[exposicoes$continua[i]]] / 10
    dados$cat  <- dados[[exposicoes$categoria[i]]]
    dados <- dados |> filter(!is.na(cont), !is.na(aps))

    # f) estratificado por categoria de ESF
    for (k in levels(dados$cat_aps)) {
      dados_k <- dados |> filter(cat_aps == k)
      m_f <- fepois(casos ~ cont:alvo | code_muni + regiao^ano, offset = ~ log(pa),
                    data = dados_k, cluster = ~ code_muni)
      resultados[[length(resultados) + 1]] <- todos_coef(m_f, dados_k, "f_estrato_aps", i) |>
        mutate(termo = k, ano_alvo = ano_alvo)
    }

    # g) interação contínua (todos os casos e só laboratoriais)
    m_g <- fepois(casos ~ cont:alvo + cont:alvo:esf10 + alvo:esf10 | code_muni + regiao^ano,
                  offset = ~ log(pa), data = dados, cluster = ~ code_muni)
    dados_lab <- dados |> mutate(casos = casos_lab)
    m_g_lab <- fepois(casos ~ cont:alvo + cont:alvo:esf10 + alvo:esf10 | code_muni + regiao^ano,
                      offset = ~ log(pa), data = dados_lab, cluster = ~ code_muni)

    resultados[[length(resultados) + 1]] <- todos_coef(m_g, dados, "g_interacao", i) |>
      mutate(termo = nomeia(termo), ano_alvo = ano_alvo)
    resultados[[length(resultados) + 1]] <- todos_coef(m_g_lab, dados_lab, "g_interacao_lab", i) |>
      mutate(termo = nomeia(termo), ano_alvo = ano_alvo)

    # efeito da exposição com ESF de 40%, 60%, 80% e 100%
    b <- coef(m_g)
    V <- vcov(m_g)
    termo_principal <- which(grepl("cont", names(b)) & !grepl("esf10", names(b)))
    termo_interacao <- which(grepl("cont", names(b)) & grepl("esf10", names(b)))
    for (nivel in c(40, 60, 80, 100)) {
      pesos <- rep(0, length(b))
      pesos[termo_principal] <- 1
      pesos[termo_interacao] <- (nivel - 80) / 10
      estimativa <- sum(pesos * b)
      ep <- sqrt(drop(t(pesos) %*% V %*% pesos))
      resultados[[length(resultados) + 1]] <- linha(m_g, dados, "g_efeito_condicional", i,
                                                     paste0("APS ", nivel, "%"), estimativa, ep) |>
        mutate(ano_alvo = ano_alvo)
    }

    # h) grade exposição x ESF
    dados_h <- dados |> filter(!is.na(cat)) |> mutate(grade = paste(cat, cat_aps, sep = " | "))
    ref_grade <- paste(exposicoes$referencia[i], "≥ 80%", sep = " | ")
    formula_h <- as.formula(paste0('casos ~ i(grade, alvo, ref = "', ref_grade, '") | code_muni + regiao^ano'))
    m_h <- fepois(formula_h, offset = ~ log(pa), data = dados_h, cluster = ~ code_muni)
    resultados[[length(resultados) + 1]] <- todos_coef(m_h, dados_h, "h_grade", i) |>
      mutate(termo = str_remove_all(termo, "^grade::|:alvo$"), ano_alvo = ano_alvo)

    # i) ESF isolada
    m_i <- fepois(casos ~ i(cat_aps, alvo, ref = "≥ 80%") | code_muni + regiao^ano,
                  offset = ~ log(pa), data = dados, cluster = ~ code_muni)
    resultados[[length(resultados) + 1]] <- todos_coef(m_i, dados, "i_aps", i) |>
      mutate(termo = str_remove_all(termo, "^cat_aps::|:alvo$"), ano_alvo = ano_alvo)
  }
}

resultados <- bind_rows(resultados) |>
  mutate(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))
salvar_csv(resultados, "modelos_aps.csv")
salvar_csv(bind_rows(descritivo), "aps_casos_por_categoria.csv")

resultados |>
  filter(modelo %in% c("g_interacao", "i_aps"), ano_alvo == 2024) |>
  select(exposicao, modelo, termo, irr, lo, hi, p) |>
  print()
