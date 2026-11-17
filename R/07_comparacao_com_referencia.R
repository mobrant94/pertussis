# ------------------------------------------------------------------------------
# 07_comparacao_com_referencia.R
# Compara a incidência de 2024 (e de 2025, preliminar) com a de 2016–2019.
#   - Razão de taxas (RR) anual: regressão binomial negativa com o ano-mês como
#     unidade, termos para mês e offset log(população), no total e por faixa
#     etária, região e critério de confirmação.
#   - RR mensal: interação entre período e mês.
#   - Casos esperados: taxas de 2016–2019 de cada faixa etária, região e mês
#     aplicadas à população do ano analisado; casos a mais = observados − esperados.
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(MASS, exclude = "select")

painel <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))

# Soma os casos por ano e mês e ajusta a binomial negativa.
# "desfecho" pode ser casos, casos_lab (laboratoriais) ou casos_nlab (clínicos).
ajustar_rr <- function(dados, desfecho = "casos", interacao = FALSE) {
  dados$y <- dados[[desfecho]]
  serie <- dados |>
    group_by(ano, mes, alvo) |>
    summarise(y = sum(y), pop = sum(pop), .groups = "drop")
  if (interacao) {
    modelo <- glm.nb(y ~ factor(alvo) * mes + offset(log(pop)), data = serie)
  } else {
    modelo <- glm.nb(y ~ alvo + mes + offset(log(pop)), data = serie)
  }
  list(modelo = modelo, serie = serie)
}

# RR anual com IC95%
rr_anual <- function(dados, desfecho = "casos") {
  ajuste <- ajustar_rr(dados, desfecho)
  b  <- coef(ajuste$modelo)["alvo"]
  ep <- sqrt(vcov(ajuste$modelo)["alvo", "alvo"])
  tibble(rr = exp(b), lo = exp(b - 1.96 * ep), hi = exp(b + 1.96 * ep),
         casos_ref_ano = sum(ajuste$serie$y[ajuste$serie$alvo == 0]) / length(anos_referencia),
         casos_2024 = sum(ajuste$serie$y[ajuste$serie$alvo == 1]))
}

# RR de cada mês: efeito principal + interação do mês
rr_mensal <- function(dados) {
  ajuste <- ajustar_rr(dados, interacao = TRUE)
  b <- coef(ajuste$modelo)
  V <- vcov(ajuste$modelo)
  resultado <- list()
  for (k in 1:12) {
    pesos <- setNames(rep(0, length(b)), names(b))
    pesos["factor(alvo)1"] <- 1
    if (k > 1) pesos[paste0("factor(alvo)1:mes", k)] <- 1
    estimativa <- sum(pesos * b)
    ep <- sqrt(drop(t(pesos) %*% V %*% pesos))
    resultado[[k]] <- tibble(mes = k, rr = exp(estimativa),
                             lo = exp(estimativa - 1.96 * ep), hi = exp(estimativa + 1.96 * ep))
  }
  bind_rows(resultado)
}

for (ano_alvo in anos_alvo) {
  dados <- painel |>
    filter(ano %in% c(anos_referencia, ano_alvo)) |>
    mutate(alvo = as.integer(ano == ano_alvo),
           mes = factor(mes, levels = 1:12))

  # 1. RR anual --------------------------------------------------------------------
  tabela <- list(rr_anual(dados) |> mutate(estrato = "Total", nivel = "< 5 anos"))
  for (f in faixas) {
    tabela[[length(tabela) + 1]] <- rr_anual(filter(dados, faixa == f)) |>
      mutate(estrato = "Faixa", nivel = f)
  }
  for (r in sort(unique(dados$regiao))) {
    tabela[[length(tabela) + 1]] <- rr_anual(filter(dados, regiao == r)) |>
      mutate(estrato = "Regiao", nivel = r)
  }
  tabela[[length(tabela) + 1]] <- rr_anual(dados, "casos_lab") |> mutate(estrato = "Criterio", nivel = "lab")
  tabela[[length(tabela) + 1]] <- rr_anual(dados, "casos_nlab") |> mutate(estrato = "Criterio", nivel = "nlab")
  tabela <- bind_rows(tabela)
  salvar_csv(tabela, paste0("rr_", ano_alvo, "_vs_ref.csv"))

  # 2. RR mensal -------------------------------------------------------------------
  mensal <- list(rr_mensal(dados) |> mutate(faixa = "total"))
  for (f in faixas) {
    mensal[[length(mensal) + 1]] <- rr_mensal(filter(dados, faixa == f)) |> mutate(faixa = f)
  }
  salvar_csv(bind_rows(mensal), paste0("rr_mensal_", ano_alvo, ".csv"))

  # 3. Casos observados, esperados e a mais ----------------------------------------
  estratos <- dados |>
    mutate(periodo = ifelse(ano == ano_alvo, "alvo", "ref"),
           mes = as.integer(as.character(mes))) |>
    group_by(faixa, regiao, mes, periodo) |>
    summarise(casos = sum(casos), lab = sum(casos_lab), nlab = sum(casos_nlab),
              pop = sum(pop), .groups = "drop") |>
    pivot_wider(names_from = periodo, values_from = c(casos, lab, nlab, pop)) |>
    mutate(esperado = casos_ref / pop_ref * pop_alvo,
           esp_lab  = lab_ref / pop_ref * pop_alvo,
           esp_nlab = nlab_ref / pop_ref * pop_alvo)

  por_faixa <- estratos |> group_by(nivel = as.character(faixa)) |>
    summarise(obs = sum(casos_alvo), esp = sum(esperado)) |> mutate(estrato = "Faixa")
  por_regiao <- estratos |> group_by(nivel = regiao) |>
    summarise(obs = sum(casos_alvo), esp = sum(esperado)) |> mutate(estrato = "Regiao")
  por_semestre <- estratos |> group_by(nivel = ifelse(mes <= 6, "s1", "s2")) |>
    summarise(obs = sum(casos_alvo), esp = sum(esperado)) |> mutate(estrato = "Semestre")
  por_criterio <- tibble(estrato = "Criterio", nivel = c("lab", "nlab"),
                         obs = c(sum(estratos$lab_alvo), sum(estratos$nlab_alvo)),
                         esp = c(sum(estratos$esp_lab), sum(estratos$esp_nlab)))

  decomposicao <- bind_rows(por_faixa, por_regiao, por_semestre, por_criterio) |>
    mutate(excesso = obs - esp) |>
    group_by(estrato) |>
    mutate(pct_excesso = 100 * excesso / sum(excesso)) |>
    ungroup()
  salvar_csv(decomposicao, paste0("decomposicao_excesso_", ano_alvo, ".csv"))

  message(ano_alvo, ": RR vs 2016–2019 = ", round(tabela$rr[1], 2))
}
