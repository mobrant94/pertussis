# ------------------------------------------------------------------------------
# 02_analise_raca_cor.R
# Incidência de coqueluche em menores de 5 anos por raça/cor, com denominadores
# específicos por raça/cor reconstruídos a partir dos nascidos vivos (SINASC).
#
# Denominador: os nascidos vivos de cada município e mês (os mesmos do artigo,
# dados/processados/nascidos_mun_mes.rds) são divididos entre os grupos de raça/cor
# segundo a distribuição observada nos microdados do SINASC naquele município e ano
# (nascidos com raça/cor ignorada distribuídos proporcionalmente; municípios com menos
# de 20 nascidos com raça/cor informada no ano usam a distribuição da UF). A população
# de cada grupo é então obtida pelo mesmo método de componentes de coorte do artigo
# (03_populacao_em_risco.R), com a mortalidade da UF e do ano de nascimento; assim, a soma
# dos grupos reproduz o denominador do artigo.
#
# Casos: casos confirmados em menores de 5 anos do SINAN, com os mesmos critérios do
# artigo. Na análise principal, os casos com raça/cor ignorada são redistribuídos entre
# os grupos na proporção dos casos com raça/cor informada da mesma UF, período e faixa
# etária; na análise de sensibilidade, são excluídos.
#
# Períodos: 2016–2019 (referência) e julho de 2024 a dezembro de 2025 (onda).
# Entrada: dados/processados/nascidos_mun_mes_raca.rds (gerado por 01_baixar_sinasc_raca.R)
# Saídas:  saidas/raca_cor/*.csv e saidas/raca_cor/fig_incidencia_raca.png
# Pacotes: dplyr, tidyr, lubridate, read.dbc, ggplot2
# ------------------------------------------------------------------------------
setwd("C:/Users/MARCOS.ANTUNES/OneDrive - PUCRS - BR/Documentações - Marcos/Área de Trabalho/Meus artigos/Coqueluche/artigo_coqueluche_2024")

suppressMessages({library(dplyr); library(tidyr); library(lubridate); library(ggplot2)})
SAI <- "saidas/raca_cor"; dir.create(SAI, showWarnings = FALSE, recursive = TRUE)
RACAS <- c("Branca", "Preta", "Parda", "Indígena", "Amarela")
REG <- c("1" = "Norte", "2" = "Nordeste", "3" = "Sudeste", "4" = "Sul", "5" = "Centro-Oeste")
FAIXAS <- c(lt_6m = "< 6 meses", `6_11m` = "6–11 meses", `1_4a` = "1–4 anos")
INI_REF <- as.Date("2016-01-01"); FIM_REF <- as.Date("2019-12-01")
INI_ON  <- as.Date("2024-07-01"); FIM_ON  <- as.Date("2025-12-01")
periodo_de <- function(d) case_when(d >= INI_REF & d <= FIM_REF ~ "2016–2019",
                                    d >= INI_ON & d <= FIM_ON ~ "jul/2024–dez/2025")

# 1. Proporção de nascidos por raça/cor em cada município e ano ------------------
municipios <- unique(readRDS("dados/processados/pop_mensal_nv.rds")$code_muni)
nascidos <- readRDS("dados/processados/nascidos_mun_mes.rds") |>
  filter(!grepl("0000$", code_muni)) |> mutate(uf = substr(code_muni, 1, 2))
nr <- readRDS("dados/processados/nascidos_mun_mes_raca.rds")
anos_raca <- sort(unique(nr$ano))
message("Anos com microdados do SINASC: ", paste(range(anos_raca), collapse = "–"))

conh <- nr |> filter(raca %in% RACAS) |> group_by(code_muni, ano, raca) |> summarise(nv = sum(nv), .groups = "drop")
p_mun <- conh |> group_by(code_muni, ano) |> mutate(total = sum(nv), p = nv / total) |> ungroup() |>
  filter(total >= 20) |> select(code_muni, ano, raca, p)
p_uf <- conh |> mutate(uf = substr(code_muni, 1, 2)) |> group_by(uf, ano, raca) |> summarise(nv = sum(nv), .groups = "drop") |>
  group_by(uf, ano) |> mutate(p = nv / sum(nv)) |> ungroup() |> select(uf, ano, raca, p_uf = p)

# anos sem microdados usam o ano disponível mais próximo
mapa_anos <- tibble(ano = 2011:2025) |> mutate(ano_r = sapply(ano, function(x) anos_raca[which.min(abs(anos_raca - x))]))
base <- nascidos |> filter(ano >= 2011) |> left_join(mapa_anos, by = "ano") |>
  crossing(raca = RACAS) |>
  left_join(p_mun |> rename(ano_r = ano), by = c("code_muni", "ano_r", "raca")) |>
  left_join(p_uf |> rename(ano_r = ano), by = c("uf", "ano_r", "raca")) |>
  group_by(code_muni, ano, mes) |>
  mutate(usa_uf = all(is.na(p)), p = ifelse(usa_uf, coalesce(p_uf, 0), coalesce(p, 0))) |>
  ungroup() |>
  mutate(nv_raca = nv * p)

# 2. População em risco por UF, mês, faixa etária e raça/cor (componentes de coorte) --
obitos <- readRDS("dados/processados/obitos_uf_ano.rds")
razoes <- nascidos |> group_by(uf, ano) |> summarise(nv = sum(nv), .groups = "drop") |>
  left_join(obitos, by = c("uf", "ano")) |>
  mutate(q_neo = d_neo / nv, q_pn = d_pn / nv, q_14 = coalesce(d_14, 0) / nv) |>
  select(uf, ano, q_neo, q_pn, q_14)
meses <- seq(as.Date("2011-01-01"), as.Date("2025-12-01"), by = "month")
grade <- base |> filter(code_muni %in% municipios) |>
  mutate(dt = make_date(ano, mes, 1)) |>
  group_by(uf, raca, dt) |> summarise(nv = sum(nv_raca), .groups = "drop") |>
  complete(uf, raca, dt = meses, fill = list(nv = 0)) |>
  mutate(ano = year(dt)) |> left_join(razoes, by = c("uf", "ano")) |>
  arrange(uf, raca, dt)
grade$lt_6m <- 0; grade$`6_11m` <- 0; grade$`1_4a` <- 0
for (a in 0:59) {
  f_neo <- ifelse(a == 0, 0.5, 1); f_pn <- ifelse(a == 0, 0, ifelse(a <= 11, (a - 0.5) / 11, 1)); f_14 <- ifelse(a <= 11, 0, (a - 11.5) / 48)
  s <- grade |> mutate(s = nv * (1 - f_neo * q_neo - f_pn * q_pn - f_14 * q_14)) |>
    group_by(uf, raca) |> mutate(s = lag(s, a, default = 0)) |> pull(s)
  if (a <= 5) grade$lt_6m <- grade$lt_6m + s else if (a <= 11) grade$`6_11m` <- grade$`6_11m` + s else grade$`1_4a` <- grade$`1_4a` + s
}
pop <- grade |> mutate(periodo = periodo_de(dt)) |> filter(!is.na(periodo)) |>
  pivot_longer(c(lt_6m, `6_11m`, `1_4a`), names_to = "faixa", values_to = "pop") |>
  group_by(uf, raca, periodo, faixa) |> summarise(pa = sum(pmax(pop, 0)) / 12, .groups = "drop")

# conferência: a soma dos grupos deve reproduzir o denominador do artigo
art <- readRDS("dados/processados/pop_mensal_nv.rds") |> mutate(periodo = periodo_de(as.Date(data))) |>
  filter(!is.na(periodo)) |> group_by(periodo) |> summarise(pa_artigo = sum(pop) / 12)
conf <- pop |> group_by(periodo) |> summarise(pa_raca = sum(pa)) |> left_join(art, by = "periodo") |>
  mutate(razao = pa_raca / pa_artigo)
write.csv(conf, file.path(SAI, "conferencia_denominador.csv"), row.names = FALSE); print(conf)

# 3. Casos confirmados por raça/cor -------------------------------------------------
le <- function(a) read.dbc::read.dbc(sprintf("dados_brutos/COQUBR%02d.dbc", a %% 100)) |>
  mutate(across(everything(), as.character)) |>
  select(any_of(c("DT_NOTIFIC", "ID_MUNICIP", "ID_UNIDADE", "DT_SIN_PRI", "ANO_NASC", "NU_IDADE_N", "CS_SEXO",
                  "ID_MN_RESI", "CLASSI_FIN", "CRITERIO", "EVOLUCAO", "CS_RACA", "CS_HOSPITA")))
casos <- bind_rows(lapply(2010:2025, le)) |>
  distinct(DT_NOTIFIC, ID_MUNICIP, ID_UNIDADE, DT_SIN_PRI, ANO_NASC, NU_IDADE_N, CS_SEXO, ID_MN_RESI, .keep_all = TRUE) |>
  mutate(idade = sprintf("%04d", as.integer(NU_IDADE_N)), un = as.integer(substr(idade, 1, 1)), v = as.integer(substr(idade, 2, 4)),
         meses = case_when(un %in% 1:2 ~ 0, un == 3 ~ v, un == 4 ~ v * 12),
         data = floor_date(as.Date(DT_SIN_PRI), "month"), code_muni = substr(ID_MN_RESI, 1, 6)) |>
  filter(CLASSI_FIN == "1", meses < 60, code_muni %in% municipios) |>
  mutate(periodo = periodo_de(data), uf = substr(code_muni, 1, 2),
         faixa = case_when(meses < 6 ~ "lt_6m", meses < 12 ~ "6_11m", TRUE ~ "1_4a"),
         raca = recode(CS_RACA, `1` = "Branca", `2` = "Preta", `3` = "Amarela", `4` = "Parda", `5` = "Indígena", .default = "Ignorada", .missing = "Ignorada"),
         hosp = as.integer(CS_HOSPITA %in% "1"), obito = as.integer(EVOLUCAO %in% "2"), lab = as.integer(CRITERIO %in% "1")) |>
  filter(!is.na(periodo))
write.csv(casos |> count(periodo, raca) |> group_by(periodo) |> mutate(pct = round(100 * n / sum(n), 1)),
          file.path(SAI, "casos_por_raca.csv"), row.names = FALSE)

agg <- casos |> group_by(uf, periodo, faixa, raca) |>
  summarise(casos = n(), hosp = sum(hosp), obitos = sum(obito), lab = sum(lab), .groups = "drop")

# redistribuição dos casos com raça/cor ignorada (na proporção dos informados da mesma UF, período e faixa)
redistribuir <- function(agg) {
  ign <- agg |> filter(raca == "Ignorada") |> select(uf, periodo, faixa, ci = casos, hi = hosp, oi = obitos, li = lab)
  inf <- agg |> filter(raca != "Ignorada") |> group_by(uf, periodo, faixa) |>
    mutate(pc = casos / sum(casos), ph = ifelse(sum(hosp) > 0, hosp / sum(hosp), pc),
           po = ifelse(sum(obitos) > 0, obitos / sum(obitos), pc), pl = ifelse(sum(lab) > 0, lab / sum(lab), pc)) |> ungroup()
  inf |> left_join(ign, by = c("uf", "periodo", "faixa")) |>
    mutate(across(c(ci, hi, oi, li), ~ coalesce(.x, 0)),
           casos = casos + ci * pc, hosp = hosp + hi * ph, obitos = obitos + oi * po, lab = lab + li * pl) |>
    select(uf, periodo, faixa, raca, casos, hosp, obitos, lab)
}
dados <- list(Principal = redistribuir(agg), `Excluindo ignorados` = agg |> filter(raca != "Ignorada"))

# 4. Taxas e razões de taxas ------------------------------------------------------
ic_taxa <- function(x, pa, k = 1e5) tibble(taxa = k * x / pa, lo = k * qchisq(.025, 2 * x) / 2 / pa, hi = k * qchisq(.975, 2 * (x + 1)) / 2 / pa)
com_negra <- function(d) bind_rows(d, d |> filter(raca %in% c("Preta", "Parda")) |> mutate(raca = "Negra (preta + parda)") |>
                                     group_by(across(-c(casos, hosp, obitos, lab, pa))) |> summarise(across(c(casos, hosp, obitos, lab, pa), sum), .groups = "drop"))
resultados <- list(); modelos <- list()
for (cen in names(dados)) {
  d <- dados[[cen]] |> full_join(pop, by = c("uf", "periodo", "faixa", "raca")) |>
    mutate(across(c(casos, hosp, obitos, lab), ~ coalesce(.x, 0)), regiao = REG[substr(uf, 1, 1)]) |>
    filter(raca %in% RACAS, pa > 0)
  # taxas por raça/cor, período e faixa etária (Brasil)
  tx <- bind_rows(d |> mutate(faixa = "Total"), d) |> select(-uf, -regiao) |>
    group_by(periodo, faixa, raca) |> summarise(across(c(casos, hosp, obitos, lab, pa), sum), .groups = "drop") |> com_negra()
  tx <- bind_cols(tx, ic_taxa(tx$casos, tx$pa)) |>
    mutate(taxa_hosp = 1e5 * hosp / pa, mortalidade_por_milhao = 1e6 * obitos / pa, taxa_lab = 1e5 * lab / pa, cenario = cen)
  resultados[[cen]] <- tx
  # razões de taxas: quasi-Poisson com offset (UF x faixa x raça/cor x período), ajustado por faixa e região
  d2 <- d |> filter(raca %in% c("Branca", "Preta", "Parda", "Indígena")) |>
    mutate(raca = factor(raca, c("Branca", "Preta", "Parda", "Indígena")), periodo = factor(periodo, c("2016–2019", "jul/2024–dez/2025")))
  for (desf in c("casos", "hosp", "obitos")) {
    for (per in levels(d2$periodo)) {
      m <- glm(as.formula(paste(desf, "~ raca + faixa + regiao")), offset = log(pa), family = quasipoisson(), data = d2 |> filter(periodo == per))
      cf <- summary(m)$coefficients; k <- grep("^raca", rownames(cf))
      modelos[[length(modelos) + 1]] <- tibble(cenario = cen, desfecho = desf, comparacao = paste("vs Branca,", per),
        grupo = sub("raca", "", rownames(cf)[k]), rr = exp(cf[k, 1]), lo = exp(cf[k, 1] - 1.96 * cf[k, 2]), hi = exp(cf[k, 1] + 1.96 * cf[k, 2]))
    }
    # aumento da onda em relação a 2016–2019 dentro de cada grupo
    for (g in levels(d2$raca)) {
      m <- glm(as.formula(paste(desf, "~ periodo + faixa + regiao")), offset = log(pa), family = quasipoisson(), data = d2 |> filter(raca == g))
      cf <- summary(m)$coefficients; k <- grep("^periodo", rownames(cf))
      modelos[[length(modelos) + 1]] <- tibble(cenario = cen, desfecho = desf, comparacao = "Onda vs 2016–2019", grupo = g,
        rr = exp(cf[k, 1]), lo = exp(cf[k, 1] - 1.96 * cf[k, 2]), hi = exp(cf[k, 1] + 1.96 * cf[k, 2]))
    }
  }
  # taxas por região na onda
  if (cen == "Principal") {
    rg <- d |> group_by(periodo, regiao, raca) |> summarise(casos = sum(casos), pa = sum(pa), .groups = "drop")
    write.csv(bind_cols(rg, ic_taxa(rg$casos, rg$pa)) |> mutate(across(where(is.numeric), ~ round(.x, 2))),
              file.path(SAI, "taxas_raca_regiao.csv"), row.names = FALSE)
  }
}
taxas <- bind_rows(resultados) |> mutate(faixa = recode(faixa, !!!FAIXAS)) |> mutate(across(where(is.numeric), ~ round(.x, 2)))
rrs <- bind_rows(modelos) |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write.csv(taxas, file.path(SAI, "taxas_raca.csv"), row.names = FALSE)
write.csv(rrs, file.path(SAI, "razoes_taxas_raca.csv"), row.names = FALSE)
write.csv(nr |> group_by(ano) |> summarise(pct_ignorada = round(100 * sum(nv[raca == "Ignorada"]) / sum(nv), 1)),
          file.path(SAI, "sinasc_raca_ignorada.csv"), row.names = FALSE)

# 5. Figura -----------------------------------------------------------------------
fig <- taxas |> filter(cenario == "Principal", raca %in% c("Branca", "Preta", "Parda", "Indígena")) |>
  mutate(faixa = factor(faixa, c("Total", FAIXAS)), raca = factor(raca, c("Branca", "Preta", "Parda", "Indígena")))
p <- ggplot(fig, aes(raca, taxa, colour = periodo)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(.5), size = .35) +
  facet_wrap(~ faixa, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = c("2016–2019" = "grey55", "jul/2024–dez/2025" = "#B2182B"), name = NULL) +
  labs(x = NULL, y = "Casos por 100.000 pessoas-ano") +
  theme_classic(base_size = 10) + theme(legend.position = "bottom", strip.background = element_blank(), strip.text = element_text(face = "bold"))
ggsave(file.path(SAI, "fig_incidencia_raca.png"), p, width = 10, height = 4, dpi = 400, bg = "white")
message("Pronto: resultados em ", SAI)
