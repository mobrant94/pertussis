# ------------------------------------------------------------------------------
# 16_figuras_complementares.R — figuras de leitura clínica direta:
# taxas por 100.000 pessoas-ano e diferenças de taxas com IC95%, números absolutos
# de casos, sem escala log. Sem título no gráfico (a legenda fica no manuscrito).
#   clin1  Curva epidêmica: casos observados e esperados (IC95% de Poisson)
#   clin2  Incidência por faixa etária, 2016–2019, 2024 e 2025 (IC95% exato)
#   clin3  Diferença de incidência 2024 − 2016–2019 por tercil de não vacinados, por região
#   clin4  Idem, por cobertura da ESF (se houver dados da ESF)
#   clin5  Lactentes < 6 meses: diferença de incidência por cobertura de dTpa, por região
#   clin6  Cobertura de DTP 2019–2023 segundo a cobertura da ESF (média ponderada, IC95%)
#   clin7  Diagrama de mediação ESF -> não vacinados -> casos (script 12)
#   clin8  Casos evitáveis em 2024 (script 13)
#   clin9  Sensibilidade à menor detecção no Norte e no Nordeste (script 14)
# ------------------------------------------------------------------------------
source("R/00_configuracao.R")
fmt <- function(x, d = 0) format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, trim = TRUE)
num <- label_number(big.mark = ".", decimal.mark = ",")

painel <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cob    <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))
f_aps  <- file.path(pasta_proc, "aps_mun.rds")
anos_pos <- intersect(anos_alvo, unique(painel$ano))
ROT_PER <- c("2016–2019", "jul/2024–dez/2025", as.character(anos_pos))
COR_PER <- setNames(c("grey55", "#67001F", "#D6604D", "#F4A582")[seq_along(ROT_PER)], ROT_PER)
REG <- c("Sul", "Sudeste", "Centro-Oeste", "Nordeste", "Norte")
tema_c <- tema + theme(panel.grid.major.y = element_line(colour = "grey92", linewidth = .3))
ic_taxa <- function(casos, pa) {        # IC95% exato de Poisson para taxa por 100.000
  tibble(lo = 1e5 * qchisq(.025, 2 * casos) / 2 / pa, hi = 1e5 * qchisq(.975, 2 * (casos + 1)) / 2 / pa)
}

# ------------------------------------------------------------------ clin1: curva epidêmica
taxa_ref <- painel |> filter(ano %in% anos_referencia) |> group_by(faixa, mes) |>
  summarise(tx = sum(casos) / sum(pop), .groups = "drop")
m <- painel |> filter(ano >= 2016) |> group_by(faixa, data, ano, mes) |>
  summarise(casos = sum(casos), pop = sum(pop), .groups = "drop") |>
  left_join(taxa_ref, by = c("faixa", "mes")) |>
  mutate(esp = tx * pop, esp_lo = qpois(.025, esp), esp_hi = qpois(.975, esp),
         faixa = factor(nomes_faixas[as.character(faixa)], levels = nomes_faixas),
         periodo = case_when(data >= as.Date(janelas$inicio[janelas$nome == janela_principal]) &
                               data <= as.Date(janelas$fim[janelas$nome == janela_principal]) ~ "Janela",
                             ano >= 2020 & ano <= 2023 ~ "Pandemia", TRUE ~ "Referência"))
p1 <- ggplot(m, aes(data)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2023-12-31"), ymin = -Inf, ymax = Inf, fill = "grey94") +
  geom_ribbon(aes(ymin = esp_lo, ymax = esp_hi), fill = "grey80", alpha = .7) +
  geom_col(aes(y = casos, fill = periodo), width = 25) +
  geom_line(aes(y = esp), linetype = "22", linewidth = .45, colour = "grey15") +
  facet_wrap(~ faixa, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Referência" = "grey55", "Pandemia" = "grey70", "Janela" = "#B2182B"), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = .01)) +
  scale_y_continuous(labels = num, expand = expansion(mult = c(0, .08))) +
  labs(x = "Mês de início dos sintomas", y = "Casos confirmados por mês") + tema_c
salvar_figura(p1, "clin1_curva_epidemica", 10, 7)

# ------------------------------------------------------------------ clin2: incidência por faixa etária
ini_j <- as.Date(janelas$inicio[janelas$nome == janela_principal]); fim_j <- as.Date(janelas$fim[janelas$nome == janela_principal])
per <- bind_rows(
  painel |> mutate(per = case_when(ano %in% anos_referencia ~ "2016–2019", ano %in% anos_pos ~ as.character(ano))),
  painel |> filter(data >= ini_j, data <= fim_j) |> mutate(per = "jul/2024–dez/2025")) |>
  filter(!is.na(per)) |>
  group_by(faixa, per) |> summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
  mutate(taxa = 1e5 * casos / pa, ic_taxa(casos, pa),
         faixa = factor(nomes_faixas[as.character(faixa)], levels = nomes_faixas), per = factor(per, levels = ROT_PER))
p2 <- ggplot(per, aes(faixa, taxa, colour = per)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = .55), size = .35, linewidth = .6) +
  geom_text(aes(label = fmt(taxa, 1), y = hi), position = position_dodge(width = .55), vjust = -.6, size = 2.7, show.legend = FALSE) +
  scale_colour_manual(values = COR_PER, name = NULL,
                      labels = function(x) ifelse(x == "2025", "2025 (preliminar)", ifelse(x == "2016–2019", "2016–2019 (média anual)", x))) +
  scale_y_continuous(labels = num, limits = c(0, NA), expand = expansion(mult = c(0, .12))) +
  labs(x = "Faixa etária", y = "Incidência (casos por 100.000 pessoas-ano)") + tema_c
salvar_figura(p2, "clin2_incidencia_faixa", 7.5, 4.5)

# ------------------------------------------------------------------ clin3–5: diferença de incidência por categoria, por região
dif_por <- function(var, lactentes = FALSE) {
  recorte(painel, janela_principal) |> filter(if (lactentes) faixa == "lt_6m" else faixa != "lt_6m") |>
    left_join(var, by = "code_muni") |> filter(!is.na(grupo_exp)) |>
    mutate(per = if_else(alvo == 1, "p24", "ref")) |>
    group_by(regiao, grupo_exp, per) |> summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
    pivot_wider(names_from = per, values_from = c(casos, pa)) |>
    mutate(dif = 1e5 * (casos_p24 / pa_p24 - casos_ref / pa_ref),
           ep = 1e5 * sqrt(casos_p24 / pa_p24^2 + casos_ref / pa_ref^2),
           lo = dif - 1.96 * ep, hi = dif + 1.96 * ep,
           regiao = factor(regiao, levels = REG))
}
graf_dif <- function(d, legenda, cores, arquivo, ylab) {
  p <- ggplot(d, aes(grupo_exp, dif, colour = grupo_exp)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = .4) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = .35, linewidth = .7) +
    facet_wrap(~ regiao, nrow = 1) +
    scale_colour_manual(values = cores, name = legenda) +
    scale_y_continuous(labels = num) +
    labs(x = NULL, y = ylab) + tema_c +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  salvar_figura(p, arquivo, 10, 4.4)
  write.csv(d, file.path(pasta_saidas, paste0(arquivo, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
}
YLAB <- "Diferença de incidência, jul/2024–dez/2025 − 2016–2019\n(casos por 100.000 pessoas-ano)"
nv <- cob |> filter(!is.na(ter_naovac)) |>
  transmute(code_muni, grupo_exp = factor(c("T1 (menor)" = "Tercil inferior", "T2" = "Tercil intermediário",
                                            "T3 (maior)" = "Tercil superior")[as.character(ter_naovac)],
                                          levels = c("Tercil inferior", "Tercil intermediário", "Tercil superior")))
graf_dif(dif_por(nv), "Proporção de crianças não vacinadas\n(DTP, coortes 2019–2023)",
         c("#9ECAE1", "#4292C6", "#08306B"), "clin3_dif_incidencia_nao_vacinados", YLAB)
tpa <- cob |> filter(!is.na(ter_dtpa)) |>
  transmute(code_muni, grupo_exp = factor(c("Menor cobertura (T1)" = "Tercil inferior", "Intermediária (T2)" = "Tercil intermediário",
                                            "Maior cobertura (T3)" = "Tercil superior")[as.character(ter_dtpa)],
                                          levels = c("Tercil inferior", "Tercil intermediário", "Tercil superior")))
graf_dif(dif_por(tpa, lactentes = TRUE), "Cobertura de dTpa em gestantes\n(2023–2024)",
         c("#FDAE6B", "#E6550D", "#7F2704"), "clin5_dif_incidencia_lactentes_dtpa",
         "Diferença de incidência em < 6 meses, jul/2024–dez/2025 − 2016–2019\n(casos por 100.000 pessoas-ano)")

if (file.exists(f_aps)) {
  aps <- readRDS(f_aps) |> filter(!is.na(cat_aps)) |>
    transmute(code_muni, grupo_exp = factor(as.character(cat_aps), levels = c("< 50%", "50–79,9%", "≥ 80%")))
  graf_dif(dif_por(aps), "Cobertura da ESF\n(2017–2020)", c("#A1D99B", "#41AB5D", "#00441B"), "clin4_dif_incidencia_esf", YLAB)

  # ---------------------------------------------------------------- clin6: ESF e cobertura de DTP
  ec <- cob |> inner_join(readRDS(f_aps) |> select(code_muni, cat_aps), by = "code_muni") |>
    mutate(regiao = regiao_do_municipio(code_muni), w = nv_1923, y = pmin(dtp_pan, 100)) |>
    filter(!is.na(y), !is.na(cat_aps), w > 0)
  resume <- function(d) d |> summarise(media = weighted.mean(y, w), n = n(),
    ep = sqrt(sum(w^2 * (y - weighted.mean(y, w))^2)) / sum(w), .groups = "drop")
  ecs <- bind_rows(ec |> group_by(regiao, cat_aps) |> resume(), ec |> group_by(cat_aps) |> resume() |> mutate(regiao = "Brasil")) |>
    mutate(lo = media - 1.96 * ep, hi = media + 1.96 * ep,
           regiao = factor(regiao, levels = c("Brasil", REG)),
           cat_aps = factor(as.character(cat_aps), levels = c("< 50%", "50–79,9%", "≥ 80%")))
  p6 <- ggplot(ecs, aes(cat_aps, media, colour = cat_aps)) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = .35, linewidth = .7) +
    geom_text(aes(label = fmt(media, 0), y = hi), vjust = -.7, size = 2.7, show.legend = FALSE) +
    facet_wrap(~ regiao, nrow = 1) +
    scale_colour_manual(values = c("#A1D99B", "#41AB5D", "#00441B"), name = "Cobertura da ESF\n(2017–2020)") +
    scale_y_continuous(limits = c(50, 100), labels = num) +
    labs(x = NULL, y = "Cobertura de DTP, 2019–2023 (%)") + tema_c +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  salvar_figura(p6, "clin6_esf_cobertura_dtp", 10, 4.2)

  # ---------------------------------------------------------------- clin7: diagrama de mediação
  f_med <- file.path(pasta_saidas, "mediacao_esf.csv"); f_ev <- file.path(pasta_saidas, "esf_cobertura_vacinal.csv")
  if (file.exists(f_med) && file.exists(f_ev)) {
    md <- read.csv(f_med, encoding = "UTF-8"); ev <- read.csv(f_ev, encoding = "UTF-8") |> filter(desfecho == "nao_vac") |> slice(1)
    g <- function(e, v) md[[v]][md$efeito == e]
    rr_t <- function(e) paste0("RR ", fmt(g(e, "irr"), 2), " (IC95% ", fmt(g(e, "irr_lo"), 2), "–", fmt(g(e, "irr_hi"), 2), ")")
    a_t <- paste0("a: ", sub("-", "−", fmt(5 * ev$b, 1)), " p.p.\n(IC95% ", sub("-", "−", fmt(5 * ev$lo, 1)), " a ", sub("-", "−", fmt(5 * ev$hi, 1)), ")")
    f_sl <- file.path(pasta_saidas, "sensibilidade_laboratorio.csv")
    bb <- if (file.exists(f_sl)) read.csv(f_sl, encoding = "UTF-8") |>
      filter(exposicao == "Não vacinados 2019–2023", desfecho == "Todos os confirmados") |> slice(1) else NULL
    b_t <- if (!is.null(bb) && nrow(bb)) paste0("b: RR ", fmt(bb$irr, 2), " por 10 p.p.\n(IC95% ", fmt(bb$lo, 2), "–", fmt(bb$hi, 2), ")") else
      paste0("b: RR ", fmt(g("total", "b_irr10"), 2), "\npor 10 p.p.")
    caixas <- tibble(x = c(0, 5, 10), y = c(0, 2.2, 0),
                     txt = c("Cobertura da ESF\n(+50 p.p.)", "Crianças não vacinadas\n(DTP, coortes 2019–2023)", "Excesso de coqueluche\nem jul/2024–dez/2025\n(6 m–4 anos)"))
    setas <- tibble(x = c(1.4, 6.4, 1.6), y = c(.45, 1.75, 0), xend = c(3.6, 8.6, 8.4), yend = c(1.75, .45, 0))
    rot <- tibble(x = c(1.7, 8.3, 5, 5), y = c(1.35, 1.35, .35, -.9), hj = c(1, 0, .5, .5),
                  txt = c(a_t, b_t, paste0("c': ", rr_t("direto")),
                          paste0("Efeito indireto (a × b): ", rr_t("indireto"), "\nEfeito total (c): ", rr_t("total"))))
    p7 <- ggplot() +
      geom_segment(data = setas, aes(x, y, xend = xend, yend = yend), linewidth = .55,
                   arrow = arrow(length = unit(.17, "cm"), type = "closed"), colour = "grey25") +
      geom_label(data = caixas, aes(x, y, label = txt), size = 3.1, label.padding = unit(.45, "lines"), fill = "white") +
      geom_text(data = rot, aes(x, y, label = txt, hjust = hj), size = 2.9, colour = "grey10", lineheight = .95) +
      coord_cartesian(xlim = c(-1.6, 11.6), ylim = c(-1.5, 2.8)) + theme_void(base_size = 10)
    salvar_figura(p7, "clin7_mediacao_esf", 8.5, 4)
  }
}

# ------------------------------------------------------------------ clin8: casos e fração atribuíveis
f_atr <- file.path(pasta_saidas, "casos_atribuiveis_2024.csv")
if (file.exists(f_atr)) {
  at <- read.csv(f_atr, encoding = "UTF-8") |>
    mutate(rot = c(lt_6m = "< 6 meses\n(dTpa em gestantes ≥ 80%)", `6m_4a` = "6 meses a 4 anos\n(não vacinados no nível de 2010–2014)")[grupo],
           pct = 100 * evitaveis / casos_2024, pct_lo = 100 * evit_lo / casos_2024, pct_hi = 100 * evit_hi / casos_2024,
           rot_txt = paste0(fmt(evitaveis), " casos (IC95% ", fmt(evit_lo), "–", fmt(evit_hi), ")\nde ", fmt(casos_2024), " confirmados"))
  p8 <- ggplot(at, aes(pct, rot)) +
    geom_col(width = .5, fill = "#B2182B") +
    geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = .15, colour = "grey20") +
    geom_text(aes(x = pct_hi, label = rot_txt), hjust = -.08, size = 2.9, colour = "grey15", lineheight = .95) +
    scale_x_continuous(labels = function(x) paste0(fmt(x), "%"), expand = expansion(mult = c(0, .55))) +
    labs(x = "Fração dos casos confirmados em jul/2024–dez/2025 atribuível à lacuna vacinal (%)", y = NULL) + tema_c +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92", linewidth = .3))
  salvar_figura(p8, "clin8_fracao_atribuivel", 8.5, 3.4)
}
# ------------------------------------------------------------------ clin9: subdetecção no Norte e no Nordeste
f_sm <- file.path(pasta_saidas, "subdeteccao_magnitude.csv"); f_sa <- file.path(pasta_saidas, "subdeteccao_modelos.csv")
if (file.exists(f_sm) && file.exists(f_sa)) {
  ORD_C <- c("Sem correção", "Fator fixo 1,5×", "Fator fixo 2,0×", "Investigação no nível de 2016–2019",
             "Fator fixo 3,0×", "Mesma variação do Sul e do Sudeste")
  rot_c <- function(x) factor(x, levels = rev(intersect(ORD_C, unique(x))))
  sm <- read.csv(f_sm, encoding = "UTF-8") |> filter(ano == 9999, regiao %in% c("Norte", "Nordeste", "Brasil")) |>
    mutate(cenario = rot_c(cenario), regiao = factor(regiao, levels = c("Brasil", "Nordeste", "Norte")))
  pa9 <- ggplot(sm, aes(oe, cenario, colour = regiao)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = .6), size = .3, linewidth = .6) +
    scale_x_log10(labels = label_number(decimal.mark = ",", accuracy = .1)) +
    scale_colour_manual(values = c(Brasil = "grey20", Nordeste = "#E6550D", Norte = "#756BB1"), name = NULL) +
    labs(x = "Casos observados / esperados em jul/2024–dez/2025 (escala log)", y = NULL, tag = "A") + tema_c +
    theme(legend.position = "bottom")
  sa <- read.csv(f_sa, encoding = "UTF-8") |> filter(ano_alvo == 9999, exposicao == "Não vacinados 2019–2023") |>
    mutate(cenario = rot_c(cenario), comparacao = factor(comparacao, levels = c("Todos os municípios", "Mesma região")))
  pb9 <- ggplot(sa, aes(irr, cenario, colour = comparacao)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = .6), size = .3, linewidth = .6) +
    scale_x_log10(labels = label_number(decimal.mark = ",", accuracy = .01)) +
    scale_colour_manual(values = c("Todos os municípios" = "grey60", "Mesma região" = "#08306B"), name = NULL) +
    labs(x = "RR do aumento de casos por 10 p.p. de crianças não vacinadas", y = NULL, tag = "B") + tema_c +
    theme(legend.position = "bottom", axis.text.y = element_blank())
  salvar_figura(pa9 + pb9, "clin9_subdeteccao", 10.5, 4.6)
}
message("Figuras complementares prontas")
