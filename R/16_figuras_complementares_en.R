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
source("R/00_en.R")
num <- label_number(big.mark = ",", decimal.mark = ".", style_negative = "minus")

painel <- readRDS(file.path(pasta_proc, "painel_mensal.rds"))
cob    <- readRDS(file.path(pasta_proc, "cobertura_mun.rds"))
f_aps  <- file.path(pasta_proc, "aps_mun.rds")
anos_pos <- intersect(anos_alvo, unique(painel$ano))
ROT_PER <- c("2016–2019", "Jul 2024–Dec 2025", as.character(anos_pos))
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
  labs(x = "Month of symptom onset", y = "Confirmed cases per month") + tema_c
salvar_figura(p1, "clin1_curva_epidemica", 10, 7)

# ------------------------------------------------------------------ clin2: incidência por faixa etária
ini_j <- as.Date(janelas$inicio[janelas$nome == janela_principal]); fim_j <- as.Date(janelas$fim[janelas$nome == janela_principal])
per <- bind_rows(
  painel |> mutate(per = case_when(ano %in% anos_referencia ~ "2016–2019", ano %in% anos_pos ~ as.character(ano))),
  painel |> filter(data >= ini_j, data <= fim_j) |> mutate(per = "Jul 2024–Dec 2025")) |>
  filter(!is.na(per)) |>
  group_by(faixa, per) |> summarise(casos = sum(casos), pa = sum(pop) / 12, .groups = "drop") |>
  mutate(taxa = 1e5 * casos / pa, ic_taxa(casos, pa),
         faixa = factor(nomes_faixas[as.character(faixa)], levels = nomes_faixas), per = factor(per, levels = ROT_PER))
p2 <- ggplot(per, aes(faixa, taxa, colour = per)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = .55), size = .35, linewidth = .6) +
  geom_text(aes(label = fmt(taxa, 1), y = hi), position = position_dodge(width = .55), vjust = -.6, size = 2.7, show.legend = FALSE) +
  scale_colour_manual(values = COR_PER, name = NULL,
                      labels = function(x) ifelse(x == "2025", "2025 (preliminary)", ifelse(x == "2016–2019", "2016–2019 (annual mean)", x))) +
  scale_y_continuous(labels = num, limits = c(0, NA), expand = expansion(mult = c(0, .12))) +
  labs(x = "Age group", y = "Incidence (cases per 100,000 person-years)") + tema_c
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
           regiao = factor(reg_en(as.character(regiao)), levels = reg_en(REG)))
}
graf_dif <- function(d, legenda, cores, arquivo, ylab, rotulos = waiver(), linhas_leg = 1) {
  p <- ggplot(d, aes(grupo_exp, dif, colour = grupo_exp)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = .4) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = .35, linewidth = .7) +
    facet_wrap(~ regiao, nrow = 1) +
    scale_colour_manual(values = cores, name = legenda, labels = rotulos) +
    guides(colour = guide_legend(nrow = linhas_leg, title.position = "top", title.hjust = .5)) +
    scale_y_continuous(labels = num) +
    labs(x = NULL, y = ylab) + tema_c +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  salvar_figura(p, arquivo, 10, 5.2)
  write.csv(d, file.path(pasta_saidas, paste0(arquivo, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
}
TER <- c("Lower tertile", "Middle tertile", "Upper tertile")
rot_ter <- function(exp, cats) setNames(paste0(TER, " (", sub("^[^\n]*\n", "", rotulo_categoria(exp)[cats]), ")"), TER)
YLAB <- "Incidence difference\nJul 2024–Dec 2025 − 2016–2019\n(cases per 100,000 person-years)"
nv <- cob |> filter(!is.na(ter_naovac)) |>
  transmute(code_muni, grupo_exp = factor(c("T1 (menor)" = "Lower tertile", "T2" = "Middle tertile",
                                            "T3 (maior)" = "Upper tertile")[as.character(ter_naovac)],
                                          levels = c("Lower tertile", "Middle tertile", "Upper tertile")))
graf_dif(dif_por(nv), "Proportion of unvaccinated children\n(DTP, 2019–2023 birth cohorts)",
         c("#9ECAE1", "#4292C6", "#08306B"), "clin3_dif_incidencia_nao_vacinados", YLAB,
         rot_ter("nao_vac", c("T1 (menor)", "T2", "T3 (maior)")), linhas_leg = 3)
tpa <- cob |> filter(!is.na(ter_dtpa)) |>
  transmute(code_muni, grupo_exp = factor(c("Menor cobertura (T1)" = "Lower tertile", "Intermediária (T2)" = "Middle tertile",
                                            "Maior cobertura (T3)" = "Upper tertile")[as.character(ter_dtpa)],
                                          levels = c("Lower tertile", "Middle tertile", "Upper tertile")))
graf_dif(dif_por(tpa, lactentes = TRUE), "Maternal Tdap coverage\n(2023–2024)",
         c("#FDAE6B", "#E6550D", "#7F2704"), "clin5_dif_incidencia_lactentes_dtpa",
         "Incidence difference, < 6 months\nJul 2024–Dec 2025 − 2016–2019\n(cases per 100,000 person-years)",
         rot_ter("dtpa", c("Menor cobertura (T1)", "Intermediária (T2)", "Maior cobertura (T3)")), linhas_leg = 3)

if (file.exists(f_aps)) {
  aps <- readRDS(f_aps) |> filter(!is.na(cat_aps)) |>
    transmute(code_muni, grupo_exp = factor(as.character(cat_aps), levels = c("< 50%", "50–79,9%", "≥ 80%")))
  graf_dif(dif_por(aps), "FHS coverage\n(2017–2020)", c("#A1D99B", "#41AB5D", "#00441B"), "clin4_dif_incidencia_esf", YLAB,
           function(x) rotulo_esf()[x])

  # ---------------------------------------------------------------- clin6: ESF e cobertura de DTP
  ec <- cob |> inner_join(readRDS(f_aps) |> select(code_muni, cat_aps), by = "code_muni") |>
    mutate(regiao = regiao_do_municipio(code_muni), w = nv_1923, y = pmin(dtp_pan, 100)) |>
    filter(!is.na(y), !is.na(cat_aps), w > 0)
  resume <- function(d) d |> summarise(media = weighted.mean(y, w), n = n(),
    ep = sqrt(sum(w^2 * (y - weighted.mean(y, w))^2)) / sum(w), .groups = "drop")
  ecs <- bind_rows(ec |> group_by(regiao, cat_aps) |> resume(), ec |> group_by(cat_aps) |> resume() |> mutate(regiao = "Brasil")) |>
    mutate(lo = media - 1.96 * ep, hi = media + 1.96 * ep,
           regiao = factor(reg_en(as.character(regiao)), levels = reg_en(c("Brasil", REG))),
           cat_aps = factor(as.character(cat_aps), levels = c("< 50%", "50–79,9%", "≥ 80%")))
  p6 <- ggplot(ecs, aes(cat_aps, media, colour = cat_aps)) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = .35, linewidth = .7) +
    geom_text(aes(label = fmt(media, 0), y = hi), vjust = -.7, size = 2.7, show.legend = FALSE) +
    facet_wrap(~ regiao, nrow = 1) +
    scale_colour_manual(values = c("#A1D99B", "#41AB5D", "#00441B"), name = "FHS coverage (2017–2020)",
                        labels = function(x) rotulo_esf()[x]) +
    guides(colour = guide_legend(nrow = 1, title.position = "top", title.hjust = .5)) +
    scale_y_continuous(limits = c(50, 100), labels = num) +
    labs(x = NULL, y = "DTP coverage, 2019–2023 (%)") + tema_c +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  salvar_figura(p6, "clin6_esf_cobertura_dtp", 10, 4.8)

  # ---------------------------------------------------------------- clin7: diagrama de mediação
  f_med <- file.path(pasta_saidas, "mediacao_esf.csv"); f_ev <- file.path(pasta_saidas, "esf_cobertura_vacinal.csv")
  if (file.exists(f_med) && file.exists(f_ev)) {
    md <- read.csv(f_med, encoding = "UTF-8"); ev <- read.csv(f_ev, encoding = "UTF-8") |> filter(desfecho == "nao_vac") |> slice(1)
    g <- function(e, v) md[[v]][md$efeito == e]
    rr_t <- function(e) paste0("RR ", fmt(g(e, "irr"), 2), " (95% CI ", fmt(g(e, "irr_lo"), 2), "–", fmt(g(e, "irr_hi"), 2), ")")
    a_t <- paste0("a: ", fmt(5 * ev$b, 1), " pp\n(95% CI ", fmt(5 * ev$lo, 1), " to ", fmt(5 * ev$hi, 1), ")")
    f_sl <- file.path(pasta_saidas, "sensibilidade_laboratorio.csv")
    bb <- if (file.exists(f_sl)) read.csv(f_sl, encoding = "UTF-8") |>
      filter(exposicao == "Não vacinados 2019–2023", desfecho == "Todos os confirmados") |> slice(1) else NULL
    b_t <- if (!is.null(bb) && nrow(bb)) paste0("b: RR ", fmt(bb$irr, 2), " per 10 pp\n(95% CI ", fmt(bb$lo, 2), "–", fmt(bb$hi, 2), ")") else
      paste0("b: RR ", fmt(g("total", "b_irr10"), 2), "\nper 10 pp")
    # caixas: ESF (exposição), não vacinados (mediador) e excesso (desfecho)
    cx <- tibble(x = c(1.9, 6, 10.1), y = c(1, 4.3, 1), w = c(3.4, 4, 3.6), h = c(1.15, 1.15, 1.45),
                 borda = c("#238B45", "#2171B5", "#B2182B"), fundo = c("#EDF8E9", "#EFF3FF", "#FEE5D9"),
                 txt = c("FHS coverage\n(+50 pp)", "Unvaccinated children\n(DTP, 2019–2023 cohorts)",
                         "Increase in pertussis\nJul 2024–Dec 2025\n(6 months–4 years)"))
    setas <- tibble(x = c(2.7, 7.3, 3.65), y = c(1.6, 3.7, 1), xend = c(4.7, 9.2, 8.25), yend = c(3.7, 1.75, 1))
    rot <- tibble(x = c(3.45, 8.55, 5.95), y = c(2.75, 2.75, 1.38), hj = c(1, 0, .5),
                  txt = c(paste0("a\n", sub("^a: ", "", a_t)), paste0("b\n", sub("^b: ", "", b_t)), paste0("c' (direct effect): ", rr_t("direto"))))
    resumo <- paste0("Indirect effect (a × b): ", rr_t("indireto"), "     Total effect (c): ", rr_t("total"))
    p7 <- ggplot() +
      geom_rect(data = cx, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2),
                fill = cx$fundo, colour = cx$borda, linewidth = .6) +
      geom_text(data = cx, aes(x, y, label = txt), size = 3.4, lineheight = .95, fontface = "bold", colour = "grey10") +
      geom_segment(data = setas, aes(x, y, xend = xend, yend = yend), linewidth = .6, colour = "grey30",
                   arrow = arrow(length = unit(2.2, "mm"), type = "closed", angle = 22)) +
      geom_text(data = rot, aes(x, y, label = txt, hjust = hj), size = 3, lineheight = .95, colour = "grey10") +
      annotate("rect", xmin = .2, xmax = 11.9, ymin = -.75, ymax = -.05, fill = "grey95", colour = NA) +
      annotate("text", x = 6.05, y = -.4, label = resumo, size = 3, colour = "grey10") +
      coord_cartesian(xlim = c(.1, 12), ylim = c(-.8, 5), expand = FALSE) + theme_void(base_size = 10, base_family = FONTE)
    salvar_figura(p7, "clin7_mediacao_esf", 8.5, 4.4)
  }
}

# ------------------------------------------------------------------ clin8: casos e fração atribuíveis
f_atr <- file.path(pasta_saidas, "casos_atribuiveis_2024.csv")
if (file.exists(f_atr)) {
  at <- read.csv(f_atr, encoding = "UTF-8") |>
    mutate(rot = c(lt_6m = "< 6 months\n(maternal Tdap ≥ 80%)", `6m_4a` = "6 months–4 years\n(unvaccinated at 2010–2014 level)")[grupo],
           pct = 100 * evitaveis / casos_2024, pct_lo = 100 * evit_lo / casos_2024, pct_hi = 100 * evit_hi / casos_2024,
           rot_txt = "")
  at <- at |> mutate(rot = factor(rot, levels = rev(rot)),
                     txt_pct = paste0(fmt(pct, 1), " (", fmt(pct_lo, 1), " to ", fmt(pct_hi, 1), ")"),
                     txt_n = paste0(fmt(evitaveis), " (", fmt(evit_lo), " to ", fmt(evit_hi), ")"),
                     txt_tot = fmt(casos_2024))
  lim8 <- c(min(-5, floor(min(at$pct_lo) / 5) * 5), ceiling(max(at$pct_hi) / 5) * 5)
  p8a <- ggplot(at, aes(pct, rot, colour = grupo)) +
    geom_vline(xintercept = 0, colour = "grey45", linewidth = .4) +
    geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), height = .12, linewidth = .7) +
    geom_point(size = 3) +
    scale_colour_manual(values = c(lt_6m = "#E6550D", `6m_4a` = "#2171B5"), guide = "none") +
    scale_x_continuous(limits = lim8, breaks = seq(lim8[1], lim8[2], 5), labels = function(x) paste0(fmt(x), "%")) +
    labs(x = "Fraction attributable to the vaccination gap (%)", y = NULL) + tema_c +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92", linewidth = .3),
          axis.text.y = element_text(size = 9.5, lineheight = .95))
  col8 <- function(v, tit) ggplot(at, aes(0, rot)) + geom_text(aes(label = .data[[v]]), size = 3.2, colour = "grey10") +
    scale_x_continuous(limits = c(-1, 1)) + labs(title = tit, x = NULL, y = NULL) + theme_void(base_family = FONTE) +
    theme(plot.title = element_text(size = 9.5, face = "bold", hjust = .5, lineheight = .95, margin = margin(b = 4)))
  p8 <- p8a + col8("txt_pct", "Attributable fraction,\n% (95% CI)") + col8("txt_n", "Attributable cases\n(95% CI)") +
    col8("txt_tot", "Confirmed\ncases") + plot_layout(widths = c(2.2, 1, 1, .6))
  salvar_figura(p8, "clin8_fracao_atribuivel", 10, 2.9)
}
# ------------------------------------------------------------------ clin9: subdetecção no Norte e no Nordeste
f_sm <- file.path(pasta_saidas, "subdeteccao_magnitude.csv"); f_sa <- file.path(pasta_saidas, "subdeteccao_modelos.csv")
if (file.exists(f_sm) && file.exists(f_sa)) {
  ORD_C <- c("Sem correção", "Fator fixo 1,5×", "Fator fixo 2,0×", "Investigação no nível de 2016–2019",
             "Fator fixo 3,0×", "Mesma variação do Sul e do Sudeste")
  C_EN <- c("No correction", "Fixed factor 1.5×", "Fixed factor 2.0×", "Notification at 2016–2019 level",
            "Fixed factor 3.0×", "Same change as South and Southeast")
  rot_c <- function(x) factor(C_EN[match(x, ORD_C)], levels = rev(C_EN[ORD_C %in% unique(x)]))
  sm <- read.csv(f_sm, encoding = "UTF-8") |> filter(ano == 9999, regiao %in% c("Norte", "Nordeste", "Brasil")) |>
    mutate(cenario = rot_c(cenario), regiao = factor(reg_en(regiao), levels = c("Brazil", "Northeast", "North")))
  pa9 <- ggplot(sm, aes(oe, cenario, colour = regiao)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = .6), size = .3, linewidth = .6) +
    scale_x_log10(labels = label_number(decimal.mark = ".", accuracy = .1)) +
    scale_colour_manual(values = c(Brazil = "grey20", Northeast = "#E6550D", North = "#756BB1"), name = NULL) +
    labs(x = "Observed / expected cases\nJul 2024–Dec 2025 (log scale)", y = NULL, tag = "A") + tema_c +
    theme(legend.position = "bottom")
  sa <- read.csv(f_sa, encoding = "UTF-8") |> filter(ano_alvo == 9999, exposicao == "Não vacinados 2019–2023") |>
    mutate(cenario = rot_c(cenario), comparacao = factor(c("Todos os municípios" = "All municipalities", "Mesma região" = "Same region")[comparacao], levels = c("All municipalities", "Same region")))
  pb9 <- ggplot(sa, aes(irr, cenario, colour = comparacao)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = .6), size = .3, linewidth = .6) +
    scale_x_log10(labels = label_number(decimal.mark = ".", accuracy = .01)) +
    scale_colour_manual(values = c("All municipalities" = "grey60", "Same region" = "#08306B"), name = NULL) +
    labs(x = "RR of the increase per 10 pp\nof unvaccinated children", y = NULL, tag = "B") + tema_c +
    theme(legend.position = "bottom", axis.text.y = element_blank())
  salvar_figura(pa9 + pb9, "clin9_subdeteccao", 10.5, 4.6)
}
message("English complementary figures ready")
