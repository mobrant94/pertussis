# ------------------------------------------------------------------------------
# 15_figuras_artigo_en.R
# English version of the figures produced by 15_figuras_artigo.R (folder figuras_en/).
# ------------------------------------------------------------------------------
source("R/00_configuracao.R")
source("R/00_en.R")

# Figura 1 — fluxograma ----------------------------------------------------------
fl <- read.csv(file.path(pasta_saidas, "fluxograma.csv"), encoding = "UTF-8"); n <- setNames(fl$n, fl$etapa)
g <- function(p) n[grep(p, names(n))][1]
cx <- tibble::tribble(~x, ~y, ~txt,
  0, 9, paste0(paste0("SINAN pertussis records, ", min(anos_casos), "–", max(anos_casos), "\nn = "), fmt(g("^Registros"))),
  0, 7, paste0(paste0("Symptom onset in ", min(anos_casos), "–", max(anos_casos), "\nn = "), fmt(g("^Registros") - g("^Início"))),
  0, 5, paste0("Eligible notifications, < 5 years\nn = ", fmt(g("^Notificações elegíveis"))),
  0, 3, paste0("Confirmed cases\nn = ", fmt(g("^Confirmados$"))),
  -2.2, 1, paste0("< 6 months\nn = ", fmt(g("Confirmados lt_6m"))),
  0, 1, paste0("6–11 months\nn = ", fmt(g("Confirmados 6_11m"))),
  2.2, 1, paste0("1–4 years\nn = ", fmt(g("Confirmados 1_4a"))))
ex <- tibble::tribble(~x, ~y, ~txt,
  3.1, 8, paste0("Excluded: missing date or outside the period\nn = ", fmt(g("^Início"))),
  3.1, 6, paste0("Excluded:\nage ≥ 5 years or unknown, n = ", fmt(g("^Idade")),
                 "\nduplicates, n = ", fmt(g("^Duplicidades")),
                 "\nunknown municipality or no denominator, n = ", fmt(g("^Município"))),
  3.1, 4, paste0("Not confirmed:\ndiscarded, n = ", fmt(g("^Descartados")),
                 "\ninconclusive, n = ", fmt(g("^Inconclusivos")),
                 "\nno final classification, n = ", fmt(g("^Sem classificação"))))
p1 <- ggplot() +
  geom_segment(aes(x = 0, xend = 0, y = c(8.55, 6.55, 4.55), yend = c(7.45, 5.45, 3.45)), arrow = arrow(length = unit(2, "mm"))) +
  geom_segment(aes(x = 0, xend = 1.55, y = c(8, 6, 4), yend = c(8, 6, 4)), arrow = arrow(length = unit(2, "mm"))) +
  geom_segment(aes(x = c(-2.2, 0, 2.2), xend = c(-2.2, 0, 2.2), y = 2.55, yend = 1.45), arrow = arrow(length = unit(2, "mm"))) +
  geom_segment(aes(x = -2.2, xend = 2.2, y = 2.55, yend = 2.55)) +
  geom_label(data = cx, aes(x, y, label = txt), size = 3.2, label.r = unit(0, "mm"), label.padding = unit(3, "mm"), family = "") +
  geom_label(data = ex, aes(x, y, label = txt), size = 2.9, label.r = unit(0, "mm"), fill = "grey96", family = "") +
  coord_cartesian(xlim = c(-3.4, 4.9), ylim = c(0.4, 9.6)) + theme_void()
# fluxograma: formato original (fonte sem serifa, 400 dpi), fora do padrão das demais figuras
ggsave(file.path(pasta_fig, "fig1_fluxograma.png"), p1, width = 9, height = 7.5, dpi = 400, bg = "white")
ggsave(file.path(pasta_fig, "fig1_fluxograma.tiff"), p1, width = 9, height = 7.5, dpi = 1000, compression = "lzw", bg = "white")
ggsave(file.path(pasta_fig, "fig1_fluxograma.pdf"), p1, width = 9, height = 7.5, device = cairo_pdf, bg = "white")

# Figura 2 — taxas anuais por faixa e por região -----------------------------------
tf <- read.csv(file.path(pasta_saidas, "taxa_anual_faixa.csv"), encoding = "UTF-8") |> mutate(faixa = factor(nomes_faixas[faixa], levels = nomes_faixas))
tr <- read.csv(file.path(pasta_saidas, "taxa_anual_regiao.csv"), encoding = "UTF-8") |> mutate(regiao = reg_en(regiao))
ax <- scale_x_continuous(breaks = min(anos_casos):max(anos_casos), labels = function(x) substr(x, 3, 4))
prel <- if (max(anos_casos) >= 2025) annotate("rect", xmin = 2024.5, xmax = max(anos_casos) + .5, ymin = -Inf, ymax = Inf,
                                             fill = "grey85", alpha = .5) else NULL   # dados preliminares
p2a <- ggplot(tf, aes(ano, taxa, colour = faixa)) + prel + geom_line(linewidth = .8) + geom_point(size = 1.4) +
  scale_y_log10(labels = label_number(big.mark = ",", decimal.mark = ".")) + ax +
  scale_colour_manual(values = c("#08306B", "#4292C6", "#9ECAE1"), name = NULL) + guides(colour = guide_legend(nrow = 2)) +
  labs(title = "A. By age group", x = "Year", y = "Cases per 100,000 person-years\n(log scale)") + tema
p2b <- ggplot(tr, aes(ano, taxa, colour = regiao)) + prel + geom_line(linewidth = .8) + geom_point(size = 1.4) + ax +
  scale_colour_brewer(palette = "Dark2", name = NULL) + guides(colour = guide_legend(nrow = 2)) +
  labs(title = "B. By region (< 5 years)", x = "Year", y = "Cases per 100,000 person-years") + tema
salvar_figura(p2a + p2b, "fig2_taxas_anuais", 10, 4.4)

# Figura 3 — RR mensal de 2024 vs 2016–2019 -----------------------------------------
meses <- month.abb
rm <- bind_rows(ler_csv("rr_mensal_2024.csv") |> mutate(ano = "2024"),
                ler_csv("rr_mensal_2025.csv") |> mutate(ano = "2025")) |>
  mutate(faixa = factor(c(total = "< 5 years", nomes_faixas)[faixa], levels = c("< 5 years", nomes_faixas)))
p3 <- ggplot(rm, aes(rr, factor(mes, levels = 12:1, labels = rev(meses)), colour = ano)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .25, position = position_dodge(width = .6)) +
  geom_point(size = 1.6, position = position_dodge(width = .6)) +
  scale_x_log10(breaks = c(.05, .2, 1, 5), labels = label_number(decimal.mark = ".", drop0trailing = TRUE)) +
  scale_colour_manual(values = c(`2024` = "black", `2025` = "#D6604D"), name = NULL) +
  facet_wrap(~ faixa, nrow = 1) + labs(x = "Rate ratio vs 2016–2019 (log scale)", y = NULL) + tema
salvar_figura(p3, "fig3_rr_mensal_2024", 10, 4.2)

# Figura 4 — decomposição do excesso da janela principal -----------------------------------------
dc <- read.csv(file.path(pasta_saidas, paste0("decomposicao_excesso_", janela_principal, ".csv")), encoding = "UTF-8") |>
  mutate(nivel = reg_en(recode(nivel, !!!nomes_faixas, s2_2024 = "Jul–Dec 2024", s1_2025 = "Jan–Jun 2025", s2_2025 = "Jul–Dec 2025",
                        lab = "Laboratory", nlab = "Clinical or epidemiological link")),
         estrato = factor(recode(estrato, Faixa = "Age group", Regiao = "Region", Semestre = "Period", Criterio = "Confirmation criterion"),
                          levels = c("Age group", "Region", "Period", "Confirmation criterion")))
p4 <- ggplot(dc, aes(excesso, reorder(nivel, excesso), fill = excesso > 0)) +
  geom_col(width = .65) + geom_vline(xintercept = 0, colour = "grey40") +
  geom_text(aes(label = paste0(fmt(excesso), " (", fmt(pct_excesso), "%)"),
                hjust = ifelse(excesso > 0, -0.1, 1.1)), size = 2.8) +
  facet_wrap(~ estrato, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(`TRUE` = "#2166AC", `FALSE` = "#B2182B"), guide = "none") +
  scale_x_continuous(labels = label_number(big.mark = ",", decimal.mark = ".", style_negative = "minus"), expand = expansion(mult = c(.7, .6))) +
  coord_cartesian(clip = "off") +
  labs(x = "Observed cases in Jul 2024–Dec 2025 minus cases expected from 2016–2019 rates", y = NULL) + tema
salvar_figura(p4, "fig4_decomposicao_excesso", 10, 6)

# Figura 5 — cobertura e excesso -------------------------------------------------------
uf <- read.csv(file.path(pasta_saidas, "uf_rr_cobertura.csv"), encoding = "UTF-8", colClasses = c(uf = "character")) |> filter(grupo == "6m_4a") |> mutate(regiao = reg_en(regiao))
co <- read.csv(file.path(pasta_saidas, "uf_correlacoes.csv"), encoding = "UTF-8") |> filter(grupo == "6m_4a")
p5a <- ggplot(uf, aes(delta_dtp, rr)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "grey30", fill = "grey85", linewidth = .6) +
  geom_point(aes(fill = regiao), size = 2.3, shape = 21, colour = "white", stroke = .3) +
  ggrepel::geom_text_repel(aes(label = sigla), size = 2.7, family = FONTE, colour = "grey15", seed = 1,
                           point.padding = unit(1.2, "mm"), box.padding = unit(1, "mm"), min.segment.length = unit(2, "mm"),
                           segment.colour = "grey60", segment.size = .25, max.overlaps = Inf) +
  scale_y_log10(labels = label_number(decimal.mark = ".", accuracy = .1)) +
  scale_x_continuous(labels = label_number(decimal.mark = ".", style_negative = "minus")) +
  scale_fill_brewer(palette = "Dark2", name = NULL) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3))) +
  labs(title = paste0("A. States (Spearman ρ = ", fmt(co$rho_delta, 2), "; p = ", fmt(co$p_delta, 2), ")"),
       x = "Change in DTP coverage (pp),\n2019–2023 vs 2010–2014",
       y = "RR Jul 2024–Dec 2025 vs 2016–2019, 6 months–4 years") + tema
md <- read.csv(file.path(pasta_saidas, "modelos_cobertura_excesso.csv"), encoding = "UTF-8") |>
  filter(modelo == "a_rr_categoria", ano_alvo == janela_principal) |>
  mutate(painel = paste(ifelse(grupo == "lt_6m", "< 6 months:", "6 months–4 years:"),
                        c("Variação da DTP" = "DTP change", "Não vacinados 2019–2023" = "unvaccinated",
                          "dTpa 2023–2024" = "maternal Tdap 2023–2024")[exposicao]),
         painel = factor(painel, levels = c("< 6 months: maternal Tdap 2023–2024", "6 months–4 years: unvaccinated",
                                            "6 months–4 years: DTP change")),
         # mesma lógica em todos os blocos: situação mais desfavorável no topo
         termo = factor(termo, levels = c("Estável ou aumento", "Queda < 10 p.p.", "Queda ≥ 10 p.p.",
                                          "T1 (menor)", "T2", "T3 (maior)",
                                          "Maior cobertura (T3)", "Intermediária (T2)", "Menor cobertura (T1)")))
rot5 <- c(rotulo_categoria("delta_dtp"), rotulo_categoria("nao_vac"), rotulo_categoria("dtpa"))
p5b <- ggplot(md, aes(irr, termo)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2) + geom_point(size = 1.8) +
  facet_wrap(~ painel, ncol = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) rot5[x]) +
  scale_x_log10(labels = label_number(decimal.mark = ".", accuracy = .1)) +
  theme(axis.text.y = element_text(size = 8, lineheight = .9)) +
  labs(title = "B. Municipalities nationwide, without regard to region", x = "RR Jul 2024–Dec 2025 vs 2016–2019 (log scale)", y = NULL) + tema
salvar_figura(p5a + p5b + plot_layout(widths = c(1.2, 1)), "fig5_cobertura_excesso", 10.5, 7.4)

# Figura S6 — ESF como modificadora de efeito ------------------------------------------
ma <- ler_csv("modelos_aps.csv") |> filter(ano_alvo == janela_principal)
rot_g <- c(`6m_4a` = "6 months–4 years", lt_6m = "< 6 months")
rot_e <- c("Variação da DTP" = "per 10-pp decline in DTP",
           "Não vacinados 2019–2023" = "per 10 pp more unvaccinated",
           "dTpa 2023–2024" = "per 10 pp less maternal Tdap")
pint <- ma |> filter(modelo == "g_interacao", termo == "interacao") |> select(grupo, exposicao, p_int = p)
a <- ma |> filter(modelo == "f_estrato_aps") |>
  left_join(pint, by = c("grupo", "exposicao")) |>
  mutate(painel = paste0(rot_g[grupo], ": ", rot_e[exposicao], "\n(p for interaction = ", fmt(p_int, 3), ")"),
         termo = factor(termo, levels = c("< 50%", "50–79,9%", "≥ 80%")))
p6a <- ggplot(a, aes(irr, termo)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .2) + geom_point(size = 1.8) +
  facet_wrap(~ painel, ncol = 1, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub(" municipalities", "", sub("\\(", "(n = ", rotulo_esf()[x]))) +
  scale_x_log10(breaks = c(.7, .8, .9, 1, 1.1, 1.25, 1.5, 2), labels = label_number(decimal.mark = ".", accuracy = .01)) +
  theme(axis.text.y = element_text(size = 8, lineheight = .9)) +
  labs(title = "A. Effect of the vaccination gap by FHS coverage",
       x = "Ratio of increases, Jul 2024–Dec 2025 (log scale)", y = "FHS coverage") + tema
b <- ma |> filter(modelo == "h_grade", grupo == "6m_4a", exposicao == "Variação da DTP") |>
  tidyr::separate(termo, c("cobertura", "aps"), sep = " \\| ") |>
  bind_rows(tibble(cobertura = "Estável ou aumento", aps = "≥ 80%", irr = 1, lo = 1, hi = 1)) |>
  mutate(aps = factor(aps, levels = c("< 50%", "50–79,9%", "≥ 80%")),
         cobertura = factor(cobertura, levels = c("Estável ou aumento", "Queda < 10 p.p.", "Queda ≥ 10 p.p.")))
p6b <- ggplot(b, aes(cobertura, irr, colour = aps)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = .5), size = .3) +
  scale_y_log10(labels = label_number(decimal.mark = ".", accuracy = .1)) +
  scale_x_discrete(labels = cat_en) +
  scale_colour_manual(values = c("#B2182B", "#F4A582", "#2166AC"), name = "FHS coverage", labels = function(x) rotulo_esf()[x]) +
  guides(colour = guide_legend(ncol = 1, title.position = "top")) +
  labs(title = "B. 6 months–4 years: change in DTP × FHS coverage",
       x = "Change in DTP coverage, 2019–2023 vs 2010–2014",
       y = "Ratio of increases, Jul 2024–Dec 2025\n(ref.: stable/increasing DTP and FHS ≥ 80%)") + tema
salvar_figura(p6a + p6b + plot_layout(widths = c(1, 1.2)), "fig6_aps_modificacao", 11, 7.6)
message("English figures ready")
