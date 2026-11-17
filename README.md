# Ressurgência da coqueluche em menores de 5 anos no Brasil, 2024–2025

Scripts em R para reproduzir as análises, as figuras e o manuscrito do estudo
ecológico sobre a ressurgência da coqueluche em menores de 5 anos no Brasil em
2024–2025, sua relação com a cobertura vacinal (DTP e dTpa em gestantes) e com a
cobertura da Estratégia Saúde da Família (ESF).

## Como reproduzir

1. Instale o R (≥ 4.2) e os pacotes:

   ```r
   install.packages(c("dplyr", "tidyr", "stringr", "lubridate", "readxl", "ggplot2",
                      "patchwork", "scales", "MASS", "fixest", "read.dbc",
                      "rmarkdown", "knitr", "flextable", "officer"))
   # pacote datasus (consultas ao TabNet): https://github.com/rpradosiqueira/datasus
   ```

2. Coloque os arquivos brutos na pasta `dados_brutos/` (lista em `dados_brutos/LEIA-ME.md`).
3. Abra `rodar_tudo.R`, ajuste a pasta do projeto na linha `setwd(...)` e rode o script.

O script `R/01_nascidos_e_obitos.R` consulta o TabNet e pode levar horas. As consultas
ficam salvas em `dados/tabnet/`; se o TabNet parar de responder, basta rodar de novo,
e só o que falta será baixado.

## Estrutura

| Script | O que faz |
|---|---|
| `R/00_configuracao.R` | Pacotes, pastas, anos do estudo e funções gerais |
| `R/01_nascidos_e_obitos.R` | Nascidos vivos (SINASC) e óbitos (SIM) pelo TabNet |
| `R/02_casos_sinan.R` | Casos de coqueluche em menores de 5 anos (SINAN) |
| `R/03_populacao_em_risco.R` | População mensal por município e faixa etária (componentes de coorte) |
| `R/04_coberturas_vacinais.R` | DTP e dTpa por município; não vacinados nas coortes 2019–2023 |
| `R/05_cobertura_esf.R` | Cobertura média da ESF em 2017–2020 |
| `R/06_painel_e_descritivo.R` | Painel município × faixa × mês, fluxograma e taxas |
| `R/07_comparacao_com_referencia.R` | 2024 e 2025 vs 2016–2019: RR e casos a mais |
| `R/08_analise_por_uf.R` | Comparação entre estados |
| `R/09_modelos_vacinacao.R` | Poisson com efeitos fixos: vacinação e aumento de casos |
| `R/10_modelos_esf.R` | ESF como modificadora de efeito |
| `R/11_laboratorio_deteccao_onda.R` | Casos laboratoriais, detecção por região, momento da onda |
| `R/12_esf_vacinacao_mediacao.R` | ESF e coberturas vacinais; análise de mediação |
| `R/13_casos_evitaveis.R` | Casos de 2024 evitáveis com maior vacinação |
| `R/14_subdeteccao.R` | Sensibilidade à menor detecção no Norte e no Nordeste |
| `R/15_figuras_artigo.R` | Figuras do artigo |
| `R/16_figuras_complementares.R` | Figuras complementares |
| `artigo.Rmd` | Manuscrito (gera `artigo.docx` com os resultados de `saidas/`) |
| `rodar_tudo.R` | Roda tudo, na ordem |

Pastas criadas pelos scripts (não vão para o GitHub): `dados/`, `saidas/` (tabelas .csv)
e `figuras/` (PNG e TIFF, 400 dpi).

## Fontes de dados

Todas públicas e sem identificação: SINAN, SINASC e SIM (DATASUS), SI-PNI, e-Gestor
Atenção Básica e Censos Demográficos de 2010 e 2022 (IBGE). Os dados de 2025 são preliminares.
