# Incidência de coqueluche por raça/cor

Rodar no R (ou RStudio), nesta ordem:

1. `source("raca_cor/01_baixar_sinasc_raca.R")`
   Baixa os microdados do SINASC 2011–2025 (por UF e ano) do FTP do DATASUS e agrega os
   nascidos vivos por município, ano, mês e raça/cor. Leva de 1 a 3 horas. Se cair a
   conexão, basta rodar de novo: continua de onde parou.
   Saída: `dados/processados/nascidos_mun_mes_raca.rds`

2. `source("raca_cor/02_analise_raca_cor.R")`
   Reconstrói a população de menores de 5 anos por raça/cor e calcula as taxas e as razões
   de taxas. Leva cerca de 5 minutos.
   Saídas em `saidas/raca_cor/`:
   - `conferencia_denominador.csv`: a soma dos grupos deve reproduzir o denominador do artigo (razão = 1)
   - `taxas_raca.csv`: incidência, internação, mortalidade e confirmação laboratorial por raça/cor, período e faixa etária
   - `razoes_taxas_raca.csv`: razões de taxas (vs. crianças brancas e onda vs. 2016–2019), ajustadas por faixa etária e região
   - `taxas_raca_regiao.csv`, `casos_por_raca.csv`, `sinasc_raca_ignorada.csv`
   - `fig_incidencia_raca.png`

Pacotes: read.dbc, dplyr, tidyr, lubridate, ggplot2.
