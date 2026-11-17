# Dados brutos

Os arquivos abaixo não estão no repositório. Coloque-os nesta pasta antes de rodar `rodar_tudo.R`.

| Arquivo | Conteúdo | Fonte |
|---|---|---|
| `COQUBR07.dbc` … `COQUBR25.dbc` | Notificações de coqueluche, 2007–2025 | DATASUS, Transferência de arquivos → SINAN → Coqueluche (https://datasus.saude.gov.br/transferencia-de-arquivos/) |
| `pni_completo.xlsx` | Coberturas vacinais municipais por ano: aba `DTP` (esquema primário) e aba `DTPA` (gestantes), colunas `Municipio`, `dtp_2007` … `dtp_2024` | SI-PNI, TabNet (http://tabnet.datasus.gov.br/cgi/dhdat.exe?bd_pni/cpnibr.def) |
| `pop10.xlsx`, `pop22.xlsx` | População de 0 a 4 anos por município nos Censos de 2010 e 2022 (usada só para comparar com a população estimada) | IBGE, SIDRA |
| `aps/cobertura_esf_harmonizada_municipio_ano_2007_atual.csv` | Cobertura anual da Estratégia Saúde da Família por município (coluna `cobertura_esf_harmonizada_pct`) | e-Gestor Atenção Básica (https://egestorab.saude.gov.br/) |

Os nascidos vivos (SINASC) e os óbitos (SIM) são baixados automaticamente do TabNet pelo script `R/01_nascidos_e_obitos.R` e salvos em `dados/tabnet/`.
