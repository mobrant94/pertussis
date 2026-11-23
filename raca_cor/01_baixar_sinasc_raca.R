# ------------------------------------------------------------------------------
# 01_baixar_sinasc_raca.R
# Baixa os microdados do SINASC (2011–2025) do FTP do DATASUS e agrega os nascidos
# vivos por município de residência, ano, mês de nascimento e raça/cor do recém-nascido.
#
# Saída: dados/processados/nascidos_mun_mes_raca.rds
#        colunas: code_muni (6 dígitos), ano, mes, raca, nv
#
# Os arquivos são baixados por UF e ano (DNxxAAAA.dbc), lidos só nas três variáveis
# necessárias (CODMUNRES, DTNASC, RACACOR) e apagados em seguida; o resultado de cada
# UF e ano fica salvo em dados/sinasc_raca/, de modo que, se o script for interrompido,
# basta rodá-lo de novo para continuar de onde parou.
#
# Tempo estimado: 1 a 3 horas, conforme a conexão (cerca de 540 arquivos).
# Pacotes: read.dbc, dplyr
# ------------------------------------------------------------------------------
setwd("C:/Users/MARCOS.ANTUNES/OneDrive - PUCRS - BR/Documentações - Marcos/Área de Trabalho/Meus artigos/Coqueluche/artigo_coqueluche_2024")

if (!requireNamespace("read.dbc", quietly = TRUE)) install.packages("read.dbc")
library(dplyr)
options(timeout = 1200)

# A população de menores de 5 anos de 2016 a 2025 depende dos nascidos desde 2011
anos <- 2011:2025
ufs  <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB",
          "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
ftp  <- c(definitivo = "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/",
          preliminar = "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRES/")
pasta <- "dados/sinasc_raca"
dir.create(pasta, showWarnings = FALSE, recursive = TRUE)

baixar <- function(uf, ano) {
  destino <- file.path(pasta, sprintf("nv_raca_%s_%d.rds", uf, ano))
  if (file.exists(destino)) return(invisible(TRUE))
  arquivo <- sprintf("DN%s%d.dbc", uf, ano)
  tmp <- file.path(tempdir(), arquivo)
  ok <- FALSE
  for (base in ftp) {                       # tenta a base definitiva e depois a preliminar
    for (tentativa in 1:3) {
      r <- try(download.file(paste0(base, arquivo), tmp, mode = "wb", quiet = TRUE), silent = TRUE)
      if (!inherits(r, "try-error") && file.exists(tmp) && file.size(tmp) > 1000) { ok <- TRUE; break }
      Sys.sleep(15)
    }
    if (ok) break
  }
  if (!ok) { message("  não disponível: ", arquivo); return(invisible(FALSE)) }
  d <- read.dbc::read.dbc(tmp)
  unlink(tmp)
  dt <- as.character(d$DTNASC)
  # DTNASC vem como ddmmaaaa; em alguns anos antigos pode vir como aaaa-mm-dd
  mes <- ifelse(grepl("-", dt), substr(dt, 6, 7), substr(dt, 3, 4))
  out <- tibble(code_muni = substr(as.character(d$CODMUNRES), 1, 6),
                ano = ano, mes = suppressWarnings(as.integer(mes)),
                raca = as.character(d$RACACOR)) |>
    mutate(raca = case_when(raca == "1" ~ "Branca", raca == "2" ~ "Preta", raca == "3" ~ "Amarela",
                            raca == "4" ~ "Parda", raca == "5" ~ "Indígena", TRUE ~ "Ignorada")) |>
    filter(!is.na(mes), mes %in% 1:12) |>
    count(code_muni, ano, mes, raca, name = "nv")
  saveRDS(out, destino)
  invisible(TRUE)
}

for (ano in anos) {
  message("Ano ", ano)
  for (uf in ufs) baixar(uf, ano)
}

arquivos <- list.files(pasta, pattern = "^nv_raca_.*\\.rds$", full.names = TRUE)
nascidos_raca <- bind_rows(lapply(arquivos, readRDS)) |>
  group_by(code_muni, ano, mes, raca) |>
  summarise(nv = sum(nv), .groups = "drop")
saveRDS(nascidos_raca, "dados/processados/nascidos_mun_mes_raca.rds")

# Conferência: anos baixados e proporção de raça/cor ignorada
print(nascidos_raca |> group_by(ano) |>
        summarise(nascidos = sum(nv), pct_ignorada = round(100 * sum(nv[raca == "Ignorada"]) / sum(nv), 1)))
message("Pronto: dados/processados/nascidos_mun_mes_raca.rds")
