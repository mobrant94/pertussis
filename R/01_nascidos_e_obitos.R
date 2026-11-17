# ------------------------------------------------------------------------------
# 01_nascidos_e_obitos.R
# Baixa do TabNet/DATASUS, com o pacote datasus:
#   - nascidos vivos por município de residência e mês de nascimento (SINASC)
#   - óbitos infantis por UF e faixa etária (SIM, mortalidade infantil)
#   - óbitos de 1 a 4 anos por UF (SIM, óbitos gerais)
# Cada consulta é salva em dados/tabnet. Se o script for rodado de novo, só baixa
# o que ainda não foi salvo. O TabNet costuma ficar instável: se uma consulta
# falhar, espere alguns minutos e rode o script outra vez.
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(datasus)

# Nomes das opções de linha e coluna no TabNet
linha_municipio <- "Município"
coluna_mes      <- "Mês do nascimento"
linha_uf        <- "Região/Unidade da Federação"
coluna_faixa_infantil <- "Faixa etária 1"
coluna_faixa_obitos   <- "Faixa Etária"

# O TabNet às vezes não responde. Cada consulta é tentada até 3 vezes,
# com pausa de 2 minutos entre as tentativas; depois de cada consulta bem-sucedida,
# o script espera 20 segundos para não sobrecarregar o servidor.

# 1. Nascidos vivos por município e mês ----------------------------------------
# Primeiro tenta o ano inteiro; se não vier completo, baixa estado por estado.
for (ano in anos_nascimento) {
  arquivo <- file.path(pasta_tabnet, paste0("nv_mun_mes_", ano, ".rds"))
  if (file.exists(arquivo)) next
  message("Nascidos vivos ", ano)

  tabela <- NULL
  for (tentativa in 1:3) {
    resposta <- try(datasus::sinasc(linha = linha_municipio, coluna = coluna_mes, periodo = ano))
    if (!inherits(resposta, "try-error")) { tabela <- as.data.frame(resposta, check.names = FALSE); break }
    Sys.sleep(120)
  }
  Sys.sleep(20)

  if (is.null(tabela) || nrow(tabela) < 5000) {
    partes <- list()
    for (sigla in siglas_uf) {
      arquivo_uf <- file.path(pasta_tabnet, paste0("nv_mun_mes_", ano, "_", sigla, ".rds"))
      if (!file.exists(arquivo_uf)) {
        message("  ", sigla)
        for (tentativa in 1:3) {
          resposta <- try(datasus::sinasc(uf = sigla, linha = linha_municipio, coluna = coluna_mes, periodo = ano))
          if (!inherits(resposta, "try-error")) break
          Sys.sleep(120)
        }
        if (inherits(resposta, "try-error")) stop("O TabNet não respondeu. Rode o script de novo mais tarde.")
        saveRDS(as.data.frame(resposta, check.names = FALSE), arquivo_uf)
        Sys.sleep(20)
      }
      partes[[sigla]] <- readRDS(arquivo_uf)
    }
    tabela <- bind_rows(partes)
  }
  saveRDS(tabela, arquivo)
}

# 2. Óbitos infantis por UF e faixa etária -------------------------------------
for (ano in anos_nascimento) {
  arquivo <- file.path(pasta_tabnet, paste0("obitos_inf_uf_", ano, ".rds"))
  if (file.exists(arquivo)) next
  message("Óbitos infantis ", ano)
  for (tentativa in 1:3) {
    resposta <- try(datasus::sim("mortalidade_infantil", abrangencia = "uf", linha = linha_uf,
                                 coluna = coluna_faixa_infantil, periodo = ano))
    if (!inherits(resposta, "try-error")) break
    Sys.sleep(120)
  }
  if (inherits(resposta, "try-error")) stop("O TabNet não respondeu. Rode o script de novo mais tarde.")
  saveRDS(as.data.frame(resposta, check.names = FALSE), arquivo)
  Sys.sleep(20)
}

# 3. Óbitos de 1 a 4 anos por UF ------------------------------------------------
for (ano in anos_nascimento) {
  arquivo <- file.path(pasta_tabnet, paste0("obitos_uf_fx_", ano, ".rds"))
  if (file.exists(arquivo)) next
  message("Óbitos por faixa etária ", ano)
  for (tentativa in 1:3) {
    resposta <- try(datasus::sim("obitos", abrangencia = "uf", linha = linha_uf,
                                 coluna = coluna_faixa_obitos, periodo = ano))
    if (!inherits(resposta, "try-error")) break
    Sys.sleep(120)
  }
  if (inherits(resposta, "try-error")) stop("O TabNet não respondeu. Rode o script de novo mais tarde.")
  saveRDS(as.data.frame(resposta, check.names = FALSE), arquivo)
  Sys.sleep(20)
}

# 4. Organização das tabelas ----------------------------------------------------
# As tabelas do TabNet vêm "largas" (uma coluna por mês ou faixa etária).
# A função abaixo passa para o formato longo: rotulo | coluna | valor.
tabnet_longo <- function(tabela) {
  names(tabela)[1] <- "rotulo"
  tabela$rotulo <- as.character(tabela$rotulo)
  tabela <- tabela[!grepl("^\\s*total\\s*$", tabela$rotulo, ignore.case = TRUE), ]
  colunas <- names(tabela)[-1]
  colunas <- colunas[!grepl("^\\s*total\\s*$", colunas, ignore.case = TRUE)]
  tabela |>
    select(rotulo, all_of(colunas)) |>
    mutate(across(all_of(colunas), ~ as.numeric(gsub("[^0-9]", "", as.character(.x))))) |>
    pivot_longer(all_of(colunas), names_to = "coluna", values_to = "valor") |>
    mutate(valor = coalesce(valor, 0))
}

# Mês: "Janeiro" -> 1, ..., "Dezembro" -> 12
meses_abrev <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
numero_do_mes <- function(texto) {
  texto <- tolower(iconv(trimws(texto), to = "ASCII//TRANSLIT"))
  match(substr(texto, 1, 3), meses_abrev)
}

# UF: as linhas vêm como ".. Rondônia"; converte o nome no código do IBGE
codigos_uf <- c("Rondonia" = "11", "Acre" = "12", "Amazonas" = "13", "Roraima" = "14",
                "Para" = "15", "Amapa" = "16", "Tocantins" = "17", "Maranhao" = "21",
                "Piaui" = "22", "Ceara" = "23", "Rio Grande do Norte" = "24", "Paraiba" = "25",
                "Pernambuco" = "26", "Alagoas" = "27", "Sergipe" = "28", "Bahia" = "29",
                "Minas Gerais" = "31", "Espirito Santo" = "32", "Rio de Janeiro" = "33",
                "Sao Paulo" = "35", "Parana" = "41", "Santa Catarina" = "42",
                "Rio Grande do Sul" = "43", "Mato Grosso do Sul" = "50", "Mato Grosso" = "51",
                "Goias" = "52", "Distrito Federal" = "53")
codigo_da_uf <- function(rotulo) {
  nome <- iconv(enc2utf8(rotulo), "UTF-8", "ASCII//TRANSLIT")
  nome <- trimws(gsub("^[\\s.]*", "", nome, perl = TRUE))
  unname(codigos_uf[nome])
}

# Nascidos vivos
lista_nv <- list()
for (ano in anos_nascimento) {
  tabela <- readRDS(file.path(pasta_tabnet, paste0("nv_mun_mes_", ano, ".rds")))
  lista_nv[[as.character(ano)]] <- tabnet_longo(tabela) |>
    mutate(code_muni = str_extract(rotulo, "^\\d{6}"),
           ano = ano,
           mes = numero_do_mes(coluna)) |>
    filter(!is.na(code_muni), !is.na(mes)) |>
    group_by(code_muni, ano, mes) |>
    summarise(nv = sum(valor), .groups = "drop")
}
nascidos <- bind_rows(lista_nv)
saveRDS(nascidos, file.path(pasta_proc, "nascidos_mun_mes.rds"))

# Óbitos infantis: neonatais (0 a 27 dias) e pós-neonatais (28 a 364 dias)
lista_inf <- list()
for (ano in anos_nascimento) {
  tabela <- readRDS(file.path(pasta_tabnet, paste0("obitos_inf_uf_", ano, ".rds")))
  lista_inf[[as.character(ano)]] <- tabnet_longo(tabela) |>
    mutate(uf = codigo_da_uf(rotulo), ano = ano) |>
    filter(!is.na(uf))
}
obitos_infantis <- bind_rows(lista_inf) |>
  mutate(grupo = case_when(
    grepl("0 a 6|7 a 27|neonat|^0 a 27", coluna, ignore.case = TRUE) &
      !grepl("p[oó]s", coluna, ignore.case = TRUE) ~ "neonatal",
    grepl("28|p[oó]s", coluna, ignore.case = TRUE) ~ "pos_neonatal",
    TRUE ~ "outro")) |>
  group_by(uf, ano) |>
  summarise(d_neo = sum(valor[grupo == "neonatal"]),
            d_pn  = sum(valor[grupo == "pos_neonatal"]),
            d_inf = sum(valor), .groups = "drop")

# Óbitos de 1 a 4 anos
lista_14 <- list()
for (ano in anos_nascimento) {
  tabela <- readRDS(file.path(pasta_tabnet, paste0("obitos_uf_fx_", ano, ".rds")))
  lista_14[[as.character(ano)]] <- tabnet_longo(tabela) |>
    mutate(uf = codigo_da_uf(rotulo), ano = ano) |>
    filter(!is.na(uf))
}
obitos_14 <- bind_rows(lista_14) |>
  filter(grepl("^\\s*0?1\\s*a\\s*0?4\\b", coluna, perl = TRUE)) |>
  group_by(uf, ano) |>
  summarise(d_14 = sum(valor), .groups = "drop")

obitos <- left_join(obitos_infantis, obitos_14, by = c("uf", "ano"))
saveRDS(obitos, file.path(pasta_proc, "obitos_uf_ano.rds"))

# Conferências
stopifnot(nrow(obitos) == 27 * length(anos_nascimento))
stopifnot(all(obitos$d_neo > 0), all(!is.na(obitos$d_14)))
message("Nascidos vivos: ", format(sum(nascidos$nv), big.mark = ".", decimal.mark = ","))
