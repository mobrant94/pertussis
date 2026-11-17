# ------------------------------------------------------------------------------
# 02_casos_sinan.R
# Casos de coqueluche em menores de 5 anos (SINAN).
# Arquivos: dados_brutos/COQUBR07.dbc ... COQUBR25.dbc
#   (https://datasus.saude.gov.br/transferencia-de-arquivos/ -> SINAN -> Coqueluche)
# Caso confirmado: CLASSI_FIN = 1. Confirmação laboratorial: CRITERIO = 1.
# ------------------------------------------------------------------------------

source("R/00_configuracao.R")
library(read.dbc)

# 1. Leitura dos arquivos -------------------------------------------------------
variaveis <- c("DT_NOTIFIC", "ID_MUNICIP", "ID_UNIDADE", "DT_SIN_PRI", "ANO_NASC",
               "NU_IDADE_N", "CS_SEXO", "ID_MN_RESI", "CLASSI_FIN", "CRITERIO", "EVOLUCAO")

arquivos <- list.files(pasta_brutos, pattern = "^COQUBR\\d{2}\\.dbc$", full.names = TRUE)
message(length(arquivos), " arquivos do SINAN encontrados")

lista <- list()
for (arquivo in arquivos) {
  dados <- read.dbc(arquivo)
  dados <- dados[, intersect(variaveis, names(dados))]
  dados[] <- lapply(dados, as.character)
  lista[[arquivo]] <- dados
}
sinan <- bind_rows(lista)

# 2. Idade, faixa etária e município de residência -----------------------------
# NU_IDADE_N: primeiro dígito = unidade (1 hora, 2 dia, 3 mês, 4 ano); demais = valor
sinan <- sinan |>
  mutate(dt_sin   = as.Date(DT_SIN_PRI),
         idade    = str_pad(NU_IDADE_N, 4, pad = "0"),
         unidade  = as.integer(substr(idade, 1, 1)),
         valor    = as.integer(substr(idade, 2, 4)),
         idade_meses = case_when(unidade == 1 ~ 0,
                                 unidade == 2 ~ valor / 30.4375,
                                 unidade == 3 ~ valor,
                                 unidade == 4 ~ valor * 12),
         faixa = case_when(idade_meses < 6  ~ "lt_6m",
                           idade_meses < 12 ~ "6_11m",
                           idade_meses < 60 ~ "1_4a"),
         code_muni = substr(str_pad(ID_MN_RESI, 6, pad = "0"), 1, 6))

# 3. Seleção, com contagem para o fluxograma ------------------------------------
n_total <- nrow(sinan)

sinan_periodo <- sinan |> filter(!is.na(dt_sin), year(dt_sin) %in% anos_casos)
sinan_lt5     <- sinan_periodo |> filter(!is.na(faixa))
sinan_unicos  <- sinan_lt5 |>
  distinct(DT_NOTIFIC, ID_MUNICIP, ID_UNIDADE, DT_SIN_PRI, ANO_NASC,
           NU_IDADE_N, CS_SEXO, ID_MN_RESI, .keep_all = TRUE)

fluxo <- tibble(
  etapa = c(paste0("Registros nos arquivos SINAN (2007–", max(anos_casos), ")"),
            paste0("Início dos sintomas ausente ou fora de ", min(anos_casos), "–", max(anos_casos)),
            "Idade ≥ 5 anos ou ignorada",
            "Duplicidades"),
  n = c(n_total,
        n_total - nrow(sinan_periodo),
        nrow(sinan_periodo) - nrow(sinan_lt5),
        nrow(sinan_lt5) - nrow(sinan_unicos)))

saveRDS(sinan_unicos, file.path(pasta_proc, "sinan_lt5.rds"))
saveRDS(fluxo, file.path(pasta_proc, "fluxo_parcial.rds"))

# 4. Casos por município, faixa etária e mês ------------------------------------
casos <- sinan_unicos |>
  mutate(data = floor_date(dt_sin, "month"),
         confirmado = CLASSI_FIN %in% "1",
         laboratorio = confirmado & CRITERIO %in% "1") |>
  group_by(code_muni, faixa, data) |>
  summarise(casos      = sum(confirmado),
            casos_lab  = sum(laboratorio),
            casos_nlab = sum(confirmado & !laboratorio),
            notif      = n(),
            obitos     = sum(confirmado & EVOLUCAO %in% "2"),
            .groups = "drop")

saveRDS(casos, file.path(pasta_proc, "casos_mun_mes.rds"))
message("Notificações elegíveis: ", nrow(sinan_unicos), " | confirmados: ", sum(casos$casos))
