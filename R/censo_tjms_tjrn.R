# ---------------------------------------------------------------------------
# Censo dos processos de trafico + lavagem - TJMS e TJRN
#
# Mesmo formato de tabela usado na amostra do TJSP
# (processo, id_interno, ano_captura, n_queries), empilhado por ano.
# Aqui NAO ha sorteio: entram todos os processos de cada tribunal.
# ---------------------------------------------------------------------------

suppressMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

ANOS      <- 2023:2025
dir_saida <- "dados-saida/amostragem"

dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)

# aba de dados e nome da coluna de ano variam entre os arquivos
TRIBUNAIS <- list(
  tjms = list(arquivo = "dados-saida/tjms_trafico_e_lavagem.xlsx",
              aba     = "tjms"),
  tjrn = list(arquivo = "dados-saida/tjrn_trafico_e_lavagem.xlsx",
              aba     = "tjrn_sentencas")
)

monta_censo <- function(arquivo, aba, prefixo) {
  d <- read_excel(arquivo, sheet = aba)

  col_ano <- intersect(c("ano", "ano_busca"), names(d))[1]
  if (is.na(col_ano)) stop("coluna de ano nao encontrada em ", arquivo)

  d %>%
    transmute(
      processo    = as.character(processo),
      ano_captura = as.integer(.data[[col_ano]]),
      n_queries   = as.integer(n_queries)
    ) %>%
    filter(!is.na(processo), ano_captura %in% ANOS) %>%
    distinct(processo, .keep_all = TRUE) %>%
    arrange(ano_captura, desc(n_queries), processo) %>%
    mutate(id_interno = sprintf(paste0(prefixo, "%03d"), row_number())) %>%
    select(processo, id_interno, ano_captura, n_queries)
}

for (tj in names(TRIBUNAIS)) {
  cfg   <- TRIBUNAIS[[tj]]
  prefixo <- paste0("TL", toupper(sub("^tj", "", tj)))
  censo   <- monta_censo(cfg$arquivo, cfg$aba, prefixo)

  base_nome <- file.path(dir_saida, sprintf("censo_%s_trafico_lavagem", tj))
  write_xlsx(censo, paste0(base_nome, ".xlsx"))
  write.csv(censo, paste0(base_nome, ".csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  # --- conferencia ---
  bruto <- read_excel(cfg$arquivo, sheet = cfg$aba)
  col_ano <- intersect(c("ano", "ano_busca"), names(bruto))[1]
  n_bruto <- sum(bruto[[col_ano]] %in% ANOS, na.rm = TRUE)

  cat("==", toupper(tj), "==\n")
  print(censo %>% count(ano_captura, name = "n"))
  print(with(censo, table(ano_captura, n_queries)))
  cat("ids:", censo$id_interno[1], "->", censo$id_interno[nrow(censo)],
      "| total:", nrow(censo), "\n")
  cat("bate com o arquivo de origem: ", nrow(censo) == n_bruto,
      sprintf(" (%d de %d)\n\n", nrow(censo), n_bruto))
}
