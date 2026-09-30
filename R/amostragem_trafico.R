# ---------------------------------------------------------------------------
# Amostragem dos processos de trafico de drogas - TJSP, TJMS e TJRN
#
# Mesmo formato das tabelas de trafico + lavagem
# (processo, id_interno, ano_captura, tribunal), um arquivo por tribunal.
#
# Aqui a GRADE tem um termo so ("trafico de drogas"), entao nao existe
# n_queries e nao ha regra de censo: e sorteio aleatorio simples de
# COTA_ANUAL casos por ano dentro de cada tribunal.
#
# Gera tambem a planilha de reposicao: N_REPOSICAO casos por ano,
# sorteados entre os processos que NAO entraram na amostra principal.
#
# Os ids do TJSP usam 4 digitos (TDSP0001); TJMS e TJRN seguem com 3.
# ---------------------------------------------------------------------------

suppressMessages({
  library(readr)
  library(dplyr)
  library(writexl)
})

SEED        <- 20260902           # semente fixa -> amostra reproduzivel
ANOS        <- 2023:2025
COTA_ANUAL  <- 100                # por ano, por tribunal
N_REPOSICAO <- 30                 # por ano, por tribunal

# Os casos de trafico + lavagem ja amostrados estao dentro deste universo.
# TRUE evita que o mesmo processo seja codificado duas vezes; FALSE sorteia
# do universo inteiro (amostra aleatoria simples sem restricao).
EXCLUIR_JA_AMOSTRADOS <- TRUE

dir_saida <- "dados-saida/amostragem"
dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)

TRIBUNAIS <- list(
  tjsp = list(arquivo = "dados-saida/trafico_tjsp.csv", fmt_id = "TDSP%04d"),
  tjms = list(arquivo = "dados-saida/trafico_tjms.csv", fmt_id = "TDMS%03d"),
  tjrn = list(arquivo = "dados-saida/trafico_tjrn.csv", fmt_id = "TDRN%03d")
)

# --- processos ja comprometidos na fase trafico + lavagem -------------------
# amostra, reposicao e censo do TJMS/TJRN, tudo que ja esta em campo.
ja_amostrados <- if (EXCLUIR_JA_AMOSTRADOS) {
  arqs <- list.files(dir_saida, pattern = "_trafico_lavagem\\.csv$|_reposicao\\.csv$",
                     full.names = TRUE)
  if (length(arqs) == 0) {
    warning("EXCLUIR_JA_AMOSTRADOS = TRUE mas nenhuma planilha anterior encontrada em ",
            dir_saida)
    character(0)
  } else {
    unique(unlist(lapply(arqs, function(a) {
      as.character(read_csv(a, col_types = cols(.default = "c"),
                            progress = FALSE)$processo)
    })))
  }
} else character(0)

# --- base -------------------------------------------------------------------
monta_base <- function(arquivo) {
  d <- read_csv(arquivo, col_types = cols(.default = "c"), progress = FALSE)

  # TJSP/TJMS trazem `ano` (ano da disponibilizacao); TJRN so tem `ano_busca`.
  col_ano <- intersect(c("ano", "ano_busca"), names(d))[1]
  if (is.na(col_ano)) stop("coluna de ano nao encontrada em ", arquivo)

  d %>%
    transmute(
      processo    = as.character(processo),
      ano_captura = as.integer(.data[[col_ano]])
    ) %>%
    filter(!is.na(processo), ano_captura %in% ANOS) %>%
    distinct(processo, .keep_all = TRUE)
}

set.seed(SEED)

for (tj in names(TRIBUNAIS)) {

  cfg  <- TRIBUNAIS[[tj]]
  base <- monta_base(cfg$arquivo)
  stopifnot(nrow(base) > 0)

  n_antes  <- nrow(base)
  elegivel <- base %>% filter(!processo %in% ja_amostrados)
  n_fora   <- n_antes - nrow(elegivel)

  # a cota tem que caber em todos os anos antes de sortear qualquer coisa
  disponivel <- elegivel %>% count(ano_captura, name = "n")
  faltando   <- disponivel %>% filter(n < COTA_ANUAL + N_REPOSICAO)
  if (nrow(faltando) > 0) {
    stop(sprintf("%s: ano(s) %s com menos de %d processos elegiveis.",
                 toupper(tj), paste(faltando$ano_captura, collapse = ", "),
                 COTA_ANUAL + N_REPOSICAO))
  }
  if (!all(ANOS %in% disponivel$ano_captura)) {
    stop(sprintf("%s: sem processos elegiveis em %s.", toupper(tj),
                 paste(setdiff(ANOS, disponivel$ano_captura), collapse = ", ")))
  }

  amostra <- elegivel %>%
    group_by(ano_captura) %>%
    slice_sample(n = COTA_ANUAL) %>%
    ungroup() %>%
    arrange(ano_captura, processo) %>%
    mutate(id_interno = sprintf(cfg$fmt_id, row_number()),
           tribunal   = toupper(tj)) %>%
    select(processo, id_interno, ano_captura, tribunal)

  reposicao <- elegivel %>%
    filter(!processo %in% amostra$processo) %>%
    group_by(ano_captura) %>%
    slice_sample(n = N_REPOSICAO) %>%
    ungroup() %>%
    arrange(ano_captura, processo) %>%
    mutate(id_interno = sprintf(cfg$fmt_id, nrow(amostra) + row_number()),
           tribunal   = toupper(tj)) %>%
    select(processo, id_interno, ano_captura, tribunal)

  # --- gravacao -------------------------------------------------------------
  base_nome <- file.path(dir_saida, sprintf("amostra_%s_trafico", tj))
  rep_nome  <- file.path(dir_saida, sprintf("amostra_%s_trafico_reposicao", tj))

  write_xlsx(amostra,   paste0(base_nome, ".xlsx"))
  write_xlsx(reposicao, paste0(rep_nome,  ".xlsx"))
  write.csv(amostra,   paste0(base_nome, ".csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(reposicao, paste0(rep_nome,  ".csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  # --- conferencia ----------------------------------------------------------
  cat("==", toupper(tj), "==\n")
  cat("universo", ANOS[1], "-", ANOS[length(ANOS)], ":", n_antes, "processos",
      if (EXCLUIR_JA_AMOSTRADOS)
        sprintf("| %d ja em campo na fase trafico+lavagem, excluido(s)", n_fora)
      else "| sem exclusao",
      "\n")
  print(disponivel)
  cat("-- amostra --\n")
  print(amostra %>% count(ano_captura, name = "n"))
  cat("ids:", amostra$id_interno[1], "->", amostra$id_interno[nrow(amostra)],
      "| total:", nrow(amostra), "\n")
  cat("-- reposicao --\n")
  print(reposicao %>% count(ano_captura, name = "n"))
  cat("ids:", reposicao$id_interno[1], "->", reposicao$id_interno[nrow(reposicao)],
      "| total:", nrow(reposicao), "\n")
  cat("sem sobreposicao amostra/reposicao: ",
      length(intersect(amostra$processo, reposicao$processo)) == 0, "\n")
  cat("processos unicos: ",
      n_distinct(c(amostra$processo, reposicao$processo)) ==
        nrow(amostra) + nrow(reposicao), "\n")
  cat("ids unicos: ",
      n_distinct(c(amostra$id_interno, reposicao$id_interno)) ==
        nrow(amostra) + nrow(reposicao), "\n")
  cat("sem colisao com a fase trafico+lavagem: ",
      length(intersect(c(amostra$processo, reposicao$processo),
                       ja_amostrados)) == 0, "\n\n")
}
