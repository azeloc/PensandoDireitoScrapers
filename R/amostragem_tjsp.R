# ---------------------------------------------------------------------------
# Amostragem dos processos de trafico + lavagem - TJSP
#
# Regra:
#   - todo processo capturado por 3 ou mais subqueries entra na amostra (censo)
#   - o restante da cota anual e completado por sorteio aleatorio simples
#     entre os processos com 1 ou 2 subqueries
#   - cotas: 68 (2023), 74 (2024), 58 (2025)
#
# Gera tambem uma planilha suplementar de reposicao: 30 casos por ano,
# sorteados entre os processos que NAO entraram na amostra principal, e uma
# planilha complementar com TODO o restante do universo (o que sobra depois da
# amostra e da reposicao), para que a fase trafico + lavagem do TJSP vire censo.
#
# Os ids do TJSP usam 4 digitos (TLSP0001) - a numeracao dos casos ja em campo
# nao muda, so ganha um zero a esquerda.
# ---------------------------------------------------------------------------

suppressMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

SEED       <- 20260821            # semente fixa -> amostra reproduzivel
CORTE_CENSO <- 3                  # n_queries >= 3 entra com 100% de chance
COTAS      <- c(`2023` = 65, `2024` = 64, `2025` = 97)
N_REPOSICAO <- 30                 # por ano
FMT_ID      <- "TLSP%04d"          # 4 digitos: TLSP0001 ... TLSP0652

arq_entrada <- "dados-saida/tjsp_trafico_e_lavagem.xlsx"
dir_saida   <- "dados-saida/amostragem"

dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)

# --- base -------------------------------------------------------------------
base <- read_excel(arq_entrada, sheet = "tjsp") %>%
  transmute(
    processo    = as.character(processo),
    ano_captura = as.integer(ano),
    n_queries   = as.integer(n_queries)
  ) %>%
  filter(!is.na(processo), ano_captura %in% as.integer(names(COTAS))) %>%
  distinct(processo, .keep_all = TRUE)

stopifnot(nrow(base) > 0)

# --- sorteio ----------------------------------------------------------------
set.seed(SEED)

sorteia_ano <- function(df, cota) {
  censo   <- df %>% filter(n_queries >= CORTE_CENSO)
  elegiveis <- df %>% filter(n_queries < CORTE_CENSO)

  faltam <- cota - nrow(censo)
  if (faltam < 0) {
    stop(sprintf("Ano %s: %d casos com %d+ subqueries excedem a cota de %d.",
                 unique(df$ano_captura), nrow(censo), CORTE_CENSO, cota))
  }
  if (faltam > nrow(elegiveis)) {
    stop(sprintf("Ano %s: faltam %d casos mas so ha %d elegiveis para sorteio.",
                 unique(df$ano_captura), faltam, nrow(elegiveis)))
  }

  sorteados <- elegiveis %>% slice_sample(n = faltam)
  bind_rows(censo, sorteados)
}

amostra <- base %>%
  group_split(ano_captura) %>%
  lapply(function(d) sorteia_ano(d, COTAS[[as.character(d$ano_captura[1])]])) %>%
  bind_rows() %>%
  arrange(ano_captura, desc(n_queries), processo) %>%
  mutate(id_interno = sprintf(FMT_ID, row_number())) %>%
  select(processo, id_interno, ano_captura, n_queries)

# --- reposicao: 30 por ano entre os nao sorteados ---------------------------
reposicao <- base %>%
  filter(!processo %in% amostra$processo) %>%
  group_by(ano_captura) %>%
  slice_sample(n = N_REPOSICAO) %>%
  ungroup() %>%
  arrange(ano_captura, desc(n_queries), processo) %>%
  mutate(id_interno = sprintf(FMT_ID, nrow(amostra) + row_number())) %>%
  select(processo, id_interno, ano_captura, n_queries)

# --- complementar: todo o resto do universo ---------------------------------
# nao ha sorteio aqui - entra tudo que ficou de fora da amostra e da reposicao.
complementar <- base %>%
  filter(!processo %in% c(amostra$processo, reposicao$processo)) %>%
  arrange(ano_captura, desc(n_queries), processo) %>%
  mutate(id_interno = sprintf(FMT_ID,
                              nrow(amostra) + nrow(reposicao) + row_number())) %>%
  select(processo, id_interno, ano_captura, n_queries)

# --- gravacao ---------------------------------------------------------------
write_xlsx(complementar, file.path(dir_saida, "amostra_tjsp_complementar.xlsx"))
write.csv(complementar, file.path(dir_saida, "amostra_tjsp_complementar.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(amostra,   file.path(dir_saida, "amostra_tjsp_trafico_lavagem.xlsx"))
write_xlsx(reposicao, file.path(dir_saida, "amostra_tjsp_reposicao.xlsx"))
write.csv(amostra,   file.path(dir_saida, "amostra_tjsp_trafico_lavagem.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(reposicao, file.path(dir_saida, "amostra_tjsp_reposicao.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# --- conferencia ------------------------------------------------------------
cat("== AMOSTRA PRINCIPAL ==\n")
print(amostra %>% count(ano_captura, name = "n"))
print(with(amostra, table(ano_captura, n_queries)))
cat("ids:", amostra$id_interno[1], "->", amostra$id_interno[nrow(amostra)],
    "| total:", nrow(amostra), "\n\n")

cat("== REPOSICAO ==\n")
print(reposicao %>% count(ano_captura, name = "n"))
print(with(reposicao, table(ano_captura, n_queries)))
cat("ids:", reposicao$id_interno[1], "->", reposicao$id_interno[nrow(reposicao)],
    "| total:", nrow(reposicao), "\n\n")

cat("== COMPLEMENTAR ==\n")
print(complementar %>% count(ano_captura, name = "n"))
print(with(complementar, table(ano_captura, n_queries)))
cat("ids:", complementar$id_interno[1], "->",
    complementar$id_interno[nrow(complementar)],
    "| total:", nrow(complementar), "\n\n")

cat("universo coberto por inteiro: ",
    setequal(base$processo,
             c(amostra$processo, reposicao$processo, complementar$processo)),
    sprintf(" (%d + %d + %d = %d de %d)\n",
            nrow(amostra), nrow(reposicao), nrow(complementar),
            nrow(amostra) + nrow(reposicao) + nrow(complementar), nrow(base)))
cat("censo conferido (todos n_queries >=", CORTE_CENSO, "na base entraram): ",
    all(base$processo[base$n_queries >= CORTE_CENSO] %in% amostra$processo), "\n")
cat("sem sobreposicao amostra/reposicao: ",
    length(intersect(amostra$processo, reposicao$processo)) == 0, "\n")
cat("ids unicos: ",
    n_distinct(c(amostra$id_interno, reposicao$id_interno,
                 complementar$id_interno)) ==
      nrow(amostra) + nrow(reposicao) + nrow(complementar), "\n")
