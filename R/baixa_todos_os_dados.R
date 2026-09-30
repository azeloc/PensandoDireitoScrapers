# ==========================================================================
# COLETA DE JURISPRUDENCIA DE PRIMEIRO GRAU: TJSP, TJMS E TJRN
#
# Uma pasta por query, com o mesmo nome (slug) nos tres tribunais:
#
#   dados-raw/
#     tjsp/<slug>/            (HTML do e-SAJ, via pacote lex)
#     tjms/<slug>/<ano>/      (HTML do e-SAJ, raspagem propria)
#     tjrn/<slug>/<ano>/      (RDS da API de jurisprudencia)
#
# Estrutura em tres etapas:
#   1) baixar  -> salva o bruto em disco, sem transformar nada
#   2) ler     -> le o bruto e devolve tibble
#   3) exportar-> Excel por tribunal, com log de totais
# ==========================================================================

library(httr)
library(jsonlite)
library(rvest)
library(xml2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(tibble)
library(lubridate)
library(writexl)

# ==========================================================================
# 0. CONFIGURACAO GERAL
# ==========================================================================

ANOS <- 2023:2025

DIR_RAW <- "dados-raw"
DIR_OUT <- "dados-saida"
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

UA <- httr::user_agent(
  "Pesquisa academica (R/httr) - contato: seu.email@instituicao.br"
)

# --------------------------------------------------------------------------
# Termos
#
# Para reativar um termo, descomente a linha. A virgula fica DENTRO do
# comentario da linha desativada, nunca sobrando no fim da linha ativa,
# senao o R le c("x", ) e para com "argument 2 is empty".
# --------------------------------------------------------------------------

TERMOS_TRAFICO <- c(
  "tráfico de drogas"
  #, "11.343"
)

TERMOS_LAVAGEM <- c(
  "9.613",
  "14.478",
  "Ocultação de Bens",
  "Ocultação de Capitais",
  "Ocultação de Valores",
  "Ocultação de Direitos",
  "Lavagem de dinheiro"
)

# ==========================================================================
# 1. HELPERS
# ==========================================================================

#' Remove acentos (para os buscadores que nao normalizam o indice)
sem_acento <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

#' Nome de pasta a partir do par de termos
slugificar <- function(x) {
  x %>%
    sem_acento() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_remove_all("^_|_$")
}

#' Monta a expressao de busca no padrao e-SAJ / TJRN
montar_query <- function(t1, t2, acentos = TRUE) {
  q <- stringr::str_c('"', t1, '" E "', t2, '"')
  if (!acentos) q <- sem_acento(q)
  q
}

formatar_cnj <- function(x) {
  d <- stringr::str_remove_all(x, "\\D")
  ifelse(
    nchar(d) == 20,
    stringr::str_replace(
      d, "^(\\d{7})(\\d{2})(\\d{4})(\\d)(\\d{2})(\\d{4})$",
      "\\1-\\2.\\3.\\4.\\5.\\6"
    ),
    x
  )
}

#' Padroniza a coluna do numero do processo.
#'
#' Os tres parsers nomeiam o campo de forma diferente: o lex devolve
#' `n_processo` com os 20 digitos crus, TJMS e TJRN devolvem `processo`
#' ja mascarado. Sem alinhar isso a deduplicacao do passo 5.3 caia no
#' guarda `!"processo" %in% names(df)` e devolvia o TJSP intacto, com
#' o mesmo processo repetido em varias linhas.
padronizar_processo <- function(df) {

  if (nrow(df) == 0) return(df)

  if (!"processo" %in% names(df)) {
    origem <- intersect(c("n_processo", "numero_processo", "nr_processo"),
                        names(df))
    if (length(origem) == 0) {
      stop("Nenhuma coluna de numero de processo encontrada. Colunas: ",
           paste(names(df), collapse = ", "))
    }
    names(df)[names(df) == origem[1]] <- "processo"
  }

  df %>%
    dplyr::mutate(
      processo = formatar_cnj(stringr::str_squish(as.character(processo))),
      processo = dplyr::na_if(processo, "")
    ) %>%
    dplyr::relocate(processo)
}

#' Limpa o inteiro_teor do TJRN, que vem como HTML e, pior, com imagens
#' embutidas em base64 (um unico documento pode passar de 13 milhoes de
#' caracteres). Remove <img>, sobras de base64, demais tags e normaliza
#' espacos, deixando so o texto util da decisao.
limpar_html <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    x %>%
      stringr::str_replace_all("(?i)<img[^>]*>", " ") %>%
      stringr::str_replace_all("(?i)data:image/[^\"')]+", " ") %>%
      stringr::str_replace_all("<[^>]+>", " ") %>%
      stringr::str_replace_all("&nbsp;", " ") %>%
      stringr::str_replace_all("&amp;", "&") %>%
      stringr::str_squish()
  )
}

#' Retentativa. Recebe uma FUNCAO, nao uma expressao: argumento em R e
#' promise e so seria avaliado uma vez, o que fazia as tentativas 2 a 4
#' devolverem o erro guardado da primeira em vez de refazer a requisicao.
.tentar <- function(f, tentativas = 4, espera = 3) {
  r <- NULL
  for (i in seq_len(tentativas)) {
    r <- try(f(), silent = TRUE)
    if (!inherits(r, "try-error") && httr::status_code(r) == 200) return(r)
    if (i < tentativas) {
      message("  falha na tentativa ", i, ", aguardando ", espera * i, "s")
      Sys.sleep(espera * i)
    }
  }
  if (inherits(r, "try-error")) stop(attr(r, "condition")$message)
  stop("Status HTTP ", httr::status_code(r), " apos ", tentativas, " tentativas.")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --------------------------------------------------------------------------
# Grade de consultas
# --------------------------------------------------------------------------

GRADE <- tidyr::expand_grid(
  termo_trafico = TERMOS_TRAFICO,
  termo_lavagem = TERMOS_LAVAGEM
) %>%
  dplyr::mutate(
    id              = dplyr::row_number(),
    slug            = stringr::str_c(slugificar(termo_trafico), "__",
                                     slugificar(termo_lavagem)),
    query_acento    = montar_query(termo_trafico, termo_lavagem, acentos = TRUE),
    query_semacento = montar_query(termo_trafico, termo_lavagem, acentos = FALSE),
    .before = 1
  )

print(GRADE, n = Inf)

# ==========================================================================
# 2. TJSP (pacote lex)
# ==========================================================================
# O lex::tjsp_cjpg_download nao filtra por data, entao a coleta traz tudo
# e o recorte 2023-2025 e feito no parse.

tjsp_baixar_grade <- function(grade = GRADE, base_dir = file.path(DIR_RAW, "tjsp")) {

  purrr::pwalk(grade, function(id, slug, query_acento, ...) {

    destino <- file.path(base_dir, slug)
    dir.create(destino, recursive = TRUE, showWarnings = FALSE)

    if (length(list.files(destino, pattern = "\\.html$")) > 0) {
      message("[TJSP] ", slug, " ja baixado, pulando")
      return(invisible(NULL))
    }

    message("[TJSP] ", id, " | ", query_acento)
    tryCatch(
      lex::tjsp_cjpg_download(query_acento, dir = destino),
      error = function(e) message("  ERRO em ", slug, ": ", conditionMessage(e))
    )
    Sys.sleep(2)
  })
}

tjsp_ler_grade <- function(grade = GRADE, base_dir = file.path(DIR_RAW, "tjsp")) {

  purrr::pmap_dfr(grade, function(id, slug, termo_trafico, termo_lavagem, ...) {

    arqs <- list.files(file.path(base_dir, slug), full.names = TRUE,
                       pattern = "\\.html$")
    if (length(arqs) == 0) return(tibble::tibble())

    purrr::map_dfr(arqs, purrr::possibly(lex::tjsp_cjpg_parse, tibble::tibble())) %>%
      dplyr::mutate(
        id_query      = id,
        slug          = slug,
        termo_trafico = termo_trafico,
        termo_lavagem = termo_lavagem
      )
  }) %>%
    padronizar_processo()
}

# ==========================================================================
# 3. TJMS (e-SAJ, raspagem propria)
# ==========================================================================

BASE_TJMS <- "https://esaj.tjms.jus.br/cjpg"

cjpg_sessao <- function(base = BASE_TJMS) {
  r <- httr::GET(paste0(base, "/"), UA, httr::timeout(120))
  httr::stop_for_status(r)
  invisible(TRUE)
}

cjpg_parms <- function(pesquisa,
                       data_inicial = "", data_final = "",
                       classe = "", assunto = "", vara = "") {
  list(
    "conversationId"                      = "",
    "dadosConsulta.pesquisaLivre"         = pesquisa,
    "tipoNumero"                          = "UNIFICADO",
    "numeroDigitoAnoUnificado"            = "",
    "foroNumeroUnificado"                 = "",
    "dadosConsulta.nuProcesso"            = "",
    "dadosConsulta.nuProcessoAntigo"      = "",
    "classeTreeSelection.values"          = classe,
    "classeTreeSelection.text"            = "",
    "assuntoTreeSelection.values"         = assunto,
    "assuntoTreeSelection.text"           = "",
    "agenteSelectedEntitiesList"          = "",
    "contadoragente"                      = "0",
    "contadorMaioragente"                 = "0",
    "cdAgente"                            = "",
    "nmAgente"                            = "",
    "dadosConsulta.dtInicio"              = data_inicial,
    "dadosConsulta.dtFim"                 = data_final,
    "varasTreeSelection.values"           = vara,
    "varasTreeSelection.text"             = "",
    "dadosConsulta.ordenacao"             = "DESC",
    "dadosConsulta.localPesquisa.cdLocal" = "-1"
  )
}

cjpg_buscar <- function(parms, base = BASE_TJMS) {
  .tentar(function() {
    httr::POST(paste0(base, "/pesquisar.do"),
               body = parms, encode = "form", UA, httr::timeout(180))
  })
}

cjpg_pagina <- function(n, base = BASE_TJMS) {
  .tentar(function() {
    httr::GET(paste0(base, "/trocarDePagina.do"),
              query = list(pagina = n, conversationId = ""),
              UA, httr::timeout(180))
  })
}

cjpg_resumo <- function(html) {
  txt <- rvest::html_text2(html)
  m <- stringr::str_match(
    txt, "Resultados?\\s+([\\d.]+)\\s+a\\s+([\\d.]+)\\s+de\\s+([\\d.]+)"
  )
  if (is.na(m[1, 1])) return(list(de = NA_integer_, ate = NA_integer_, total = 0L))
  n <- function(x) as.integer(stringr::str_remove_all(x, "\\."))
  list(de = n(m[1, 2]), ate = n(m[1, 3]), total = n(m[1, 4]))
}

#' Baixa uma query em um ano e devolve o total informado pelo tribunal
cjpg_baixar <- function(pesquisa, data_inicial = "", data_final = "",
                        diretorio, max_paginas = Inf, intervalo = 2,
                        base = BASE_TJMS, ...) {

  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
  cjpg_sessao(base)

  parms <- cjpg_parms(pesquisa, data_inicial, data_final, ...)
  r1 <- cjpg_buscar(parms, base)
  h1 <- httr::content(r1, "text", encoding = "UTF-8")

  info <- cjpg_resumo(xml2::read_html(h1))
  if (is.na(info$total) || info$total == 0) {
    message("  nenhum resultado")
    return(0L)
  }

  por_pagina <- max(info$ate - info$de + 1L, 1L)
  n_pag <- min(ceiling(info$total / por_pagina), max_paginas)
  message("  ", info$total, " decisoes | ", n_pag, " pagina(s)")

  writeLines(h1, file.path(diretorio, "pagina_0001.html"), useBytes = TRUE)

  if (n_pag > 1) {
    for (p in 2:n_pag) {
      Sys.sleep(intervalo)
      rp <- cjpg_pagina(p, base)
      writeLines(httr::content(rp, "text", encoding = "UTF-8"),
                 file.path(diretorio, sprintf("pagina_%04d.html", p)),
                 useBytes = TRUE)
    }
  }
  as.integer(info$total)
}

.campo <- function(txt, rotulo) {
  padrao <- paste0(rotulo, ":\\s*(.*?)\\s*(?=\\n|$)")
  stringr::str_squish(stringr::str_match(txt, stringr::regex(padrao))[, 2])
}

cjpg_ler_arquivo <- function(arquivo) {

  h <- xml2::read_html(arquivo, encoding = "UTF-8")

  blocos <- rvest::html_elements(h, xpath = "//tr[contains(@class,'fundocinza1')]")
  if (length(blocos) == 0) {
    blocos <- rvest::html_elements(h, xpath = "//table[contains(@class,'fundocinza1')]")
  }
  if (length(blocos) == 0) return(tibble::tibble())

  purrr::map_dfr(blocos, function(b) {

    txt <- rvest::html_text2(b)

    link <- rvest::html_element(b, xpath = ".//a[contains(@class,'downloadEmenta')]")
    processo <- rvest::html_text2(link)
    if (is.na(processo)) {
      processo <- stringr::str_extract(
        txt, "\\d{7}-\\d{2}\\.\\d{4}\\.\\d\\.\\d{2}\\.\\d{4}"
      )
    }

    divs <- rvest::html_elements(b, xpath = ".//div|.//td[@align='justify']")
    julgado <- if (length(divs) > 0) {
      cand <- rvest::html_text2(divs); cand[which.max(nchar(cand))]
    } else NA_character_

    tibble::tibble(
      processo              = formatar_cnj(stringr::str_squish(processo)),
      cd_documento          = rvest::html_attr(link, "cdacordao"),
      classe                = .campo(txt, "Classe"),
      assunto               = .campo(txt, "Assunto"),
      magistrado            = .campo(txt, "Magistrado"),
      comarca               = .campo(txt, "Comarca"),
      foro                  = .campo(txt, "Foro"),
      vara                  = .campo(txt, "Vara"),
      data_disponibilizacao = .campo(txt, "Data de Disponibiliza..o"),
      julgado               = stringr::str_squish(julgado),
      arquivo               = basename(arquivo)
    )
  })
}

cjpg_ler <- function(diretorio) {
  arqs <- list.files(diretorio, pattern = "\\.html$", full.names = TRUE,
                     recursive = TRUE)
  if (length(arqs) == 0) return(tibble::tibble())
  purrr::map_dfr(arqs, purrr::possibly(cjpg_ler_arquivo, tibble::tibble())) %>%
    dplyr::distinct(processo, .keep_all = TRUE)
}

tjms_baixar_grade <- function(grade = GRADE, anos = ANOS,
                              base_dir = file.path(DIR_RAW, "tjms")) {

  purrr::pmap_dfr(grade, function(id, slug, query_semacento, ...) {
    purrr::map_dfr(anos, function(ano) {

      destino <- file.path(base_dir, slug, ano)
      if (length(list.files(destino, pattern = "\\.html$")) > 0) {
        message("[TJMS] ", slug, " ", ano, " ja baixado, pulando")
        return(tibble::tibble(id_query = id, slug = slug, ano = ano,
                              total_informado = NA_integer_))
      }

      message("[TJMS] ", id, " | ", ano, " | ", query_semacento)
      total <- tryCatch(
        cjpg_baixar(
          pesquisa     = query_semacento,
          data_inicial = paste0("01/01/", ano),
          data_final   = paste0("31/12/", ano),
          diretorio    = destino
        ),
        error = function(e) {
          message("  ERRO: ", conditionMessage(e))
          NA_integer_
        }
      )

      Sys.sleep(2)
      tibble::tibble(id_query = id, slug = slug, ano = ano,
                     total_informado = as.integer(total))
    })
  })
}

tjms_ler_grade <- function(grade = GRADE, anos = ANOS,
                           base_dir = file.path(DIR_RAW, "tjms")) {

  purrr::pmap_dfr(grade, function(id, slug, termo_trafico, termo_lavagem, ...) {
    purrr::map_dfr(anos, function(ano) {
      cjpg_ler(file.path(base_dir, slug, ano)) %>%
        dplyr::mutate(
          id_query      = id,
          slug          = slug,
          ano_busca     = ano,
          termo_trafico = termo_trafico,
          termo_lavagem = termo_lavagem
        )
    })
  }) %>%
    padronizar_processo()
}

# ==========================================================================
# 4. TJRN (API de jurisprudencia)
# ==========================================================================

URL_TJRN <- "https://jurisprudencia.tjrn.jus.br/api/pesquisar"

tjrn_corpo <- function(termo, dt_inicio, dt_fim, pagina = 1) {
  list(
    jurisprudencia = list(
      ementa = "", inteiro_teor = termo, nr_processo = "",
      id_classe_judicial = "", id_orgao_julgador = "", id_relator = "",
      id_colegiado = "", id_juiz = "", id_vara = "",
      dt_inicio = dt_inicio, dt_fim = dt_fim,
      origem = "", sistema = "", decisoes = "", jurisdicoes = "",
      grau = "1"
    ),
    page = pagina,
    usuario = list(matricula = "", token = "")
  )
}

tjrn_post <- function(termo, dt_inicio, dt_fim, pagina) {
  .tentar(function() {
    httr::POST(
      url = URL_TJRN,
      httr::add_headers(`Content-Type` = "application/json"),
      body = jsonlite::toJSON(tjrn_corpo(termo, dt_inicio, dt_fim, pagina),
                              auto_unbox = TRUE),
      encode = "raw",
      httr::timeout(180)
    )
  })
}

tjrn_baixar <- function(termo, ano, diretorio, intervalo = 1) {

  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
  dt_inicio <- stringr::str_glue("01-01-{ano}")
  dt_fim    <- stringr::str_glue("31-12-{ano}")

  r <- tjrn_post(termo, dt_inicio, dt_fim, 1)
  ct <- httr::content(r)

  total <- ct$hits$total
  if (is.list(total)) total <- total$value
  total <- total %||% 0

  if (total == 0) {
    message("  nenhum resultado")
    return(0L)
  }
  message("  ", total, " resultado(s)")

  saveRDS(ct, file.path(diretorio, "pagina1.rds"))

  n_pag <- ceiling(total / 10)
  if (n_pag > 1) {
    for (p in 2:n_pag) {
      Sys.sleep(intervalo)
      rp <- tjrn_post(termo, dt_inicio, dt_fim, p)
      saveRDS(httr::content(rp), file.path(diretorio, paste0("pagina", p, ".rds")))
    }
  }
  as.integer(total)
}

tjrn_baixar_grade <- function(grade = GRADE, anos = ANOS,
                              base_dir = file.path(DIR_RAW, "tjrn")) {

  purrr::pmap_dfr(grade, function(id, slug, query_acento, ...) {
    purrr::map_dfr(anos, function(ano) {

      destino <- file.path(base_dir, slug, ano)
      if (length(list.files(destino, pattern = "\\.rds$")) > 0) {
        message("[TJRN] ", slug, " ", ano, " ja baixado, pulando")
        return(tibble::tibble(id_query = id, slug = slug, ano = ano,
                              total_informado = NA_integer_))
      }

      message("[TJRN] ", id, " | ", ano, " | ", query_acento)
      total <- tryCatch(
        tjrn_baixar(query_acento, ano, destino),
        error = function(e) {
          message("  ERRO: ", conditionMessage(e))
          NA_integer_
        }
      )

      tibble::tibble(id_query = id, slug = slug, ano = ano,
                     total_informado = as.integer(total))
    })
  })
}

#' Le um RDS paginado da API e devolve os hits
#'
#' Nomes de campo conferidos contra a resposta real da API (formato
#' Elasticsearch): cada documento tem _id e _source, e dentro de _source
#' o processo esta em `numero_processo` (nao `nr_processo`) e o tipo em
#' `tipo_teor`, com valores "Sentenca" ou "Decisao". Nao existe campo
#' `vara` separado: a vara vem dentro de `orgao_julgador`.
tjrn_ler_arquivo <- function(arquivo) {
  ct <- readRDS(arquivo)
  hits <- ct$hits$hits %||% list()
  if (length(hits) == 0) return(tibble::tibble())

  purrr::map_dfr(hits, function(h) {
    s <- h$`_source` %||% list()
    tibble::tibble(
      id_es          = as.character(h$`_id` %||% NA),
      id_documento   = as.character(s$id_documento_teor %||% NA),
      processo       = formatar_cnj(as.character(s$numero_processo %||% NA)),
      tipo           = as.character(s$tipo_teor %||% NA),
      classe         = as.character(s$classe_judicial %||% NA),
      orgao_julgador = as.character(s$orgao_julgador %||% NA),
      magistrado     = as.character(s$magistrado %||% NA),
      grau           = as.integer(s$grau %||% NA),
      sistema        = as.character(s$sistema %||% NA),
      dt_assinatura  = as.character(s$dt_assinatura_teor %||% NA),
      dt_publicacao  = as.character(s$dt_publicacao %||% NA),
      inteiro_teor   = limpar_html(as.character(s$inteiro_teor %||% NA)),
      arquivo        = basename(arquivo)
    )
  })
}

tjrn_ler_grade <- function(grade = GRADE, anos = ANOS,
                           base_dir = file.path(DIR_RAW, "tjrn")) {

  purrr::pmap_dfr(grade, function(id, slug, termo_trafico, termo_lavagem, ...) {
    purrr::map_dfr(anos, function(ano) {
      arqs <- list.files(file.path(base_dir, slug, ano), pattern = "\\.rds$",
                         full.names = TRUE)
      if (length(arqs) == 0) return(tibble::tibble())
      purrr::map_dfr(arqs, purrr::possibly(tjrn_ler_arquivo, tibble::tibble())) %>%
        dplyr::mutate(
          id_query      = id,
          slug          = slug,
          ano_busca     = ano,
          termo_trafico = termo_trafico,
          termo_lavagem = termo_lavagem
        )
    })
  }) %>%
    padronizar_processo()
}

# ==========================================================================
# 5. EXECUCAO
# ==========================================================================

# ---- 5.1 Download ---------------------------------------------------------

tjsp_baixar_grade()
log_tjms <- tjms_baixar_grade()
log_tjrn <- tjrn_baixar_grade()

# ---- 5.2 Parse ------------------------------------------------------------

df_tjsp <- tjsp_ler_grade()
if (nrow(df_tjsp) > 0) {
  df_tjsp <- df_tjsp %>%
    dplyr::mutate(
      data_de_disponibilizacao = lubridate::dmy(data_de_disponibilizacao),
      ano = lubridate::year(data_de_disponibilizacao)
    ) %>%
    dplyr::filter(ano %in% ANOS)
}

df_tjms <- tjms_ler_grade()
if (nrow(df_tjms) > 0) {
  df_tjms <- df_tjms %>%
    dplyr::mutate(
      data_disponibilizacao = lubridate::dmy(data_disponibilizacao),
      ano = lubridate::year(data_disponibilizacao)
    )
}

df_tjrn <- tjrn_ler_grade()

# O TJRN devolve sentencas e decisoes interlocutorias mesmo com grau = 1.
# So as sentencas interessam, entao as decisoes sao descartadas aqui, antes
# de qualquer contagem. O bruto continua em dados-raw/tjrn/, entao da para
# reparsear sem baixar de novo se um dia as decisoes fizerem falta.
df_tjrn_sentencas <- if (nrow(df_tjrn) > 0) {
  eh_sentenca <- stringr::str_detect(
    df_tjrn$tipo, stringr::regex("senten", ignore_case = TRUE)
  )
  message("[TJRN] ", sum(!eh_sentenca, na.rm = TRUE), " de ", nrow(df_tjrn),
          " documento(s) descartado(s) por nao serem sentenca")
  dplyr::filter(df_tjrn, eh_sentenca)
} else df_tjrn

# ---- 5.3 Deduplicacao pela coluna processo --------------------------------
# A mesma sentenca costuma aparecer em varias queries (mesmo processo,
# termos diferentes). Aqui colapsamos para UMA linha por processo, sem
# perder rastreabilidade das buscas que o trouxeram. Duas trilhas, uma
# em texto e outra em colunas:
#
#   n_queries      -> em quantas buscas distintas o processo apareceu
#   queries        -> quais buscas (slug, mesmo nome da pasta em dados-raw)
#   termos_trafico -> quais termos do lado do trafico o encontraram
#   termos_lavagem -> quais termos do lado da lavagem o encontraram
#   busca_<slug>   -> uma coluna logica por busca da grade: TRUE se aquela
#                     busca trouxe o processo. Serve para filtrar e cruzar
#                     no Excel sem ter que quebrar a coluna de texto.
#
# Todas as buscas da GRADE viram coluna, mesmo as que nao acharam nada
# (ficam FALSE), para as tres planilhas terem o mesmo layout.
#
# As colunas no singular (id_query, slug, termo_trafico, termo_lavagem)
# saem do resultado: elas descrevem so a linha que sobreviveu e seriam
# lidas como "o processo veio dessa busca", o que e falso quando ele veio
# de varias. Quem quer o detalhe por busca usa contagem_por_query, que
# roda sobre os dados ANTES da deduplicacao.
#
# Entre linhas do mesmo processo fica a de texto mais longo (a mais
# completa); os demais campos, se divergirem, vem dessa linha.

dedup_processo <- function(df, rotulo = "", col_texto = NULL, grade = GRADE) {

  if (nrow(df) == 0) return(df)

  if (!"processo" %in% names(df)) {
    stop("dedup_processo [", rotulo, "]: coluna `processo` ausente. Colunas: ",
         paste(names(df), collapse = ", "))
  }

  n_sem_numero <- sum(is.na(df$processo))
  if (n_sem_numero > 0) {
    message("[", rotulo, "] ", n_sem_numero,
            " linha(s) sem numero de processo, descartada(s)")
  }

  base <- dplyr::filter(df, !is.na(processo))
  if (nrow(base) == 0) return(base)

  # trilha em texto
  trilha <- base %>%
    dplyr::group_by(processo) %>%
    dplyr::summarise(
      n_queries      = dplyr::n_distinct(slug),
      queries        = paste(sort(unique(slug)),          collapse = "; "),
      termos_trafico = paste(sort(unique(termo_trafico)), collapse = "; "),
      termos_lavagem = paste(sort(unique(termo_lavagem)), collapse = "; "),
      .groups = "drop"
    )

  # trilha em colunas, uma por busca. O union com os slugs observados
  # garante que nada se perca se os dados tiverem uma busca que ja saiu
  # da GRADE (termo comentado depois do download).
  cols_busca <- paste0("busca_", union(grade$slug, sort(unique(base$slug))))

  marcas <- base %>%
    dplyr::distinct(processo, slug) %>%
    dplyr::transmute(processo, coluna = paste0("busca_", slug), presente = TRUE) %>%
    tidyr::pivot_wider(names_from = coluna, values_from = presente,
                       values_fill = FALSE)

  faltando <- setdiff(cols_busca, names(marcas))
  if (length(faltando) > 0) marcas[faltando] <- FALSE
  marcas <- dplyr::select(marcas, processo, dplyr::all_of(cols_busca))

  # uma linha por processo: a de texto mais longo, empate pela ordem original
  if (!is.null(col_texto) && col_texto %in% names(base)) {
    base <- base %>%
      dplyr::mutate(
        .n_texto = dplyr::coalesce(nchar(as.character(.data[[col_texto]])), 0L)
      ) %>%
      dplyr::arrange(dplyr::desc(.n_texto)) %>%
      dplyr::select(-.n_texto)
  }

  out <- base %>%
    dplyr::distinct(processo, .keep_all = TRUE) %>%
    dplyr::select(-dplyr::any_of(c("id_query", "slug",
                                   "termo_trafico", "termo_lavagem"))) %>%
    dplyr::left_join(trilha, by = "processo") %>%
    dplyr::left_join(marcas, by = "processo") %>%
    dplyr::relocate(processo, n_queries, queries, termos_trafico,
                    termos_lavagem, dplyr::all_of(cols_busca))

  # a garantia de uma linha por processo e explicita: se algum dia o
  # parser mudar e quebrar isso, o script para aqui em vez de exportar
  # planilha duplicada em silencio
  stopifnot(
    !any(duplicated(out$processo)),
    !any(is.na(out$processo))
  )

  message("[", rotulo, "] ", nrow(df), " linha(s) -> ", nrow(out),
          " processo(s) unico(s)")
  out
}

df_tjsp_final           <- dedup_processo(df_tjsp, "TJSP",
                                          col_texto = "resumo")
df_tjms_final           <- dedup_processo(df_tjms, "TJMS",
                                          col_texto = "julgado")
df_tjrn_sentencas_final <- dedup_processo(df_tjrn_sentencas, "TJRN",
                                          col_texto = "inteiro_teor")

# ---- 5.4 Contagens --------------------------------------------------------
# (a) por query: quantos hits cada busca trouxe, com o mesmo processo
#     podendo contar em mais de uma query. Serve para conferir paginacao.
# (b) unica: quantos processos DISTINTOS por tribunal e ano, ja sem
#     duplicacao. E o N real para a apresentacao.
# (c) sobreposicao: quantos processos vieram de 1, 2, 3... buscas. E a
#     medida de quanto os termos de lavagem se recobrem entre si.

.contar_query <- function(df, tribunal, col_ano) {
  if (nrow(df) == 0 || !col_ano %in% names(df)) {
    return(tibble::tibble(tribunal = character(), slug = character(),
                          ano = numeric(), n = integer()))
  }
  df %>%
    dplyr::count(slug, ano = .data[[col_ano]], name = "n") %>%
    dplyr::mutate(tribunal = tribunal) %>%
    dplyr::select(tribunal, slug, ano, n)
}

.contar_unica <- function(df, tribunal, col_ano) {
  if (nrow(df) == 0 || !col_ano %in% names(df)) {
    return(tibble::tibble(tribunal = character(), ano = numeric(),
                          n_processos = integer()))
  }
  df %>%
    dplyr::count(ano = .data[[col_ano]], name = "n_processos") %>%
    dplyr::mutate(tribunal = tribunal) %>%
    dplyr::select(tribunal, ano, n_processos)
}

.contar_sobreposicao <- function(df, tribunal) {
  if (nrow(df) == 0 || !"n_queries" %in% names(df)) {
    return(tibble::tibble(tribunal = character(), n_queries = integer(),
                          n_processos = integer()))
  }
  df %>%
    dplyr::count(n_queries, name = "n_processos") %>%
    dplyr::mutate(tribunal = tribunal) %>%
    dplyr::select(tribunal, n_queries, n_processos)
}

# contagem por query usa os dfs ANTES da deduplicacao
contagem_por_query <- dplyr::bind_rows(
  .contar_query(df_tjsp, "TJSP", "ano"),
  .contar_query(df_tjms, "TJMS", "ano"),
  .contar_query(df_tjrn_sentencas, "TJRN", "ano_busca")
) %>%
  dplyr::arrange(tribunal, slug, ano)

# contagem unica usa os dfs DEPOIS da deduplicacao
contagem_unica <- dplyr::bind_rows(
  .contar_unica(df_tjsp_final, "TJSP", "ano"),
  .contar_unica(df_tjms_final, "TJMS", "ano"),
  .contar_unica(df_tjrn_sentencas_final, "TJRN", "ano_busca")
) %>%
  dplyr::arrange(tribunal, ano)

# sobreposicao entre buscas, ja com uma linha por processo
contagem_sobreposicao <- dplyr::bind_rows(
  .contar_sobreposicao(df_tjsp_final,           "TJSP"),
  .contar_sobreposicao(df_tjms_final,           "TJMS"),
  .contar_sobreposicao(df_tjrn_sentencas_final, "TJRN")
) %>%
  dplyr::arrange(tribunal, n_queries)

print(contagem_por_query, n = Inf)
print(contagem_unica, n = Inf)
print(contagem_sobreposicao, n = Inf)

# ---- 5.5 Exportacao -------------------------------------------------------
# O Excel corta celulas acima de 32.767 caracteres

cortar <- function(df, col) {
  if (col %in% names(df)) df[[col]] <- stringr::str_sub(df[[col]], 1, 32000)
  df
}

writexl::write_xlsx(
  list(
    tjsp           = cortar(df_tjsp_final, "resumo"),
    contagem_query = contagem_por_query    %>% dplyr::filter(tribunal == "TJSP"),
    contagem_unica = contagem_unica        %>% dplyr::filter(tribunal == "TJSP"),
    sobreposicao   = contagem_sobreposicao %>% dplyr::filter(tribunal == "TJSP")
  ),
  file.path(DIR_OUT, "tjsp_trafico_e_lavagem.xlsx")
)

writexl::write_xlsx(
  list(
    tjms           = cortar(df_tjms_final, "julgado"),
    contagem_query = contagem_por_query    %>% dplyr::filter(tribunal == "TJMS"),
    contagem_unica = contagem_unica        %>% dplyr::filter(tribunal == "TJMS"),
    sobreposicao   = contagem_sobreposicao %>% dplyr::filter(tribunal == "TJMS")
  ),
  file.path(DIR_OUT, "tjms_trafico_e_lavagem.xlsx")
)

# Uma unica aba de dados: so sentencas, uma linha por processo. A antiga
# aba tjrn_tudo (sentencas + decisoes, sem deduplicar) saiu porque, filtrada
# por sentenca e deduplicada, seria copia identica de tjrn_sentencas.
writexl::write_xlsx(
  list(
    tjrn_sentencas = cortar(df_tjrn_sentencas_final, "inteiro_teor"),
    contagem_query = contagem_por_query    %>% dplyr::filter(tribunal == "TJRN"),
    contagem_unica = contagem_unica        %>% dplyr::filter(tribunal == "TJRN"),
    sobreposicao   = contagem_sobreposicao %>% dplyr::filter(tribunal == "TJRN")
  ),
  file.path(DIR_OUT, "tjrn_trafico_e_lavagem.xlsx")
)

saveRDS(
  list(
    tjsp           = df_tjsp_final,
    tjms           = df_tjms_final,
    tjrn_sentencas = df_tjrn_sentencas_final,
    grade          = GRADE,
    contagem_por_query    = contagem_por_query,
    contagem_unica        = contagem_unica,
    contagem_sobreposicao = contagem_sobreposicao
  ),
  file.path(DIR_OUT, "coleta_tres_tribunais.rds")
)
