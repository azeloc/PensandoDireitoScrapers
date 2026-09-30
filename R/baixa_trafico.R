# ==========================================================================
# COLETA DE JURISPRUDENCIA DE PRIMEIRO GRAU: SO "TRAFICO DE DROGAS"
#
# Irmao do baixa_todos_os_dados.R, que cruza trafico COM lavagem. Aqui a
# busca e de um termo so, o que muda a escala em duas ordens de grandeza:
# ~133 mil decisoes contra ~1.100. Tudo que e diferente vem disso.
#
#   dados-raw-trafico/
#     tjsp/<slug>/<ano>/   (HTML do e-SAJ)
#     tjms/<slug>/<ano>/   (HTML do e-SAJ)
#     tjrn/<slug>/<ano>/   (RDS da API de jurisprudencia)
#
# Estrutura em quatro etapas:
#   0) sondar   -> pergunta quantos resultados existem, sem baixar nada
#   1) baixar   -> salva o bruto em disco, retomavel pagina a pagina
#   2) ler      -> le o bruto e devolve tibble, so metadados
#   3) exportar -> CSV + Excel por tribunal, com conferencia de completude
#
# TRES DIFERENCAS QUE IMPORTAM em relacao ao script irmao:
#
#   1. ACENTO NO TJSP. O indice do e-SAJ paulista NAO normaliza acento.
#      Medido em 19/08/2026, ano de 2023:
#         "trafico de drogas"    ->    556 resultados
#         "tráfico de drogas"    -> 34.951 resultados
#      TJMS e TJRN dao o mesmo numero das duas formas. Por isso a busca
#      vai COM acento nos tres (ver TRIBUNAIS abaixo). Errar isso aqui
#      perde 98% da base sem dar nenhum sinal de erro.
#
#   2. TJSP SEM O PACOTE lex. O lex::tjsp_cjpg_download baixa as paginas
#      em sequencia sem intervalo nenhum entre requisicoes. Para as 233
#      paginas do script irmao tudo bem; para as 11.284 daqui seria
#      abuso de um servico publico. TJSP e TJMS rodam os dois no mesmo
#      raspador e-SAJ deste arquivo, com intervalo configuravel.
#
#   3. RETOMADA POR PAGINA. Com 3.500 paginas por ano, "a pasta tem algum
#      arquivo, entao ja baixei" e perigoso: se a coleta morre na pagina
#      500, a rodada seguinte pula o ano inteiro e a base fica com 14%
#      dos dados sem avisar. Cada ano so e dado por pronto quando grava
#      _completo.rds; sem esse marcador, a retomada baixa exatamente as
#      paginas que faltam.
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

DIR_RAW <- "dados-raw-trafico"
DIR_OUT <- "dados-saida"
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

# Guardar o corpo da decisao? FALSE = so metadados.
#
# Nao e economia de vaidade: o campo de texto do e-SAJ tem em media 25.813
# caracteres. Nas ~113 mil decisoes do TJSP isso daria ~2,9 GB de string
# so para ser descartado depois. Com FALSE o texto nunca chega a entrar
# na memoria, e o inteiro_teor do TJRN (que vem com imagens em base64,
# um documento passando de 13 milhoes de caracteres) e removido antes de
# gravar o RDS bruto.
GUARDAR_TEXTO <- FALSE

# Intervalo entre requisicoes, em segundos. Sao ~13.300 requisicoes no
# total, entao cada 0,5 s aqui vira ~1,8 h de relogio. Nao baixe disso
# sem necessidade: do outro lado tem tribunal publico.
INTERVALO_ESAJ <- 1.5
INTERVALO_TJRN <- 1

# De quantas em quantas paginas a busca e refeita para renovar a sessao.
# O e-SAJ derruba a sessao depois de algumas horas de paginacao e passa a
# responder HTTP 200 com um corpo de 19 bytes ("Sessao"). Renovar antes
# disso e mais barato que descobrir depois. Ver cjpg_pagina_valida().
RENOVAR_SESSAO_A_CADA <- 400

# Teto de paginas por ano. Inf = tudo. Util para testar o script inteiro
# em poucos minutos antes de soltar a coleta de verdade.
MAX_PAGINAS <- Inf

UA <- httr::user_agent(
  "Pesquisa academica (R/httr) - contato: seu.email@instituicao.br"
)

# --------------------------------------------------------------------------
# Termos
#
# Um termo por linha. A grade e a lista de termos, sem cruzamento: aqui
# nao existe o par trafico x lavagem do script irmao. Com mais de um termo
# ativo a rastreabilidade de origem liga sozinha (ver secao 5.3).
# --------------------------------------------------------------------------

TERMOS <- c(
  "tráfico de drogas"
  #, "11.343"
  #, "entorpecente"
)

# --------------------------------------------------------------------------
# Tribunais e-SAJ
#
# `acentos` controla se a busca vai com ou sem acento. TRUE nos dois: no
# TJSP e obrigatorio (60x mais resultados) e no TJMS e indiferente, entao
# fica igual para o codigo ter um caminho so.
# --------------------------------------------------------------------------

TRIBUNAIS <- list(
  tjsp = list(nome = "TJSP", base = "https://esaj.tjsp.jus.br/cjpg", acentos = TRUE),
  tjms = list(nome = "TJMS", base = "https://esaj.tjms.jus.br/cjpg", acentos = TRUE)
)

# ==========================================================================
# 1. HELPERS
# ==========================================================================

#' Remove acentos (para os buscadores que nao normalizam o indice)
sem_acento <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

#' Nome de pasta a partir do termo
slugificar <- function(x) {
  x %>%
    sem_acento() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_remove_all("^_|_$")
}

#' Monta a expressao de busca. Um termo so, entre aspas, para o buscador
#' tratar como frase exata em vez de palavras soltas.
montar_query <- function(termo, acentos = TRUE) {
  q <- stringr::str_c('"', termo, '"')
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

#' Padroniza a coluna do numero do processo (mesma logica do script irmao)
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

#' Retentativa. Recebe uma FUNCAO, nao uma expressao: argumento em R e
#' promise e so seria avaliado uma vez, o que faria as tentativas 2 a 4
#' devolverem o erro guardado da primeira em vez de refazer a requisicao.
.tentar <- function(f, tentativas = 4, espera = 3) {
  r <- NULL
  for (i in seq_len(tentativas)) {
    r <- try(f(), silent = TRUE)
    if (!inherits(r, "try-error") && httr::status_code(r) == 200) return(r)
    if (i < tentativas) {
      message("    falha na tentativa ", i, ", aguardando ", espera * i, "s")
      Sys.sleep(espera * i)
    }
  }
  if (inherits(r, "try-error")) stop(attr(r, "condition")$message)
  stop("Status HTTP ", httr::status_code(r), " apos ", tentativas, " tentativas.")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Milhar com ponto. format() com big.mark = "." avisa no locale pt_BR,
#' onde decimal.mark tambem e "."; formatC nao tem essa colisao.
.num <- function(x) formatC(as.integer(x), format = "d", big.mark = ".", decimal.mark = ",")

#' Formata segundos como "2h13m", para os avisos de tempo estimado
.duracao <- function(seg) {
  if (!is.finite(seg)) return("indeterminado")
  h <- floor(seg / 3600); m <- round((seg %% 3600) / 60)
  if (h > 0) paste0(h, "h", sprintf("%02d", m), "m") else paste0(m, "m")
}

# --------------------------------------------------------------------------
# Grade de consultas: uma linha por termo
# --------------------------------------------------------------------------

GRADE <- tibble::tibble(termo = TERMOS) %>%
  dplyr::mutate(
    id              = dplyr::row_number(),
    slug            = slugificar(termo),
    query_acento    = montar_query(termo, acentos = TRUE),
    query_semacento = montar_query(termo, acentos = FALSE),
    .before = 1
  )

print(GRADE, n = Inf)

# ==========================================================================
# 2. E-SAJ (TJSP e TJMS: mesmo software, mesmo raspador)
# ==========================================================================

cjpg_sessao <- function(base) {
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

cjpg_buscar <- function(parms, base) {
  .tentar(function() {
    httr::POST(paste0(base, "/pesquisar.do"),
               body = parms, encode = "form", UA, httr::timeout(180))
  })
}

cjpg_pagina <- function(n, base) {
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

#' Quantos resultados o tribunal diz ter, sem baixar nada
cjpg_sondar <- function(pesquisa, ano, base) {
  cjpg_sessao(base)
  r <- cjpg_buscar(
    cjpg_parms(pesquisa, paste0("01/01/", ano), paste0("31/12/", ano)), base
  )
  cjpg_resumo(xml2::read_html(httr::content(r, "text", encoding = "UTF-8")))$total
}

.arquivo_pagina <- function(diretorio, p) {
  file.path(diretorio, sprintf("pagina_%05d.html", p))
}
.marcador <- function(diretorio) file.path(diretorio, "_completo.rds")

#' Uma pagina de resultados so vale se tiver bloco de decisao dentro.
#'
#' Quando a sessao expira no meio da paginacao, o e-SAJ nao devolve erro:
#' responde HTTP 200 com um corpo de 19 bytes escrito "Sessao". Checar so
#' o status code aceita isso como pagina boa. Foi assim que a primeira
#' coleta gravou 2.018 paginas vazias no TJSP/2025 e ainda marcou o ano
#' como completo -- 20 mil decisoes perdidas sem nenhum sinal de erro.
cjpg_pagina_valida <- function(txt) {
  !is.na(txt) && nchar(txt) > 5000 && stringr::str_detect(txt, "fundocinza1")
}

#' Numeros das paginas que estao em disco E tem conteudo de verdade.
#' Arquivo de sessao expirada tem ~19 bytes; pagina real passa de 100 KB.
.paginas_boas <- function(diretorio, n_pag) {
  if (n_pag < 1) return(integer(0))
  arq <- .arquivo_pagina(diretorio, seq_len(n_pag))
  tam <- file.size(arq)
  which(!is.na(tam) & tam > 5000)
}

#' Baixa uma query em um ano, retomando de onde parou.
#'
#' Devolve o total informado pelo tribunal. A pagina 1 sempre e refeita
#' (e ela que restabelece a busca na sessao antes de o trocarDePagina.do
#' funcionar), mas as paginas 2..n ja em disco sao puladas.
cjpg_baixar <- function(pesquisa, ano, diretorio, base,
                        max_paginas = MAX_PAGINAS, intervalo = INTERVALO_ESAJ) {

  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)

  # O marcador nao basta: a coleta anterior pode te-lo gravado por cima de
  # paginas invalidas. Conferir o disco custa um file.size por arquivo e
  # torna a retomada auto-corretiva.
  if (file.exists(.marcador(diretorio))) {
    m <- readRDS(.marcador(diretorio))
    ruins <- m$n_pag - length(.paginas_boas(diretorio, m$n_pag))
    if (ruins == 0) {
      message("    ja completo (", .num(m$total), " resultados), pulando")
      return(m$total)
    }
    message("    marcador presente, mas ", .num(ruins),
            " pagina(s) invalida(s): refazendo essas paginas")
    unlink(.marcador(diretorio))
  }

  cjpg_sessao(base)
  r1 <- cjpg_buscar(
    cjpg_parms(pesquisa, paste0("01/01/", ano), paste0("31/12/", ano)), base
  )
  h1 <- httr::content(r1, "text", encoding = "UTF-8")

  info <- cjpg_resumo(xml2::read_html(h1))
  if (is.na(info$total) || info$total == 0) {
    message("    nenhum resultado")
    saveRDS(list(total = 0L, n_pag = 0L), .marcador(diretorio))
    return(0L)
  }

  por_pagina <- max(info$ate - info$de + 1L, 1L)
  n_pag <- min(ceiling(info$total / por_pagina), max_paginas)

  writeLines(h1, .arquivo_pagina(diretorio, 1), useBytes = TRUE)

  # pagina invalida conta como faltante, nao como baixada
  faltando <- if (n_pag > 1) {
    setdiff(2:n_pag, .paginas_boas(diretorio, n_pag))
  } else integer(0)

  message("    ", .num(info$total), " resultados | ",
          n_pag, " paginas | faltam ", length(faltando),
          " (~", .duracao(length(faltando) * (intervalo + 1)), ")")

  # refaz a busca do zero: e o que restabelece a sessao antes de o
  # trocarDePagina.do voltar a funcionar
  renovar <- function() {
    cjpg_sessao(base)
    cjpg_buscar(
      cjpg_parms(pesquisa, paste0("01/01/", ano), paste0("31/12/", ano)), base
    )
    invisible(TRUE)
  }

  for (i in seq_along(faltando)) {
    p <- faltando[i]

    # renovacao preventiva, antes de a sessao cair sozinha
    if (i > 1 && i %% RENOVAR_SESSAO_A_CADA == 0) {
      message("      renovando sessao (pagina ", i, ")")
      renovar()
      Sys.sleep(intervalo)
    }

    Sys.sleep(intervalo)
    txt <- httr::content(cjpg_pagina(p, base), "text", encoding = "UTF-8")

    # renovacao reativa: se veio corpo vazio, refaz a busca e tenta de novo
    if (!cjpg_pagina_valida(txt)) {
      message("      pagina ", p, " veio invalida, renovando sessao")
      renovar()
      Sys.sleep(intervalo)
      txt <- httr::content(cjpg_pagina(p, base), "text", encoding = "UTF-8")
      if (!cjpg_pagina_valida(txt)) {
        stop("pagina ", p, " continua invalida apos renovar a sessao. ",
             "Ano NAO marcado como completo; rode de novo para retomar.")
      }
    }

    writeLines(txt, .arquivo_pagina(diretorio, p), useBytes = TRUE)
    if (i %% 200 == 0) {
      message("      ", i, "/", length(faltando), " paginas")
    }
  }

  # conferencia final antes de dar o ano por pronto
  ruins <- n_pag - length(.paginas_boas(diretorio, n_pag))
  if (ruins > 0) {
    stop(.num(ruins), " pagina(s) invalida(s) em ", diretorio,
         ". Ano NAO marcado como completo; rode de novo para retomar.")
  }

  # o marcador so e gravado depois que todas as paginas estao em disco:
  # se o processo morrer no meio, a rodada seguinte retoma daqui
  if (is.finite(max_paginas)) {
    message("    ATENCAO: MAX_PAGINAS = ", max_paginas,
            ", ano nao marcado como completo")
  } else {
    saveRDS(list(total = as.integer(info$total), n_pag = n_pag),
            .marcador(diretorio))
  }
  as.integer(info$total)
}

.campo <- function(txt, rotulo) {
  padrao <- paste0(rotulo, ":\\s*(.*?)\\s*(?=\\n|$)")
  stringr::str_squish(stringr::str_match(txt, stringr::regex(padrao))[, 2])
}

#' Le uma pagina de resultados do e-SAJ.
#'
#' Com GUARDAR_TEXTO = FALSE o corpo da decisao nao chega a ser extraido:
#' em 113 mil linhas, extrair para descartar depois custaria ~2,9 GB de
#' memoria a toa.
cjpg_ler_arquivo <- function(arquivo, guardar_texto = GUARDAR_TEXTO) {

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

    linha <- tibble::tibble(
      processo              = formatar_cnj(stringr::str_squish(processo)),
      cd_documento          = rvest::html_attr(link, "cdacordao"),
      classe                = .campo(txt, "Classe"),
      assunto               = .campo(txt, "Assunto"),
      magistrado            = .campo(txt, "Magistrado"),
      comarca               = .campo(txt, "Comarca"),
      foro                  = .campo(txt, "Foro"),
      vara                  = .campo(txt, "Vara"),
      data_disponibilizacao = .campo(txt, "Data de Disponibiliza..o"),
      arquivo               = basename(arquivo)
    )

    if (guardar_texto) {
      divs <- rvest::html_elements(b, xpath = ".//div|.//td[@align='justify']")
      julgado <- if (length(divs) > 0) {
        cand <- rvest::html_text2(divs); cand[which.max(nchar(cand))]
      } else NA_character_
      linha$julgado <- stringr::str_squish(julgado)
    }
    linha
  })
}

cjpg_ler <- function(diretorio) {
  arqs <- list.files(diretorio, pattern = "^pagina_\\d+\\.html$",
                     full.names = TRUE)
  if (length(arqs) == 0) return(tibble::tibble())
  purrr::map_dfr(arqs, purrr::possibly(cjpg_ler_arquivo, tibble::tibble()))
}

esaj_baixar_grade <- function(trib, grade = GRADE, anos = ANOS) {

  cfg <- TRIBUNAIS[[trib]]
  purrr::pmap_dfr(grade, function(id, slug, query_acento, query_semacento, ...) {
    pesquisa <- if (cfg$acentos) query_acento else query_semacento
    purrr::map_dfr(anos, function(ano) {
      message("[", cfg$nome, "] ", ano, " | ", pesquisa)
      total <- tryCatch(
        cjpg_baixar(pesquisa, ano,
                    file.path(DIR_RAW, trib, slug, ano), cfg$base),
        error = function(e) {
          message("    ERRO: ", conditionMessage(e))
          NA_integer_
        }
      )
      tibble::tibble(tribunal = cfg$nome, id_query = id, slug = slug,
                     ano = ano, total_informado = as.integer(total))
    })
  })
}

esaj_ler_grade <- function(trib, grade = GRADE, anos = ANOS) {

  cfg <- TRIBUNAIS[[trib]]
  purrr::pmap_dfr(grade, function(id, slug, termo, ...) {
    purrr::map_dfr(anos, function(ano) {
      d <- cjpg_ler(file.path(DIR_RAW, trib, slug, ano))
      if (nrow(d) == 0) return(d)
      dplyr::mutate(d, id_query = id, slug = slug, ano_busca = ano, termo = termo)
    })
  }) %>%
    padronizar_processo()
}

# ==========================================================================
# 3. TJRN (API de jurisprudencia)
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

tjrn_total <- function(ct) {
  t <- ct$hits$total
  if (is.list(t)) t <- t$value
  as.integer(t %||% 0)
}

#' Tira o inteiro_teor antes de gravar o bruto.
#'
#' O campo vem com as imagens do documento embutidas em base64. Nas ~9,2
#' mil decisoes do TJRN isso seriam centenas de MB de RDS para um dado
#' que, em modo metadados, ninguem vai ler.
tjrn_enxugar <- function(ct, guardar_texto = GUARDAR_TEXTO) {
  if (guardar_texto) return(ct)
  hits <- ct$hits$hits %||% list()
  if (length(hits) == 0) return(ct)
  ct$hits$hits <- lapply(hits, function(h) {
    h$`_source`$inteiro_teor <- NULL
    h
  })
  ct
}

tjrn_sondar <- function(termo, ano) {
  tjrn_total(httr::content(
    tjrn_post(termo, paste0("01-01-", ano), paste0("31-12-", ano), 1)
  ))
}

tjrn_baixar <- function(termo, ano, diretorio, max_paginas = MAX_PAGINAS,
                        intervalo = INTERVALO_TJRN) {

  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)

  if (file.exists(.marcador(diretorio))) {
    m <- readRDS(.marcador(diretorio))
    message("    ja completo (", m$total, " resultados), pulando")
    return(m$total)
  }

  dt_inicio <- paste0("01-01-", ano)
  dt_fim    <- paste0("31-12-", ano)

  ct <- httr::content(tjrn_post(termo, dt_inicio, dt_fim, 1))
  total <- tjrn_total(ct)

  if (total == 0) {
    message("    nenhum resultado")
    saveRDS(list(total = 0L, n_pag = 0L), .marcador(diretorio))
    return(0L)
  }

  n_pag <- min(ceiling(total / 10), max_paginas)
  arq <- function(p) file.path(diretorio, sprintf("pagina_%05d.rds", p))

  saveRDS(tjrn_enxugar(ct), arq(1))
  faltando <- if (n_pag > 1) {
    setdiff(2:n_pag, which(file.exists(vapply(1:n_pag, arq, character(1)))))
  } else integer(0)

  message("    ", .num(total), " resultados | ", n_pag,
          " paginas | faltam ", length(faltando),
          " (~", .duracao(length(faltando) * (intervalo + 1)), ")")

  for (i in seq_along(faltando)) {
    p <- faltando[i]
    Sys.sleep(intervalo)
    saveRDS(tjrn_enxugar(httr::content(tjrn_post(termo, dt_inicio, dt_fim, p))),
            arq(p))
    if (i %% 200 == 0) message("      ", i, "/", length(faltando), " paginas")
  }

  if (is.finite(max_paginas)) {
    message("    ATENCAO: MAX_PAGINAS = ", max_paginas,
            ", ano nao marcado como completo")
  } else {
    saveRDS(list(total = total, n_pag = n_pag), .marcador(diretorio))
  }
  total
}

tjrn_baixar_grade <- function(grade = GRADE, anos = ANOS) {
  purrr::pmap_dfr(grade, function(id, slug, termo, query_acento, ...) {
    purrr::map_dfr(anos, function(ano) {
      message("[TJRN] ", ano, " | ", termo)
      total <- tryCatch(
        tjrn_baixar(termo, ano, file.path(DIR_RAW, "tjrn", slug, ano)),
        error = function(e) {
          message("    ERRO: ", conditionMessage(e))
          NA_integer_
        }
      )
      tibble::tibble(tribunal = "TJRN", id_query = id, slug = slug,
                     ano = ano, total_informado = as.integer(total))
    })
  })
}

#' Nomes de campo conferidos contra a resposta real da API (formato
#' Elasticsearch): o processo esta em `numero_processo` (nao `nr_processo`)
#' e o tipo em `tipo_teor`, com valores "Sentenca" ou "Decisao". Nao existe
#' campo `vara` separado: a vara vem dentro de `orgao_julgador`.
tjrn_ler_arquivo <- function(arquivo, guardar_texto = GUARDAR_TEXTO) {
  ct <- readRDS(arquivo)
  hits <- ct$hits$hits %||% list()
  if (length(hits) == 0) return(tibble::tibble())

  purrr::map_dfr(hits, function(h) {
    s <- h$`_source` %||% list()
    linha <- tibble::tibble(
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
      arquivo        = basename(arquivo)
    )
    if (guardar_texto) linha$inteiro_teor <- as.character(s$inteiro_teor %||% NA)
    linha
  })
}

tjrn_ler_grade <- function(grade = GRADE, anos = ANOS) {
  purrr::pmap_dfr(grade, function(id, slug, termo, ...) {
    purrr::map_dfr(anos, function(ano) {
      arqs <- list.files(file.path(DIR_RAW, "tjrn", slug, ano),
                         pattern = "^pagina_\\d+\\.rds$", full.names = TRUE)
      if (length(arqs) == 0) return(tibble::tibble())
      d <- purrr::map_dfr(arqs, purrr::possibly(tjrn_ler_arquivo, tibble::tibble()))
      if (nrow(d) == 0) return(d)
      dplyr::mutate(d, id_query = id, slug = slug, ano_busca = ano, termo = termo)
    })
  }) %>%
    padronizar_processo()
}

# ==========================================================================
# 4. SONDAGEM (etapa 0: quanto tem antes de baixar)
# ==========================================================================

sondar_grade <- function(grade = GRADE, anos = ANOS) {

  esaj <- purrr::map_dfr(names(TRIBUNAIS), function(trib) {
    cfg <- TRIBUNAIS[[trib]]
    purrr::pmap_dfr(grade, function(slug, query_acento, query_semacento, ...) {
      pesquisa <- if (cfg$acentos) query_acento else query_semacento
      purrr::map_dfr(anos, function(ano) {
        n <- tryCatch(cjpg_sondar(pesquisa, ano, cfg$base),
                      error = function(e) NA_integer_)
        Sys.sleep(INTERVALO_ESAJ)
        tibble::tibble(tribunal = cfg$nome, slug = slug, ano = ano,
                       total_informado = as.integer(n))
      })
    })
  })

  rn <- purrr::pmap_dfr(grade, function(slug, termo, ...) {
    purrr::map_dfr(anos, function(ano) {
      n <- tryCatch(tjrn_sondar(termo, ano), error = function(e) NA_integer_)
      Sys.sleep(INTERVALO_TJRN)
      tibble::tibble(tribunal = "TJRN", slug = slug, ano = ano,
                     total_informado = as.integer(n))
    })
  })

  dplyr::bind_rows(esaj, rn) %>%
    dplyr::mutate(paginas = ceiling(total_informado / 10)) %>%
    dplyr::arrange(tribunal, slug, ano)
}

# ==========================================================================
# 5. EXECUCAO
# ==========================================================================

# ---- 5.0 Sondagem ---------------------------------------------------------

sondagem <- sondar_grade()
print(sondagem, n = Inf)

message("\n=== VOLUME A COLETAR ===")
message("decisoes: ", .num(sum(sondagem$total_informado, na.rm = TRUE)))
message("paginas : ", .num(sum(sondagem$paginas, na.rm = TRUE)))
message("tempo   : ~", .duracao(sum(sondagem$paginas, na.rm = TRUE) *
                                (INTERVALO_ESAJ + 1)), " se baixar do zero\n")

# ---- 5.1 Download ---------------------------------------------------------

# Ordem deliberada: TJRN (~30 min) e TJMS (~45 min) antes do TJSP (~8 h).
# Se a coleta for interrompida, os dois tribunais menores ja estao inteiros
# em vez de nenhum estar. O script e retomavel: rodar de novo continua o
# TJSP exatamente da pagina onde parou.
log_tjrn <- tjrn_baixar_grade()
log_tjms <- esaj_baixar_grade("tjms")
log_tjsp <- esaj_baixar_grade("tjsp")

log_download <- dplyr::bind_rows(log_tjsp, log_tjms, log_tjrn)

# ---- 5.2 Parse ------------------------------------------------------------

df_tjsp <- esaj_ler_grade("tjsp")
if (nrow(df_tjsp) > 0) {
  df_tjsp <- df_tjsp %>%
    dplyr::mutate(
      data_disponibilizacao = lubridate::dmy(data_disponibilizacao),
      ano = lubridate::year(data_disponibilizacao)
    )
}

df_tjms <- esaj_ler_grade("tjms")
if (nrow(df_tjms) > 0) {
  df_tjms <- df_tjms %>%
    dplyr::mutate(
      data_disponibilizacao = lubridate::dmy(data_disponibilizacao),
      ano = lubridate::year(data_disponibilizacao)
    )
}

df_tjrn <- tjrn_ler_grade()

# O TJRN devolve sentencas e decisoes interlocutorias mesmo com grau = 1.
# So as sentencas interessam; o bruto continua em disco para reparsear.
df_tjrn_sentencas <- if (nrow(df_tjrn) > 0) {
  eh_sentenca <- stringr::str_detect(
    df_tjrn$tipo, stringr::regex("senten", ignore_case = TRUE)
  )
  message("[TJRN] ", sum(!eh_sentenca, na.rm = TRUE), " de ", nrow(df_tjrn),
          " documento(s) descartado(s) por nao serem sentenca")
  dplyr::filter(df_tjrn, eh_sentenca)
} else df_tjrn

# ---- 5.3 Deduplicacao pela coluna processo --------------------------------
# Uma linha por processo. As colunas de origem (n_queries, queries e uma
# busca_<slug> por termo) so aparecem quando ha mais de um termo na GRADE:
# com um termo so elas seriam constantes e so poluiriam a planilha.

dedup_processo <- function(df, rotulo = "", grade = GRADE) {

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

  multi <- dplyr::n_distinct(base$slug) > 1

  if (multi) {
    trilha <- base %>%
      dplyr::group_by(processo) %>%
      dplyr::summarise(
        n_queries = dplyr::n_distinct(slug),
        queries   = paste(sort(unique(slug)), collapse = "; "),
        termos    = paste(sort(unique(termo)), collapse = "; "),
        .groups   = "drop"
      )

    cols_busca <- paste0("busca_", union(grade$slug, sort(unique(base$slug))))
    marcas <- base %>%
      dplyr::distinct(processo, slug) %>%
      dplyr::transmute(processo, coluna = paste0("busca_", slug), presente = TRUE) %>%
      tidyr::pivot_wider(names_from = coluna, values_from = presente,
                         values_fill = FALSE)
    faltando <- setdiff(cols_busca, names(marcas))
    if (length(faltando) > 0) marcas[faltando] <- FALSE
    marcas <- dplyr::select(marcas, processo, dplyr::all_of(cols_busca))
  }

  out <- base %>%
    dplyr::distinct(processo, .keep_all = TRUE) %>%
    dplyr::select(-dplyr::any_of(c("id_query", "slug", "termo")))

  if (multi) {
    out <- out %>%
      dplyr::left_join(trilha, by = "processo") %>%
      dplyr::left_join(marcas, by = "processo") %>%
      dplyr::relocate(processo, n_queries, queries, termos,
                      dplyr::all_of(cols_busca))
  } else {
    out <- dplyr::relocate(out, processo)
  }

  stopifnot(!any(duplicated(out$processo)), !any(is.na(out$processo)))
  message("[", rotulo, "] ", .num(nrow(df)), " linha(s) -> ",
          .num(nrow(out)), " processo(s) unico(s)")
  out
}

df_tjsp_final           <- dedup_processo(df_tjsp, "TJSP")
df_tjms_final           <- dedup_processo(df_tjms, "TJMS")
df_tjrn_sentencas_final <- dedup_processo(df_tjrn_sentencas, "TJRN")

# ---- 5.4 Conferencia de completude ----------------------------------------
# A checagem que importa nesta escala: o tribunal disse ter N resultados,
# quantos foram efetivamente parseados? Uma paginacao interrompida aparece
# aqui como um deficit grande, em vez de passar como base menor do que
# deveria. Diferenca pequena e normal (a mesma decisao pode vir repetida
# entre paginas, e o e-SAJ reordena entre requisicoes).

.contar_lidos <- function(df, tribunal, col_ano) {
  if (nrow(df) == 0 || !col_ano %in% names(df)) {
    return(tibble::tibble(tribunal = character(), ano = numeric(),
                          n_lidos = integer()))
  }
  df %>%
    dplyr::count(ano = .data[[col_ano]], name = "n_lidos") %>%
    dplyr::mutate(tribunal = tribunal) %>%
    dplyr::select(tribunal, ano, n_lidos)
}

completude <- log_download %>%
  dplyr::group_by(tribunal, ano) %>%
  dplyr::summarise(total_informado = sum(total_informado, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(
    dplyr::bind_rows(
      .contar_lidos(df_tjsp, "TJSP", "ano_busca"),
      .contar_lidos(df_tjms, "TJMS", "ano_busca"),
      .contar_lidos(df_tjrn, "TJRN", "ano_busca")
    ),
    by = c("tribunal", "ano")
  ) %>%
  dplyr::mutate(
    n_lidos = dplyr::coalesce(n_lidos, 0L),
    deficit = total_informado - n_lidos,
    pct_lido = round(100 * n_lidos / dplyr::na_if(total_informado, 0), 1)
  )

# processos unicos por tribunal e ano, ja deduplicado. E o N da apresentacao.
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

contagem_unica <- dplyr::bind_rows(
  .contar_unica(df_tjsp_final, "TJSP", "ano_busca"),
  .contar_unica(df_tjms_final, "TJMS", "ano_busca"),
  .contar_unica(df_tjrn_sentencas_final, "TJRN", "ano_busca")
) %>%
  dplyr::arrange(tribunal, ano)

print(completude, n = Inf)
print(contagem_unica, n = Inf)

if (any(completude$pct_lido < 90, na.rm = TRUE)) {
  message("\nATENCAO: algum tribunal/ano ficou abaixo de 90% do informado. ",
          "Rode de novo para retomar as paginas que faltam.")
}

# ---- 5.5 Exportacao -------------------------------------------------------
# CSV e o formato principal: sao ~113 mil linhas no TJSP, e o Excel, alem
# de lento nesse volume, silenciosamente corta celula acima de 32.767
# caracteres. O xlsx sai junto pela conveniencia, mas o CSV e a fonte.

cortar <- function(df, col) {
  if (col %in% names(df)) df[[col]] <- stringr::str_sub(df[[col]], 1, 32000)
  df
}

exportar <- function(df, nome, col_texto = NULL) {
  if (nrow(df) == 0) {
    message("[", nome, "] vazio, nada a exportar")
    return(invisible(NULL))
  }
  readr_ok <- requireNamespace("readr", quietly = TRUE)
  csv <- file.path(DIR_OUT, paste0(nome, ".csv"))
  if (readr_ok) readr::write_excel_csv(df, csv) else utils::write.csv(df, csv, row.names = FALSE)

  d <- if (!is.null(col_texto)) cortar(df, col_texto) else df
  writexl::write_xlsx(
    list(
      dados          = d,
      completude     = completude     %>% dplyr::filter(tribunal == toupper(sub("trafico_", "", nome))),
      contagem_unica = contagem_unica %>% dplyr::filter(tribunal == toupper(sub("trafico_", "", nome)))
    ),
    file.path(DIR_OUT, paste0(nome, ".xlsx"))
  )
  message("[", nome, "] ", .num(nrow(df)), " linhas exportadas")
}

exportar(df_tjsp_final,           "trafico_tjsp", "julgado")
exportar(df_tjms_final,           "trafico_tjms", "julgado")
exportar(df_tjrn_sentencas_final, "trafico_tjrn", "inteiro_teor")

saveRDS(
  list(
    tjsp           = df_tjsp_final,
    tjms           = df_tjms_final,
    tjrn_sentencas = df_tjrn_sentencas_final,
    grade          = GRADE,
    sondagem       = sondagem,
    log_download   = log_download,
    completude     = completude,
    contagem_unica = contagem_unica
  ),
  file.path(DIR_OUT, "coleta_trafico.rds")
)

message("\nPronto. Saidas em ", DIR_OUT, "/trafico_*.{csv,xlsx} e coleta_trafico.rds")
