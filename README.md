# Tráfico de drogas e lavagem de dinheiro na jurisprudência de 1º grau (TJSP, TJMS, TJRN)

Código de coleta e tabelas finais de censo dos processos de primeiro grau que
mencionam **tráfico de drogas** junto com **lavagem de dinheiro**, julgados
em 2023, 2024 e 2025 no TJSP, no TJMS e no TJRN.

As strings de busca exatas, os endpoints e os recortes de data de cada
tribunal estão em [CONSULTAS.md](CONSULTAS.md).

## Conteúdo

```
R/
  baixa_todos_os_dados.R   coleta fase 1: tráfico × lavagem (7 consultas, 3 tribunais)
  baixa_trafico.R          coleta fase 2: só "tráfico de drogas" (3 tribunais)
  censo_tjms_tjrn.R        tabelas de censo do TJMS e do TJRN
  amostragem_tjsp.R        tabelas do TJSP (amostra + reposição + complementar = censo)
  amostragem_trafico.R     amostra da fase 2 (só tráfico), por tribunal
dados-saida/amostragem/    tabelas finais de censo (CSV e XLSX)
CONSULTAS.md               consultas exatas, por tribunal
```

## Tabelas de censo

Todas têm o mesmo layout: `processo` (número CNJ), `id_interno`, `ano_captura`
e `n_queries` (em quantas das 7 consultas o processo apareceu).

| Tribunal | Arquivo(s) | IDs | Processos |
|---|---|---|---:|
| TJMS | `censo_tjms_trafico_lavagem` | TLMS001–TLMS080 | 80 |
| TJRN | `censo_tjrn_trafico_lavagem` | TLRN001–TLRN062 | 62 |
| TJSP | `amostra_tjsp_trafico_lavagem` | TLSP0001–TLSP0226 | 226 |
| TJSP | `amostra_tjsp_reposicao` | TLSP0227–TLSP0316 | 90 |
| TJSP | `amostra_tjsp_complementar` | TLSP0317–TLSP0652 | 336 |

No TJMS e no TJRN o censo é direto: entram todos os processos.

O TJSP começou como amostra e depois foi ampliado para censo, por isso tem três
arquivos que, somados, cobrem todo o universo de 652 processos sem sobreposição:

1. **Amostra principal** (65 em 2023, 64 em 2024, 97 em 2025). Todo processo
   com `n_queries >= 3` entra obrigatoriamente, e o resto da cota anual é
   sorteado entre os processos com 1 ou 2 consultas (semente `20260821`).
2. **Reposição**: 30 por ano, sorteados entre os que ficaram fora da amostra.
3. **Complementar**: todo o restante do universo, sem sorteio.

A numeração dos IDs segue essa ordem, então os casos já codificados mantêm o
mesmo ID.

## Como reproduzir

Os scripts usam caminhos relativos à raiz do repositório. Abra o `.Rproj` ou
rode `Rscript R/<script>.R` a partir da raiz.

```r
# 1. coleta fase 1: grava o bruto em dados-raw/ e as planilhas
#    dados-saida/{tjsp,tjms,tjrn}_trafico_e_lavagem.xlsx
source("R/baixa_todos_os_dados.R")

# 2. tabelas de censo, lidas das planilhas do passo 1
source("R/censo_tjms_tjrn.R")
source("R/amostragem_tjsp.R")

# 3. coleta fase 2: grava o bruto em dados-raw-trafico/ e as planilhas
#    dados-saida/trafico_{tjsp,tjms,tjrn}.csv
source("R/baixa_trafico.R")

# 4. amostra da fase 2: 100 processos por ano e por tribunal, mais 30 de
#    reposição, sorteados (semente 20260902) fora dos processos que já estão
#    nas tabelas de censo
source("R/amostragem_trafico.R")
```

O passo 4 lê as tabelas de censo de `dados-saida/amostragem/` para não
sortear um processo que já está em campo. Rode esse passo com só as tabelas de
censo na pasta. As saídas do próprio passo 4 (`*_trafico_reposicao.csv`)
também casam com o padrão de exclusão, então numa segunda rodada elas seriam
excluídas e a amostra sairia diferente.

Antes de coletar, troque o e-mail de contato no `user_agent` dos scripts de
coleta (`seu.email@instituicao.br`).

**Os resultados de uma nova coleta não vão bater com as tabelas daqui.** Os
bancos de jurisprudência dos tribunais mudam o tempo todo: entram decisões
novas e a indexação é refeita. As tabelas deste repositório vêm da coleta da
fase 1 feita em agosto de 2026 (bruto baixado em 13/08/2026, planilhas
exportadas em 19/08/2026). Com essas mesmas planilhas intermediárias, os
passos de censo reproduzem os cinco CSVs byte a byte, o que foi conferido.
Os brutos e as planilhas intermediárias não vêm no repositório (ver
`.gitignore`).

## Dependências

R 4.4.1, com os pacotes nas versões usadas:

| Pacote | Versão | Pacote | Versão |
|---|---|---|---|
| dplyr | 1.1.4 | readr | 2.1.5 |
| httr | 1.4.7 | readxl | 1.4.5 |
| jsonlite | 2.0.0 | rvest | 1.0.4 |
| lubridate | 1.9.4 | stringi | 1.8.7 |
| purrr | 1.2.0 | stringr | 1.6.0 |
| tibble | 3.3.0 | tidyr | 1.3.1 |
| writexl | 1.5.0 | xml2 | 1.3.8 |
| lex | 0.2.0.9000 | | |

```r
install.packages(c("dplyr", "httr", "jsonlite", "lubridate", "purrr", "readr",
                   "readxl", "rvest", "stringi", "stringr", "tibble", "tidyr",
                   "writexl", "xml2"))
```

**`lex` é software proprietário da Associação Brasileira de Jurimetria (ABJ)**
(`abjur/lex`) e não está disponível publicamente. Ele só é usado na coleta do TJSP da fase 1
(`lex::tjsp_cjpg_download()` e `lex::tjsp_cjpg_parse()`). A versão usada
corresponde ao commit `97babfb46d69282d8caddfa2f019ecc8c4f0ae95`. A fase 2 não
depende dele: o TJSP é coletado pelo raspador e-SAJ que está no próprio
script.
