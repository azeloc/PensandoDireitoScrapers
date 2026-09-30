# Consultas exatas

Todas as buscas são em **jurisprudência de primeiro grau**, anos de **2023 a 2025**.
As strings abaixo são exatamente as enviadas aos tribunais, geradas por
`montar_query()` nos scripts de coleta. O operador `E` é o AND dos buscadores
e as aspas pedem frase exata.

## Fase 1: tráfico de drogas × lavagem de dinheiro

Script: [`R/baixa_todos_os_dados.R`](R/baixa_todos_os_dados.R). A grade cruza
1 termo de tráfico com 7 termos de lavagem, o que dá 7 consultas por tribunal:

| # | Consulta com acento (TJSP, TJRN) | Consulta sem acento (TJMS) | Pasta (`slug`) |
|---|---|---|---|
| 1 | `"tráfico de drogas" E "9.613"` | `"trafico de drogas" E "9.613"` | `trafico_de_drogas__9_613` |
| 2 | `"tráfico de drogas" E "14.478"` | `"trafico de drogas" E "14.478"` | `trafico_de_drogas__14_478` |
| 3 | `"tráfico de drogas" E "Ocultação de Bens"` | `"trafico de drogas" E "Ocultacao de Bens"` | `trafico_de_drogas__ocultacao_de_bens` |
| 4 | `"tráfico de drogas" E "Ocultação de Capitais"` | `"trafico de drogas" E "Ocultacao de Capitais"` | `trafico_de_drogas__ocultacao_de_capitais` |
| 5 | `"tráfico de drogas" E "Ocultação de Valores"` | `"trafico de drogas" E "Ocultacao de Valores"` | `trafico_de_drogas__ocultacao_de_valores` |
| 6 | `"tráfico de drogas" E "Ocultação de Direitos"` | `"trafico de drogas" E "Ocultacao de Direitos"` | `trafico_de_drogas__ocultacao_de_direitos` |
| 7 | `"tráfico de drogas" E "Lavagem de dinheiro"` | `"trafico de drogas" E "Lavagem de dinheiro"` | `trafico_de_drogas__lavagem_de_dinheiro` |

### Como cada tribunal é consultado

| Tribunal | Endpoint | Forma da consulta | Recorte de data |
|---|---|---|---|
| TJSP | e-SAJ CJPG (`https://esaj.tjsp.jus.br/cjpg`), via `lex::tjsp_cjpg_download()` | com acento | nenhum na requisição; filtra no parse por ano de `data_de_disponibilizacao` ∈ 2023–2025 |
| TJMS | `POST https://esaj.tjms.jus.br/cjpg/pesquisar.do`, campo `dadosConsulta.pesquisaLivre` | sem acento | uma consulta por ano: `dtInicio = 01/01/AAAA`, `dtFim = 31/12/AAAA` |
| TJRN | `POST https://jurisprudencia.tjrn.jus.br/api/pesquisar` (JSON), campo `inteiro_teor`, `grau = "1"` | com acento | uma consulta por ano: `dt_inicio = 01-01-AAAA`, `dt_fim = 31-12-AAAA` |

No TJRN a API devolve sentenças e decisões mesmo com `grau = "1"`. Só ficam
os documentos cujo `tipo_teor` casa com `senten` (sem diferenciar maiúsculas).

### Resultados por consulta (antes da deduplicação)

Consultas que não aparecem na tabela de um tribunal voltaram com zero
resultados. Um mesmo processo pode aparecer em mais de uma consulta.

| Tribunal | Consulta | 2023 | 2024 | 2025 |
|---|---|---:|---:|---:|
| TJSP | 9.613 | 57 | 161 | 113 |
| TJSP | Lavagem de dinheiro | 131 | 127 | 203 |
| TJSP | Ocultação de Bens | 21 | 28 | 56 |
| TJSP | Ocultação de Capitais | 0 | 2 | 4 |
| TJSP | Ocultação de Valores | 1 | 7 | 12 |
| TJMS | 9.613 | 19 | 16 | 12 |
| TJMS | Lavagem de dinheiro | 19 | 30 | 17 |
| TJMS | Ocultação de Bens | 4 | 7 | 3 |
| TJMS | Ocultação de Valores | 1 | 2 | 1 |
| TJRN | 9.613 | 1 | 6 | 6 |
| TJRN | Lavagem de dinheiro | 11 | 22 | 22 |
| TJRN | Ocultação de Bens | 2 | 1 | 4 |

Depois da deduplicação por número de processo (CNJ) ficam uma linha por
processo e a coluna `n_queries`, que conta em quantas das 7 consultas o
processo apareceu:

| Tribunal | 2023 | 2024 | 2025 | Total |
|---|---:|---:|---:|---:|
| TJSP | 142 | 248 | 262 | 652 |
| TJMS | 26 | 31 | 23 | 80 |
| TJRN | 13 | 25 | 24 | 62 |

## Fase 2: só tráfico de drogas

Script: [`R/baixa_trafico.R`](R/baixa_trafico.R). É uma consulta única, com
acento nos três tribunais:

```
"tráfico de drogas"
```

| Tribunal | Endpoint | Recorte de data |
|---|---|---|
| TJSP | `POST https://esaj.tjsp.jus.br/cjpg/pesquisar.do` (raspador próprio, sem `lex`) | uma consulta por ano: `01/01/AAAA` a `31/12/AAAA` |
| TJMS | `POST https://esaj.tjms.jus.br/cjpg/pesquisar.do` | uma consulta por ano: `01/01/AAAA` a `31/12/AAAA` |
| TJRN | `POST https://jurisprudencia.tjrn.jus.br/api/pesquisar`, `grau = "1"`, só sentenças | uma consulta por ano: `01-01-AAAA` a `31-12-AAAA` |

**O acento importa no TJSP.** O índice do e-SAJ paulista não normaliza
acentos. Medido em 19/08/2026 para o ano de 2023, `"trafico de drogas"`
trouxe 556 resultados e `"tráfico de drogas"` trouxe 34.951. No TJMS e no TJRN
as duas formas dão o mesmo número.
