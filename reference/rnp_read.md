# Leitura rapida de bases de dados delimitadas

Wrapper de
[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
otimizado para os microdados do INEP (delimitador padrao `"|"` e
codificacao `Latin-1`).

## Usage

``` r
rnp_read(
  base,
  sep = "|",
  dec = ".",
  encoding = "Latin-1",
  nrows = Inf,
  verbose = TRUE,
  select = NULL,
  ...
)
```

## Arguments

- base:

  Local e nome do arquivo.

- sep:

  Separador de colunas.

- dec:

  Separador decimal.

- encoding:

  Codificacao do arquivo.

- nrows:

  Numero de linhas a ler (`Inf` = todas).

- verbose:

  Logico. Exibe mensagens de progresso/diagnostico.

- select:

  Vetor de colunas de interesse (default: todas).

- ...:

  Argumentos extras para
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html).

## Value

Um [tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Examples

``` r
if (FALSE) { # \dontrun{
rnp_read("dados.csv", sep = "|", encoding = "Latin-1")
} # }
```
