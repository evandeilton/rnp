# Download dados do INEP

Baixa microdados do censo do ensino superior do INEP.

## Usage

``` r
rnp_get_inep_censo(ano = 2020, url = NULL, salvar = NULL)
```

## Arguments

- ano:

  Ano (1995-2023).

- url:

  URL customizada (sobrescreve ano).

- salvar:

  Caminho de destino.

## Value

Arquivo .zip baixado.

## Examples

``` r
if (FALSE) { # \dontrun{
rnp_get_inep_censo(ano = 2020)
} # }
```
