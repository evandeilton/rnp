# Tabela de termos (tidy) de um resultado do rnp

Extrai a tabela de coeficientes/estimativas de um objeto
`rnp_resultado`, no formato esperado pelo `broom`/`tidymodels`.

## Usage

``` r
# S3 method for class 'rnp_resultado'
tidy(x, ...)
```

## Arguments

- x:

  Objeto de classe `rnp_resultado`.

- ...:

  Ignorado.

## Value

Um [tibble](https://tibble.tidyverse.org/reference/tibble.html) com um
termo por linha.
