# Distribuicao de Poisson (wrapper)

Distribuicao de Poisson (wrapper)

## Usage

``` r
rnp_distribuicao_poisson(
  fun = c("d", "p", "q", "r"),
  lambda,
  x = NULL,
  q = NULL,
  p = NULL,
  n = NULL
)
```

## Arguments

- fun:

  String: `"d"` (massa), `"p"` (cumulativa), `"q"` (quantil), `"r"`
  (amostra).

- lambda:

  Taxa media (positivo).

- x, q, p, n:

  Argumentos conforme `fun`.

## Value

Vetor numerico.

## Examples

``` r
rnp_distribuicao_poisson("d", x = 0:5, lambda = 2)
#> [1] 0.13533528 0.27067057 0.27067057 0.18044704 0.09022352 0.03608941
```
