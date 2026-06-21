# Distribuicao hipergeometrica (wrapper)

Distribuicao hipergeometrica (wrapper)

## Usage

``` r
rnp_distribuicao_hipergeometrica(
  fun = c("d", "p", "q", "r"),
  m,
  nn,
  k,
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

- m:

  Numero de sucessos na populacao.

- nn:

  Numero de fracassos na populacao (n em
  [`stats::dhyper`](https://rdrr.io/r/stats/Hypergeometric.html)).

- k:

  Tamanho da amostra.

- x, q, p, n:

  Argumentos conforme `fun`.

## Value

Vetor numerico.

## Examples

``` r
rnp_distribuicao_hipergeometrica("d", x = 0:3, m = 7, nn = 3, k = 5)
#> [1] 0.00000000 0.00000000 0.08333333 0.41666667
```
