# Distribuicao binomial negativa (wrapper)

Distribuicao binomial negativa (wrapper)

## Usage

``` r
rnp_distribuicao_negbinom(
  fun = c("d", "p", "q", "r"),
  size,
  prob,
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

- size:

  Numero de sucessos alvo.

- prob:

  Probabilidade de sucesso em cada ensaio.

- x, q, p, n:

  Argumentos conforme `fun`.

## Value

Vetor numerico.

## Examples

``` r
rnp_distribuicao_negbinom("d", x = 0:5, size = 3, prob = .5)
#> [1] 0.12500000 0.18750000 0.18750000 0.15625000 0.11718750 0.08203125
```
