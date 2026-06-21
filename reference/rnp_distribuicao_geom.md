# Distribuicao geometrica (wrapper)

Distribuicao geometrica (wrapper)

## Usage

``` r
rnp_distribuicao_geom(
  fun = c("d", "p", "q", "r"),
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

- prob:

  Probabilidade de sucesso em cada ensaio.

- x, q, p, n:

  Argumentos conforme `fun`.

## Value

Vetor numerico.

## Examples

``` r
rnp_distribuicao_geom("d", x = 0:5, prob = .2)
#> [1] 0.200000 0.160000 0.128000 0.102400 0.081920 0.065536
```
