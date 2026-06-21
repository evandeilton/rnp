# Distribuicao binomial (wrapper)

Atalho para `rnp_distribuicao("binom", ...)`.

## Usage

``` r
rnp_distribuicao_binomial(
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

  Numero de ensaios.

- prob:

  Probabilidade de sucesso.

- x, q, p, n:

  Argumentos conforme `fun`.

## Value

Vetor numerico.

## Examples

``` r
rnp_distribuicao_binomial("d", x = 0:5, size = 10, prob = .3)
#> [1] 0.02824752 0.12106082 0.23347444 0.26682793 0.20012095 0.10291935
rnp_distribuicao_binomial("p", q = 5, size = 10, prob = .3)
#> [1] 0.952651
```
