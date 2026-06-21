# Distribuicoes de probabilidade

Wrapper unificado para funcoes de densidade (d), cumulativa (p),
quantilica (q) e geradora (r) das principais distribuicoes.

## Usage

``` r
rnp_distribuicao(
  dist = c("norm", "t", "chisq", "f", "binom", "pois", "geom", "nbinom", "hyper", "unif",
    "exp", "gamma", "weibull", "beta", "lnorm"),
  fun = c("d", "p", "q", "r"),
  x = NULL,
  q = NULL,
  p = NULL,
  n = NULL,
  ...
)
```

## Arguments

- dist:

  String com nome da distribuicao: `"norm"`, `"t"`, `"chisq"`, `"f"`,
  `"binom"`, `"pois"`, `"geom"`, `"nbinom"`, `"hyper"`, `"unif"`,
  `"exp"`, `"gamma"`, `"weibull"`, `"beta"`.

- fun:

  String: `"d"` (densidade/massa), `"p"` (cumulativa), `"q"` (quantil),
  `"r"` (amostra aleatoria).

- x, q:

  Vetor de quantis.

- p:

  Vetor de probabilidades em \[0, 1\].

- n:

  Inteiro. Tamanho da amostra (apenas para fun = "r").

- ...:

  Argumentos da distribuicao (ex.: `mean`, `sd`, `size`, `prob`,
  `lambda`, `df`, etc.).

## Value

Vetor numerico.

## Examples

``` r
rnp_distribuicao("norm", "d", x = 0, mean = 0, sd = 1)
#> [1] 0.3989423
rnp_distribuicao("binom", "p", q = 3, size = 10, prob = 0.4)
#> [1] 0.3822806
rnp_distribuicao("pois", "r", n = 5, lambda = 2)
#> [1] 1 1 0 1 4
```
