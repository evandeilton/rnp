# Distribuicao Beta

Distribuicao Beta

## Usage

``` r
rnp_distribuicao_beta(
  fun = c("d", "p", "q", "r"),
  a,
  b,
  x = NULL,
  q = NULL,
  p = NULL,
  n = NULL
)
```

## Arguments

- fun:

  String: `"d"`, `"p"`, `"q"`, `"r"`.

- a, b:

  Parametros de forma (shape1, shape2), positivos.

- x, q, p, n:

  Argumentos conforme `fun`.

## Value

Vetor numerico.

## See also

Other distribuicoes:
[`rnp_ajuste_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_ajuste_distribuicao.md),
[`rnp_distribuicao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_exponencial.md),
[`rnp_distribuicao_f()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_f.md),
[`rnp_distribuicao_gama()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_gama.md),
[`rnp_distribuicao_lognormal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_lognormal.md),
[`rnp_distribuicao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_multinomial.md),
[`rnp_distribuicao_normal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_normal.md),
[`rnp_distribuicao_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_qui_quadrado.md),
[`rnp_distribuicao_t()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_t.md),
[`rnp_distribuicao_uniforme()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_uniforme.md),
[`rnp_distribuicao_weibull()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_weibull.md),
[`rnp_grafico_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_distribuicao.md)

## Examples

``` r
rnp_distribuicao_beta("d", x = 0.5, a = 2, b = 2)
#> [1] 1.5
```
