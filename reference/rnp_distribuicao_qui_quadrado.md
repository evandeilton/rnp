# Distribuicao Qui-quadrado

Distribuicao Qui-quadrado

## Usage

``` r
rnp_distribuicao_qui_quadrado(
  fun = c("d", "p", "q", "r"),
  gl,
  x = NULL,
  q = NULL,
  p = NULL,
  n = NULL
)
```

## Arguments

- fun:

  String: `"d"`, `"p"`, `"q"`, `"r"`.

- gl:

  Graus de liberdade, positivo.

- x, q, p, n:

  Argumentos conforme `fun`.

## Value

Vetor numerico.

## See also

Other distribuicoes:
[`rnp_ajuste_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_ajuste_distribuicao.md),
[`rnp_distribuicao_beta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_beta.md),
[`rnp_distribuicao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_exponencial.md),
[`rnp_distribuicao_f()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_f.md),
[`rnp_distribuicao_gama()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_gama.md),
[`rnp_distribuicao_lognormal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_lognormal.md),
[`rnp_distribuicao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_multinomial.md),
[`rnp_distribuicao_normal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_normal.md),
[`rnp_distribuicao_t()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_t.md),
[`rnp_distribuicao_uniforme()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_uniforme.md),
[`rnp_distribuicao_weibull()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_weibull.md),
[`rnp_grafico_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_distribuicao.md)

## Examples

``` r
rnp_distribuicao_qui_quadrado("q", p = 0.95, gl = 3)
#> [1] 7.814728
```
