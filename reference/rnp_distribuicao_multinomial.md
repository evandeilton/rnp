# Distribuicao Multinomial

Distribuicao Multinomial

## Usage

``` r
rnp_distribuicao_multinomial(
  fun = c("d", "r"),
  tamanho,
  probs,
  x = NULL,
  n = NULL
)
```

## Arguments

- fun:

  String: `"d"` (massa conjunta) ou `"r"` (amostra).

- tamanho:

  Numero de ensaios (size).

- probs:

  Vetor de probabilidades das categorias (soma 1).

- x:

  Vetor de contagens (para `fun = "d"`), de mesmo tamanho de `probs`.

- n:

  Numero de amostras (para `fun = "r"`).

## Value

Escalar (d) ou matriz de contagens (r).

## See also

Other distribuicoes:
[`rnp_ajuste_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_ajuste_distribuicao.md),
[`rnp_distribuicao_beta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_beta.md),
[`rnp_distribuicao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_exponencial.md),
[`rnp_distribuicao_f()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_f.md),
[`rnp_distribuicao_gama()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_gama.md),
[`rnp_distribuicao_lognormal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_lognormal.md),
[`rnp_distribuicao_normal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_normal.md),
[`rnp_distribuicao_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_qui_quadrado.md),
[`rnp_distribuicao_t()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_t.md),
[`rnp_distribuicao_uniforme()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_uniforme.md),
[`rnp_distribuicao_weibull()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_weibull.md),
[`rnp_grafico_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_distribuicao.md)

## Examples

``` r
rnp_distribuicao_multinomial("d", tamanho = 10, probs = c(.2, .3, .5),
                             x = c(2, 3, 5))
#> [1] 0.08505
rnp_distribuicao_multinomial("r", tamanho = 10, probs = c(.2, .3, .5), n = 4)
#>      [,1] [,2] [,3]
#> [1,]    1    3    6
#> [2,]    2    3    5
#> [3,]    1    5    4
#> [4,]    2    4    4
```
