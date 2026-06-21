# Modelo aditivo generalizado (GAM)

Ajusta um GAM via
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), permitindo
termos suaves `s()`.

## Usage

``` r
rnp_gam(formula, data, familia = "gaussian", digits = 4L)
```

## Arguments

- formula:

  Formula com termos `s()` (ex.: `y ~ s(x1) + x2`).

- data:

  data.frame.

- familia:

  String aceita por
  [`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md)
  (mapeada para a familia mgcv).

- digits:

  Inteiro.

## Value

Uma lista com `parametricos` (tibble), `suaves` (tibble com `edf`,
estatistica e p), `modelo` (r2 ajustado, deviance explicada, AIC) e
`objeto`.

## See also

Other glm:
[`rnp_binomial_negativa()`](https://evandeilton.github.io/rnp/reference/rnp_binomial_negativa.md),
[`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md),
[`rnp_glm_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_glm_diagnosticos.md),
[`rnp_grafico_efeitos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_efeitos.md),
[`rnp_modelo_misto()`](https://evandeilton.github.io/rnp/reference/rnp_modelo_misto.md),
[`rnp_regressao_ordinal()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ordinal.md)

## Examples

``` r
set.seed(1)
d <- data.frame(x = runif(200)); d$y <- sin(2 * pi * d$x) + rnorm(200, 0, 0.3)
rnp_gam(y ~ s(x), d)$suaves
#> # A tibble: 1 × 4
#>   termo   edf estatistica p_valor
#>   <chr> <dbl>       <dbl>   <dbl>
#> 1 s(x)   6.44        169.       0
```
