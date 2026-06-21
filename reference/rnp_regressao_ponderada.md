# Regressao linear ponderada (WLS)

Minimos quadrados ponderados via backend C++.

## Usage

``` r
rnp_regressao_ponderada(formula, data, pesos, digits = 4L)
```

## Arguments

- formula:

  Formula.

- data:

  data.frame.

- pesos:

  Vetor de pesos (positivos), comprimento = nrow(data).

- digits:

  Inteiro.

## Value

tibble com `termo`, `estimativa`, `erro_padrao`, `estatistica_t`,
`p_valor`.

## See also

Other regressao:
[`rnp_anova_modelos()`](https://evandeilton.github.io/rnp/reference/rnp_anova_modelos.md),
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_elastic_net()`](https://evandeilton.github.io/rnp/reference/rnp_elastic_net.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_predicao()`](https://evandeilton.github.io/rnp/reference/rnp_predicao.md),
[`rnp_regressao_lasso()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_lasso.md),
[`rnp_regressao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_multinomial.md),
[`rnp_regressao_nao_linear()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_nao_linear.md),
[`rnp_regressao_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_poisson.md),
[`rnp_regressao_polinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_polinomial.md),
[`rnp_regressao_ridge()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ridge.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
rnp_regressao_ponderada(mpg ~ wt, mtcars, pesos = 1 / mtcars$wt)
#> # A tibble: 2 × 5
#>   termo       estimativa erro_padrao estatistica_t p_valor
#>   <chr>            <dbl>       <dbl>         <dbl>   <dbl>
#> 1 (Intercept)      39.3        1.76           22.3       0
#> 2 wt               -5.96       0.575         -10.4       0
```
