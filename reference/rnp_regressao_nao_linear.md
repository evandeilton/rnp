# Regressao nao-linear

Wrapper tidy de [`stats::nls()`](https://rdrr.io/r/stats/nls.html).

## Usage

``` r
rnp_regressao_nao_linear(formula, data, inicio, digits = 4L)
```

## Arguments

- formula:

  Formula nao-linear (ex.: `y ~ a * exp(b * x)`).

- data:

  data.frame.

- inicio:

  Lista de valores iniciais dos parametros.

- digits:

  Inteiro.

## Value

tibble com `parametro`, `estimativa`, `erro_padrao`, `p_valor`.

## See also

Other regressao:
[`rnp_anova_modelos()`](https://evandeilton.github.io/rnp/reference/rnp_anova_modelos.md),
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_elastic_net()`](https://evandeilton.github.io/rnp/reference/rnp_elastic_net.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_predicao()`](https://evandeilton.github.io/rnp/reference/rnp_predicao.md),
[`rnp_regressao_lasso()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_lasso.md),
[`rnp_regressao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_multinomial.md),
[`rnp_regressao_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_poisson.md),
[`rnp_regressao_polinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_polinomial.md),
[`rnp_regressao_ponderada()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ponderada.md),
[`rnp_regressao_ridge()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ridge.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
df <- data.frame(x = 1:10, y = 2 * exp(0.3 * (1:10)) + rnorm(10))
rnp_regressao_nao_linear(y ~ a * exp(b * x), df, inicio = list(a = 1, b = 0.1))
#> # A tibble: 2 × 4
#>   parametro estimativa erro_padrao p_valor
#>   <chr>          <dbl>       <dbl>   <dbl>
#> 1 a              2.10       0.174        0
#> 2 b              0.299      0.0093       0
```
