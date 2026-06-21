# Elastic net (L1 + L2)

Combina penalizacoes lasso (L1) e ridge (L2) controladas por `alpha`
(`alpha = 1` lasso, `alpha = 0` ridge), por coordinate descent (C++).

## Usage

``` r
rnp_elastic_net(formula, data, lambda = 0.1, alpha = 0.5, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ x1 + x2 + ...`.

- data:

  data.frame.

- lambda:

  Parametro de penalizacao (\>= 0).

- alpha:

  Mistura L1/L2 em \[0, 1\].

- digits:

  Inteiro.

## Value

tibble com `termo` e `estimativa`.

## See also

Other regressao:
[`rnp_anova_modelos()`](https://evandeilton.github.io/rnp/reference/rnp_anova_modelos.md),
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_predicao()`](https://evandeilton.github.io/rnp/reference/rnp_predicao.md),
[`rnp_regressao_lasso()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_lasso.md),
[`rnp_regressao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_multinomial.md),
[`rnp_regressao_nao_linear()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_nao_linear.md),
[`rnp_regressao_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_poisson.md),
[`rnp_regressao_polinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_polinomial.md),
[`rnp_regressao_ponderada()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ponderada.md),
[`rnp_regressao_ridge()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ridge.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
rnp_elastic_net(mpg ~ wt + hp + disp, mtcars, lambda = 0.3, alpha = 0.5)
#> # A tibble: 4 × 2
#>   termo       estimativa
#>   <chr>            <dbl>
#> 1 (Intercept)    34.2   
#> 2 wt             -2.58  
#> 3 hp             -0.024 
#> 4 disp           -0.0099
```
