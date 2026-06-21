# Regressao lasso (L1)

Ajusta regressao com penalizacao L1 (lasso) por coordinate descent
(C++), produzindo selecao de variaveis (coeficientes exatamente zero).

## Usage

``` r
rnp_regressao_lasso(formula, data, lambda = 0.1, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ x1 + x2 + ...`.

- data:

  data.frame.

- lambda:

  Parametro de penalizacao (\>= 0).

- digits:

  Inteiro.

## Value

tibble com `termo` e `estimativa`.

## See also

Other regressao:
[`rnp_anova_modelos()`](https://evandeilton.github.io/rnp/reference/rnp_anova_modelos.md),
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_elastic_net()`](https://evandeilton.github.io/rnp/reference/rnp_elastic_net.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_predicao()`](https://evandeilton.github.io/rnp/reference/rnp_predicao.md),
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
rnp_regressao_lasso(mpg ~ wt + hp + disp + drat, mtcars, lambda = 0.5)
#> # A tibble: 5 × 2
#>   termo       estimativa
#>   <chr>            <dbl>
#> 1 (Intercept)    30.5   
#> 2 wt             -3.15  
#> 3 hp             -0.0276
#> 4 disp            0     
#> 5 drat            1.04  
```
