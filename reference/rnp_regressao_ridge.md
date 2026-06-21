# Regressao ridge (L2)

Ajusta regressao linear com penalizacao L2 (ridge) via backend C++. Os
preditores sao padronizados e os coeficientes devolvidos na escala
original. O intercepto nao e penalizado.

## Usage

``` r
rnp_regressao_ridge(formula, data, lambda = 1, digits = 4L)
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
[`rnp_regressao_lasso()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_lasso.md),
[`rnp_regressao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_multinomial.md),
[`rnp_regressao_nao_linear()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_nao_linear.md),
[`rnp_regressao_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_poisson.md),
[`rnp_regressao_polinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_polinomial.md),
[`rnp_regressao_ponderada()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ponderada.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
rnp_regressao_ridge(mpg ~ wt + hp + disp, mtcars, lambda = 1)
#> # A tibble: 4 × 2
#>   termo       estimativa
#>   <chr>            <dbl>
#> 1 (Intercept)    36.2   
#> 2 wt             -3.35  
#> 3 hp             -0.0286
#> 4 disp           -0.005 
```
