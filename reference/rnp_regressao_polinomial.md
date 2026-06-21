# Regressao polinomial

Ajusta um polinomio de grau `grau` de um unico preditor, via backend C++
(OLS por QR).

## Usage

``` r
rnp_regressao_polinomial(formula, data, grau = 2L, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ x` (um unico preditor).

- data:

  data.frame.

- grau:

  Inteiro. Grau do polinomio.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (tibble) e `modelo` (tibble com `r2`,
`r2_ajustado`, `sigma`, `nobs`).

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
[`rnp_regressao_ponderada()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ponderada.md),
[`rnp_regressao_ridge()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ridge.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
rnp_regressao_polinomial(dist ~ speed, cars, grau = 2)
#> $coeficientes
#> # A tibble: 3 × 3
#>   termo       estimativa erro_padrao
#>   <chr>            <dbl>       <dbl>
#> 1 (Intercept)      2.47       14.8  
#> 2 speed^1          0.913       2.03 
#> 3 speed^2          0.1         0.066
#> 
#> $modelo
#> # A tibble: 1 × 4
#>      r2 r2_ajustado sigma  nobs
#>   <dbl>       <dbl> <dbl> <int>
#> 1 0.667       0.653  15.2    50
#> 
```
