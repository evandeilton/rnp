# Regressao de Poisson (contagens)

GLM Poisson com saida tidy e razao de taxas (IRR = exp(beta)).

## Usage

``` r
rnp_regressao_poisson(formula, data, digits = 4L)
```

## Arguments

- formula:

  Formula.

- data:

  data.frame.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (tibble com `irr`) e `modelo` (tibble).

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
[`rnp_regressao_polinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_polinomial.md),
[`rnp_regressao_ponderada()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ponderada.md),
[`rnp_regressao_ridge()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ridge.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
rnp_regressao_poisson(carb ~ hp + wt, mtcars)
#> $coeficientes
#> # A tibble: 3 × 5
#>   termo       estimativa erro_padrao p_valor   irr
#>   <chr>            <dbl>       <dbl>   <dbl> <dbl>
#> 1 (Intercept)     0.139       0.399   0.728   1.15
#> 2 hp              0.0055      0.0016  0.0008  1.01
#> 3 wt              0.0045      0.131   0.973   1.00
#> 
#> $modelo
#> # A tibble: 1 × 4
#>     aic deviance deviance_nula  nobs
#>   <dbl>    <dbl>         <dbl> <int>
#> 1  108.     12.3          27.0    32
#> 
```
