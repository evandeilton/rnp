# Regressao logistica multinomial

Ajusta um modelo logistico multinomial (softmax) por maxima
verossimilhanca via
[`stats::optim()`](https://rdrr.io/r/stats/optim.html), com a primeira
categoria como referencia. Implementacao propria (sem dependencias
externas).

## Usage

``` r
rnp_regressao_multinomial(formula, data, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ x1 + ...` (y fator com \>= 3 niveis).

- data:

  data.frame.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (tibble: `classe`, `termo`, `estimativa`) e
`referencia`.

## See also

Other regressao:
[`rnp_anova_modelos()`](https://evandeilton.github.io/rnp/reference/rnp_anova_modelos.md),
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_elastic_net()`](https://evandeilton.github.io/rnp/reference/rnp_elastic_net.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_predicao()`](https://evandeilton.github.io/rnp/reference/rnp_predicao.md),
[`rnp_regressao_lasso()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_lasso.md),
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
rnp_regressao_multinomial(Species ~ Sepal.Length + Petal.Length, iris)
#> $coeficientes
#> # A tibble: 6 × 3
#>   classe     termo        estimativa
#>   <chr>      <chr>             <dbl>
#> 1 versicolor (Intercept)       14.1 
#> 2 versicolor Sepal.Length      -9.59
#> 3 versicolor Petal.Length      13.7 
#> 4 virginica  (Intercept)      -25.9 
#> 5 virginica  Sepal.Length     -13.6 
#> 6 virginica  Petal.Length      27.0 
#> 
#> $referencia
#> [1] "setosa"
#> 
```
