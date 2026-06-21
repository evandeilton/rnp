# Regressao robusta (M-estimadores via IRLS)

Ajuste robusto por minimos quadrados reponderados iterativamente, com
funcoes psi de Huber ou bisquare, via backend C++.

## Usage

``` r
rnp_regressao_robusta(
  formula,
  data,
  metodo = c("huber", "bisquare"),
  digits = 4L
)
```

## Arguments

- formula:

  Formula.

- data:

  data.frame.

- metodo:

  String: `"huber"` ou `"bisquare"`.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (tibble) e `iteracoes`.

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
[`rnp_regressao_ridge()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ridge.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
rnp_regressao_robusta(mpg ~ wt + hp, mtcars)
#> $coeficientes
#> # A tibble: 3 × 2
#>   termo       estimativa
#>   <chr>            <dbl>
#> 1 (Intercept)    36.6   
#> 2 wt             -3.90  
#> 3 hp             -0.0293
#> 
#> $iteracoes
#> [1] 11
#> 
```
