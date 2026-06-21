# Selecao de variaveis (stepwise)

Selecao automatica por AIC ou BIC usando
[`stats::step()`](https://rdrr.io/r/stats/step.html).

## Usage

``` r
rnp_regressao_stepwise(
  formula,
  data,
  direcao = c("both", "backward", "forward"),
  criterio = c("AIC", "BIC"),
  digits = 4L
)
```

## Arguments

- formula:

  Formula do modelo completo.

- data:

  data.frame.

- direcao:

  String: `"both"`, `"backward"`, `"forward"`.

- criterio:

  String: `"AIC"` ou `"BIC"`.

- digits:

  Inteiro.

## Value

Uma lista com `formula_final`, `coeficientes` (tibble) e `criterio`.

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
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
rnp_regressao_stepwise(mpg ~ wt + hp + disp + drat + qsec, mtcars)
#> $formula_final
#> mpg ~ wt + drat + qsec
#> <environment: 0x55edd1a57aa8>
#> 
#> $coeficientes
#> # A tibble: 4 × 4
#>   termo       estimativa erro_padrao p_valor
#>   <chr>            <dbl>       <dbl>   <dbl>
#> 1 (Intercept)     11.4         8.07   0.169 
#> 2 wt              -4.40        0.678  0     
#> 3 drat             1.66        1.23   0.188 
#> 4 qsec             0.946       0.262  0.0012
#> 
#> $criterio
#> [1] "AIC"
#> 
```
