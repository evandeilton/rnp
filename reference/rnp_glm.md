# Modelo linear generalizado (GLM)

Ajuste unificado de GLM para as familias mais comuns, com saida tidy.

## Usage

``` r
rnp_glm(
  formula,
  data,
  familia = c("gaussian", "binomial", "poisson", "gamma", "inverse_gaussian"),
  ligacao = NULL,
  conf = 0.95,
  digits = 4L
)
```

## Arguments

- formula:

  Formula.

- data:

  data.frame.

- familia:

  String: `"gaussian"`, `"binomial"`, `"poisson"`, `"gamma"`,
  `"inverse_gaussian"`.

- ligacao:

  String opcional (funcao de ligacao). `NULL` usa o padrao canonico da
  familia.

- conf:

  Nivel de confianca para os IC (Wald).

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (tibble), `modelo` (AIC, deviance,
dispersao, nobs) e `objeto` (`glm`).

## See also

Other glm:
[`rnp_binomial_negativa()`](https://evandeilton.github.io/rnp/reference/rnp_binomial_negativa.md),
[`rnp_gam()`](https://evandeilton.github.io/rnp/reference/rnp_gam.md),
[`rnp_glm_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_glm_diagnosticos.md),
[`rnp_grafico_efeitos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_efeitos.md),
[`rnp_modelo_misto()`](https://evandeilton.github.io/rnp/reference/rnp_modelo_misto.md),
[`rnp_regressao_ordinal()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ordinal.md)

## Examples

``` r
rnp_glm(am ~ mpg + wt, mtcars, familia = "binomial")$coeficientes
#> # A tibble: 3 × 7
#>   termo       estimativa erro_padrao estatistica p_valor  ic_inf ic_sup
#>   <chr>            <dbl>       <dbl>       <dbl>   <dbl>   <dbl>  <dbl>
#> 1 (Intercept)     25.9        12.2          2.12  0.0338   1.99  49.8  
#> 2 mpg             -0.324       0.240       -1.35  0.176   -0.794  0.145
#> 3 wt              -6.42        2.55        -2.52  0.0118 -11.4   -1.42 
rnp_glm(carb ~ hp + wt, mtcars, familia = "poisson")$coeficientes
#> # A tibble: 3 × 7
#>   termo       estimativa erro_padrao estatistica p_valor  ic_inf ic_sup
#>   <chr>            <dbl>       <dbl>       <dbl>   <dbl>   <dbl>  <dbl>
#> 1 (Intercept)     0.139       0.399       0.348   0.728  -0.643  0.920 
#> 2 hp              0.0055      0.0016      3.34    0.0008  0.0023 0.0087
#> 3 wt              0.0045      0.131       0.0342  0.973  -0.252  0.261 
```
