# Modelo parametrico de sobrevivencia (AFT)

Ajusta um modelo de tempo de falha acelerado (`survreg`) com
distribuicao configuravel.

## Usage

``` r
rnp_sobrevivencia_parametrica(
  formula,
  data,
  dist = c("weibull", "exponential", "lognormal", "loglogistic"),
  digits = 4L
)
```

## Arguments

- formula:

  Formula com `Surv()` no lado esquerdo.

- data:

  data.frame.

- dist:

  String: `"weibull"`, `"exponential"`, `"lognormal"`, `"loglogistic"`.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (tibble), `escala`, `aic` e `objeto`.

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
rnp_sobrevivencia_parametrica(survival::Surv(time, status) ~ age + sex,
                              survival::lung, dist = "weibull")$coeficientes
#> # A tibble: 4 × 5
#>   termo       estimativa erro_padrao     z p_valor
#>   <chr>            <dbl>       <dbl> <dbl>   <dbl>
#> 1 (Intercept)     6.27        0.481  13.0   0     
#> 2 age            -0.0123      0.007  -1.76  0.0781
#> 3 sex             0.382       0.128   3.00  0.0027
#> 4 Log(scale)     -0.282       0.0619 -4.56  0     
```
