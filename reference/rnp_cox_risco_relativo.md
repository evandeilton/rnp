# Risco relativo predito pelo modelo de Cox

Calcula o risco relativo (em relacao a media das covariaveis) para novos
perfis de covariaveis.

## Usage

``` r
rnp_cox_risco_relativo(modelo, novos_dados, digits = 4L)
```

## Arguments

- modelo:

  Objeto `coxph` ou saida de
  [`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md).

- novos_dados:

  data.frame com as covariaveis dos perfis.

- digits:

  Inteiro.

## Value

tibble com o risco relativo predito por perfil.

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
fit <- rnp_cox(survival::Surv(time, status) ~ age + sex, survival::lung)
rnp_cox_risco_relativo(fit, data.frame(age = c(50, 70), sex = c(1, 1)))
#> # A tibble: 2 × 3
#>     age   sex risco_relativo
#>   <dbl> <dbl>          <dbl>
#> 1    50     1          0.990
#> 2    70     1          1.39 
```
