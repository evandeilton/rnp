# Diagnostico da hipotese de riscos proporcionais

Testa a hipotese de proporcionalidade dos riscos (Schoenfeld, `cox.zph`)
e opcionalmente exibe os residuos de Schoenfeld.

## Usage

``` r
rnp_cox_diagnosticos(modelo, grafico = FALSE, digits = 4L)
```

## Arguments

- modelo:

  Objeto `coxph` ou saida de
  [`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md).

- grafico:

  Logico. Retorna tambem o grafico dos residuos.

- digits:

  Inteiro.

## Value

Uma lista com `teste` (tibble por covariavel + global) e, se solicitado,
`grafico` (`ggplot`).

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
fit <- rnp_cox(survival::Surv(time, status) ~ age + sex, survival::lung)
rnp_cox_diagnosticos(fit)$teste
#> # A tibble: 3 × 5
#>   termo  chisq    gl p_valor interpretacao   
#>   <chr>  <dbl> <dbl>   <dbl> <chr>           
#> 1 age    0.209     1   0.647 PH nao rejeitada
#> 2 sex    2.61      1   0.106 PH nao rejeitada
#> 3 GLOBAL 2.77      2   0.250 PH nao rejeitada
```
