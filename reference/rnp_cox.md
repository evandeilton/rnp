# Modelo de Cox de riscos proporcionais

Ajusta o modelo semiparametrico de Cox. A formula deve ter
`survival::Surv(tempo, evento)` no lado esquerdo.

## Usage

``` r
rnp_cox(formula, data, conf = 0.95, digits = 4L)
```

## Arguments

- formula:

  Formula, ex.: `Surv(time, status) ~ age + sex`.

- data:

  data.frame.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (tibble com `hazard_ratio`, IC, p),
`modelo` (concordancia, AIC, testes globais) e `objeto` (`coxph`).

## See also

Other sobrevivencia:
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
rnp_cox(survival::Surv(time, status) ~ age + sex + ph.ecog,
        data = survival::lung)$coeficientes
#> # A tibble: 3 × 8
#>   termo      coef hazard_ratio erro_padrao     z p_valor ic_inf ic_sup
#>   <chr>     <dbl>        <dbl>       <dbl> <dbl>   <dbl>  <dbl>  <dbl>
#> 1 age      0.0111        1.01       0.0093  1.19   0.232  0.993  1.03 
#> 2 sex     -0.553         0.575      0.168  -3.29   0.001  0.414  0.799
#> 3 ph.ecog  0.464         1.59       0.114   4.08   0      1.27   1.99 
```
