# Teste log-rank

Compara curvas de sobrevivencia entre grupos. Com `rho = 0` obtem-se o
teste log-rank; com `rho = 1`, o teste de Gehan-Wilcoxon (peso de Peto).

## Usage

``` r
rnp_log_rank(tempo, evento, grupo, rho = 0, digits = 4L)
```

## Arguments

- tempo:

  Vetor de tempos.

- evento:

  Vetor 0/1.

- grupo:

  Fator de grupos.

- rho:

  Peso (0 = log-rank; 1 = Wilcoxon).

- digits:

  Inteiro.

## Value

tibble com `estatistica`, `gl`, `p_valor`, `metodo`.

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
rnp_log_rank(survival::lung$time, survival::lung$status == 2,
             survival::lung$sex)
#> # A tibble: 1 × 4
#>   estatistica    gl p_valor metodo  
#>         <dbl> <int>   <dbl> <chr>   
#> 1        10.3     1  0.0013 log-rank
```
