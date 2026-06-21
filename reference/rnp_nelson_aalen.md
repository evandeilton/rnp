# Risco acumulado de Nelson-Aalen

Estima a funcao de risco acumulado H(t) pelo estimador de Nelson-Aalen.

## Usage

``` r
rnp_nelson_aalen(tempo, evento, digits = 4L)
```

## Arguments

- tempo:

  Vetor de tempos.

- evento:

  Vetor 0/1.

- digits:

  Inteiro.

## Value

tibble com `tempo`, `n_risco`, `n_evento`, `risco_acumulado`, `ep`.

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md),
[`rnp_tabela_vida()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_vida.md)

## Examples

``` r
head(rnp_nelson_aalen(survival::lung$time, survival::lung$status == 2))
#> # A tibble: 6 × 5
#>   tempo n_risco n_evento risco_acumulado     ep
#>   <dbl>   <dbl>    <dbl>           <dbl>  <dbl>
#> 1     5     228        1          0.0044 0.0044
#> 2    11     227        3          0.0176 0.0088
#> 3    12     224        1          0.0221 0.0099
#> 4    13     223        2          0.031  0.0117
#> 5    15     221        1          0.0356 0.0126
#> 6    26     220        1          0.0401 0.0134
```
