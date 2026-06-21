# Tabela de vida atuarial

Constroi a tabua de vida (metodo atuarial de Cutler-Ederer) agrupando os
tempos em intervalos.

## Usage

``` r
rnp_tabela_vida(tempo, evento, intervalos, digits = 4L)
```

## Arguments

- tempo:

  Vetor de tempos.

- evento:

  Vetor 0/1.

- intervalos:

  Vetor de cortes (limites dos intervalos).

- digits:

  Inteiro.

## Value

tibble com `intervalo`, `n_inicio`, `n_evento`, `n_censura`, `n_risco`,
`prob_evento`, `prob_sobrevivencia`, `sobrevivencia_acumulada`.

## See also

Other sobrevivencia:
[`rnp_cox()`](https://evandeilton.github.io/rnp/reference/rnp_cox.md),
[`rnp_cox_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_cox_diagnosticos.md),
[`rnp_cox_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_cox_risco_relativo.md),
[`rnp_grafico_sobrevivencia()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_sobrevivencia.md),
[`rnp_kaplan_meier()`](https://evandeilton.github.io/rnp/reference/rnp_kaplan_meier.md),
[`rnp_log_rank()`](https://evandeilton.github.io/rnp/reference/rnp_log_rank.md),
[`rnp_nelson_aalen()`](https://evandeilton.github.io/rnp/reference/rnp_nelson_aalen.md),
[`rnp_sobrevivencia_parametrica()`](https://evandeilton.github.io/rnp/reference/rnp_sobrevivencia_parametrica.md)

## Examples

``` r
rnp_tabela_vida(survival::lung$time, survival::lung$status == 2,
                intervalos = seq(0, 1000, by = 200))
#> # A tibble: 5 × 8
#>   intervalo   n_inicio n_evento n_censura n_risco prob_evento prob_sobrevivencia
#>   <chr>          <int>    <int>     <int>   <dbl>       <dbl>              <dbl>
#> 1 [0,200)          226       72        12   220         0.327              0.673
#> 2 [200,400)        142       54        33   126.        0.430              0.570
#> 3 [400,600)         55       22        11    49.5       0.444              0.556
#> 4 [600,800)         22       15         1    21.5       0.698              0.302
#> 5 [800,1e+03]        6        2         4     4         0.5                0.5  
#> # ℹ 1 more variable: sobrevivencia_acumulada <dbl>
```
