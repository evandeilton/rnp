# Metricas de regressao

Metricas de regressao

## Usage

``` r
rnp_metricas_regressao(observado, predito, digits = 4L)
```

## Arguments

- observado:

  Vetor numerico observado.

- predito:

  Vetor numerico predito.

- digits:

  Inteiro.

## Value

tibble com `rmse`, `mae`, `mape`, `r2`, `rmse_relativo`.

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_comparar_roc()`](https://evandeilton.github.io/rnp/reference/rnp_comparar_roc.md),
[`rnp_curva_ganho()`](https://evandeilton.github.io/rnp/reference/rnp_curva_ganho.md),
[`rnp_curva_lift()`](https://evandeilton.github.io/rnp/reference/rnp_curva_lift.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_ks_classificador()`](https://evandeilton.github.io/rnp/reference/rnp_ks_classificador.md),
[`rnp_metricas_classificacao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_classificacao.md)

## Examples

``` r
rnp_metricas_regressao(mtcars$mpg, predict(lm(mpg ~ wt, mtcars)))
#> # A tibble: 5 × 2
#>   metrica        valor
#>   <chr>          <dbl>
#> 1 rmse           2.95 
#> 2 mae            2.34 
#> 3 mape          12.6  
#> 4 r2             0.753
#> 5 rmse_relativo  0.147
```
