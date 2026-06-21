# Curva ROC e AUC

Curva ROC e AUC

## Usage

``` r
rnp_curva_roc(observado, escores, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Vetor binario (0/1 ou factor 2 niveis).

- escores:

  Vetor numerico de probabilidades preditas.

- positivo:

  Nivel da classe positiva (default primeiro nivel).

- digits:

  Inteiro.

## Value

lista:

- `curva`: tibble com fpr, tpr, limiar.

- `auc`: escalar.

## Examples

``` r
obs <- sample(0:1, 100, TRUE)
esc <- runif(100)
rnp_curva_roc(obs, esc, positivo = 1)
#> $curva
#> # A tibble: 100 × 3
#>       fpr    tpr limiar
#>     <dbl>  <dbl>  <dbl>
#>  1 0.0217 0       1.000
#>  2 0.0217 0.0185  0.996
#>  3 0.0435 0.0185  0.961
#>  4 0.0652 0.0185  0.944
#>  5 0.0652 0.037   0.943
#>  6 0.087  0.037   0.930
#>  7 0.087  0.0556  0.907
#>  8 0.109  0.0556  0.906
#>  9 0.130  0.0556  0.902
#> 10 0.152  0.0556  0.901
#> # ℹ 90 more rows
#> 
#> $auc
#> [1] 0.4622
#> 
```
