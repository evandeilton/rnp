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
#>  1 0.0213 0       0.961
#>  2 0.0213 0.0189  0.954
#>  3 0.0426 0.0189  0.953
#>  4 0.0426 0.0377  0.948
#>  5 0.0638 0.0377  0.944
#>  6 0.0851 0.0377  0.943
#>  7 0.0851 0.0566  0.935
#>  8 0.106  0.0566  0.933
#>  9 0.106  0.0755  0.927
#> 10 0.106  0.0943  0.924
#> # ℹ 90 more rows
#> 
#> $auc
#> [1] 0.4251
#> 
```
