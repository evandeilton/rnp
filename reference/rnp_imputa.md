# Imputacao de valores faltantes

Imputa NA por media/mediana/moda (por coluna) ou por k vizinhos mais
proximos (backend C++) para variaveis numericas.

## Usage

``` r
rnp_imputa(base, metodo = c("media", "mediana", "moda", "knn"), k = 5L)
```

## Arguments

- base:

  data.frame ou matriz.

- metodo:

  String: `"media"`, `"mediana"`, `"moda"` ou `"knn"`.

- k:

  Inteiro. Numero de vizinhos (apenas `"knn"`).

## Value

data.frame com os NA imputados.

## See also

Other preprocessamento:
[`rnp_discretiza()`](https://evandeilton.github.io/rnp/reference/rnp_discretiza.md),
[`rnp_dummy()`](https://evandeilton.github.io/rnp/reference/rnp_dummy.md),
[`rnp_normaliza()`](https://evandeilton.github.io/rnp/reference/rnp_normaliza.md),
[`rnp_padroniza()`](https://evandeilton.github.io/rnp/reference/rnp_padroniza.md),
[`rnp_winsoriza()`](https://evandeilton.github.io/rnp/reference/rnp_winsoriza.md)

## Examples

``` r
df <- data.frame(a = c(1, NA, 3, 4), b = c(NA, 2, 3, 4))
rnp_imputa(df, metodo = "media")
#> # A tibble: 4 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1  1        3
#> 2  2.67     2
#> 3  3        3
#> 4  4        4
rnp_imputa(df, metodo = "knn", k = 2)
#> # A tibble: 4 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1   1     3.5
#> 2   3.5   2  
#> 3   3     3  
#> 4   4     4  
```
