# Escalonamento multidimensional (MDS)

Wrapper de [`stats::cmdscale()`](https://rdrr.io/r/stats/cmdscale.html).

## Usage

``` r
rnp_mds(d, k = 2, digits = 4L)
```

## Arguments

- d:

  Objeto `dist` ou matriz de distancias.

- k:

  Inteiro. Numero de dimensoes.

- digits:

  Inteiro.

## Value

lista:

- `pontos`: tibble com coordenadas.

- `eigenvalues`: vetor.

- `GOF`: vetor com goodness-of-fit.

## Examples

``` r
d <- dist(mtcars[1:10, c("mpg", "hp", "wt")])
rnp_mds(d, k = 2)
#> $pontos
#> # A tibble: 10 × 2
#>      Dim1    Dim2
#>     <dbl>   <dbl>
#>  1 -12.8  -0.0367
#>  2 -12.8  -0.007 
#>  3 -29.9  -0.997 
#>  4 -12.8  -0.364 
#>  5  52.2  -0.947 
#>  6 -17.7   3.19  
#>  7 122.   -0.108 
#>  8 -60.9  -0.915 
#>  9 -27.9  -1.00  
#> 10   0.26  1.19  
#> 
#> $eigenvalues
#>  [1]  2.388475e+04  1.546647e+01  9.277763e-01  7.005724e-13  5.113322e-13
#>  [6]  4.496739e-13  3.848454e-13 -1.662670e-13 -2.149564e-13 -2.944394e-12
#> 
#> $GOF
#> [1] 0.9999612 0.9999612
#> 
```
