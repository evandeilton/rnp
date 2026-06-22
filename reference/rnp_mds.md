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
#> 
#> ── Escalonamento multidimensional (MDS) ────────────────────────────────────────
#> 
#> ── Pontos 
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
#> Eigenvalues: 23884.7538629634, 15.466470720828, 0.927776315768773,
#> 7.00572428952474e-13, 5.11332177006373e-13, 4.49673892335548e-13,
#> 3.84845423824949e-13, -1.66267017750589e-13, -2.14956440399115e-13, and
#> -2.94439373864587e-12
#> GOF: 0.999961182771995 and 0.999961182771995
```
