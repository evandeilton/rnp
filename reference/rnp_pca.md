# Analise de componentes principais (PCA)

Wrapper de [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) com
saida tidy e variância explicada.

## Usage

``` r
rnp_pca(base, scale = TRUE, n_comp = NULL, digits = 4L)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- scale:

  Logico. Escalonar variaveis (default TRUE).

- n_comp:

  Inteiro. Numero de componentes a reter. Default NULL = todos.

- digits:

  Inteiro.

## Value

lista:

- `modelo`: objeto `prcomp`.

- `scores`: tibble com PC1, PC2, ...

- `loadings`: tibble.

- `variância`: tibble com PC, variância, variância acumulada,
  percentual.

## Examples

``` r
rnp_pca(mtcars[, c("mpg", "disp", "hp", "drat", "wt")])
#> 
#> ── Analise de componentes principais (PCA) ─────────────────────────────────────
#> Modelo: objeto <prcomp>
#> 
#> ── Scores 
#> # A tibble: 32 × 5
#>       PC1     PC2     PC3     PC4     PC5
#>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 -1.08   0.0884  0.120   0.321  -0.211 
#>  2 -0.955  0.0551  0.285   0.282  -0.0584
#>  3 -1.63  -0.160  -0.0891  0.41   -0.0998
#>  4  0.164 -1.07   -0.275  -0.242  -0.132 
#>  5  1.22  -0.278  -0.392  -0.479  -0.344 
#>  6  0.619 -1.55   -0.316   0.336   0.0184
#>  7  2.00   0.532  -0.625   0.0241 -0.186 
#>  8 -1.26  -0.797   0.515  -0.093   0.202 
#>  9 -1.14  -0.137   0.524   0.111   0.229 
#> 10 -0.451  0.167   0.637   0.378   0.194 
#> # ℹ 22 more rows
#> 
#> ── Loadings 
#> # A tibble: 5 × 6
#>   variavel    PC1     PC2     PC3     PC4    PC5
#>   <chr>     <dbl>   <dbl>   <dbl>   <dbl>  <dbl>
#> 1 1        -0.472 -0.0974 -0.198  -0.768   0.373
#> 2 2         0.479  0.0635  0.0781 -0.620  -0.613
#> 3 3         0.414  0.660  -0.512  -0.0318  0.359
#> 4 4        -0.397  0.731   0.540  -0.0461 -0.119
#> 5 5         0.468 -0.128   0.632  -0.150   0.585
#> 
#> ── Variancia 
#> # A tibble: 5 × 4
#>   componente variancia percentual acumulada
#>   <chr>          <dbl>      <dbl>     <dbl>
#> 1 PC1           3.97       0.795      0.795
#> 2 PC2           0.566      0.113      0.908
#> 3 PC3           0.239      0.0478     0.956
#> 4 PC4           0.154      0.0308     0.987
#> 5 PC5           0.0664     0.0133     1    
```
