# Cluster K-Means

Wrapper de [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html) com
saida tidy e metricas de avaliação.

## Usage

``` r
rnp_kmeans(base, k, nstart = 25L, scale = TRUE, seed = 42L, digits = 4L)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- k:

  Inteiro. Numero de clusters.

- nstart:

  Inteiro. Numero de inicializacoes aleatorias.

- scale:

  Logico. Escalonar variaveis.

- seed:

  Inteiro. Semente aleatoria.

- digits:

  Inteiro.

## Value

lista:

- `modelo`: objeto `kmeans`.

- `clusters`: tibble com cluster atribuido a cada observacao.

- `centros`: tibble com centroides.

- `metricas`: tibble com wss_total, between_ss, ratio_ss.

## Examples

``` r
rnp_kmeans(mtcars[, c("mpg", "hp", "wt")], k = 3)
#> 
#> ── Cluster k-medias ────────────────────────────────────────────────────────────
#> Modelo: objeto <kmeans>
#> 
#> ── Clusters 
#> # A tibble: 32 × 2
#>    observacao cluster
#>         <int>   <int>
#>  1          1       2
#>  2          2       2
#>  3          3       2
#>  4          4       2
#>  5          5       2
#>  6          6       2
#>  7          7       1
#>  8          8       2
#>  9          9       2
#> 10         10       2
#> # ℹ 22 more rows
#> 
#> ── Centros 
#> # A tibble: 3 × 4
#>   cluster     mpg     hp      wt
#>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1       1 -0.964   1.18   0.979 
#> 2       2 -0.0181 -0.351 -0.0965
#> 3       3  1.66   -1.04  -1.37  
#> 
#> ── Metricas 
#> # A tibble: 1 × 5
#>   wss_total between_ss ratio_ss     k  nobs
#>       <dbl>      <dbl>    <dbl> <dbl> <int>
#> 1      23.7       69.3    0.745     3    32
```
