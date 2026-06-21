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
#> $modelo
#> K-means clustering with 3 clusters of sizes 10, 16, 6
#> 
#> Cluster means:
#>           mpg         hp          wt
#> 1 -0.96410736  1.1844968  0.97873444
#> 2 -0.01814766 -0.3509553 -0.09651672
#> 3  1.65523937 -1.0382807 -1.37384616
#> 
#> Clustering vector:
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   2                   2                   2                   2 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   2                   2                   1                   2 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   2                   2                   2                   1 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   1                   1                   1                   1 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   1                   3                   3                   3 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   2                   2                   2                   1 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   2                   3                   3                   3 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   1                   2                   1                   2 
#> 
#> Within cluster sum of squares by cluster:
#> [1] 12.374572  9.211340  2.152706
#>  (between_SS / total_SS =  74.5 %)
#> 
#> Available components:
#> 
#> [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#> [6] "betweenss"    "size"         "iter"         "ifault"      
#> 
#> $clusters
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
#> $centros
#> # A tibble: 3 × 4
#>   cluster     mpg     hp      wt
#>     <dbl>   <dbl>  <dbl>   <dbl>
#> 1       1 -0.964   1.18   0.979 
#> 2       2 -0.0181 -0.351 -0.0965
#> 3       3  1.66   -1.04  -1.37  
#> 
#> $metricas
#> # A tibble: 1 × 5
#>   wss_total between_ss ratio_ss     k  nobs
#>       <dbl>      <dbl>    <dbl> <dbl> <int>
#> 1      23.7       69.3    0.745     3    32
#> 
```
