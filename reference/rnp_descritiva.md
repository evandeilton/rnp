# Estatisticas descritivas robustas

Calcula um conjunto completo de estatisticas descritivas com tratamento
de valores faltantes e saida em tibble.

## Usage

``` r
rnp_descritiva(x, digits = 4L, na.rm = TRUE)
```

## Arguments

- x:

  Vetor numerico.

- digits:

  Inteiro. Numero de casas decimais na saida.

- na.rm:

  Logico. Remove NA antes do calculo.

## Value

tibble com 1 linha e colunas: `n`, `n_validos`, `n_faltantes`, `soma`,
`media`, `mediana`, `moda`, `desvio`, `variancia`, `min`, `q1`, `q3`,
`max`, `amplitude`, `iqr`, `cv`, `se_media`, `ic_inf`, `ic_sup`,
`assimetria`, `curtose`.

## Examples

``` r
rnp_descritiva(mtcars$mpg)
#> # A tibble: 1 × 21
#>       n n_validos n_faltantes  soma media mediana  moda desvio variancia   min
#>   <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>     <dbl> <dbl>
#> 1    32        32           0  643.  20.1    19.2    21   6.03      36.3  10.4
#> # ℹ 11 more variables: q1 <dbl>, q3 <dbl>, max <dbl>, amplitude <dbl>,
#> #   iqr <dbl>, cv <dbl>, se_media <dbl>, ic_inf <dbl>, ic_sup <dbl>,
#> #   assimetria <dbl>, curtose <dbl>
rnp_descritiva(airquality$Wind, na.rm = TRUE)
#> # A tibble: 1 × 21
#>       n n_validos n_faltantes  soma media mediana  moda desvio variancia   min
#>   <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>     <dbl> <dbl>
#> 1   153       153           0 1524.  9.96     9.7  11.5   3.52      12.4   1.7
#> # ℹ 11 more variables: q1 <dbl>, q3 <dbl>, max <dbl>, amplitude <dbl>,
#> #   iqr <dbl>, cv <dbl>, se_media <dbl>, ic_inf <dbl>, ic_sup <dbl>,
#> #   assimetria <dbl>, curtose <dbl>
```
