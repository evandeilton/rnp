# Estrutura de um objeto (glance)

Resume a estrutura de um vetor, `data.frame` ou `tibble`: classe de cada
variavel, numero de observacoes e contagem de valores faltantes.
Substitui a antiga `rnp_atributos`, agora com saida em `tibble`.

## Usage

``` r
rnp_estrutura(obj)
```

## Arguments

- obj:

  Vetor, `data.frame` ou `tibble`.

## Value

tibble com `variavel`, `classe`, `n`, `n_faltantes`, `p_faltantes`.

## Examples

``` r
rnp_estrutura(mtcars)
#> # A tibble: 11 × 5
#>    variavel classe      n n_faltantes p_faltantes
#>    <chr>    <chr>   <int>       <int>       <dbl>
#>  1 mpg      numeric    32           0           0
#>  2 cyl      numeric    32           0           0
#>  3 disp     numeric    32           0           0
#>  4 hp       numeric    32           0           0
#>  5 drat     numeric    32           0           0
#>  6 wt       numeric    32           0           0
#>  7 qsec     numeric    32           0           0
#>  8 vs       numeric    32           0           0
#>  9 am       numeric    32           0           0
#> 10 gear     numeric    32           0           0
#> 11 carb     numeric    32           0           0
rnp_estrutura(airquality$Ozone)
#> # A tibble: 1 × 5
#>   variavel classe      n n_faltantes p_faltantes
#>   <chr>    <chr>   <int>       <int>       <dbl>
#> 1 x        integer   153          37       0.242
```
