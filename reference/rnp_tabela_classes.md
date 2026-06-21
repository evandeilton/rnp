# Tabela de classes (frequencias agrupadas)

Constroi tabela de frequencias para variaveis quantitativas continuas,
com regra de amplitude configuravel: Sturges, Scott, Freedman-Diaconis,
Rice, Yule, sqrt(n) ou numero fixo de classes.

## Usage

``` r
rnp_tabela_classes(
  x,
  regra = c("sturges", "scott", "fd", "rice", "yule", "sqrt", "fixa"),
  k = NULL,
  amplitudes = NULL,
  right = TRUE,
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico.

- regra:

  String: `"sturges"`, `"scott"`, `"fd"`, `"rice"`, `"yule"`, `"sqrt"`,
  `"fixa"`.

- k:

  Inteiro. Numero de classes (quando `regra = "fixa"`).

- amplitudes:

  Vetor opcional de quebras customizadas. Sobrescreve regra.

- right:

  Logico. Intervalo fechado a direita (ver
  [`cut()`](https://rdrr.io/r/base/cut.html)).

- digits:

  Inteiro. Casas decimais.

## Value

tibble com colunas: `classe`, `lim_inf`, `lim_sup`, `ponto_medio`, `fa`,
`fr`, `fa_acumulada`, `fr_acumulada`.

## Examples

``` r
rnp_tabela_classes(rnorm(500))
#> # A tibble: 10 × 8
#>    classe      lim_inf lim_sup ponto_medio    fa    fr fa_acumulada fr_acumulada
#>    <chr>         <dbl>   <dbl>       <dbl> <int> <dbl>        <int>        <dbl>
#>  1 [-2.61,-1.…  -2.61   -1.99       -2.30     11 0.022           11        0.022
#>  2 (-1.99,-1.…  -1.99   -1.37       -1.68     27 0.054           38        0.076
#>  3 (-1.37,-0.…  -1.37   -0.754      -1.06     77 0.154          115        0.23 
#>  4 (-0.754,-0…  -0.754  -0.134      -0.444   104 0.208          219        0.438
#>  5 (-0.134,0.…  -0.134   0.486       0.176   116 0.232          335        0.67 
#>  6 (0.486,1.1…   0.486   1.11        0.795    93 0.186          428        0.856
#>  7 (1.11,1.73]   1.11    1.73        1.42     49 0.098          477        0.954
#>  8 (1.73,2.34]   1.73    2.35        2.04     18 0.036          495        0.99 
#>  9 (2.34,2.96]   2.35    2.96        2.65      4 0.008          499        0.998
#> 10 (2.96,3.58]   2.96    3.58        3.27      1 0.002          500        1    
rnp_tabela_classes(faithful$eruptions, regra = "scott")
#> # A tibble: 6 × 8
#>   classe      lim_inf lim_sup ponto_medio    fa     fr fa_acumulada fr_acumulada
#>   <chr>         <dbl>   <dbl>       <dbl> <int>  <dbl>        <int>        <dbl>
#> 1 [1.6,2.18]     1.6     2.18        1.89    71 0.261            71        0.261
#> 2 (2.18,2.77]    2.18    2.77        2.48    23 0.0846           94        0.346
#> 3 (2.77,3.35]    2.77    3.35        3.06     7 0.0257          101        0.371
#> 4 (3.35,3.93]    3.35    3.93        3.64    29 0.107           130        0.478
#> 5 (3.93,4.52]    3.93    4.52        4.22    85 0.312           215        0.790
#> 6 (4.52,5.1]     4.52    5.1         4.81    57 0.210           272        1    
rnp_tabela_classes(1:100, regra = "fixa", k = 5)
#> # A tibble: 5 × 8
#>   classe      lim_inf lim_sup ponto_medio    fa    fr fa_acumulada fr_acumulada
#>   <chr>         <dbl>   <dbl>       <dbl> <int> <dbl>        <int>        <dbl>
#> 1 [1,20.8]        1      20.8        10.9    20   0.2           20          0.2
#> 2 (20.8,40.6]    20.8    40.6        30.7    20   0.2           40          0.4
#> 3 (40.6,60.4]    40.6    60.4        50.5    20   0.2           60          0.6
#> 4 (60.4,80.2]    60.4    80.2        70.3    20   0.2           80          0.8
#> 5 (80.2,100]     80.2   100          90.1    20   0.2          100          1  
```
