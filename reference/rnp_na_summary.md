# Resumo de valores faltantes

Mapa de missing por variavel e observacao.

## Usage

``` r
rnp_na_summary(base, digits = 4L)
```

## Arguments

- base:

  data.frame.

- digits:

  Inteiro.

## Value

lista:

- `por_variavel`: tibble com variavel, n_faltantes, percentual.

- `por_observacao`: tibble com observacao, n_faltantes, percentual.

- `padrao`: tibble com padroes de missing (se naniar disponivel).

## Examples

``` r
rnp_na_summary(airquality)
#> 
#> ── Resumo de valores faltantes ─────────────────────────────────────────────────
#> 
#> ── Por variavel 
#> # A tibble: 6 × 3
#>   variavel n_faltantes percentual
#>   <chr>          <dbl>      <dbl>
#> 1 Ozone             37     0.242 
#> 2 Solar.R            7     0.0458
#> 3 Wind               0     0     
#> 4 Temp               0     0     
#> 5 Month              0     0     
#> 6 Day                0     0     
#> 
#> ── Por observacao 
#> # A tibble: 153 × 3
#>    observacao n_faltantes percentual
#>         <dbl>       <dbl>      <dbl>
#>  1          1           0      0    
#>  2          2           0      0    
#>  3          3           0      0    
#>  4          4           0      0    
#>  5          5           2      0.333
#>  6          6           1      0.167
#>  7          7           0      0    
#>  8          8           0      0    
#>  9          9           0      0    
#> 10         10           1      0.167
#> # ℹ 143 more rows
#> 
#> ── Padrao 
#> # A tibble: 4 × 7
#>   Ozone Solar.R Wind  Temp  Month Day   n_casos
#>   <lgl> <lgl>   <lgl> <lgl> <lgl> <lgl>   <int>
#> 1 FALSE FALSE   FALSE FALSE FALSE FALSE     111
#> 2 TRUE  FALSE   FALSE FALSE FALSE FALSE      35
#> 3 FALSE TRUE    FALSE FALSE FALSE FALSE       5
#> 4 TRUE  TRUE    FALSE FALSE FALSE FALSE       2
```
