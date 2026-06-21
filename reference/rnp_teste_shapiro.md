# Teste de Shapiro-Wilk (normalidade)

Teste de Shapiro-Wilk (normalidade)

## Usage

``` r
rnp_teste_shapiro(x, digits = 4L)
```

## Arguments

- x:

  Vetor numerico.

- digits:

  Inteiro.

## Value

tibble com `estatistica`, `gl`, `p_valor`, `metodo`.

## Examples

``` r
rnp_teste_shapiro(rnorm(50))
#> # A tibble: 1 × 4
#>   estatistica    gl p_valor metodo      
#>         <dbl> <int>   <dbl> <chr>       
#> 1       0.984    50   0.750 shapiro-wilk
```
