# Teste de Kruskal-Wallis

Teste de Kruskal-Wallis

## Usage

``` r
rnp_kruskal(x, g = NULL, digits = 4L, data = NULL)
```

## Arguments

- x:

  Vetor numerico ou formula.

- g:

  Vetor de grupos.

- digits:

  Inteiro.

- data:

  data.frame opcional (usado quando `x` e uma formula).

## Value

tibble.

## Examples

``` r
rnp_kruskal(mtcars$mpg, as.factor(mtcars$cyl))
#> # A tibble: 1 × 4
#>   estatistica    gl p_valor metodo        
#>         <dbl> <int>   <dbl> <chr>         
#> 1        25.7     2       0 kruskal-wallis
```
