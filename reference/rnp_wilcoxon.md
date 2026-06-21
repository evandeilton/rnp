# Teste de Wilcoxon para amostras pareadas

Atalho para Wilcoxon signed-rank.

## Usage

``` r
rnp_wilcoxon(x, y, lado = c("bilateral", "direita", "esquerda"), digits = 4L)
```

## Arguments

- x, y:

  Vetores numericos pareados.

- lado:

  String.

- digits:

  Inteiro.

## Value

tibble.

## Examples

``` r
rnp_wilcoxon(rnorm(20, 5), rnorm(20, 5.2))
#> # A tibble: 1 × 4
#>   estatistica p_valor metodo                          alternativa
#>         <dbl>   <dbl> <chr>                           <chr>      
#> 1          84   0.452 Wilcoxon signed rank exact test bilateral  
```
