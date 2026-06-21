# Teste de Mann-Whitney (Wilcoxon duas amostras independentes)

Teste de Mann-Whitney (Wilcoxon duas amostras independentes)

## Usage

``` r
rnp_mann_whitney(
  x,
  y,
  lado = c("bilateral", "direita", "esquerda"),
  digits = 4L
)
```

## Arguments

- x, y:

  Vetores numericos.

- lado:

  String.

- digits:

  Inteiro.

## Value

tibble.

## Examples

``` r
rnp_mann_whitney(rnorm(20, 5), rnorm(20, 6))
#> # A tibble: 1 × 4
#>   estatistica p_valor metodo                       alternativa
#>         <dbl>   <dbl> <chr>                        <chr>      
#> 1         122   0.035 Wilcoxon rank sum exact test bilateral  
```
