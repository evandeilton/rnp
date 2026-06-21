# Teste de variancias (F, Bartlett, Levene, Fligner-Killeen)

Teste de variancias (F, Bartlett, Levene, Fligner-Killeen)

## Usage

``` r
rnp_teste_variancias(
  x,
  g = NULL,
  method = c("bartlett", "levene", "fligner", "f"),
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico ou formula.

- g:

  Vetor de grupos (fator/character). Necessario se x nao for formula.

- method:

  String: `"bartlett"`, `"levene"`, `"fligner"`, `"f"` (apenas 2
  grupos).

- digits:

  Inteiro.

## Value

tibble com `estatistica`, `gl1`, `gl2`, `p_valor`, `metodo`.

## Examples

``` r
rnp_teste_variancias(mtcars$mpg, as.factor(mtcars$cyl), method = "bartlett")
#> # A tibble: 1 × 5
#>   estatistica   gl1   gl2 p_valor metodo  
#>         <dbl> <dbl> <int>   <dbl> <chr>   
#> 1        8.39     2    NA   0.015 bartlett
```
