# Teste de Levene (alias direto)

Atalho para
[`rnp_teste_variancias()`](https://evandeilton.github.io/rnp/reference/rnp_teste_variancias.md)
com method = "levene".

## Usage

``` r
rnp_teste_levene(x, g = NULL, digits = 4L)
```

## Arguments

- x:

  Vetor numerico ou formula.

- g:

  Vetor de grupos (fator/character). Necessario se x nao for formula.

- digits:

  Inteiro.

## Value

tibble.

## Examples

``` r
rnp_teste_levene(mtcars$mpg, as.factor(mtcars$cyl))
#> # A tibble: 1 × 5
#>   estatistica   gl1   gl2 p_valor metodo
#>         <dbl> <dbl> <int>   <dbl> <chr> 
#> 1        5.51     2    29  0.0094 levene
```
