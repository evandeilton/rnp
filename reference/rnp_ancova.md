# Analise de covariancia (ANCOVA)

Ajusta um modelo com um fator e uma covariavel continua, reportando a
tabela ANCOVA (tipo I).

## Usage

``` r
rnp_ancova(formula, data, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ fator + covariavel`.

- data:

  data.frame.

- digits:

  Inteiro.

## Value

tibble com a tabela ANOVA do modelo.

## See also

Other experimental:
[`rnp_anova_medidas_repetidas()`](https://evandeilton.github.io/rnp/reference/rnp_anova_medidas_repetidas.md),
[`rnp_contrastes()`](https://evandeilton.github.io/rnp/reference/rnp_contrastes.md),
[`rnp_dbc()`](https://evandeilton.github.io/rnp/reference/rnp_dbc.md)

## Examples

``` r
rnp_ancova(mpg ~ factor(cyl) + wt, mtcars)
#> # A tibble: 3 × 6
#>   fonte          gl    sq     qm estatistica_f p_valor
#>   <chr>       <int> <dbl>  <dbl>         <dbl>   <dbl>
#> 1 factor(cyl)     2  825. 412.            63.1  0     
#> 2 wt              1  118. 118.            18.1  0.0002
#> 3 Residuals      28  183.   6.54          NA   NA     
```
