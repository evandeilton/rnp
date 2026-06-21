# ANOVA de dois fatores

ANOVA de dois fatores

## Usage

``` r
rnp_anova_dois_fatores(formula, data = NULL, digits = 4L)
```

## Arguments

- formula:

  Formula `y ~ A * B` ou `y ~ A + B`.

- data:

  data.frame.

- digits:

  Inteiro.

## Value

tibble com a tabela ANOVA expandida.

## Examples

``` r
rnp_anova_dois_fatores(breaks ~ wool * tension, warpbreaks)
#> # A tibble: 4 × 6
#>   fonte             gl soma_quadrados media_quadrados estatistica_F p_valor
#>   <chr>          <dbl>          <dbl>           <dbl>         <dbl>   <dbl>
#> 1 "wool        "     1           451.            451.          3.77  0.0582
#> 2 "tension     "     2          2034.           1017.          8.50  0.0007
#> 3 "wool:tension"     2          1003.            501.          4.19  0.021 
#> 4 "Residuals   "    48          5745.            120.         NA    NA     
```
