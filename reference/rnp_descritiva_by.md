# Estatisticas descritivas por grupo

Versao grouped de
[`rnp_descritiva()`](https://evandeilton.github.io/rnp/reference/rnp_descritiva.md),
construida sobre `dplyr` (sem dependencia do `plyr` descontinuado).

## Usage

``` r
rnp_descritiva_by(base, variavel, grupos, digits = 4L)
```

## Arguments

- base:

  data.frame ou tibble.

- variavel:

  Nome (string) da coluna numerica a analisar.

- grupos:

  Vetor de strings com nomes das colunas de agrupamento.

- digits:

  Inteiro. Casas decimais.

## Value

tibble com colunas de grupo + estatisticas (ver
[`rnp_descritiva()`](https://evandeilton.github.io/rnp/reference/rnp_descritiva.md)).

## Examples

``` r
rnp_descritiva_by(mtcars, "mpg", "cyl")
#> # A tibble: 3 × 22
#>     cyl     n n_validos n_faltantes  soma media mediana  moda desvio variancia
#>   <dbl> <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>     <dbl>
#> 1     4    11        11           0  293.  26.7    26    22.8   4.51     20.3 
#> 2     6     7         7           0  138.  19.7    19.7  21     1.45      2.11
#> 3     8    14        14           0  211.  15.1    15.2  15.2   2.56      6.55
#> # ℹ 12 more variables: min <dbl>, q1 <dbl>, q3 <dbl>, max <dbl>,
#> #   amplitude <dbl>, iqr <dbl>, cv <dbl>, se_media <dbl>, ic_inf <dbl>,
#> #   ic_sup <dbl>, assimetria <dbl>, curtose <dbl>
rnp_descritiva_by(mtcars, "wt", c("gear", "cyl"))
#> # A tibble: 8 × 23
#>    gear   cyl     n n_validos n_faltantes  soma media mediana  moda desvio
#>   <dbl> <dbl> <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>
#> 1     3     4     1         1           0  2.46  2.46    2.46  2.46 NA    
#> 2     3     6     2         2           0  6.68  3.34    3.34  3.22  0.173
#> 3     3     8    12        12           0 49.2   4.10    3.81  3.44  0.768
#> 4     4     4     8         8           0 19.0   2.38    2.26  2.32  0.601
#> 5     4     6     4         4           0 12.4   3.09    3.16  3.44  0.413
#> 6     5     4     2         2           0  3.65  1.83    1.83  2.14  0.443
#> 7     5     6     1         1           0  2.77  2.77    2.77  2.77 NA    
#> 8     5     8     2         2           0  6.74  3.37    3.37  3.17  0.283
#> # ℹ 13 more variables: variancia <dbl>, min <dbl>, q1 <dbl>, q3 <dbl>,
#> #   max <dbl>, amplitude <dbl>, iqr <dbl>, cv <dbl>, se_media <dbl>,
#> #   ic_inf <dbl>, ic_sup <dbl>, assimetria <dbl>, curtose <dbl>
```
