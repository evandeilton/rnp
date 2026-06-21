# Tabela de frequencias para variavel categorica

Constroi tabela de frequencias absolutas e relativas (simples e
acumuladas) para um vetor categorico ou fator.

## Usage

``` r
rnp_tabela_frequencia(x, ordenar = FALSE, digits = 4L)
```

## Arguments

- x:

  Vetor categorico, fator ou caractere.

- ordenar:

  Logico. Ordena por frequencia decrescente.

- digits:

  Inteiro. Casas decimais.

## Value

tibble com `categoria`, `fa`, `fr`, `fa_acumulada`, `fr_acumulada`.

## See also

Other descritiva:
[`rnp_intervalo_classes()`](https://evandeilton.github.io/rnp/reference/rnp_intervalo_classes.md),
[`rnp_medias()`](https://evandeilton.github.io/rnp/reference/rnp_medias.md),
[`rnp_momentos()`](https://evandeilton.github.io/rnp/reference/rnp_momentos.md),
[`rnp_tabela_contingencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_contingencia.md)

## Examples

``` r
rnp_tabela_frequencia(mtcars$cyl)
#> # A tibble: 3 × 5
#>   categoria    fa    fr fa_acumulada fr_acumulada
#>   <chr>     <int> <dbl>        <int>        <dbl>
#> 1 4            11 0.344           11        0.344
#> 2 6             7 0.219           18        0.562
#> 3 8            14 0.438           32        1    
rnp_tabela_frequencia(letters[sample(3, 50, TRUE)], ordenar = TRUE)
#> # A tibble: 3 × 5
#>   categoria    fa    fr fa_acumulada fr_acumulada
#>   <chr>     <int> <dbl>        <int>        <dbl>
#> 1 a            19  0.38           19         0.38
#> 2 c            16  0.32           35         0.7 
#> 3 b            15  0.3            50         1   
```
