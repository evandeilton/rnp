# Normaliza (min-max para \[0, 1\])

Reescala para o intervalo \[0, 1\]: (x - min) / (max - min).

## Usage

``` r
rnp_normaliza(x, na.rm = TRUE)
```

## Arguments

- x:

  Vetor numerico.

- na.rm:

  Logico.

## Value

Vetor numerico em \[0, 1\].

## See also

Other preprocessamento:
[`rnp_discretiza()`](https://evandeilton.github.io/rnp/reference/rnp_discretiza.md),
[`rnp_dummy()`](https://evandeilton.github.io/rnp/reference/rnp_dummy.md),
[`rnp_imputa()`](https://evandeilton.github.io/rnp/reference/rnp_imputa.md),
[`rnp_padroniza()`](https://evandeilton.github.io/rnp/reference/rnp_padroniza.md),
[`rnp_winsoriza()`](https://evandeilton.github.io/rnp/reference/rnp_winsoriza.md)

## Examples

``` r
rnp_normaliza(mtcars$hp)
#>  [1] 0.20494700 0.20494700 0.14487633 0.20494700 0.43462898 0.18727915
#>  [7] 0.68197880 0.03533569 0.15194346 0.25088339 0.25088339 0.45229682
#> [13] 0.45229682 0.45229682 0.54063604 0.57597173 0.62897527 0.04946996
#> [19] 0.00000000 0.04593640 0.15901060 0.34628975 0.34628975 0.68197880
#> [25] 0.43462898 0.04946996 0.13780919 0.21554770 0.74911661 0.43462898
#> [31] 1.00000000 0.20141343
```
