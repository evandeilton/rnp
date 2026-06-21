# Padroniza (z-score)

Centra na media e escala pelo desvio-padrao: z = (x - media) / dp.

## Usage

``` r
rnp_padroniza(x, na.rm = TRUE)
```

## Arguments

- x:

  Vetor numerico.

- na.rm:

  Logico.

## Value

Vetor numerico padronizado.

## See also

Other preprocessamento:
[`rnp_discretiza()`](https://evandeilton.github.io/rnp/reference/rnp_discretiza.md),
[`rnp_dummy()`](https://evandeilton.github.io/rnp/reference/rnp_dummy.md),
[`rnp_imputa()`](https://evandeilton.github.io/rnp/reference/rnp_imputa.md),
[`rnp_normaliza()`](https://evandeilton.github.io/rnp/reference/rnp_normaliza.md),
[`rnp_winsoriza()`](https://evandeilton.github.io/rnp/reference/rnp_winsoriza.md)

## Examples

``` r
rnp_padroniza(mtcars$mpg)
#>  [1]  0.15088482  0.15088482  0.44954345  0.21725341 -0.23073453 -0.33028740
#>  [7] -0.96078893  0.71501778  0.44954345 -0.14777380 -0.38006384 -0.61235388
#> [13] -0.46302456 -0.81145962 -1.60788262 -1.60788262 -0.89442035  2.04238943
#> [19]  1.71054652  2.29127162  0.23384555 -0.76168319 -0.81145962 -1.12671039
#> [25] -0.14777380  1.19619000  0.98049211  1.71054652 -0.71190675 -0.06481307
#> [31] -0.84464392  0.21725341
```
