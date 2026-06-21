# Winsorizacao de outliers

Limita os valores extremos aos quantis `p` e `1 - p` (winsorizacao
simetrica), reduzindo o impacto de outliers.

## Usage

``` r
rnp_winsoriza(x, p = 0.05, na.rm = TRUE)
```

## Arguments

- x:

  Vetor numerico.

- p:

  Proporcao a winsorizar em cada cauda (ex.: 0.05).

- na.rm:

  Logico.

## Value

Vetor numerico winsorizado.

## See also

Other preprocessamento:
[`rnp_discretiza()`](https://evandeilton.github.io/rnp/reference/rnp_discretiza.md),
[`rnp_dummy()`](https://evandeilton.github.io/rnp/reference/rnp_dummy.md),
[`rnp_imputa()`](https://evandeilton.github.io/rnp/reference/rnp_imputa.md),
[`rnp_normaliza()`](https://evandeilton.github.io/rnp/reference/rnp_normaliza.md),
[`rnp_padroniza()`](https://evandeilton.github.io/rnp/reference/rnp_padroniza.md)

## Examples

``` r
rnp_winsoriza(c(-100, 1:20, 200), p = 0.05)
#>  [1]  1.05  1.05  2.00  3.00  4.00  5.00  6.00  7.00  8.00  9.00 10.00 11.00
#> [13] 12.00 13.00 14.00 15.00 16.00 17.00 18.00 19.00 19.95 19.95
```
