# Coeficiente de assimetria (skewness)

Calcula o coeficiente de assimetria. Metodos: `pearson`, `fisher`
(bias-corrected, default), `bowley` (quartis).

## Usage

``` r
rnp_skewness(x, method = c("fisher", "pearson", "bowley"), na.rm = TRUE)
```

## Arguments

- x:

  Vetor numerico.

- method:

  String: `"fisher"`, `"pearson"` ou `"bowley"`.

- na.rm:

  Logico.

## Value

Escalar numerico.

## Examples

``` r
rnp_skewness(rnorm(1000))
#> [1] -0.03204998
rnp_skewness(1:100, method = "bowley")
#> [1] 0
```
