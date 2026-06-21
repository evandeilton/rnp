# Quantis com metodo configuravel

Wrapper de [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)
expondo o argumento `type` (Hyndman-Fan, 1 a 9).

## Usage

``` r
rnp_quantis(
  x,
  probs = c(0.25, 0.5, 0.75),
  type = 7L,
  na.rm = TRUE,
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico.

- probs:

  Vetor de probabilidades em \[0, 1\].

- type:

  Inteiro 1-9. Metodo do quantil.

- na.rm:

  Logico.

- digits:

  Inteiro. Casas decimais.

## Value

Vetor nomeado.

## Examples

``` r
rnp_quantis(1:100, c(.1, .25, .5, .75, .9), type = 7)
#>   10%   25%   50%   75%   90% 
#> 10.90 25.75 50.50 75.25 90.10 
```
