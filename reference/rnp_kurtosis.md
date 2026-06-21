# Coeficiente de curtose

Calcula curtose excessiva. Metodos: `fisher` (default, bias-corrected),
`pearson` (sem correcao), `winkler` (correcao de Winker).

## Usage

``` r
rnp_kurtosis(x, method = c("fisher", "pearson", "winkler"), na.rm = TRUE)
```

## Arguments

- x:

  Vetor numerico.

- method:

  String: `"fisher"`, `"pearson"` ou `"winkler"`.

- na.rm:

  Logico.

## Value

Escalar numerico.

## Examples

``` r
rnp_kurtosis(rnorm(1000))
#> [1] -0.003408659
```
