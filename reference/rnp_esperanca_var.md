# Esperanca e variancia teoricas

Calcula E\[X\] e Var\[X\] para distribuicoes nomeadas comuns a partir
dos parametros.

## Usage

``` r
rnp_esperanca_var(
  dist = c("norm", "unif", "exp", "gamma", "weibull", "binom", "pois", "geom", "nbinom",
    "hyper", "beta"),
  ...
)
```

## Arguments

- dist:

  String: `"norm"`, `"unif"`, `"exp"`, `"gamma"`, `"weibull"`,
  `"binom"`, `"pois"`, `"geom"`, `"nbinom"`, `"hyper"`, `"beta"`.

- ...:

  Parametros da distribuicao.

## Value

tibble com colunas `distribuicao`, `esperanca`, `variancia`, `desvio`.

## Examples

``` r
rnp_esperanca_var("binom", size = 10, prob = .5)
#> # A tibble: 1 × 4
#>   distribuicao esperanca variancia desvio
#>   <chr>            <dbl>     <dbl>  <dbl>
#> 1 binom                5       2.5   1.58
rnp_esperanca_var("pois", lambda = 3)
#> # A tibble: 1 × 4
#>   distribuicao esperanca variancia desvio
#>   <chr>            <dbl>     <dbl>  <dbl>
#> 1 pois                 3         3   1.73
```
