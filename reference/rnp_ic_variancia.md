# Intervalo de confianca para variancia (chi-quadrado)

Intervalo de confianca para variancia (chi-quadrado)

## Usage

``` r
rnp_ic_variancia(x, conf = 0.95, na.rm = TRUE, digits = 4L)
```

## Arguments

- x:

  Vetor numerico.

- conf:

  Nivel de confianca.

- na.rm:

  Logico.

- digits:

  Inteiro.

## Value

tibble com `variancia`, `limite_inferior`, `limite_superior`, `n`, `gl`.

## Examples

``` r
rnp_ic_variancia(rnorm(30, sd = 2))
#> # A tibble: 1 × 5
#>   variancia limite_inferior limite_superior     n    gl
#>       <dbl>           <dbl>           <dbl> <int> <int>
#> 1      2.52            1.60            4.56    30    29
```
