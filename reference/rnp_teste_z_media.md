# Teste Z para media (sigma conhecido)

Teste Z para media (sigma conhecido)

## Usage

``` r
rnp_teste_z_media(
  x,
  mu = 0,
  sigma,
  lado = c("bilateral", "direita", "esquerda"),
  conf = 0.95,
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico.

- mu:

  Media sob H0.

- sigma:

  Desvio-padrao populacional conhecido.

- lado:

  String: bilateral, direita ou esquerda.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

tibble.

## Examples

``` r
rnp_teste_z_media(rnorm(100, mean = 10, sd = 2), mu = 10, sigma = 2)
#> # A tibble: 1 × 8
#>   estatistica p_valor media_x erro_padrao ic_inf ic_sup hipotese_nula
#>         <dbl>   <dbl>   <dbl>       <dbl>  <dbl>  <dbl>         <dbl>
#> 1      -0.059   0.953    9.99         0.2   9.60   10.4            10
#> # ℹ 1 more variable: alternativa <chr>
```
