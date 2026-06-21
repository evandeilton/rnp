# Teste Z para proporcao (uma amostra)

Teste Z para proporcao (uma amostra)

## Usage

``` r
rnp_teste_z_proporcao(
  sucesso,
  n,
  p0 = 0.5,
  lado = c("bilateral", "direita", "esquerda"),
  conf = 0.95,
  digits = 4L
)
```

## Arguments

- sucesso:

  Numerico. Numero de sucessos.

- n:

  Inteiro. Tamanho da amostra.

- p0:

  Proporcao sob H0 (default 0.5).

- lado:

  String.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

tibble.

## Examples

``` r
rnp_teste_z_proporcao(55, 100, p0 = 0.5)
#> # A tibble: 1 × 9
#>   estatistica p_valor proporcao    p0 erro_padrao ic_inf ic_sup     n
#>         <dbl>   <dbl>     <dbl> <dbl>       <dbl>  <dbl>  <dbl> <dbl>
#> 1           1   0.317      0.55   0.5        0.05  0.452  0.648   100
#> # ℹ 1 more variable: alternativa <chr>
```
