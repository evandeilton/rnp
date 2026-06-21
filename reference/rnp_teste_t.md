# Teste t (uma ou duas amostras)

Wrapper consolidado para
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html).

## Usage

``` r
rnp_teste_t(
  x,
  y = NULL,
  mu = 0,
  pareado = FALSE,
  var_iguais = FALSE,
  lado = c("bilateral", "direita", "esquerda"),
  conf = 0.95,
  digits = 4L
)
```

## Arguments

- x, y:

  Vetores numericos (y ausente -\> uma amostra).

- mu:

  Hipotese nula para a media.

- pareado:

  Logico.

- var_iguais:

  Logico.

- lado:

  String: `"bilateral"`, `"direita"`, `"esquerda"`.

- conf:

  Nivel de confianca do IC.

- digits:

  Inteiro.

## Value

tibble com `estatistica`, `gl`, `p_valor`, `media_x`, `media_y`, `diff`,
`ic_inf`, `ic_sup`, `hipotese_nula`, `alternativa`.

## Examples

``` r
rnp_teste_t(rnorm(30, mean = 5), mu = 5)
#> # A tibble: 1 × 10
#>   estatistica    gl p_valor media_x media_y   diff ic_inf ic_sup hipotese_nula
#>         <dbl> <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>         <dbl>
#> 1       0.546    29   0.590    5.07      NA 0.0738   4.80   5.35             5
#> # ℹ 1 more variable: alternativa <chr>
rnp_teste_t(rnorm(30, 5), rnorm(30, 5.5))
#> # A tibble: 1 × 10
#>   estatistica    gl p_valor media_x media_y  diff ic_inf ic_sup hipotese_nula
#>         <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>         <dbl>
#> 1       -4.45  58.0       0    4.59    5.76 -1.17  -1.70 -0.646             0
#> # ℹ 1 more variable: alternativa <chr>
```
