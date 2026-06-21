# Intervalo de confianca para a media

IC para a media populacional usando distribuicao t (default) ou Z
(quando sigma conhecido ou amostra grande).

## Usage

``` r
rnp_ic_media(x, conf = 0.95, sigma = NULL, na.rm = TRUE, digits = 4L)
```

## Arguments

- x:

  Vetor numerico.

- conf:

  Nivel de confianca (ex.: 0.95).

- sigma:

  Desvio-padrao populacional, se conhecido. Default NULL -\> t.

- na.rm:

  Logico.

- digits:

  Inteiro.

## Value

tibble com colunas `media`, `erro_padrao`, `limite_inferior`,
`limite_superior`, `n`, `nivel_confianca`, `distribuicao`.

## Examples

``` r
rnp_ic_media(rnorm(50, mean = 10, sd = 2))
#> # A tibble: 1 × 7
#>   media erro_padrao limite_inferior limite_superior     n nivel_confianca
#>   <dbl>       <dbl>           <dbl>           <dbl> <dbl>           <dbl>
#> 1  10.3       0.269            9.81            10.9    50            0.95
#> # ℹ 1 more variable: distribuicao <chr>
rnp_ic_media(rnorm(50), sigma = 1)
#> # A tibble: 1 × 7
#>    media erro_padrao limite_inferior limite_superior     n nivel_confianca
#>    <dbl>       <dbl>           <dbl>           <dbl> <dbl>           <dbl>
#> 1 0.0548       0.141          -0.222           0.332    50            0.95
#> # ℹ 1 more variable: distribuicao <chr>
```
