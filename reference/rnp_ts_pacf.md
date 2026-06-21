# Funcao de autocorrelacao parcial (PACF)

Calcula a PACF ate `lag_max` por Durbin-Levinson (backend C++).

## Usage

``` r
rnp_ts_pacf(x, lag_max = NULL, digits = 4L)
```

## Arguments

- x:

  Vetor numerico ou `ts`.

- lag_max:

  Inteiro. Numero maximo de defasagens.

- digits:

  Inteiro.

## Value

tibble com `lag`, `pacf`, `lim_inf`, `lim_sup`.

## See also

Other series:
[`rnp_grafico_acf()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_acf.md),
[`rnp_grafico_serie()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_serie.md),
[`rnp_media_movel()`](https://evandeilton.github.io/rnp/reference/rnp_media_movel.md),
[`rnp_suavizacao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_suavizacao_exponencial.md),
[`rnp_ts_acf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_acf.md),
[`rnp_ts_decomposicao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_decomposicao.md),
[`rnp_ts_diferenciacao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_diferenciacao.md),
[`rnp_ts_holt_winters()`](https://evandeilton.github.io/rnp/reference/rnp_ts_holt_winters.md),
[`rnp_ts_ljung_box()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ljung_box.md),
[`rnp_ts_periodograma()`](https://evandeilton.github.io/rnp/reference/rnp_ts_periodograma.md)

## Examples

``` r
rnp_ts_pacf(as.numeric(AirPassengers), lag_max = 20)
#> # A tibble: 20 × 4
#>      lag    pacf lim_inf lim_sup
#>    <int>   <dbl>   <dbl>   <dbl>
#>  1     1  0.948   -0.163   0.163
#>  2     2 -0.229   -0.163   0.163
#>  3     3  0.0381  -0.163   0.163
#>  4     4  0.0938  -0.163   0.163
#>  5     5  0.0736  -0.163   0.163
#>  6     6  0.0077  -0.163   0.163
#>  7     7  0.126   -0.163   0.163
#>  8     8  0.09    -0.163   0.163
#>  9     9  0.232   -0.163   0.163
#> 10    10  0.166   -0.163   0.163
#> 11    11  0.171   -0.163   0.163
#> 12    12 -0.135   -0.163   0.163
#> 13    13 -0.540   -0.163   0.163
#> 14    14 -0.0266  -0.163   0.163
#> 15    15  0.0908  -0.163   0.163
#> 16    16  0.025   -0.163   0.163
#> 17    17  0.0325  -0.163   0.163
#> 18    18  0.0734  -0.163   0.163
#> 19    19  0.0484  -0.163   0.163
#> 20    20 -0.0455  -0.163   0.163
```
