# Funcao de autocorrelacao (ACF)

Calcula a ACF ate `lag_max` (backend C++), com bandas de confianca.

## Usage

``` r
rnp_ts_acf(x, lag_max = NULL, digits = 4L)
```

## Arguments

- x:

  Vetor numerico ou `ts`.

- lag_max:

  Inteiro. Numero maximo de defasagens.

- digits:

  Inteiro.

## Value

tibble com `lag`, `acf`, `lim_inf`, `lim_sup`.

## See also

Other series:
[`rnp_arima()`](https://evandeilton.github.io/rnp/reference/rnp_arima.md),
[`rnp_auto_arima()`](https://evandeilton.github.io/rnp/reference/rnp_auto_arima.md),
[`rnp_grafico_acf()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_acf.md),
[`rnp_grafico_serie()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_serie.md),
[`rnp_media_movel()`](https://evandeilton.github.io/rnp/reference/rnp_media_movel.md),
[`rnp_sarima()`](https://evandeilton.github.io/rnp/reference/rnp_sarima.md),
[`rnp_suavizacao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_suavizacao_exponencial.md),
[`rnp_ts_adf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_adf.md),
[`rnp_ts_ccf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ccf.md),
[`rnp_ts_decomposicao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_decomposicao.md),
[`rnp_ts_diferenciacao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_diferenciacao.md),
[`rnp_ts_garch()`](https://evandeilton.github.io/rnp/reference/rnp_ts_garch.md),
[`rnp_ts_holt_winters()`](https://evandeilton.github.io/rnp/reference/rnp_ts_holt_winters.md),
[`rnp_ts_kpss()`](https://evandeilton.github.io/rnp/reference/rnp_ts_kpss.md),
[`rnp_ts_ljung_box()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ljung_box.md),
[`rnp_ts_pacf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_pacf.md),
[`rnp_ts_periodograma()`](https://evandeilton.github.io/rnp/reference/rnp_ts_periodograma.md),
[`rnp_ts_previsao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_previsao.md),
[`rnp_ts_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_ts_residuos.md),
[`rnp_ts_var()`](https://evandeilton.github.io/rnp/reference/rnp_ts_var.md)

## Examples

``` r
rnp_ts_acf(as.numeric(AirPassengers), lag_max = 20)
#> # A tibble: 21 × 4
#>      lag   acf lim_inf lim_sup
#>    <int> <dbl>   <dbl>   <dbl>
#>  1     0 1      -0.163   0.163
#>  2     1 0.948  -0.163   0.163
#>  3     2 0.876  -0.163   0.163
#>  4     3 0.807  -0.163   0.163
#>  5     4 0.753  -0.163   0.163
#>  6     5 0.714  -0.163   0.163
#>  7     6 0.682  -0.163   0.163
#>  8     7 0.663  -0.163   0.163
#>  9     8 0.656  -0.163   0.163
#> 10     9 0.671  -0.163   0.163
#> # ℹ 11 more rows
```
