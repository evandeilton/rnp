# Previsao a partir de modelo ARIMA/SARIMA

Previsao a partir de modelo ARIMA/SARIMA

## Usage

``` r
rnp_ts_previsao(modelo, h = 10, conf = 0.95, digits = 4L)
```

## Arguments

- modelo:

  Saida de
  [`rnp_arima()`](https://evandeilton.github.io/rnp/reference/rnp_arima.md)/[`rnp_sarima()`](https://evandeilton.github.io/rnp/reference/rnp_sarima.md)
  (ou objeto `Arima`).

- h:

  Inteiro. Horizonte de previsao.

- conf:

  Nivel de confianca dos intervalos.

- digits:

  Inteiro.

## Value

Uma lista com `previsao` (tibble) e `grafico` (`ggplot`).

## See also

Other series:
[`rnp_arima()`](https://evandeilton.github.io/rnp/reference/rnp_arima.md),
[`rnp_auto_arima()`](https://evandeilton.github.io/rnp/reference/rnp_auto_arima.md),
[`rnp_grafico_acf()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_acf.md),
[`rnp_grafico_serie()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_serie.md),
[`rnp_media_movel()`](https://evandeilton.github.io/rnp/reference/rnp_media_movel.md),
[`rnp_sarima()`](https://evandeilton.github.io/rnp/reference/rnp_sarima.md),
[`rnp_suavizacao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_suavizacao_exponencial.md),
[`rnp_ts_acf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_acf.md),
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
[`rnp_ts_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_ts_residuos.md),
[`rnp_ts_var()`](https://evandeilton.github.io/rnp/reference/rnp_ts_var.md)

## Examples

``` r
m <- rnp_arima(lh, c(1, 0, 0))
rnp_ts_previsao(m, h = 6)$previsao
#> # A tibble: 6 × 4
#>   passo previsao ic_inf ic_sup
#>   <int>    <dbl>  <dbl>  <dbl>
#> 1     1     2.69   1.82   3.56
#> 2     2     2.57   1.57   3.58
#> 3     3     2.51   1.46   3.55
#> 4     4     2.47   1.41   3.52
#> 5     5     2.44   1.38   3.51
#> 6     6     2.43   1.37   3.49
```
