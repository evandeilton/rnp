# Grafico de ACF ou PACF

Grafico de ACF ou PACF

## Usage

``` r
rnp_grafico_acf(x, tipo = c("acf", "pacf"), lag_max = NULL)
```

## Arguments

- x:

  Vetor numerico ou `ts`.

- tipo:

  String: `"acf"` ou `"pacf"`.

- lag_max:

  Inteiro. Numero maximo de defasagens.

## Value

Objeto `ggplot`.

## See also

Other series:
[`rnp_arima()`](https://evandeilton.github.io/rnp/reference/rnp_arima.md),
[`rnp_auto_arima()`](https://evandeilton.github.io/rnp/reference/rnp_auto_arima.md),
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
[`rnp_ts_previsao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_previsao.md),
[`rnp_ts_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_ts_residuos.md),
[`rnp_ts_var()`](https://evandeilton.github.io/rnp/reference/rnp_ts_var.md)

## Examples

``` r
rnp_grafico_acf(as.numeric(AirPassengers), tipo = "acf")
```
