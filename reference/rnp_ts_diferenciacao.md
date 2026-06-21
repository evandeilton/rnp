# Diferenciacao de serie

Aplica diferenciacao regular (ordem `d`) e/ou sazonal (ordem `D`,
periodo `s`).

## Usage

``` r
rnp_ts_diferenciacao(x, d = 1L, D = 0L, s = 12L)
```

## Arguments

- x:

  Vetor numerico.

- d:

  Inteiro. Ordem da diferenciacao regular.

- D:

  Inteiro. Ordem da diferenciacao sazonal.

- s:

  Inteiro. Periodo sazonal.

## Value

Vetor numerico diferenciado.

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
rnp_ts_diferenciacao(as.numeric(AirPassengers), d = 1, D = 1, s = 12)
#>   [1]   5   1  -3  -2  10   8   0   0  -8  -4  12   8  -6  13  -9  19 -18   0
#>  [19]   0  -3   3   3  -6   0   4 -15   3  -7  29  -9  12 -18   4  -3   2  -3
#>  [37]  -9  27  11  -8 -21   9  -4  -2  -8 -12  -1   1 -16   7  -7  13  16  17
#>  [55] -17   1  -4   5   5  10   7 -13  10  -6  15  11  -8  -1  -8 -11  15  -7
#>  [73]   2   6  -6   4  11 -10   9 -15 -11   2  -6   3  -7  15  -4   2  11   4
#>  [91]  10 -13  -8  -7  -4  -5  -8 -11  -6   8   5  13  12 -38  12  -7  -4  19
#> [109]   4  20   4   9 -20  20  -3   5 -11   4  16 -11  -8 -36  52 -13  11  11
#> [127] -27  -2   9 -26  -1
```
