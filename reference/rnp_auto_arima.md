# Selecao automatica de ordem ARIMA

Busca em grade a melhor ordem ARIMA(p, d, q) (opcionalmente sazonal)
pelo criterio escolhido. Substitui `forecast::auto.arima` com
implementacao propria sobre
[`stats::arima()`](https://rdrr.io/r/stats/arima.html).

## Usage

``` r
rnp_auto_arima(
  x,
  max_p = 3,
  max_d = 2,
  max_q = 3,
  periodo = 1,
  ic = c("aicc", "aic", "bic"),
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico ou `ts`.

- max_p, max_d, max_q:

  Ordens maximas nao-sazonais.

- periodo:

  Periodo sazonal (1 = sem sazonalidade).

- ic:

  String: `"aicc"`, `"aic"` ou `"bic"`.

- digits:

  Inteiro.

## Value

Uma lista como
[`rnp_arima()`](https://evandeilton.github.io/rnp/reference/rnp_arima.md),
com `selecao` (tibble dos melhores candidatos) adicional.

## See also

Other series:
[`rnp_arima()`](https://evandeilton.github.io/rnp/reference/rnp_arima.md),
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
[`rnp_ts_previsao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_previsao.md),
[`rnp_ts_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_ts_residuos.md),
[`rnp_ts_var()`](https://evandeilton.github.io/rnp/reference/rnp_ts_var.md)

## Examples

``` r
rnp_auto_arima(lh, max_p = 2, max_q = 2)$modelo
#> # A tibble: 1 × 5
#>   log_veross   aic   bic  aicc sigma2
#>        <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1      -27.5  63.1  68.7  63.6  0.182
```
