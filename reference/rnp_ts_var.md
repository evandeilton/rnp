# Autorregressao vetorial (VAR)

Ajusta um VAR(p) por minimos quadrados equacao-a-equacao e testa
causalidade de Granger entre as series.

## Usage

``` r
rnp_ts_var(dados, p = 1, digits = 4L)
```

## Arguments

- dados:

  data.frame/matriz com as series em colunas.

- p:

  Ordem do VAR.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (por equacao) e `granger` (tibble de testes
de causalidade).

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
[`rnp_ts_previsao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_previsao.md),
[`rnp_ts_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_ts_residuos.md)

## Examples

``` r
df <- data.frame(m = as.numeric(mdeaths), f = as.numeric(fdeaths))
rnp_ts_var(df, p = 2)$granger
#> # A tibble: 2 × 4
#>   causa efeito estatistica_f p_valor
#>   <chr> <chr>          <dbl>   <dbl>
#> 1 f     m               1.44  0.244 
#> 2 m     f               2.73  0.0724
```
