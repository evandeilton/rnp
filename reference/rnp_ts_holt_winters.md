# Suavizacao de Holt-Winters

Wrapper de
[`stats::HoltWinters()`](https://rdrr.io/r/stats/HoltWinters.html)
(nivel, tendencia e sazonalidade).

## Usage

``` r
rnp_ts_holt_winters(
  x,
  frequency = 12L,
  sazonal = c("additive", "multiplicative"),
  digits = 4L
)
```

## Arguments

- x:

  Objeto `ts` (ou numerico, convertido com `frequency`).

- frequency:

  Inteiro. Periodicidade (se `x` nao for `ts`).

- sazonal:

  String: `"additive"` ou `"multiplicative"`.

- digits:

  Inteiro.

## Value

Uma lista com `modelo` (HoltWinters), `parametros` (tibble: alpha, beta,
gamma) e `sse`.

## See also

Other series:
[`rnp_grafico_acf()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_acf.md),
[`rnp_grafico_serie()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_serie.md),
[`rnp_media_movel()`](https://evandeilton.github.io/rnp/reference/rnp_media_movel.md),
[`rnp_suavizacao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_suavizacao_exponencial.md),
[`rnp_ts_acf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_acf.md),
[`rnp_ts_decomposicao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_decomposicao.md),
[`rnp_ts_diferenciacao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_diferenciacao.md),
[`rnp_ts_ljung_box()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ljung_box.md),
[`rnp_ts_pacf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_pacf.md),
[`rnp_ts_periodograma()`](https://evandeilton.github.io/rnp/reference/rnp_ts_periodograma.md)

## Examples

``` r
rnp_ts_holt_winters(AirPassengers)$parametros
#> # A tibble: 1 × 3
#>   alpha   beta gamma
#>   <dbl>  <dbl> <dbl>
#> 1 0.248 0.0345     1
```
