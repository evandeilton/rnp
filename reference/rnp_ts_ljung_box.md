# Teste de Ljung-Box (autocorrelacao)

Testa a presenca de autocorrelacao ate `lag` defasagens (H0: ruido
branco).

## Usage

``` r
rnp_ts_ljung_box(x, lag = 10L, fitdf = 0L, digits = 4L)
```

## Arguments

- x:

  Vetor numerico (ex.: residuos de um modelo).

- lag:

  Inteiro. Numero de defasagens.

- fitdf:

  Inteiro. Graus de liberdade gastos no ajuste (subtraidos).

- digits:

  Inteiro.

## Value

tibble com `estatistica`, `gl`, `p_valor`.

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
[`rnp_ts_pacf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_pacf.md),
[`rnp_ts_periodograma()`](https://evandeilton.github.io/rnp/reference/rnp_ts_periodograma.md)

## Examples

``` r
rnp_ts_ljung_box(rnorm(200), lag = 10)
#> # A tibble: 1 × 3
#>   estatistica    gl p_valor
#>         <dbl> <int>   <dbl>
#> 1        4.03    10   0.946
```
