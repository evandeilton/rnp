# Suavizacao exponencial

Suavizacao exponencial simples (EWMA), via backend C++.

## Usage

``` r
rnp_suavizacao_exponencial(x, alpha = 0.3, digits = 4L)
```

## Arguments

- x:

  Vetor numerico.

- alpha:

  Fator de suavizacao em (0, 1).

- digits:

  Inteiro.

## Value

tibble com `tempo`, `original` e `suavizada`.

## See also

Other series:
[`rnp_grafico_acf()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_acf.md),
[`rnp_grafico_serie()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_serie.md),
[`rnp_media_movel()`](https://evandeilton.github.io/rnp/reference/rnp_media_movel.md),
[`rnp_ts_acf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_acf.md),
[`rnp_ts_decomposicao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_decomposicao.md),
[`rnp_ts_diferenciacao()`](https://evandeilton.github.io/rnp/reference/rnp_ts_diferenciacao.md),
[`rnp_ts_holt_winters()`](https://evandeilton.github.io/rnp/reference/rnp_ts_holt_winters.md),
[`rnp_ts_ljung_box()`](https://evandeilton.github.io/rnp/reference/rnp_ts_ljung_box.md),
[`rnp_ts_pacf()`](https://evandeilton.github.io/rnp/reference/rnp_ts_pacf.md),
[`rnp_ts_periodograma()`](https://evandeilton.github.io/rnp/reference/rnp_ts_periodograma.md)

## Examples

``` r
rnp_suavizacao_exponencial(as.numeric(AirPassengers), alpha = 0.3)
#> # A tibble: 144 × 3
#>    tempo original suavizada
#>    <int>    <dbl>     <dbl>
#>  1     1      112      112 
#>  2     2      118      114.
#>  3     3      132      119.
#>  4     4      129      122.
#>  5     5      121      122.
#>  6     6      135      126.
#>  7     7      148      132.
#>  8     8      148      137.
#>  9     9      136      137.
#> 10    10      119      131.
#> # ℹ 134 more rows
```
