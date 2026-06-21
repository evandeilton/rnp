# MANOVA (analise de variancia multivariada)

Wrapper de [`stats::manova()`](https://rdrr.io/r/stats/manova.html)
reportando as estatisticas de Wilks e Pillai.

## Usage

``` r
rnp_manova(formula, data, digits = 4L)
```

## Arguments

- formula:

  Formula `cbind(y1, y2, ...) ~ grupo`.

- data:

  data.frame.

- digits:

  Inteiro.

## Value

tibble com `teste`, `estatistica`, `aprox_f`, `p_valor`.

## See also

Other multivariada:
[`rnp_analise_fatorial()`](https://evandeilton.github.io/rnp/reference/rnp_analise_fatorial.md),
[`rnp_biplot()`](https://evandeilton.github.io/rnp/reference/rnp_biplot.md),
[`rnp_cluster_hierarquico()`](https://evandeilton.github.io/rnp/reference/rnp_cluster_hierarquico.md),
[`rnp_correlacao_canonica()`](https://evandeilton.github.io/rnp/reference/rnp_correlacao_canonica.md),
[`rnp_correspondencia()`](https://evandeilton.github.io/rnp/reference/rnp_correspondencia.md),
[`rnp_grafico_correlograma()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_correlograma.md),
[`rnp_grafico_dendrograma()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_dendrograma.md),
[`rnp_grafico_dispersao_matriz()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_dispersao_matriz.md),
[`rnp_hotelling()`](https://evandeilton.github.io/rnp/reference/rnp_hotelling.md),
[`rnp_kmedoids()`](https://evandeilton.github.io/rnp/reference/rnp_kmedoids.md),
[`rnp_lda()`](https://evandeilton.github.io/rnp/reference/rnp_lda.md),
[`rnp_matriz_correlacao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_correlacao.md),
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md)

## Examples

``` r
rnp_manova(cbind(Sepal.Length, Petal.Length) ~ Species, iris)
#> # A tibble: 2 × 4
#>   teste  estatistica aprox_f p_valor
#>   <chr>        <dbl>   <dbl>   <dbl>
#> 1 Wilks       0.0399   293.        0
#> 2 Pillai      0.988     71.8       0
```
