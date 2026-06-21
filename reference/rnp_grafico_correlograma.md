# Correlograma (mapa de calor de correlacoes)

Correlograma (mapa de calor de correlacoes)

## Usage

``` r
rnp_grafico_correlograma(base, metodo = c("pearson", "spearman"), digits = 2L)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- metodo:

  String: `"pearson"` ou `"spearman"`.

- digits:

  Inteiro.

## Value

Objeto `ggplot`.

## See also

Other multivariada:
[`rnp_analise_fatorial()`](https://evandeilton.github.io/rnp/reference/rnp_analise_fatorial.md),
[`rnp_biplot()`](https://evandeilton.github.io/rnp/reference/rnp_biplot.md),
[`rnp_cluster_hierarquico()`](https://evandeilton.github.io/rnp/reference/rnp_cluster_hierarquico.md),
[`rnp_correlacao_canonica()`](https://evandeilton.github.io/rnp/reference/rnp_correlacao_canonica.md),
[`rnp_correspondencia()`](https://evandeilton.github.io/rnp/reference/rnp_correspondencia.md),
[`rnp_grafico_dendrograma()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_dendrograma.md),
[`rnp_grafico_dispersao_matriz()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_dispersao_matriz.md),
[`rnp_hotelling()`](https://evandeilton.github.io/rnp/reference/rnp_hotelling.md),
[`rnp_kmedoids()`](https://evandeilton.github.io/rnp/reference/rnp_kmedoids.md),
[`rnp_lda()`](https://evandeilton.github.io/rnp/reference/rnp_lda.md),
[`rnp_manova()`](https://evandeilton.github.io/rnp/reference/rnp_manova.md),
[`rnp_matriz_correlacao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_correlacao.md),
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md)

## Examples

``` r
rnp_grafico_correlograma(mtcars[, c("mpg", "hp", "wt", "disp", "drat")])
```
