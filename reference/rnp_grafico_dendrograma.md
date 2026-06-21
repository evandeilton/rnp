# Dendrograma

Dendrograma

## Usage

``` r
rnp_grafico_dendrograma(cluster_hier, k = NULL)
```

## Arguments

- cluster_hier:

  Saida de
  [`rnp_cluster_hierarquico()`](https://evandeilton.github.io/rnp/reference/rnp_cluster_hierarquico.md)
  ou objeto `hclust`.

- k:

  Inteiro opcional. Desenha `k` retangulos de corte (informativo).

## Value

Objeto `ggplot`.

## See also

Other multivariada:
[`rnp_analise_fatorial()`](https://evandeilton.github.io/rnp/reference/rnp_analise_fatorial.md),
[`rnp_biplot()`](https://evandeilton.github.io/rnp/reference/rnp_biplot.md),
[`rnp_cluster_hierarquico()`](https://evandeilton.github.io/rnp/reference/rnp_cluster_hierarquico.md),
[`rnp_correlacao_canonica()`](https://evandeilton.github.io/rnp/reference/rnp_correlacao_canonica.md),
[`rnp_correspondencia()`](https://evandeilton.github.io/rnp/reference/rnp_correspondencia.md),
[`rnp_grafico_correlograma()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_correlograma.md),
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
ch <- rnp_cluster_hierarquico(mtcars[, c("mpg", "hp", "wt")], k = 3)
rnp_grafico_dendrograma(ch)
```
