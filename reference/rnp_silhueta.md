# Analise de silhueta

Avalia a qualidade de um agrupamento pelo indice de silhueta (backend
C++).

## Usage

``` r
rnp_silhueta(base, clusters, metodo_dist = "euclidean", digits = 4L)
```

## Arguments

- base:

  data.frame/matriz numerica OU objeto `dist`.

- clusters:

  Vetor de rotulos de cluster.

- metodo_dist:

  String aceito por
  [`rnp_distancia()`](https://evandeilton.github.io/rnp/reference/rnp_distancia.md)
  (se `base` nao for dist).

- digits:

  Inteiro.

## Value

Uma lista com `silhuetas` (tibble: `observacao`, `cluster`, `silhueta`)
e `media` (silhueta media global).

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
[`rnp_manova()`](https://evandeilton.github.io/rnp/reference/rnp_manova.md),
[`rnp_matriz_correlacao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_correlacao.md),
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md)

## Examples

``` r
km <- rnp_kmeans(mtcars[, c("mpg", "hp", "wt")], k = 3)
rnp_silhueta(mtcars[, c("mpg", "hp", "wt")], km$clusters$cluster)$media
#> [1] 0.2593
```
