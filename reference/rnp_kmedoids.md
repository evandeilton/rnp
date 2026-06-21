# k-medoids (PAM)

Particionamento em torno de medoides (mais robusto que k-means a
outliers). Implementacao propria (build + swap simplificado) usando
distancias.

## Usage

``` r
rnp_kmedoids(
  base,
  k = 2L,
  metodo_dist = "euclidean",
  escalar = TRUE,
  max_iter = 100L,
  digits = 4L
)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- k:

  Inteiro. Numero de clusters.

- metodo_dist:

  String aceito por
  [`rnp_distancia()`](https://evandeilton.github.io/rnp/reference/rnp_distancia.md).

- escalar:

  Logico.

- max_iter:

  Inteiro.

- digits:

  Inteiro.

## Value

Uma lista com `clusters` (tibble), `medoides` (indices) e `custo` (soma
das distancias aos medoides).

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
[`rnp_lda()`](https://evandeilton.github.io/rnp/reference/rnp_lda.md),
[`rnp_manova()`](https://evandeilton.github.io/rnp/reference/rnp_manova.md),
[`rnp_matriz_correlacao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_correlacao.md),
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md)

## Examples

``` r
rnp_kmedoids(mtcars[, c("mpg", "hp", "wt")], k = 3)$medoides
#> [1] 10 24 26
```
