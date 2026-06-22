# Cluster hierarquico

Agrupamento hierarquico aglomerativo via
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html), com corte em
`k` grupos.

## Usage

``` r
rnp_cluster_hierarquico(
  base,
  k = 2L,
  metodo_dist = "euclidean",
  metodo_lig = "complete",
  escalar = TRUE,
  digits = 4L
)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- k:

  Inteiro. Numero de grupos para o corte.

- metodo_dist:

  String aceito por
  [`rnp_distancia()`](https://evandeilton.github.io/rnp/reference/rnp_distancia.md).

- metodo_lig:

  String de ligacao: `"complete"`, `"average"`, `"single"`, `"ward.D2"`.

- escalar:

  Logico. Padroniza as variaveis.

- digits:

  Inteiro.

## Value

Uma lista com `modelo` (hclust), `grupos` (tibble: `observacao`,
`grupo`) e `altura_cortes` (vetor das ultimas alturas de fusao).

## See also

Other multivariada:
[`rnp_analise_fatorial()`](https://evandeilton.github.io/rnp/reference/rnp_analise_fatorial.md),
[`rnp_biplot()`](https://evandeilton.github.io/rnp/reference/rnp_biplot.md),
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
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md)

## Examples

``` r
rnp_cluster_hierarquico(mtcars[, c("mpg", "hp", "wt")], k = 3)
#> 
#> ── Cluster hierarquico ─────────────────────────────────────────────────────────
#> Modelo: objeto <hclust>
#> 
#> ── Grupos 
#> # A tibble: 32 × 2
#>    observacao grupo
#>         <int> <int>
#>  1          1     1
#>  2          2     1
#>  3          3     1
#>  4          4     1
#>  5          5     1
#>  6          6     1
#>  7          7     2
#>  8          8     1
#>  9          9     1
#> 10         10     1
#> # ℹ 22 more rows
#> Altura cortes: 5.7831, 4.0573, and 2.69
```
