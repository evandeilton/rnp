# Analise de correspondencia (CA)

Decompoe uma tabela de contingencia para mapear linhas e colunas em um
espaco de baixa dimensao (via SVD dos residuos padronizados).

## Usage

``` r
rnp_correspondencia(tabela, n_dim = 2L, digits = 4L)
```

## Arguments

- tabela:

  Matriz/tabela de contingencia (contagens).

- n_dim:

  Inteiro. Numero de dimensoes a reter.

- digits:

  Inteiro.

## Value

Uma lista com `inercia` (tibble), `coord_linhas` (tibble) e
`coord_colunas` (tibble).

## See also

Other multivariada:
[`rnp_analise_fatorial()`](https://evandeilton.github.io/rnp/reference/rnp_analise_fatorial.md),
[`rnp_biplot()`](https://evandeilton.github.io/rnp/reference/rnp_biplot.md),
[`rnp_cluster_hierarquico()`](https://evandeilton.github.io/rnp/reference/rnp_cluster_hierarquico.md),
[`rnp_correlacao_canonica()`](https://evandeilton.github.io/rnp/reference/rnp_correlacao_canonica.md),
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
rnp_correspondencia(table(mtcars$cyl, mtcars$gear))
#> $inercia
#> # A tibble: 2 × 3
#>   dimensao inercia   prop
#>      <int>   <dbl>  <dbl>
#> 1        1  0.563  0.998 
#> 2        2  0.0011 0.0019
#> 
#> $coord_linhas
#> # A tibble: 3 × 3
#>   categoria   Dim1    Dim2
#>   <chr>      <dbl>   <dbl>
#> 1 4         -0.799  0.0286
#> 2 6         -0.415 -0.0587
#> 3 8          0.835  0.0069
#> 
#> $coord_colunas
#> # A tibble: 3 × 3
#>   categoria    Dim1    Dim2
#>   <chr>       <dbl>   <dbl>
#> 1 3          0.746  -0.0123
#> 2 4         -0.895  -0.016 
#> 3 5         -0.0913  0.0754
#> 
```
