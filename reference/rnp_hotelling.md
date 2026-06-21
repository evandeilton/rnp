# Teste T2 de Hotelling (medias multivariadas)

Compara o vetor de medias de duas amostras multivariadas (ou contra um
vetor de referencia, uma amostra).

## Usage

``` r
rnp_hotelling(X, Y = NULL, mu0 = NULL, digits = 4L)
```

## Arguments

- X:

  Matriz/data.frame do grupo 1.

- Y:

  Matriz/data.frame do grupo 2 (duas amostras) ou `NULL`.

- mu0:

  Vetor de referencia (uma amostra). Default zeros.

- digits:

  Inteiro.

## Value

tibble com `t2`, `estatistica_f`, `gl1`, `gl2`, `p_valor`.

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
[`rnp_kmedoids()`](https://evandeilton.github.io/rnp/reference/rnp_kmedoids.md),
[`rnp_lda()`](https://evandeilton.github.io/rnp/reference/rnp_lda.md),
[`rnp_manova()`](https://evandeilton.github.io/rnp/reference/rnp_manova.md),
[`rnp_matriz_correlacao()`](https://evandeilton.github.io/rnp/reference/rnp_matriz_correlacao.md),
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md)

## Examples

``` r
rnp_hotelling(iris[1:50, 1:4], iris[51:100, 1:4])
#> # A tibble: 1 × 5
#>      t2 estatistica_f   gl1   gl2 p_valor
#>   <dbl>         <dbl> <int> <dbl>   <dbl>
#> 1 2581.          625.     4    95       0
```
