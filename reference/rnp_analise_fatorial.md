# Analise fatorial exploratoria

Estima cargas fatoriais por
[`stats::factanal()`](https://rdrr.io/r/stats/factanal.html) (maxima
verossimilhanca).

## Usage

``` r
rnp_analise_fatorial(
  base,
  n_fatores = 2L,
  rotacao = c("varimax", "promax", "none"),
  digits = 4L
)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- n_fatores:

  Inteiro. Numero de fatores.

- rotacao:

  String: `"varimax"`, `"promax"` ou `"none"`.

- digits:

  Inteiro.

## Value

Uma lista com `cargas` (tibble), `variancia` (tibble) e `modelo`.

## See also

Other multivariada:
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
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md)

## Examples

``` r
rnp_analise_fatorial(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")],
                     n_fatores = 2)$cargas
#> # A tibble: 6 × 3
#>   variavel  Fator1 Fator2
#>   <chr>      <dbl>  <dbl>
#> 1 mpg      -0.842   0.376
#> 2 disp      0.864  -0.389
#> 3 hp        0.596  -0.698
#> 4 drat     -0.745   0.05 
#> 5 wt        0.974  -0.116
#> 6 qsec     -0.0655  0.962
```
