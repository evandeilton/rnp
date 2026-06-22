# Matriz de correlacao com p-valores

Calcula a matriz de correlacao (Pearson ou Spearman, via backend C++) e
a matriz de p-valores dos testes de correlacao, em formato longo (tidy).

## Usage

``` r
rnp_matriz_correlacao(base, metodo = c("pearson", "spearman"), digits = 4L)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- metodo:

  String: `"pearson"` ou `"spearman"`.

- digits:

  Inteiro.

## Value

Uma lista com `matriz` (matriz de correlacao), `p_valores` (matriz) e
`tidy` (tibble: `var1`, `var2`, `correlacao`, `p_valor`).

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
[`rnp_normalidade_multivariada()`](https://evandeilton.github.io/rnp/reference/rnp_normalidade_multivariada.md),
[`rnp_silhueta()`](https://evandeilton.github.io/rnp/reference/rnp_silhueta.md)

## Examples

``` r
rnp_matriz_correlacao(mtcars[, c("mpg", "hp", "wt", "disp")])
#> 
#> ── Matriz de correlacao ────────────────────────────────────────────────────────
#> Matriz: vetor de 16 valores
#> P valores: vetor de 16 valores
#> 
#> ── Tidy 
#> # A tibble: 16 × 4
#>    var1  var2  correlacao p_valor
#>    <chr> <chr>      <dbl>   <dbl>
#>  1 mpg   mpg        1           0
#>  2 hp    mpg       -0.776       0
#>  3 wt    mpg       -0.868       0
#>  4 disp  mpg       -0.848       0
#>  5 mpg   hp        -0.776       0
#>  6 hp    hp         1           0
#>  7 wt    hp         0.659       0
#>  8 disp  hp         0.791       0
#>  9 mpg   wt        -0.868       0
#> 10 hp    wt         0.659       0
#> 11 wt    wt         1           0
#> 12 disp  wt         0.888       0
#> 13 mpg   disp      -0.848       0
#> 14 hp    disp       0.791       0
#> 15 wt    disp       0.888       0
#> 16 disp  disp       1           0
```
