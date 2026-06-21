# Compara modelos por validacao cruzada

Avalia varias especificacoes por reamostragem e compara as metricas.

## Usage

``` r
rnp_ml_comparar(specs, formula, reamostras, digits = 4L)
```

## Arguments

- specs:

  Lista nomeada de especificacoes de modelo.

- formula:

  Formula.

- reamostras:

  Reamostras de
  [`rnp_ml_cv()`](https://evandeilton.github.io/rnp/reference/rnp_ml_cv.md).

- digits:

  Inteiro.

## Value

Uma lista com `tabela` (tibble) e `grafico` (`ggplot`).

## See also

Other ml:
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md),
[`rnp_ml_arvore()`](https://evandeilton.github.io/rnp/reference/rnp_ml_arvore.md),
[`rnp_ml_boosting()`](https://evandeilton.github.io/rnp/reference/rnp_ml_boosting.md),
[`rnp_ml_cv()`](https://evandeilton.github.io/rnp/reference/rnp_ml_cv.md),
[`rnp_ml_floresta()`](https://evandeilton.github.io/rnp/reference/rnp_ml_floresta.md),
[`rnp_ml_importancia()`](https://evandeilton.github.io/rnp/reference/rnp_ml_importancia.md),
[`rnp_ml_knn()`](https://evandeilton.github.io/rnp/reference/rnp_ml_knn.md),
[`rnp_ml_particao()`](https://evandeilton.github.io/rnp/reference/rnp_ml_particao.md),
[`rnp_ml_prever()`](https://evandeilton.github.io/rnp/reference/rnp_ml_prever.md),
[`rnp_ml_receita()`](https://evandeilton.github.io/rnp/reference/rnp_ml_receita.md),
[`rnp_ml_regularizada()`](https://evandeilton.github.io/rnp/reference/rnp_ml_regularizada.md),
[`rnp_ml_svm()`](https://evandeilton.github.io/rnp/reference/rnp_ml_svm.md),
[`rnp_ml_tunagem()`](https://evandeilton.github.io/rnp/reference/rnp_ml_tunagem.md)

## Examples

``` r
# \donttest{
specs <- list(arvore = rnp_ml_arvore("classificacao"))
rnp_ml_comparar(specs, Species ~ ., rnp_ml_cv(iris, v = 3))$tabela
#> # A tibble: 3 × 7
#>   .metric     .estimator   mean     n std_err .config         modelo
#>   <chr>       <chr>       <dbl> <dbl>   <dbl> <chr>           <chr> 
#> 1 accuracy    multiclass 0.94       3  0.0231 pre0_mod0_post0 arvore
#> 2 brier_class multiclass 0.0565     3  0.0212 pre0_mod0_post0 arvore
#> 3 roc_auc     hand_till  0.966      3  0.0132 pre0_mod0_post0 arvore
# }
```
