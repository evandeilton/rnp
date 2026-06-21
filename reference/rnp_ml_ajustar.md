# Ajusta e avalia um modelo na particao

Treina o modelo no conjunto de treino e avalia no teste
([`tune::last_fit()`](https://tune.tidymodels.org/reference/last_fit.html)).

## Usage

``` r
rnp_ml_ajustar(spec, formula, split, digits = 4L)
```

## Arguments

- spec:

  Especificacao de modelo (`rnp_ml_*`) ou `recipe`/`workflow`.

- formula:

  Formula do modelo.

- split:

  Particao de
  [`rnp_ml_particao()`](https://evandeilton.github.io/rnp/reference/rnp_ml_particao.md).

- digits:

  Inteiro.

## Value

Uma lista com `metricas` (no teste), `predicoes`, `modelo` (workflow
ajustado) e `resultado`.

## See also

Other ml:
[`rnp_ml_arvore()`](https://evandeilton.github.io/rnp/reference/rnp_ml_arvore.md),
[`rnp_ml_boosting()`](https://evandeilton.github.io/rnp/reference/rnp_ml_boosting.md),
[`rnp_ml_comparar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_comparar.md),
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
sp <- rnp_ml_particao(iris, estrato = "Species")
rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)$metricas
#> # A tibble: 3 × 4
#>   .metric     .estimator .estimate .config        
#>   <chr>       <chr>          <dbl> <chr>          
#> 1 accuracy    multiclass    0.923  pre0_mod0_post0
#> 2 roc_auc     hand_till     0.942  pre0_mod0_post0
#> 3 brier_class multiclass    0.0771 pre0_mod0_post0
# }
```
