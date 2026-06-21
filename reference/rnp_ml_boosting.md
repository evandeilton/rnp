# Especificacao de gradient boosting

Especificacao de gradient boosting

## Usage

``` r
rnp_ml_boosting(
  modo = c("classificacao", "regressao"),
  arvores = 500,
  profundidade = 6,
  taxa_aprendizado = 0.1
)
```

## Arguments

- modo:

  `"classificacao"` ou `"regressao"`.

- arvores:

  Numero de arvores.

- profundidade:

  Profundidade das arvores.

- taxa_aprendizado:

  Taxa de aprendizado.

## Value

Especificacao `model_spec` (engine xgboost).

## See also

Other ml:
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md),
[`rnp_ml_arvore()`](https://evandeilton.github.io/rnp/reference/rnp_ml_arvore.md),
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
 rnp_ml_boosting("regressao") 
#> Boosted Tree Model Specification (regression)
#> 
#> Main Arguments:
#>   trees = 500
#>   tree_depth = 6
#>   learn_rate = 0.1
#> 
#> Computational engine: xgboost 
#> 
```
