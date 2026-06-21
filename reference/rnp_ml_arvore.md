# Especificacao de arvore de decisao

Especificacao de arvore de decisao

## Usage

``` r
rnp_ml_arvore(
  modo = c("classificacao", "regressao"),
  custo_complexidade = 0.01,
  profundidade_max = 30,
  min_n = 2
)
```

## Arguments

- modo:

  `"classificacao"` ou `"regressao"`.

- custo_complexidade:

  Parametro de poda (cp). Pode ser `tune()`.

- profundidade_max:

  Profundidade maxima.

- min_n:

  Minimo de observacoes por no.

## Value

Especificacao `model_spec` (engine rpart).

## See also

Other ml:
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md),
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
 rnp_ml_arvore("classificacao") 
#> Decision Tree Model Specification (classification)
#> 
#> Main Arguments:
#>   cost_complexity = 0.01
#>   tree_depth = 30
#>   min_n = 2
#> 
#> Computational engine: rpart 
#> 
```
