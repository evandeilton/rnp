# Especificacao de floresta aleatoria

Especificacao de floresta aleatoria

## Usage

``` r
rnp_ml_floresta(
  modo = c("classificacao", "regressao"),
  arvores = 500,
  mtry = NULL,
  min_n = 5
)
```

## Arguments

- modo:

  `"classificacao"` ou `"regressao"`.

- arvores:

  Numero de arvores.

- mtry:

  Variaveis sorteadas por divisao. `NULL` usa o padrao.

- min_n:

  Minimo de observacoes por no.

## Value

Especificacao `model_spec` (engine ranger).

## See also

Other ml:
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md),
[`rnp_ml_arvore()`](https://evandeilton.github.io/rnp/reference/rnp_ml_arvore.md),
[`rnp_ml_boosting()`](https://evandeilton.github.io/rnp/reference/rnp_ml_boosting.md),
[`rnp_ml_comparar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_comparar.md),
[`rnp_ml_cv()`](https://evandeilton.github.io/rnp/reference/rnp_ml_cv.md),
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
 rnp_ml_floresta("classificacao") 
#> Random Forest Model Specification (classification)
#> 
#> Main Arguments:
#>   trees = 500
#>   min_n = 5
#> 
#> Engine-Specific Arguments:
#>   importance = impurity
#> 
#> Computational engine: ranger 
#> 
```
