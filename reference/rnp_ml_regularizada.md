# Especificacao de modelo linear regularizado (glmnet)

Lasso/ridge/elastic net via glmnet (`mistura = 1` lasso; `0` ridge).

## Usage

``` r
rnp_ml_regularizada(
  modo = c("classificacao", "regressao"),
  penalidade = 0.1,
  mistura = 1
)
```

## Arguments

- modo:

  `"classificacao"` ou `"regressao"`.

- penalidade:

  Forca da penalizacao (lambda). Pode ser `tune()`.

- mistura:

  Mistura L1/L2 (alpha) em \[0, 1\].

## Value

Especificacao `model_spec` (engine glmnet).

## See also

Other ml:
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md),
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
[`rnp_ml_svm()`](https://evandeilton.github.io/rnp/reference/rnp_ml_svm.md),
[`rnp_ml_tunagem()`](https://evandeilton.github.io/rnp/reference/rnp_ml_tunagem.md)

## Examples

``` r
 rnp_ml_regularizada("regressao", penalidade = 0.1) 
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 0.1
#>   mixture = 1
#> 
#> Computational engine: glmnet 
#> 
```
