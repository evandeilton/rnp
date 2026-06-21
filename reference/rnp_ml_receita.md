# Receita de pre-processamento

Constroi uma `recipe` com passos comuns, indicados por nomes em
portugues.

## Usage

``` r
rnp_ml_receita(formula, data, passos = NULL)
```

## Arguments

- formula:

  Formula `y ~ .`.

- data:

  data.frame.

- passos:

  Vetor de passos: `"normalizar"`, `"dummy"`, `"imputar_media"`,
  `"imputar_moda"`, `"zero_var"`, `"correlacao"`, `"boxcox"`.

## Value

Objeto `recipe`.

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
[`rnp_ml_regularizada()`](https://evandeilton.github.io/rnp/reference/rnp_ml_regularizada.md),
[`rnp_ml_svm()`](https://evandeilton.github.io/rnp/reference/rnp_ml_svm.md),
[`rnp_ml_tunagem()`](https://evandeilton.github.io/rnp/reference/rnp_ml_tunagem.md)

## Examples

``` r
# \donttest{
rnp_ml_receita(Species ~ ., iris, passos = c("normalizar"))
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 4
#> 
#> ── Operations 
#> • Centering and scaling for: recipes::all_numeric_predictors()
# }
```
