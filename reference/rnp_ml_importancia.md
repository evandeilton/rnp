# Importancia de variaveis

Extrai a importancia das variaveis do modelo (engines rpart e ranger).

## Usage

``` r
rnp_ml_importancia(modelo, digits = 4L)
```

## Arguments

- modelo:

  Workflow ajustado ou parsnip fit.

- digits:

  Inteiro.

## Value

tibble com `variavel` e `importancia`, em ordem decrescente.

## See also

Other ml:
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md),
[`rnp_ml_arvore()`](https://evandeilton.github.io/rnp/reference/rnp_ml_arvore.md),
[`rnp_ml_boosting()`](https://evandeilton.github.io/rnp/reference/rnp_ml_boosting.md),
[`rnp_ml_comparar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_comparar.md),
[`rnp_ml_cv()`](https://evandeilton.github.io/rnp/reference/rnp_ml_cv.md),
[`rnp_ml_floresta()`](https://evandeilton.github.io/rnp/reference/rnp_ml_floresta.md),
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
fit <- rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)
rnp_ml_importancia(fit$modelo)
#> # A tibble: 4 × 2
#>   variavel     importancia
#>   <chr>              <dbl>
#> 1 Petal.Width         70.1
#> 2 Petal.Length        66.7
#> 3 Sepal.Length        43.2
#> 4 Sepal.Width         29.7
# }
```
