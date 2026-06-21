# Particao treino/teste

Divide os dados em conjuntos de treino e teste
([`rsample::initial_split()`](https://rsample.tidymodels.org/reference/initial_split.html)).

## Usage

``` r
rnp_ml_particao(data, prop = 0.75, estrato = NULL, seed = 42L)
```

## Arguments

- data:

  data.frame.

- prop:

  Proporcao para treino.

- estrato:

  Nome (string) da variavel de estratificacao. Opcional.

- seed:

  Inteiro. Semente.

## Value

Um objeto de particao `rsplit` (use
[`rnp_ml_ajustar()`](https://evandeilton.github.io/rnp/reference/rnp_ml_ajustar.md)
ou
[`rsample::training()`](https://rsample.tidymodels.org/reference/initial_split.html)/`testing()`).

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
[`rnp_ml_prever()`](https://evandeilton.github.io/rnp/reference/rnp_ml_prever.md),
[`rnp_ml_receita()`](https://evandeilton.github.io/rnp/reference/rnp_ml_receita.md),
[`rnp_ml_regularizada()`](https://evandeilton.github.io/rnp/reference/rnp_ml_regularizada.md),
[`rnp_ml_svm()`](https://evandeilton.github.io/rnp/reference/rnp_ml_svm.md),
[`rnp_ml_tunagem()`](https://evandeilton.github.io/rnp/reference/rnp_ml_tunagem.md)

## Examples

``` r
# \donttest{
rnp_ml_particao(iris, prop = 0.75, estrato = "Species")
#> <Training/Testing/Total>
#> <111/39/150>
# }
```
