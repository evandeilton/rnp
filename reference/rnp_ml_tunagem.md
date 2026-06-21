# Tunagem de hiperparametros

Busca em grade os melhores hiperparametros por validacao cruzada
([`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)).
A `spec` deve ter parametros marcados com
[`hardhat::tune()`](https://hardhat.tidymodels.org/reference/tune.html).

## Usage

``` r
rnp_ml_tunagem(
  spec,
  formula,
  reamostras,
  grade = 10,
  metrica = NULL,
  digits = 4L
)
```

## Arguments

- spec:

  Especificacao com parametros a tunar.

- formula:

  Formula.

- reamostras:

  Reamostras de
  [`rnp_ml_cv()`](https://evandeilton.github.io/rnp/reference/rnp_ml_cv.md).

- grade:

  Tamanho da grade (inteiro) ou `data.frame` de candidatos.

- metrica:

  Metrica para ordenar (`NULL` usa a primeira disponivel).

- digits:

  Inteiro.

## Value

Uma lista com `melhores` (tibble), `melhor_param` e `resultado`.

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
[`rnp_ml_regularizada()`](https://evandeilton.github.io/rnp/reference/rnp_ml_regularizada.md),
[`rnp_ml_svm()`](https://evandeilton.github.io/rnp/reference/rnp_ml_svm.md)

## Examples

``` r
# \donttest{
sp <- rnp_ml_arvore("classificacao", custo_complexidade = hardhat::tune())
rnp_ml_tunagem(sp, Species ~ ., rnp_ml_cv(iris, v = 3), grade = 5)$melhores
#> # A tibble: 5 × 7
#>   cost_complexity .metric  .estimator  mean     n std_err .config        
#>             <dbl> <chr>    <chr>      <dbl> <dbl>   <dbl> <chr>          
#> 1          0      accuracy multiclass 0.927     3  0.024  pre0_mod1_post0
#> 2          0      accuracy multiclass 0.927     3  0.024  pre0_mod2_post0
#> 3          0      accuracy multiclass 0.927     3  0.024  pre0_mod3_post0
#> 4          0.0005 accuracy multiclass 0.927     3  0.024  pre0_mod4_post0
#> 5          0.0978 accuracy multiclass 0.92      3  0.0115 pre0_mod5_post0
# }
```
