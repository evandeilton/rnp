# Comparacao de duas curvas ROC (teste de DeLong)

Compara as areas sob duas curvas ROC obtidas nos mesmos individuos
(modelos correlacionados), pelo teste de DeLong.

## Usage

``` r
rnp_comparar_roc(observado, escore1, escore2, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Vetor binario.

- escore1, escore2:

  Escores dos dois modelos.

- positivo:

  Classe positiva.

- digits:

  Inteiro.

## Value

tibble com `auc1`, `auc2`, `diferenca`, `z`, `p_valor`.

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_curva_ganho()`](https://evandeilton.github.io/rnp/reference/rnp_curva_ganho.md),
[`rnp_curva_lift()`](https://evandeilton.github.io/rnp/reference/rnp_curva_lift.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_ks_classificador()`](https://evandeilton.github.io/rnp/reference/rnp_ks_classificador.md),
[`rnp_metricas_classificacao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_classificacao.md),
[`rnp_metricas_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_regressao.md)

## Examples

``` r
set.seed(1); y <- rbinom(300, 1, 0.4)
s1 <- y * 0.6 + runif(300); s2 <- y * 0.2 + runif(300)
rnp_comparar_roc(y, s1, s2, positivo = 1)
#> # A tibble: 1 × 5
#>    auc1  auc2 diferenca     z p_valor
#>   <dbl> <dbl>     <dbl> <dbl>   <dbl>
#> 1 0.914 0.689     0.225  6.46       0
```
