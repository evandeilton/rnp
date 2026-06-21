# Metricas de classificacao

Calcula metricas a partir de classes preditas. Para problemas binarios,
reporta acuracia, precisao, revocacao (sensibilidade), especificidade,
F1/F-beta, coeficiente de correlacao de Matthews (MCC) e acuracia
balanceada. Para multiclasse, usa media macro.

## Usage

``` r
rnp_metricas_classificacao(
  observado,
  predito,
  positivo = NULL,
  beta = 1,
  digits = 4L
)
```

## Arguments

- observado:

  Vetor de classes observadas.

- predito:

  Vetor de classes preditas.

- positivo:

  Classe positiva (binario). Default: primeiro nivel.

- beta:

  Peso da revocacao no F-beta.

- digits:

  Inteiro.

## Value

tibble com `metrica` e `valor`.

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_comparar_roc()`](https://evandeilton.github.io/rnp/reference/rnp_comparar_roc.md),
[`rnp_curva_ganho()`](https://evandeilton.github.io/rnp/reference/rnp_curva_ganho.md),
[`rnp_curva_lift()`](https://evandeilton.github.io/rnp/reference/rnp_curva_lift.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_ks_classificador()`](https://evandeilton.github.io/rnp/reference/rnp_ks_classificador.md),
[`rnp_metricas_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_regressao.md)

## Examples

``` r
obs <- factor(c("sim","sim","nao","nao","sim","nao"))
pred <- factor(c("sim","nao","nao","nao","sim","sim"))
rnp_metricas_classificacao(obs, pred, positivo = "sim")
#> # A tibble: 8 × 2
#>   metrica             valor
#>   <chr>               <dbl>
#> 1 acuracia            0.667
#> 2 precisao            0.667
#> 3 revocacao           0.667
#> 4 especificidade      0.667
#> 5 f1                  0.667
#> 6 f_beta              0.667
#> 7 mcc                 0.333
#> 8 acuracia_balanceada 0.667
```
