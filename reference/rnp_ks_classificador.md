# Estatistica KS de classificador

Maxima distancia entre as distribuicoes acumuladas dos escores das
classes positiva e negativa.

## Usage

``` r
rnp_ks_classificador(observado, escore, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Vetor binario.

- escore:

  Escores/probabilidades.

- positivo:

  Classe positiva.

- digits:

  Inteiro.

## Value

Uma lista com `ks` (escalar) e `grafico` (`ggplot`).

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_comparar_roc()`](https://evandeilton.github.io/rnp/reference/rnp_comparar_roc.md),
[`rnp_curva_ganho()`](https://evandeilton.github.io/rnp/reference/rnp_curva_ganho.md),
[`rnp_curva_lift()`](https://evandeilton.github.io/rnp/reference/rnp_curva_lift.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_metricas_classificacao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_classificacao.md),
[`rnp_metricas_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_regressao.md)

## Examples

``` r
set.seed(1); y <- rbinom(300, 1, 0.3); s <- y * 0.4 + runif(300)
rnp_ks_classificador(y, s, positivo = 1)$ks
#> [1] 0.4234
```
