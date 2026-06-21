# Curva de ganho acumulado

Curva de ganho acumulado

## Usage

``` r
rnp_curva_ganho(observado, escore, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Vetor binario (ou fator de 2 niveis).

- escore:

  Probabilidades/escores preditos.

- positivo:

  Classe positiva.

- digits:

  Inteiro.

## Value

Uma lista com `tabela` e `grafico` (`ggplot`).

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_comparar_roc()`](https://evandeilton.github.io/rnp/reference/rnp_comparar_roc.md),
[`rnp_curva_lift()`](https://evandeilton.github.io/rnp/reference/rnp_curva_lift.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_ks_classificador()`](https://evandeilton.github.io/rnp/reference/rnp_ks_classificador.md),
[`rnp_metricas_classificacao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_classificacao.md),
[`rnp_metricas_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_regressao.md)

## Examples

``` r
set.seed(1); y <- rbinom(200, 1, 0.3); s <- y * 0.5 + runif(200)
rnp_curva_ganho(y, s, positivo = 1)$grafico
```
