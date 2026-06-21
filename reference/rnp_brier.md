# Escore de Brier

Escore de Brier

## Usage

``` r
rnp_brier(observado, prob, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Vetor binario.

- prob:

  Probabilidades preditas.

- positivo:

  Classe positiva.

- digits:

  Inteiro.

## Value

tibble com `brier` e `brier_referencia` (modelo que prediz a
prevalencia).

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_comparar_roc()`](https://evandeilton.github.io/rnp/reference/rnp_comparar_roc.md),
[`rnp_curva_ganho()`](https://evandeilton.github.io/rnp/reference/rnp_curva_ganho.md),
[`rnp_curva_lift()`](https://evandeilton.github.io/rnp/reference/rnp_curva_lift.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_ks_classificador()`](https://evandeilton.github.io/rnp/reference/rnp_ks_classificador.md),
[`rnp_metricas_classificacao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_classificacao.md),
[`rnp_metricas_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_regressao.md)

## Examples

``` r
set.seed(1); p <- runif(300); y <- rbinom(300, 1, p)
rnp_brier(y, p, positivo = 1)
#> # A tibble: 1 × 3
#>   brier brier_referencia escore_habilidade
#>   <dbl>            <dbl>             <dbl>
#> 1 0.174             0.25             0.304
```
