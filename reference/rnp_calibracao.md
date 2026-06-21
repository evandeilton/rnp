# Calibracao de probabilidades

Compara probabilidades preditas com frequencias observadas (por faixa) e
aplica o teste de Hosmer-Lemeshow.

## Usage

``` r
rnp_calibracao(observado, prob, positivo = NULL, n_grupos = 10, digits = 4L)
```

## Arguments

- observado:

  Vetor binario.

- prob:

  Probabilidades preditas em \[0, 1\].

- positivo:

  Classe positiva.

- n_grupos:

  Numero de faixas.

- digits:

  Inteiro.

## Value

Uma lista com `tabela`, `hosmer_lemeshow` (tibble) e `grafico`.

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
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
rnp_calibracao(y, p, positivo = 1)$hosmer_lemeshow
#> # A tibble: 1 × 3
#>   estatistica    gl p_valor
#>         <dbl> <int>   <dbl>
#> 1        5.30     8   0.725
```
