# Curva de lift

Curva de lift

## Usage

``` r
rnp_curva_lift(observado, escore, positivo = NULL, n_grupos = 10, digits = 4L)
```

## Arguments

- observado:

  Vetor binario (ou fator de 2 niveis).

- escore:

  Probabilidades/escores preditos.

- positivo:

  Classe positiva.

- n_grupos:

  Numero de faixas (deciles por padrao).

- digits:

  Inteiro.

## Value

Uma lista com `tabela` (tibble) e `grafico` (`ggplot`).

## See also

Other avaliacao:
[`rnp_acuracia_diagnostica()`](https://evandeilton.github.io/rnp/reference/rnp_acuracia_diagnostica.md),
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
[`rnp_calibracao()`](https://evandeilton.github.io/rnp/reference/rnp_calibracao.md),
[`rnp_comparar_roc()`](https://evandeilton.github.io/rnp/reference/rnp_comparar_roc.md),
[`rnp_curva_ganho()`](https://evandeilton.github.io/rnp/reference/rnp_curva_ganho.md),
[`rnp_curva_precisao_revocacao()`](https://evandeilton.github.io/rnp/reference/rnp_curva_precisao_revocacao.md),
[`rnp_ks_classificador()`](https://evandeilton.github.io/rnp/reference/rnp_ks_classificador.md),
[`rnp_metricas_classificacao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_classificacao.md),
[`rnp_metricas_regressao()`](https://evandeilton.github.io/rnp/reference/rnp_metricas_regressao.md)

## Examples

``` r
set.seed(1); y <- rbinom(200, 1, 0.3); s <- y * 0.5 + runif(200)
rnp_curva_lift(y, s, positivo = 1)$tabela
#> # A tibble: 10 × 3
#>    grupo taxa_grupo  lift
#>    <int>      <dbl> <dbl>
#>  1     1       1    3.23 
#>  2     2       0.5  1.61 
#>  3     3       0.3  0.968
#>  4     4       0.45 1.45 
#>  5     5       0.5  1.61 
#>  6     6       0.35 1.13 
#>  7     7       0    0    
#>  8     8       0    0    
#>  9     9       0    0    
#> 10    10       0    0    
```
