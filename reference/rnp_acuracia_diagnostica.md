# Acuracia diagnostica

Metricas de avaliacao de testes diagnosticos: sensibilidade,
especificidade, valores preditivos e razoes de verossimilhanca.

## Usage

``` r
rnp_acuracia_diagnostica(observado, predito, positivo = NULL, digits = 4L)
```

## Arguments

- observado:

  Condicao verdadeira (binaria).

- predito:

  Resultado do teste (binario).

- positivo:

  Nivel "doente"/positivo.

- digits:

  Inteiro.

## Value

tibble com `metrica` e `valor`.

## See also

Other avaliacao:
[`rnp_brier()`](https://evandeilton.github.io/rnp/reference/rnp_brier.md),
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
obs <- c("D","D","D","S","S","S","S","D")
tst <- c("+","+","-","-","-","+","-","+")
rnp_acuracia_diagnostica(obs, tst, positivo = "D")
#> # A tibble: 8 × 2
#>   metrica          valor
#>   <chr>            <dbl>
#> 1 sensibilidade    0.75 
#> 2 especificidade   0.75 
#> 3 vpp              0.75 
#> 4 vpn              0.75 
#> 5 razao_veross_pos 3    
#> 6 razao_veross_neg 0.333
#> 7 acuracia         0.75 
#> 8 prevalencia      0.5  
```
