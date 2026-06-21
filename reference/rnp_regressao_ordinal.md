# Regressao ordinal (odds proporcionais)

Modelo de odds proporcionais para respostas ordinais, via
[`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html).

## Usage

``` r
rnp_regressao_ordinal(formula, data, pesos = NULL, digits = 4L)
```

## Arguments

- formula:

  Formula com resposta `factor` ordenado.

- data:

  data.frame.

- pesos:

  Vetor opcional de pesos (frequencias).

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (com `odds_ratio`), `limiares`
(interceptos) e `objeto`.

## See also

Other glm:
[`rnp_binomial_negativa()`](https://evandeilton.github.io/rnp/reference/rnp_binomial_negativa.md),
[`rnp_gam()`](https://evandeilton.github.io/rnp/reference/rnp_gam.md),
[`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md),
[`rnp_glm_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_glm_diagnosticos.md),
[`rnp_grafico_efeitos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_efeitos.md),
[`rnp_modelo_misto()`](https://evandeilton.github.io/rnp/reference/rnp_modelo_misto.md)

## Examples

``` r
rnp_regressao_ordinal(Sat ~ Infl + Type, MASS::housing,
                      pesos = MASS::housing$Freq)$coeficientes
#> # A tibble: 5 × 5
#>   termo         estimativa erro_padrao p_valor odds_ratio
#>   <chr>              <dbl>       <dbl>   <dbl>      <dbl>
#> 1 InflMedium         0.548       0.104  0           1.73 
#> 2 InflHigh           1.24        0.126  0           3.45 
#> 3 TypeApartment     -0.522       0.118  0           0.594
#> 4 TypeAtrium        -0.289       0.153  0.0592      0.749
#> 5 TypeTerrace       -1.01        0.150  0           0.363
```
