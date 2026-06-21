# Diagnostico de GLM

Avalia ajuste e superdispersao de um GLM: residuos de deviance/Pearson,
dispersao estimada e teste de superdispersao.

## Usage

``` r
rnp_glm_diagnosticos(modelo, grafico = FALSE, digits = 4L)
```

## Arguments

- modelo:

  Objeto `glm` ou saida de
  [`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md).

- grafico:

  Logico. Retorna grafico de residuos.

- digits:

  Inteiro.

## Value

Uma lista com `testes` (tibble) e, se solicitado, `grafico` (`ggplot`).

## See also

Other glm:
[`rnp_binomial_negativa()`](https://evandeilton.github.io/rnp/reference/rnp_binomial_negativa.md),
[`rnp_gam()`](https://evandeilton.github.io/rnp/reference/rnp_gam.md),
[`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md),
[`rnp_grafico_efeitos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_efeitos.md),
[`rnp_modelo_misto()`](https://evandeilton.github.io/rnp/reference/rnp_modelo_misto.md),
[`rnp_regressao_ordinal()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ordinal.md)

## Examples

``` r
rnp_glm_diagnosticos(glm(carb ~ hp + wt, mtcars, family = poisson()))$testes
#> # A tibble: 3 × 4
#>   medida                   valor p_valor interpretacao  
#>   <chr>                    <dbl>   <dbl> <chr>          
#> 1 dispersao                0.460  NA     dispersao ok   
#> 2 deviance/gl              0.423  NA     NA             
#> 3 qui-quadrado de Pearson 13.3     0.994 ajuste adequado
```
