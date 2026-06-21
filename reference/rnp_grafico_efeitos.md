# Grafico de efeitos parciais

Exibe a contribuicao parcial de cada termo (efeitos parciais via
`predict(type = "terms")`), util para GLM e GAM.

## Usage

``` r
rnp_grafico_efeitos(modelo)
```

## Arguments

- modelo:

  Objeto `glm`/`gam`/`lm` ou saida das funcoes `rnp_glm`/`rnp_gam`.

## Value

Objeto `ggplot` facetado por termo.

## See also

Other glm:
[`rnp_binomial_negativa()`](https://evandeilton.github.io/rnp/reference/rnp_binomial_negativa.md),
[`rnp_gam()`](https://evandeilton.github.io/rnp/reference/rnp_gam.md),
[`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md),
[`rnp_glm_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_glm_diagnosticos.md),
[`rnp_modelo_misto()`](https://evandeilton.github.io/rnp/reference/rnp_modelo_misto.md),
[`rnp_regressao_ordinal()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ordinal.md)

## Examples

``` r
set.seed(1)
d <- data.frame(x = runif(200)); d$y <- sin(2 * pi * d$x) + rnorm(200, 0, 0.3)
rnp_grafico_efeitos(rnp_gam(y ~ s(x), d))
```
