# Regressao binomial negativa

Para contagens com superdispersao (variancia maior que a media), via
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html).

## Usage

``` r
rnp_binomial_negativa(formula, data, digits = 4L)
```

## Arguments

- formula:

  Formula.

- data:

  data.frame.

- digits:

  Inteiro.

## Value

Uma lista com `coeficientes` (com razao de taxas `irr`), `theta`
(parametro de dispersao) e `objeto`.

## See also

Other glm:
[`rnp_gam()`](https://evandeilton.github.io/rnp/reference/rnp_gam.md),
[`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md),
[`rnp_glm_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_glm_diagnosticos.md),
[`rnp_grafico_efeitos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_efeitos.md),
[`rnp_modelo_misto()`](https://evandeilton.github.io/rnp/reference/rnp_modelo_misto.md),
[`rnp_regressao_ordinal()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ordinal.md)

## Examples

``` r
rnp_binomial_negativa(Days ~ Sex + Age, MASS::quine)$theta
#> [1] 1.1474
```
