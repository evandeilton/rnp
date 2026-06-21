# Modelo linear de efeitos mistos

Ajusta um modelo misto (efeitos fixos + aleatorios) via
[`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html).

## Usage

``` r
rnp_modelo_misto(fixos, data, aleatorio, digits = 4L)
```

## Arguments

- fixos:

  Formula dos efeitos fixos (ex.: `distance ~ age`).

- data:

  data.frame.

- aleatorio:

  Formula dos efeitos aleatorios (ex.: `~ 1 | Subject`).

- digits:

  Inteiro.

## Value

Uma lista com `efeitos_fixos` (tibble), `variancia` (componentes de
variancia + ICC) e `objeto`.

## See also

Other glm:
[`rnp_binomial_negativa()`](https://evandeilton.github.io/rnp/reference/rnp_binomial_negativa.md),
[`rnp_gam()`](https://evandeilton.github.io/rnp/reference/rnp_gam.md),
[`rnp_glm()`](https://evandeilton.github.io/rnp/reference/rnp_glm.md),
[`rnp_glm_diagnosticos()`](https://evandeilton.github.io/rnp/reference/rnp_glm_diagnosticos.md),
[`rnp_grafico_efeitos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_efeitos.md),
[`rnp_regressao_ordinal()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ordinal.md)

## Examples

``` r
rnp_modelo_misto(distance ~ age, nlme::Orthodont,
                 aleatorio = ~ 1 | Subject)$efeitos_fixos
#> # A tibble: 2 × 5
#>   termo       estimativa erro_padrao    gl p_valor
#>   <chr>            <dbl>       <dbl> <dbl>   <dbl>
#> 1 (Intercept)     16.8        0.802     80       0
#> 2 age              0.660      0.0616    80       0
```
