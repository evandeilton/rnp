# Predicao com intervalo

Predicao de um `lm` com intervalo de confianca (media) ou de predicao
(nova observacao).

## Usage

``` r
rnp_predicao(
  modelo,
  novos_dados,
  tipo = c("confianca", "predicao"),
  conf = 0.95,
  digits = 4L
)
```

## Arguments

- modelo:

  Objeto `lm`.

- novos_dados:

  data.frame com os preditores.

- tipo:

  String: `"confianca"` ou `"predicao"`.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

tibble com `ajuste`, `limite_inferior`, `limite_superior`.

## See also

Other regressao:
[`rnp_anova_modelos()`](https://evandeilton.github.io/rnp/reference/rnp_anova_modelos.md),
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_elastic_net()`](https://evandeilton.github.io/rnp/reference/rnp_elastic_net.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_regressao_lasso()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_lasso.md),
[`rnp_regressao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_multinomial.md),
[`rnp_regressao_nao_linear()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_nao_linear.md),
[`rnp_regressao_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_poisson.md),
[`rnp_regressao_polinomial()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_polinomial.md),
[`rnp_regressao_ponderada()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ponderada.md),
[`rnp_regressao_ridge()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_ridge.md),
[`rnp_regressao_robusta()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_robusta.md),
[`rnp_regressao_stepwise()`](https://evandeilton.github.io/rnp/reference/rnp_regressao_stepwise.md),
[`rnp_vif()`](https://evandeilton.github.io/rnp/reference/rnp_vif.md)

## Examples

``` r
fit <- lm(mpg ~ wt, mtcars)
rnp_predicao(fit, data.frame(wt = c(2, 3, 4)))
#> # A tibble: 3 × 3
#>   ajuste limite_inferior limite_superior
#>    <dbl>           <dbl>           <dbl>
#> 1   26.6            24.8            28.4
#> 2   21.3            20.1            22.4
#> 3   15.9            14.5            17.3
```
