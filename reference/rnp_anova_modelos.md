# Comparacao de modelos aninhados (ANOVA)

Compara dois ou mais modelos `lm`/`glm` aninhados via
[`stats::anova()`](https://rdrr.io/r/stats/anova.html).

## Usage

``` r
rnp_anova_modelos(..., digits = 4L)
```

## Arguments

- ...:

  Modelos `lm`/`glm` na ordem crescente de complexidade.

- digits:

  Inteiro.

## Value

tibble com a tabela de comparacao.

## See also

Other regressao:
[`rnp_box_cox()`](https://evandeilton.github.io/rnp/reference/rnp_box_cox.md),
[`rnp_elastic_net()`](https://evandeilton.github.io/rnp/reference/rnp_elastic_net.md),
[`rnp_grafico_residuos()`](https://evandeilton.github.io/rnp/reference/rnp_grafico_residuos.md),
[`rnp_predicao()`](https://evandeilton.github.io/rnp/reference/rnp_predicao.md),
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
m1 <- lm(mpg ~ wt, mtcars); m2 <- lm(mpg ~ wt + hp, mtcars)
rnp_anova_modelos(m1, m2)
#> # A tibble: 2 × 6
#>   Res.Df   RSS    Df Sum.of.Sq     F  Pr..F.
#>    <dbl> <dbl> <dbl>     <dbl> <dbl>   <dbl>
#> 1     30  278.    NA      NA    NA   NA     
#> 2     29  195.     1      83.3  12.4  0.0015
```
