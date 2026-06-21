# ANOVA de medidas repetidas

ANOVA com um fator intra-sujeito, ajustada via modelo com estrato de
erro por sujeito ([`stats::aov()`](https://rdrr.io/r/stats/aov.html) com
`Error()`).

## Usage

``` r
rnp_anova_medidas_repetidas(formula, data, sujeito, digits = 4L)
```

## Arguments

- formula:

  Formula `resposta ~ fator`.

- data:

  data.frame em formato longo.

- sujeito:

  String com o nome da coluna identificadora do sujeito.

- digits:

  Inteiro.

## Value

tibble com a tabela ANOVA do estrato intra-sujeito.

## See also

Other experimental:
[`rnp_ancova()`](https://evandeilton.github.io/rnp/reference/rnp_ancova.md),
[`rnp_contrastes()`](https://evandeilton.github.io/rnp/reference/rnp_contrastes.md),
[`rnp_dbc()`](https://evandeilton.github.io/rnp/reference/rnp_dbc.md)

## Examples

``` r
df <- data.frame(
  resp = c(10, 12, 14, 9, 11, 13, 8, 10, 12),
  cond = factor(rep(c("A", "B", "C"), 3)),
  suj  = factor(rep(1:3, each = 3)))
rnp_anova_medidas_repetidas(resp ~ cond, df, sujeito = "suj")
#> # A tibble: 2 × 6
#>   fonte        gl    sq    qm estatistica_f p_valor
#>   <chr>     <dbl> <dbl> <dbl>         <dbl>   <dbl>
#> 1 cond          2    24    12       5.39e30       0
#> 2 Residuals     4     0     0      NA            NA
```
