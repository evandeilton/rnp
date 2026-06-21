# Teste exato de Fisher

Wrapper de
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) para
tabelas de contingencia.

## Usage

``` r
rnp_teste_fisher(tabela, digits = 4L)
```

## Arguments

- tabela:

  Tabela/matriz de contingencia (2x2 ou r x c).

- digits:

  Inteiro.

## Value

tibble com `p_valor` e, no caso 2x2, `odds_ratio`, `ic_inf`, `ic_sup`.

## See also

Other categoricos:
[`rnp_kappa()`](https://evandeilton.github.io/rnp/reference/rnp_kappa.md),
[`rnp_odds_ratio()`](https://evandeilton.github.io/rnp/reference/rnp_odds_ratio.md),
[`rnp_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_risco_relativo.md)

## Examples

``` r
rnp_teste_fisher(matrix(c(8, 2, 1, 5), 2, 2))
#> # A tibble: 1 × 4
#>   p_valor odds_ratio ic_inf ic_sup
#>     <dbl>      <dbl>  <dbl>  <dbl>
#> 1   0.035       15.5   1.01  1050.
```
