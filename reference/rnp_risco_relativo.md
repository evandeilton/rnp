# Risco relativo com IC

Calcula o risco relativo de uma tabela 2x2 (exposicao nas linhas;
primeira coluna = evento) e o IC pela aproximacao log.

## Usage

``` r
rnp_risco_relativo(tabela, conf = 0.95, digits = 4L)
```

## Arguments

- tabela:

  Matriz 2x2 de contagens.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

tibble com `risco_relativo`, `ic_inf`, `ic_sup`, `risco_expostos`,
`risco_nao_expostos`.

## See also

Other categoricos:
[`rnp_kappa()`](https://evandeilton.github.io/rnp/reference/rnp_kappa.md),
[`rnp_odds_ratio()`](https://evandeilton.github.io/rnp/reference/rnp_odds_ratio.md),
[`rnp_teste_fisher()`](https://evandeilton.github.io/rnp/reference/rnp_teste_fisher.md)

## Examples

``` r
rnp_risco_relativo(matrix(c(20, 80, 10, 90), 2, 2, byrow = TRUE))
#> # A tibble: 1 × 5
#>   risco_relativo ic_inf ic_sup risco_expostos risco_nao_expostos
#>            <dbl>  <dbl>  <dbl>          <dbl>              <dbl>
#> 1              2  0.987   4.05            0.2                0.1
```
