# Razao de chances (odds ratio) com IC

Calcula a razao de chances de uma tabela 2x2 e o IC pelo metodo de Woolf
(log). A tabela deve ter exposicao nas linhas e desfecho nas colunas,
com a primeira coluna sendo o evento.

## Usage

``` r
rnp_odds_ratio(tabela, conf = 0.95, digits = 4L)
```

## Arguments

- tabela:

  Matriz 2x2 de contagens.

- conf:

  Nivel de confianca.

- digits:

  Inteiro.

## Value

tibble com `odds_ratio`, `ic_inf`, `ic_sup`, `log_or`, `ep_log`.

## See also

Other categoricos:
[`rnp_kappa()`](https://evandeilton.github.io/rnp/reference/rnp_kappa.md),
[`rnp_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_risco_relativo.md),
[`rnp_teste_fisher()`](https://evandeilton.github.io/rnp/reference/rnp_teste_fisher.md)

## Examples

``` r
rnp_odds_ratio(matrix(c(20, 10, 15, 25), 2, 2, byrow = TRUE))
#> # A tibble: 1 × 5
#>   odds_ratio ic_inf ic_sup log_or ep_log
#>        <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1       3.33   1.23   9.00   1.20  0.507
```
