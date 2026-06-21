# Kappa de Cohen (concordancia entre dois avaliadores)

Mede a concordancia entre dois avaliadores categoricos, ajustada pelo
acaso.

## Usage

``` r
rnp_kappa(avaliador1, avaliador2, ponderado = FALSE, digits = 4L)
```

## Arguments

- avaliador1, avaliador2:

  Vetores de classificacoes (mesmos niveis).

- ponderado:

  Logico. Kappa ponderado (quadratico) para escalas ordinais.

- digits:

  Inteiro.

## Value

tibble com `kappa`, `concordancia_observada`, `concordancia_esperada`.

## See also

Other categoricos:
[`rnp_odds_ratio()`](https://evandeilton.github.io/rnp/reference/rnp_odds_ratio.md),
[`rnp_risco_relativo()`](https://evandeilton.github.io/rnp/reference/rnp_risco_relativo.md),
[`rnp_teste_fisher()`](https://evandeilton.github.io/rnp/reference/rnp_teste_fisher.md)

## Examples

``` r
a1 <- c("a", "b", "a", "c", "b", "a")
a2 <- c("a", "b", "a", "c", "c", "a")
rnp_kappa(a1, a2)
#> # A tibble: 1 × 3
#>   kappa concordancia_observada concordancia_esperada
#>   <dbl>                  <dbl>                 <dbl>
#> 1 0.739                  0.833                 0.361
```
