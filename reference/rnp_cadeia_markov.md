# Cadeia de Markov de tempo discreto

A partir de uma matriz de transicao, calcula a distribuicao apos `n`
passos, a matriz de transicao em `n` passos (P^n) e a distribuicao
estacionaria. Usa backend C++ (RcppArmadillo).

## Usage

``` r
rnp_cadeia_markov(P, estado_inicial = 1L, n = 10L, digits = 4L)
```

## Arguments

- P:

  Matriz de transicao (linhas somam 1).

- estado_inicial:

  Vetor de distribuicao inicial (soma 1) ou indice inteiro do estado de
  partida.

- n:

  Inteiro. Numero de passos.

- digits:

  Inteiro. Casas decimais.

## Value

Uma lista com:

- `distribuicao_n`: tibble com a distribuicao apos `n` passos.

- `estacionaria`: tibble com a distribuicao estacionaria.

- `matriz_n`: matriz P^n.

## See also

Other probabilidade:
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_distribuicao_conjunta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_conjunta.md),
[`rnp_esperanca_condicional()`](https://evandeilton.github.io/rnp/reference/rnp_esperanca_condicional.md),
[`rnp_lei_grandes_numeros()`](https://evandeilton.github.io/rnp/reference/rnp_lei_grandes_numeros.md),
[`rnp_monte_carlo()`](https://evandeilton.github.io/rnp/reference/rnp_monte_carlo.md),
[`rnp_passeio_aleatorio()`](https://evandeilton.github.io/rnp/reference/rnp_passeio_aleatorio.md),
[`rnp_processo_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_processo_poisson.md),
[`rnp_simula_aceitacao_rejeicao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_aceitacao_rejeicao.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md),
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md)

## Examples

``` r
P <- matrix(c(0.9, 0.1, 0.5, 0.5), 2, 2, byrow = TRUE)
rnp_cadeia_markov(P, estado_inicial = 1, n = 10)
#> 
#> ── Cadeia de Markov ────────────────────────────────────────────────────────────
#> 
#> ── Distribuicao n 
#> # A tibble: 2 × 2
#>   estado probabilidade
#>   <chr>          <dbl>
#> 1 E1             0.833
#> 2 E2             0.167
#> 
#> ── Estacionaria 
#> # A tibble: 2 × 2
#>   estado probabilidade
#>   <chr>          <dbl>
#> 1 E1             0.833
#> 2 E2             0.167
#> Matriz n: 0.8334, 0.8332, 0.1666, and 0.1668
```
