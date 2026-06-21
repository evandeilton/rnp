# Processo de Poisson homogeneo (simulacao)

Simula os tempos de chegada de um processo de Poisson de taxa `taxa` no
intervalo `\[0, t_max\]`, via tempos entre chegadas exponenciais.

## Usage

``` r
rnp_processo_poisson(taxa = 1, t_max = 10, seed = 42L)
```

## Arguments

- taxa:

  Taxa media de ocorrencias por unidade de tempo (positivo).

- t_max:

  Horizonte de tempo.

- seed:

  Inteiro. Semente.

## Value

tibble com `evento`, `tempo_chegada` e `intervalo`.

## See also

Other probabilidade:
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
[`rnp_distribuicao_conjunta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_conjunta.md),
[`rnp_esperanca_condicional()`](https://evandeilton.github.io/rnp/reference/rnp_esperanca_condicional.md),
[`rnp_lei_grandes_numeros()`](https://evandeilton.github.io/rnp/reference/rnp_lei_grandes_numeros.md),
[`rnp_monte_carlo()`](https://evandeilton.github.io/rnp/reference/rnp_monte_carlo.md),
[`rnp_passeio_aleatorio()`](https://evandeilton.github.io/rnp/reference/rnp_passeio_aleatorio.md),
[`rnp_simula_aceitacao_rejeicao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_aceitacao_rejeicao.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md),
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md)

## Examples

``` r
rnp_processo_poisson(taxa = 2, t_max = 5)
#> # A tibble: 14 × 3
#>    evento tempo_chegada intervalo
#>     <int>         <dbl>     <dbl>
#>  1      1        0.0992    0.0992
#>  2      2        0.430     0.330 
#>  3      3        0.571     0.142 
#>  4      4        0.590     0.0191
#>  5      5        0.827     0.237 
#>  6      6        1.56      0.732 
#>  7      7        1.72      0.157 
#>  8      8        1.92      0.205 
#>  9      9        2.52      0.596 
#> 10     10        2.87      0.357 
#> 11     11        3.55      0.672 
#> 12     12        4.75      1.20  
#> 13     13        4.80      0.0481
#> 14     14        4.83      0.0286
```
