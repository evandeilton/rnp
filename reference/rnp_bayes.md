# Teorema de Bayes (particao do espaco amostral)

Calcula as probabilidades a posteriori P(H_i \| E) a partir das
probabilidades a priori P(H_i) e das verossimilhancas P(E \| H_i), para
uma particao de hipoteses mutuamente exclusivas e exaustivas. O caso
classico de dois eventos e obtido com vetores de comprimento 2.

## Usage

``` r
rnp_bayes(priori, verossimilhanca, hipoteses = NULL, digits = 4L)
```

## Arguments

- priori:

  Vetor de probabilidades a priori (deve somar 1).

- verossimilhanca:

  Vetor P(E \| H_i), mesmo comprimento de `priori`.

- hipoteses:

  Vetor opcional de rotulos das hipoteses.

- digits:

  Inteiro. Casas decimais.

## Value

tibble com `hipotese`, `priori`, `verossimilhanca`, `conjunta` (P(H_i e
E)) e `posteriori` (P(H_i \| E)).

## See also

Other probabilidade:
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
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
# Teste diagnostico: prevalencia 1%, sensibilidade 99%, especificidade 95%
rnp_bayes(priori = c(doente = 0.01, sadio = 0.99),
          verossimilhanca = c(0.99, 0.05))
#> # A tibble: 2 × 5
#>   hipotese priori verossimilhanca conjunta posteriori
#>   <chr>     <dbl>           <dbl>    <dbl>      <dbl>
#> 1 doente     0.01            0.99   0.0099      0.167
#> 2 sadio      0.99            0.05   0.0495      0.833
```
