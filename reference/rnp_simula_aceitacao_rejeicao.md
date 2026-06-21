# Simulacao pelo metodo de aceitacao-rejeicao

Gera amostras de uma densidade alvo `f` usando uma densidade proposta
`g` (da qual se sabe amostrar) e uma constante `M` tal que f(x) \<= M
g(x).

## Usage

``` r
rnp_simula_aceitacao_rejeicao(
  f,
  gerador,
  densidade_g,
  M,
  n = 1000L,
  seed = 42L
)
```

## Arguments

- f:

  Densidade alvo (funcao vetorizada).

- gerador:

  Funcao que recebe `n` e devolve `n` amostras de g.

- densidade_g:

  Densidade da proposta g (funcao vetorizada).

- M:

  Constante de cobertura (M \>= sup f/g).

- n:

  Inteiro. Numero de amostras desejadas.

- seed:

  Inteiro. Semente.

## Value

Uma lista com `amostra` (vetor) e `taxa_aceitacao` (escalar).

## See also

Other probabilidade:
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
[`rnp_distribuicao_conjunta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_conjunta.md),
[`rnp_esperanca_condicional()`](https://evandeilton.github.io/rnp/reference/rnp_esperanca_condicional.md),
[`rnp_lei_grandes_numeros()`](https://evandeilton.github.io/rnp/reference/rnp_lei_grandes_numeros.md),
[`rnp_monte_carlo()`](https://evandeilton.github.io/rnp/reference/rnp_monte_carlo.md),
[`rnp_passeio_aleatorio()`](https://evandeilton.github.io/rnp/reference/rnp_passeio_aleatorio.md),
[`rnp_processo_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_processo_poisson.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md),
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md)

## Examples

``` r
# Alvo: Beta(2,2) usando proposta Uniforme(0,1), M = 1.5
set.seed(1)
rnp_simula_aceitacao_rejeicao(
  f = function(x) dbeta(x, 2, 2),
  gerador = function(n) runif(n),
  densidade_g = function(x) dunif(x),
  M = 1.5, n = 500)$taxa_aceitacao
#> [1] 0.5
```
