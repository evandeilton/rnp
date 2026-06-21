# Lei dos Grandes Numeros (simulacao)

Simula a convergencia da media amostral para a media teorica conforme o
tamanho da amostra cresce, ilustrando a Lei dos Grandes Numeros.

## Usage

``` r
rnp_lei_grandes_numeros(
  gerador = function(n) sample(1:6, n, TRUE),
  n_max = 1000L,
  media_teorica = NULL,
  seed = 42L
)
```

## Arguments

- gerador:

  Funcao de geracao que recebe `n` e devolve `n` valores (ex.:
  `function(n) rnorm(n)`). Default: lancamentos de um dado honesto.

- n_max:

  Inteiro. Numero maximo de observacoes.

- media_teorica:

  Valor de referencia (linha horizontal). Se `NULL`, tenta estimar por
  uma amostra grande.

- seed:

  Inteiro. Semente.

## Value

Objeto `ggplot` com a media acumulada versus n.

## See also

Other probabilidade:
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
[`rnp_distribuicao_conjunta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_conjunta.md),
[`rnp_esperanca_condicional()`](https://evandeilton.github.io/rnp/reference/rnp_esperanca_condicional.md),
[`rnp_monte_carlo()`](https://evandeilton.github.io/rnp/reference/rnp_monte_carlo.md),
[`rnp_passeio_aleatorio()`](https://evandeilton.github.io/rnp/reference/rnp_passeio_aleatorio.md),
[`rnp_processo_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_processo_poisson.md),
[`rnp_simula_aceitacao_rejeicao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_aceitacao_rejeicao.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md),
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md)

## Examples

``` r
rnp_lei_grandes_numeros(function(n) rbinom(n, 1, 0.3), media_teorica = 0.3)
```
