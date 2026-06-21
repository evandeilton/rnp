# Teorema Central do Limite (simulacao)

Gera `n_amostras` amostras de tamanho `n` a partir de um gerador
qualquer e exibe o histograma das medias padronizadas comparado a
Normal(0, 1), ilustrando o TCL.

## Usage

``` r
rnp_tcl_simulacao(
  gerador = function(n) rexp(n),
  n = 30L,
  n_amostras = 1000L,
  seed = 42L
)
```

## Arguments

- gerador:

  Funcao que recebe `n` e devolve `n` valores. Default: distribuicao
  exponencial (assimetrica), para evidenciar o efeito.

- n:

  Inteiro. Tamanho de cada amostra.

- n_amostras:

  Inteiro. Numero de amostras (medias).

- seed:

  Inteiro. Semente.

## Value

Objeto `ggplot`.

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
[`rnp_simula_aceitacao_rejeicao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_aceitacao_rejeicao.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md)

## Examples

``` r
rnp_tcl_simulacao(function(n) rexp(n), n = 30, n_amostras = 1000)
```
