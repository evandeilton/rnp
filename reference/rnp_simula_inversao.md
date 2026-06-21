# Simulacao pelo metodo da transformacao inversa

Gera amostras de uma distribuicao a partir de sua funcao quantil
(inversa da acumulada), aplicada a uniformes(0, 1).

## Usage

``` r
rnp_simula_inversao(f_inv, n = 1000L, seed = 42L)
```

## Arguments

- f_inv:

  Funcao quantil (inversa da F), vetorizada.

- n:

  Inteiro. Tamanho da amostra.

- seed:

  Inteiro. Semente.

## Value

Vetor numerico simulado.

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
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md)

## Examples

``` r
# Exponencial(taxa = 2): F^{-1}(u) = -log(1 - u) / 2
x <- rnp_simula_inversao(function(u) -log(1 - u) / 2, n = 1000)
```
