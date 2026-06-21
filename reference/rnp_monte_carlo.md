# Integracao por Monte Carlo

Estima a integral de `integrando` em `\[a, b\]` por amostragem uniforme,
com erro-padrao e intervalo de confianca.

## Usage

``` r
rnp_monte_carlo(
  integrando,
  limites = c(0, 1),
  n = 10000L,
  conf = 0.95,
  seed = 42L,
  digits = 4L
)
```

## Arguments

- integrando:

  Funcao vetorizada a integrar.

- limites:

  Vetor `c(a, b)` com os limites de integracao.

- n:

  Inteiro. Numero de pontos de Monte Carlo.

- conf:

  Nivel de confianca do IC.

- seed:

  Inteiro. Semente.

- digits:

  Inteiro. Casas decimais.

## Value

tibble com `estimativa`, `erro_padrao`, `ic_inf`, `ic_sup`, `n`.

## See also

Other probabilidade:
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
[`rnp_distribuicao_conjunta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_conjunta.md),
[`rnp_esperanca_condicional()`](https://evandeilton.github.io/rnp/reference/rnp_esperanca_condicional.md),
[`rnp_lei_grandes_numeros()`](https://evandeilton.github.io/rnp/reference/rnp_lei_grandes_numeros.md),
[`rnp_passeio_aleatorio()`](https://evandeilton.github.io/rnp/reference/rnp_passeio_aleatorio.md),
[`rnp_processo_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_processo_poisson.md),
[`rnp_simula_aceitacao_rejeicao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_aceitacao_rejeicao.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md),
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md)

## Examples

``` r
# integral de x^2 em [0, 1] = 1/3
rnp_monte_carlo(function(x) x^2, limites = c(0, 1), n = 1e5)
#> # A tibble: 1 × 5
#>   estimativa erro_padrao ic_inf ic_sup      n
#>        <dbl>       <dbl>  <dbl>  <dbl>  <dbl>
#> 1      0.334      0.0009  0.333  0.336 100000
```
