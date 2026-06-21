# Esperanca e variancia condicionais E\[Y \| X = x\]

Para uma distribuicao conjunta discreta, calcula a esperanca e a
variancia de Y condicionadas a cada valor de X.

## Usage

``` r
rnp_esperanca_condicional(p, valores_x = NULL, valores_y = NULL, digits = 4L)
```

## Arguments

- p:

  Matriz de probabilidades conjuntas (linhas = X, colunas = Y).

- valores_x, valores_y:

  Vetores numericos com os valores de X e Y. Se `NULL`, usa os nomes de
  linha/coluna convertidos para numero.

- digits:

  Inteiro. Casas decimais.

## Value

tibble com `x`, `e_y_dado_x` e `var_y_dado_x`.

## See also

Other probabilidade:
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
[`rnp_distribuicao_conjunta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_conjunta.md),
[`rnp_lei_grandes_numeros()`](https://evandeilton.github.io/rnp/reference/rnp_lei_grandes_numeros.md),
[`rnp_monte_carlo()`](https://evandeilton.github.io/rnp/reference/rnp_monte_carlo.md),
[`rnp_passeio_aleatorio()`](https://evandeilton.github.io/rnp/reference/rnp_passeio_aleatorio.md),
[`rnp_processo_poisson()`](https://evandeilton.github.io/rnp/reference/rnp_processo_poisson.md),
[`rnp_simula_aceitacao_rejeicao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_aceitacao_rejeicao.md),
[`rnp_simula_inversao()`](https://evandeilton.github.io/rnp/reference/rnp_simula_inversao.md),
[`rnp_tcl_simulacao()`](https://evandeilton.github.io/rnp/reference/rnp_tcl_simulacao.md)

## Examples

``` r
p <- matrix(c(0.1, 0.2, 0.2, 0.5), 2, 2,
            dimnames = list(c("0", "1"), c("0", "1")))
rnp_esperanca_condicional(p)
#> # A tibble: 2 × 3
#>       x e_y_dado_x var_y_dado_x
#>   <dbl>      <dbl>        <dbl>
#> 1     0      0.667        0.222
#> 2     1      0.714        0.204
```
