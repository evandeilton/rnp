# Distribuicao conjunta discreta: marginais, momentos e dependencia

A partir de uma matriz de probabilidades conjuntas P(X = x, Y = y),
calcula as distribuicoes marginais, E\[X\], E\[Y\], variancias,
covariancia e correlacao. Os valores de X e Y sao lidos dos nomes de
linha/coluna (se numericos) ou informados explicitamente.

## Usage

``` r
rnp_distribuicao_conjunta(p, valores_x = NULL, valores_y = NULL, digits = 4L)
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

Uma lista com `marginal_x`, `marginal_y` (tibbles) e `resumo` (tibble
com `e_x`, `e_y`, `var_x`, `var_y`, `cov_xy`, `cor_xy`).

## See also

Other probabilidade:
[`rnp_bayes()`](https://evandeilton.github.io/rnp/reference/rnp_bayes.md),
[`rnp_cadeia_markov()`](https://evandeilton.github.io/rnp/reference/rnp_cadeia_markov.md),
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
p <- matrix(c(0.1, 0.2, 0.2, 0.5), 2, 2,
            dimnames = list(c("0", "1"), c("0", "1")))
rnp_distribuicao_conjunta(p)
#> $marginal_x
#> # A tibble: 2 × 2
#>       x     p
#>   <dbl> <dbl>
#> 1     0   0.3
#> 2     1   0.7
#> 
#> $marginal_y
#> # A tibble: 2 × 2
#>       y     p
#>   <dbl> <dbl>
#> 1     0   0.3
#> 2     1   0.7
#> 
#> $resumo
#> # A tibble: 1 × 6
#>     e_x   e_y var_x var_y cov_xy cor_xy
#>   <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
#> 1   0.7   0.7  0.21  0.21   0.01 0.0476
#> 
```
