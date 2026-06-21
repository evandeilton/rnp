# Teste qui-quadrado (independencia ou aderencia)

Com `x` e `y`, testa independencia entre duas categoricas. Com `x`
(contagens observadas) e `p` (probabilidades esperadas), testa
aderencia.

## Usage

``` r
rnp_teste_qui_quadrado(x, y = NULL, p = NULL, corrigir = FALSE, digits = 4L)
```

## Arguments

- x:

  Vetor categorico (independencia) ou de contagens (aderencia).

- y:

  Vetor categorico (independencia). Default `NULL`.

- p:

  Vetor de probabilidades esperadas (aderencia). Default `NULL`.

- corrigir:

  Logico. Correcao de continuidade de Yates (2x2).

- digits:

  Inteiro.

## Value

tibble com `estatistica`, `gl`, `p_valor`, `metodo` e, na independencia,
`v_cramer`.

## See also

Other inferencia:
[`rnp_bayes_conjugada()`](https://evandeilton.github.io/rnp/reference/rnp_bayes_conjugada.md),
[`rnp_bootstrap()`](https://evandeilton.github.io/rnp/reference/rnp_bootstrap.md),
[`rnp_bootstrap_parametrico()`](https://evandeilton.github.io/rnp/reference/rnp_bootstrap_parametrico.md),
[`rnp_emv()`](https://evandeilton.github.io/rnp/reference/rnp_emv.md),
[`rnp_ic_bootstrap()`](https://evandeilton.github.io/rnp/reference/rnp_ic_bootstrap.md),
[`rnp_ic_verossimilhanca()`](https://evandeilton.github.io/rnp/reference/rnp_ic_verossimilhanca.md),
[`rnp_informacao_fisher()`](https://evandeilton.github.io/rnp/reference/rnp_informacao_fisher.md),
[`rnp_jackknife()`](https://evandeilton.github.io/rnp/reference/rnp_jackknife.md),
[`rnp_log_verossimilhanca()`](https://evandeilton.github.io/rnp/reference/rnp_log_verossimilhanca.md),
[`rnp_metodo_momentos()`](https://evandeilton.github.io/rnp/reference/rnp_metodo_momentos.md),
[`rnp_poder_teste()`](https://evandeilton.github.io/rnp/reference/rnp_poder_teste.md),
[`rnp_tamanho_amostra_teste()`](https://evandeilton.github.io/rnp/reference/rnp_tamanho_amostra_teste.md),
[`rnp_teste_aderencia()`](https://evandeilton.github.io/rnp/reference/rnp_teste_aderencia.md),
[`rnp_teste_binomial()`](https://evandeilton.github.io/rnp/reference/rnp_teste_binomial.md),
[`rnp_teste_grubbs()`](https://evandeilton.github.io/rnp/reference/rnp_teste_grubbs.md),
[`rnp_teste_ks()`](https://evandeilton.github.io/rnp/reference/rnp_teste_ks.md),
[`rnp_teste_normalidade()`](https://evandeilton.github.io/rnp/reference/rnp_teste_normalidade.md),
[`rnp_teste_permutacao()`](https://evandeilton.github.io/rnp/reference/rnp_teste_permutacao.md),
[`rnp_teste_proporcoes()`](https://evandeilton.github.io/rnp/reference/rnp_teste_proporcoes.md),
[`rnp_teste_razao_veross()`](https://evandeilton.github.io/rnp/reference/rnp_teste_razao_veross.md),
[`rnp_teste_runs()`](https://evandeilton.github.io/rnp/reference/rnp_teste_runs.md),
[`rnp_teste_score()`](https://evandeilton.github.io/rnp/reference/rnp_teste_score.md),
[`rnp_teste_sinais()`](https://evandeilton.github.io/rnp/reference/rnp_teste_sinais.md),
[`rnp_teste_wald()`](https://evandeilton.github.io/rnp/reference/rnp_teste_wald.md)

## Examples

``` r
rnp_teste_qui_quadrado(mtcars$cyl, mtcars$gear)
#> Warning: Chi-squared approximation may be incorrect
#> # A tibble: 1 × 5
#>   estatistica    gl p_valor v_cramer metodo       
#>         <dbl> <int>   <dbl>    <dbl> <chr>        
#> 1        18.0     4  0.0012    0.531 independencia
rnp_teste_qui_quadrado(c(20, 30, 50), p = c(0.25, 0.25, 0.5))
#> # A tibble: 1 × 4
#>   estatistica    gl p_valor metodo   
#>         <dbl> <dbl>   <dbl> <chr>    
#> 1           2     2   0.368 aderencia
```
