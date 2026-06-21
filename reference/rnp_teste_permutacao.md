# Teste de permutacao (diferenca de medias)

Testa a igualdade de medias de dois grupos por reamostragem dos rotulos
(teste de permutacao), com backend C++. Hipotese nula: distribuicoes
iguais.

## Usage

``` r
rnp_teste_permutacao(
  x,
  y,
  B = 5000L,
  lado = c("bilateral", "direita", "esquerda"),
  seed = 42L,
  digits = 4L
)
```

## Arguments

- x, y:

  Vetores numericos dos dois grupos.

- B:

  Inteiro. Numero de permutacoes.

- lado:

  String: `"bilateral"`, `"direita"`, `"esquerda"`.

- seed:

  Inteiro. Semente.

- digits:

  Inteiro.

## Value

tibble com `diff_observada`, `p_valor`, `B`, `alternativa`.

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
[`rnp_teste_proporcoes()`](https://evandeilton.github.io/rnp/reference/rnp_teste_proporcoes.md),
[`rnp_teste_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_teste_qui_quadrado.md),
[`rnp_teste_razao_veross()`](https://evandeilton.github.io/rnp/reference/rnp_teste_razao_veross.md),
[`rnp_teste_runs()`](https://evandeilton.github.io/rnp/reference/rnp_teste_runs.md),
[`rnp_teste_score()`](https://evandeilton.github.io/rnp/reference/rnp_teste_score.md),
[`rnp_teste_sinais()`](https://evandeilton.github.io/rnp/reference/rnp_teste_sinais.md),
[`rnp_teste_wald()`](https://evandeilton.github.io/rnp/reference/rnp_teste_wald.md)

## Examples

``` r
set.seed(1)
rnp_teste_permutacao(rnorm(30, 0), rnorm(30, 0.8), B = 2000)
#> # A tibble: 1 × 4
#>   diff_observada p_valor     B alternativa
#>            <dbl>   <dbl> <dbl> <chr>      
#> 1         -0.850  0.0005  2000 bilateral  
```
