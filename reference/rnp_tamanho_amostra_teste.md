# Tamanho de amostra por poder (teste t)

Encontra o menor `n` que atinge o poder desejado para um teste t.

## Usage

``` r
rnp_tamanho_amostra_teste(
  efeito,
  poder = 0.8,
  alpha = 0.05,
  tipo = c("duas", "uma"),
  lado = c("bilateral", "direita", "esquerda"),
  digits = 4L
)
```

## Arguments

- efeito:

  Tamanho de efeito d de Cohen.

- poder:

  Poder desejado (ex.: 0.80).

- alpha:

  Nivel de significancia.

- tipo:

  String: `"uma"` ou `"duas"` amostras.

- lado:

  String: `"bilateral"`, `"direita"`, `"esquerda"`.

- digits:

  Inteiro.

## Value

tibble com `efeito`, `poder_alvo`, `alpha`, `n`, `poder_obtido`.

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
[`rnp_teste_aderencia()`](https://evandeilton.github.io/rnp/reference/rnp_teste_aderencia.md),
[`rnp_teste_binomial()`](https://evandeilton.github.io/rnp/reference/rnp_teste_binomial.md),
[`rnp_teste_grubbs()`](https://evandeilton.github.io/rnp/reference/rnp_teste_grubbs.md),
[`rnp_teste_ks()`](https://evandeilton.github.io/rnp/reference/rnp_teste_ks.md),
[`rnp_teste_normalidade()`](https://evandeilton.github.io/rnp/reference/rnp_teste_normalidade.md),
[`rnp_teste_permutacao()`](https://evandeilton.github.io/rnp/reference/rnp_teste_permutacao.md),
[`rnp_teste_proporcoes()`](https://evandeilton.github.io/rnp/reference/rnp_teste_proporcoes.md),
[`rnp_teste_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_teste_qui_quadrado.md),
[`rnp_teste_razao_veross()`](https://evandeilton.github.io/rnp/reference/rnp_teste_razao_veross.md),
[`rnp_teste_runs()`](https://evandeilton.github.io/rnp/reference/rnp_teste_runs.md),
[`rnp_teste_score()`](https://evandeilton.github.io/rnp/reference/rnp_teste_score.md),
[`rnp_teste_sinais()`](https://evandeilton.github.io/rnp/reference/rnp_teste_sinais.md),
[`rnp_teste_wald()`](https://evandeilton.github.io/rnp/reference/rnp_teste_wald.md)

## Examples

``` r
rnp_tamanho_amostra_teste(efeito = 0.5, poder = 0.8, tipo = "duas")
#> # A tibble: 1 × 5
#>   efeito poder_alvo alpha     n poder_obtido
#>    <dbl>      <dbl> <dbl> <int>        <dbl>
#> 1    0.5        0.8  0.05    64        0.802
```
