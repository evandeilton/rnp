# Poder de um teste t e curva de poder

Calcula o poder de um teste t (uma ou duas amostras) para um tamanho de
efeito (d de Cohen) e devolve tambem a curva de poder versus n.

## Usage

``` r
rnp_poder_teste(
  efeito,
  n,
  alpha = 0.05,
  tipo = c("duas", "uma"),
  lado = c("bilateral", "direita", "esquerda"),
  digits = 4L
)
```

## Arguments

- efeito:

  Tamanho de efeito d de Cohen.

- n:

  Tamanho amostral (por grupo, se duas amostras).

- alpha:

  Nivel de significancia.

- tipo:

  String: `"uma"` ou `"duas"` amostras.

- lado:

  String: `"bilateral"`, `"direita"`, `"esquerda"`.

- digits:

  Inteiro.

## Value

Uma lista com `poder` (tibble) e `grafico` (ggplot da curva de poder).

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
[`rnp_tamanho_amostra_teste()`](https://evandeilton.github.io/rnp/reference/rnp_tamanho_amostra_teste.md),
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
rnp_poder_teste(efeito = 0.5, n = 30, tipo = "duas")$poder
#> # A tibble: 1 × 5
#>   efeito     n alpha poder tipo 
#>    <dbl> <dbl> <dbl> <dbl> <chr>
#> 1    0.5    30  0.05 0.478 duas 
```
