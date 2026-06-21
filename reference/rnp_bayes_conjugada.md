# Estimacao bayesiana conjugada

Atualiza distribuicoes a priori conjugadas com os dados para tres
modelos classicos: Beta-Binomial, Gama-Poisson e Normal-Normal
(variancia conhecida).

## Usage

``` r
rnp_bayes_conjugada(
  familia = c("beta_binomial", "gama_poisson", "normal_normal"),
  priori,
  dados,
  conf = 0.95,
  digits = 4L
)
```

## Arguments

- familia:

  String: `"beta_binomial"`, `"gama_poisson"` ou `"normal_normal"`.

- priori:

  Vetor nomeado de hiperparametros a priori (`beta_binomial`: `a`, `b`;
  `gama_poisson`: `a`, `b`; `normal_normal`: `mu0`, `sigma0`).

- dados:

  Lista com os dados (`beta_binomial`: `sucessos`, `n`; `gama_poisson`:
  `soma`, `n`; `normal_normal`: `x` e `sigma` conhecido).

- conf:

  Nivel de credibilidade do intervalo.

- digits:

  Inteiro.

## Value

tibble com `parametro`, `valor` (hiperparametros a posteriori), alem de
`media_post`, `ic_inf`, `ic_sup`.

## See also

Other inferencia:
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
[`rnp_teste_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_teste_qui_quadrado.md),
[`rnp_teste_razao_veross()`](https://evandeilton.github.io/rnp/reference/rnp_teste_razao_veross.md),
[`rnp_teste_runs()`](https://evandeilton.github.io/rnp/reference/rnp_teste_runs.md),
[`rnp_teste_score()`](https://evandeilton.github.io/rnp/reference/rnp_teste_score.md),
[`rnp_teste_sinais()`](https://evandeilton.github.io/rnp/reference/rnp_teste_sinais.md),
[`rnp_teste_wald()`](https://evandeilton.github.io/rnp/reference/rnp_teste_wald.md)

## Examples

``` r
rnp_bayes_conjugada("beta_binomial", priori = c(a = 1, b = 1),
                    dados = list(sucessos = 8, n = 10))
#> # A tibble: 2 × 5
#>   parametro valor media_post ic_inf ic_sup
#>   <chr>     <dbl>      <dbl>  <dbl>  <dbl>
#> 1 a             9       0.75  0.482  0.940
#> 2 b             3       0.75  0.482  0.940
```
