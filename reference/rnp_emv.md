# Estimador de maxima verossimilhanca (generico)

Maximiza uma funcao de log-verossimilhanca via
[`stats::optim()`](https://rdrr.io/r/stats/optim.html) e estima
erros-padrao pela inversa da informacao de Fisher observada (Hessiana).

## Usage

``` r
rnp_emv(log_veross, inicio, nomes = NULL, metodo = "Nelder-Mead", digits = 4L)
```

## Arguments

- log_veross:

  Funcao `function(theta) ...` que devolve a log-verossimilhanca
  (escalar) para o vetor de parametros `theta`.

- inicio:

  Vetor de valores iniciais para `theta`.

- nomes:

  Vetor opcional de nomes dos parametros.

- metodo:

  Metodo de [`stats::optim()`](https://rdrr.io/r/stats/optim.html).

- digits:

  Inteiro.

## Value

Uma lista com `estimativas` (tibble: `parametro`, `estimativa`,
`erro_padrao`, `z`, `ic_inf`, `ic_sup`) e `ajuste` (tibble:
`log_veross`, `aic`, `bic`... `convergiu`).

## See also

Other inferencia:
[`rnp_bayes_conjugada()`](https://evandeilton.github.io/rnp/reference/rnp_bayes_conjugada.md),
[`rnp_bootstrap()`](https://evandeilton.github.io/rnp/reference/rnp_bootstrap.md),
[`rnp_bootstrap_parametrico()`](https://evandeilton.github.io/rnp/reference/rnp_bootstrap_parametrico.md),
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
set.seed(1); x <- rnorm(200, 5, 2)
ll <- function(th) sum(dnorm(x, th[1], th[2], log = TRUE))
rnp_emv(ll, inicio = c(0, 1), nomes = c("media", "dp"))$estimativas
#> # A tibble: 2 × 6
#>   parametro estimativa erro_padrao     z ic_inf ic_sup
#>   <chr>          <dbl>       <dbl> <dbl>  <dbl>  <dbl>
#> 1 media           5.07      0.131   38.7   4.81   5.33
#> 2 dp              1.85      0.0927  20.0   1.67   2.04
```
