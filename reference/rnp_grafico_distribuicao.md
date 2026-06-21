# Grafico de uma distribuicao de probabilidade

Plota a densidade (continuas) ou a massa (discretas) de uma
distribuicao, usando os wrappers `rnp_distribuicao_*`.

## Usage

``` r
rnp_grafico_distribuicao(
  dist,
  ...,
  limites = NULL,
  discreta = NULL,
  titulo = NULL
)
```

## Arguments

- dist:

  String com o nome da distribuicao para
  [`rnp_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao.md)
  (ex.: `"norm"`, `"binom"`, `"pois"`, `"gamma"`).

- ...:

  Parametros da distribuicao (ex.: `mean`, `sd`, `size`, `prob`).

- limites:

  Vetor de 2 valores com o intervalo do eixo x. Se `NULL`, escolhido
  automaticamente.

- discreta:

  Logico. Trata como discreta (barras). Se `NULL`, inferido.

- titulo:

  Titulo opcional.

## Value

Objeto `ggplot`.

## See also

Other distribuicoes:
[`rnp_ajuste_distribuicao()`](https://evandeilton.github.io/rnp/reference/rnp_ajuste_distribuicao.md),
[`rnp_distribuicao_beta()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_beta.md),
[`rnp_distribuicao_exponencial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_exponencial.md),
[`rnp_distribuicao_f()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_f.md),
[`rnp_distribuicao_gama()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_gama.md),
[`rnp_distribuicao_lognormal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_lognormal.md),
[`rnp_distribuicao_multinomial()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_multinomial.md),
[`rnp_distribuicao_normal()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_normal.md),
[`rnp_distribuicao_qui_quadrado()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_qui_quadrado.md),
[`rnp_distribuicao_t()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_t.md),
[`rnp_distribuicao_uniforme()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_uniforme.md),
[`rnp_distribuicao_weibull()`](https://evandeilton.github.io/rnp/reference/rnp_distribuicao_weibull.md)

## Examples

``` r
rnp_grafico_distribuicao("norm", mean = 0, sd = 1)

rnp_grafico_distribuicao("binom", size = 10, prob = 0.3)
```
