# Grafico QQ (quantil-quantil)

Grafico QQ (quantil-quantil)

## Usage

``` r
rnp_grafico_qq(
  x,
  distribuicao = "norm",
  ...,
  titulo = NULL,
  xlab = "Quantis Teoricos",
  ylab = "Quantis Amostrais",
  tema = rnp_tema_rnp()
)
```

## Arguments

- x:

  Vetor numerico.

- distribuicao:

  String: `"norm"`, `"t"`, `"chisq"`, etc.

- ...:

  Argumentos da distribuicao (ex.: `df` para t).

- titulo, xlab, ylab:

  String.

- tema:

  Objeto `theme`.

## Value

Objeto ggplot.

## Examples

``` r
rnp_grafico_qq(rnorm(100))
```
