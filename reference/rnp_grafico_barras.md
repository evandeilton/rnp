# Grafico de barras

Wrapper ggplot2 para grafico de barras.

## Usage

``` r
rnp_grafico_barras(
  base,
  x,
  y = NULL,
  fill = NULL,
  position = c("dodge", "stack", "fill"),
  titulo = NULL,
  xlab = NULL,
  ylab = NULL,
  tema = rnp_tema_rnp()
)
```

## Arguments

- base:

  data.frame.

- x:

  Nome da coluna (string) para eixo x.

- y:

  Nome opcional da coluna (string) para eixo y. Se NULL, conta.

- fill:

  Nome opcional da coluna para preenchimento.

- position:

  String: `"dodge"`, `"stack"`, `"fill"`.

- titulo:

  String. Titulo do grafico.

- xlab, ylab:

  String. Rotulos dos eixos.

- tema:

  Objeto `theme` do ggplot2. Default
  [`rnp_tema_rnp()`](https://evandeilton.github.io/rnp/reference/rnp_tema_rnp.md).

## Value

Objeto ggplot.

## Examples

``` r
rnp_grafico_barras(mtcars, "cyl")
```
