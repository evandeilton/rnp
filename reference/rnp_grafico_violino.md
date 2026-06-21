# Grafico de violino

Grafico de violino

## Usage

``` r
rnp_grafico_violino(
  base,
  x,
  y,
  fill = NULL,
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

  Nome da coluna (string) para eixo x (categorica).

- y:

  Nome da coluna (string) para eixo y (numerica).

- fill:

  Nome opcional da coluna para preenchimento.

- titulo, xlab, ylab:

  String.

- tema:

  Objeto `theme`.

## Value

Objeto ggplot.

## Examples

``` r
rnp_grafico_violino(mtcars, "cyl", "mpg")
#> Warning: Orientation is not uniquely specified when both the x and y aesthetics are
#> continuous. Picking default orientation 'x'.
#> Warning: Continuous x aesthetic
#> ℹ did you forget `aes(group = ...)`?
```
