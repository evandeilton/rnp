# Grafico de histograma

Grafico de histograma

## Usage

``` r
rnp_grafico_histograma(
  base,
  x,
  bins = 30,
  fill = rnp_paleta_rnp("rnp_qual", 1),
  densidade = FALSE,
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

  Nome da coluna (string).

- bins:

  Inteiro. Numero de bins.

- fill:

  Cor de preenchimento.

- densidade:

  Logico. Mostrar densidade em vez de contagem.

- titulo, xlab, ylab:

  String.

- tema:

  Objeto `theme`.

## Value

Objeto ggplot.

## Examples

``` r
rnp_grafico_histograma(mtcars, "mpg")
```
