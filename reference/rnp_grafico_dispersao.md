# Grafico de dispersao

Grafico de dispersao

## Usage

``` r
rnp_grafico_dispersao(
  base,
  x,
  y,
  cor = NULL,
  tamanho = 2,
  suavizar = FALSE,
  titulo = NULL,
  xlab = NULL,
  ylab = NULL,
  tema = rnp_tema_rnp()
)
```

## Arguments

- base:

  data.frame.

- x, y:

  Nomes das colunas (strings).

- cor:

  Nome opcional da coluna para cor.

- tamanho:

  Escalar ou nome de coluna para tamanho dos pontos.

- suavizar:

  Logico. Adicionar linha de suavizacao (loess).

- titulo, xlab, ylab:

  String.

- tema:

  Objeto `theme`.

## Value

Objeto ggplot.

## Examples

``` r
rnp_grafico_dispersao(mtcars, "wt", "mpg")
```
