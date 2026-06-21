# Tema ggplot2 do projeto R NA PRATICA

Tema minimalista com cores e tipografia educacionais.

## Usage

``` r
rnp_tema_rnp(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Escalar. Tamanho base da fonte.

- base_family:

  String. Familia de fonte.

## Value

Objeto `theme` do ggplot2.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + geom_point() + rnp_tema_rnp()
```
