# Discretizacao (binning)

Converte uma variavel continua em categorias por largura igual,
frequencia igual (quantis) ou k-means.

## Usage

``` r
rnp_discretiza(
  x,
  k = 4L,
  metodo = c("largura", "frequencia", "kmeans"),
  rotulos = NULL
)
```

## Arguments

- x:

  Vetor numerico.

- k:

  Inteiro. Numero de classes.

- metodo:

  String: `"largura"`, `"frequencia"` ou `"kmeans"`.

- rotulos:

  Vetor opcional de rotulos das classes.

## Value

Fator com `k` niveis.

## See also

Other preprocessamento:
[`rnp_dummy()`](https://evandeilton.github.io/rnp/reference/rnp_dummy.md),
[`rnp_imputa()`](https://evandeilton.github.io/rnp/reference/rnp_imputa.md),
[`rnp_normaliza()`](https://evandeilton.github.io/rnp/reference/rnp_normaliza.md),
[`rnp_padroniza()`](https://evandeilton.github.io/rnp/reference/rnp_padroniza.md),
[`rnp_winsoriza()`](https://evandeilton.github.io/rnp/reference/rnp_winsoriza.md)

## Examples

``` r
rnp_discretiza(mtcars$mpg, k = 3, metodo = "frequencia")
#>  [1] (16.7,21.4] (16.7,21.4] (21.4,33.9] (16.7,21.4] (16.7,21.4] (16.7,21.4]
#>  [7] [10.4,16.7] (21.4,33.9] (21.4,33.9] (16.7,21.4] (16.7,21.4] [10.4,16.7]
#> [13] (16.7,21.4] [10.4,16.7] [10.4,16.7] [10.4,16.7] [10.4,16.7] (21.4,33.9]
#> [19] (21.4,33.9] (21.4,33.9] (21.4,33.9] [10.4,16.7] [10.4,16.7] [10.4,16.7]
#> [25] (16.7,21.4] (21.4,33.9] (21.4,33.9] (21.4,33.9] [10.4,16.7] (16.7,21.4]
#> [31] [10.4,16.7] (16.7,21.4]
#> Levels: [10.4,16.7] (16.7,21.4] (21.4,33.9]
```
