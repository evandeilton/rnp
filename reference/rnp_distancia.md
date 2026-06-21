# Distancias entre observacoes

Calcula matriz de distancias usando varios metodos.

## Usage

``` r
rnp_distancia(
  base,
  method = c("euclidean", "manhattan", "minkowski", "canberra", "mahalanobis"),
  p = 2,
  digits = 4L
)
```

## Arguments

- base:

  data.frame ou matriz numerica.

- method:

  String: `"euclidean"`, `"manhattan"`, `"minkowski"`, `"canberra"`,
  `"mahalanobis"`.

- p:

  Escalar. Potencia (apenas Minkowski).

- digits:

  Inteiro.

## Value

Matriz de distancias (classe `dist`).

## Examples

``` r
rnp_distancia(mtcars[1:5, c("mpg", "hp")], method = "euclidean")
#>          1        2        3        4
#> 2  0.00000                           
#> 3 17.09503 17.09503                  
#> 4  0.40000  0.40000 17.05755         
#> 5 65.04068 65.04068 82.10244 65.05605
```
