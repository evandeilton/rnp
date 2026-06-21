# Tabela de contingencia (dupla entrada)

Tabela de frequencias conjuntas de duas variaveis categoricas, com
frequencias relativas total, por linha e por coluna, e marginais.
Substitui a antiga `rnp_2freq` (cuja frequencia relativa estava
incorreta).

## Usage

``` r
rnp_tabela_contingencia(
  x,
  y,
  tipo = c("fa", "fr", "fr_linha", "fr_coluna"),
  digits = 4L
)
```

## Arguments

- x, y:

  Vetores categoricos/fatores de mesmo comprimento.

- tipo:

  String: `"fa"` (absoluta), `"fr"` (relativa ao total), `"fr_linha"`
  (relativa por linha) ou `"fr_coluna"` (relativa por coluna).

- digits:

  Inteiro. Casas decimais.

## Value

tibble em formato largo com a variavel `x` nas linhas, as categorias de
`y` nas colunas e a marginal `Total`.

## See also

Other descritiva:
[`rnp_intervalo_classes()`](https://evandeilton.github.io/rnp/reference/rnp_intervalo_classes.md),
[`rnp_medias()`](https://evandeilton.github.io/rnp/reference/rnp_medias.md),
[`rnp_momentos()`](https://evandeilton.github.io/rnp/reference/rnp_momentos.md),
[`rnp_tabela_frequencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_frequencia.md)

## Examples

``` r
rnp_tabela_contingencia(mtcars$cyl, mtcars$gear)
#> # A tibble: 3 × 5
#>   categoria   `3`   `4`   `5` Total
#>   <chr>     <int> <int> <int> <dbl>
#> 1 4             1     8     2    11
#> 2 6             2     4     1     7
#> 3 8            12     0     2    14
rnp_tabela_contingencia(mtcars$cyl, mtcars$gear, tipo = "fr_linha")
#> # A tibble: 3 × 5
#>   categoria    `3`   `4`   `5` Total
#>   <chr>      <dbl> <dbl> <dbl> <dbl>
#> 1 4         0.0909 0.727 0.182     1
#> 2 6         0.286  0.571 0.143     1
#> 3 8         0.857  0     0.143     1
```
