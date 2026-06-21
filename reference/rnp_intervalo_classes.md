# Numero/limites de classes por regra classica

Calcula o numero de classes e os limites para agrupamento de uma
variavel continua, segundo a regra escolhida (Sturges, Scott,
Freedman-Diaconis, raiz, Rice ou numero fixo).

## Usage

``` r
rnp_intervalo_classes(
  x,
  regra = c("sturges", "scott", "fd", "sqrt", "rice", "fixa"),
  k = NULL,
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico.

- regra:

  String: `"sturges"`, `"scott"`, `"fd"`, `"sqrt"`, `"rice"` ou
  `"fixa"`.

- k:

  Inteiro. Numero de classes quando `regra = "fixa"`.

- digits:

  Inteiro. Casas decimais.

## Value

Uma lista com `n_classes`, `amplitude_classe` e o vetor `limites`.

## See also

Other descritiva:
[`rnp_medias()`](https://evandeilton.github.io/rnp/reference/rnp_medias.md),
[`rnp_momentos()`](https://evandeilton.github.io/rnp/reference/rnp_momentos.md),
[`rnp_tabela_contingencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_contingencia.md),
[`rnp_tabela_frequencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_frequencia.md)

## Examples

``` r
rnp_intervalo_classes(rnorm(1000))
#> $n_classes
#> [1] 11
#> 
#> $amplitude_classe
#> [1] 0.6198
#> 
#> $limites
#>  [1] -3.0080 -2.3882 -1.7684 -1.1485 -0.5287  0.0912  0.7110  1.3309  1.9507
#> [10]  2.5706  3.1904  3.8103
#> 
rnp_intervalo_classes(mtcars$mpg, regra = "fd")
#> $n_classes
#> [1] 6
#> 
#> $amplitude_classe
#> [1] 3.9167
#> 
#> $limites
#> [1] 10.4000 14.3167 18.2333 22.1500 26.0667 29.9833 33.9000
#> 
```
