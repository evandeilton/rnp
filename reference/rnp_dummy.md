# Codificacao dummy (one-hot)

Converte variaveis categoricas em colunas indicadoras (0/1).

## Usage

``` r
rnp_dummy(base, colunas = NULL, remover_primeira = FALSE)
```

## Arguments

- base:

  data.frame.

- colunas:

  Vetor opcional de nomes de colunas a codificar. Default: todas as
  colunas categoricas/fator.

- remover_primeira:

  Logico. Remove a primeira categoria (referencia) de cada variavel para
  evitar colinearidade.

## Value

tibble com as colunas dummy adicionadas e as originais removidas.

## See also

Other preprocessamento:
[`rnp_discretiza()`](https://evandeilton.github.io/rnp/reference/rnp_discretiza.md),
[`rnp_imputa()`](https://evandeilton.github.io/rnp/reference/rnp_imputa.md),
[`rnp_normaliza()`](https://evandeilton.github.io/rnp/reference/rnp_normaliza.md),
[`rnp_padroniza()`](https://evandeilton.github.io/rnp/reference/rnp_padroniza.md),
[`rnp_winsoriza()`](https://evandeilton.github.io/rnp/reference/rnp_winsoriza.md)

## Examples

``` r
rnp_dummy(data.frame(cor = c("a", "b", "a", "c"), x = 1:4))
#> # A tibble: 4 × 4
#>       x cor_a cor_b cor_c
#>   <int> <int> <int> <int>
#> 1     1     1     0     0
#> 2     2     0     1     0
#> 3     3     1     0     0
#> 4     4     0     0     1
```
