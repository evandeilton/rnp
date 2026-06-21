# Medias (aritmetica, geometrica, harmonica e quadratica)

Calcula, de forma unificada, as principais medias de posicao, com
suporte a ponderacao. Consolida as antigas `media_aritmetica`,
`media_geometrica` e `media_harmonica`.

## Usage

``` r
rnp_medias(
  x,
  peso = NULL,
  tipo = c("aritmetica", "geometrica", "harmonica", "quadratica"),
  na.rm = TRUE,
  digits = 4L
)
```

## Arguments

- x:

  Vetor numerico.

- peso:

  Vetor opcional de pesos (mesmo comprimento de `x`).

- tipo:

  Vetor com um ou mais de `"aritmetica"`, `"geometrica"`, `"harmonica"`,
  `"quadratica"` (default: todas).

- na.rm:

  Logico. Remove NA (e pesos correspondentes) antes do calculo.

- digits:

  Inteiro. Casas decimais.

## Value

tibble com colunas `tipo` e `valor`.

## Details

Medias geometrica e harmonica exigem `x > 0`.

## See also

Other descritiva:
[`rnp_intervalo_classes()`](https://evandeilton.github.io/rnp/reference/rnp_intervalo_classes.md),
[`rnp_momentos()`](https://evandeilton.github.io/rnp/reference/rnp_momentos.md),
[`rnp_tabela_contingencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_contingencia.md),
[`rnp_tabela_frequencia()`](https://evandeilton.github.io/rnp/reference/rnp_tabela_frequencia.md)

## Examples

``` r
rnp_medias(c(2, 4, 8))
#> # A tibble: 4 × 2
#>   tipo       valor
#>   <chr>      <dbl>
#> 1 aritmetica  4.67
#> 2 geometrica  4   
#> 3 harmonica   3.43
#> 4 quadratica  5.29
rnp_medias(c(2, 4, 8), peso = c(1, 2, 1), tipo = c("aritmetica", "harmonica"))
#> # A tibble: 2 × 2
#>   tipo       valor
#>   <chr>      <dbl>
#> 1 aritmetica  4.5 
#> 2 harmonica   3.56
```
