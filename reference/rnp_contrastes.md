# Contrastes lineares

Testa contrastes lineares entre medias de grupos de uma ANOVA de um
fator.

## Usage

``` r
rnp_contrastes(resposta, grupo, contrastes, digits = 4L)
```

## Arguments

- resposta:

  Vetor numerico.

- grupo:

  Fator de grupos.

- contrastes:

  Matriz (cada coluna = um contraste; linhas = niveis do fator,
  coeficientes somando zero).

- digits:

  Inteiro.

## Value

tibble com `contraste`, `estimativa`, `erro_padrao`, `t`, `p_valor`.

## See also

Other experimental:
[`rnp_ancova()`](https://evandeilton.github.io/rnp/reference/rnp_ancova.md),
[`rnp_anova_medidas_repetidas()`](https://evandeilton.github.io/rnp/reference/rnp_anova_medidas_repetidas.md),
[`rnp_dbc()`](https://evandeilton.github.io/rnp/reference/rnp_dbc.md)

## Examples

``` r
# compara grupo 1 vs media de 2 e 3; e grupo 2 vs 3
set.seed(1)
y <- rnorm(30, rep(c(5, 6, 9), each = 10)); g <- factor(rep(1:3, each = 10))
C <- cbind(c(2, -1, -1), c(0, 1, -1))
rnp_contrastes(y, g, C)
#> # A tibble: 2 × 5
#>   contraste estimativa erro_padrao     t p_valor
#>   <chr>          <dbl>       <dbl> <dbl>   <dbl>
#> 1 C1             -4.85       0.730 -6.64       0
#> 2 C2             -2.62       0.422 -6.21       0
```
