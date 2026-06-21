# Delineamento em blocos casualizados (DBC)

ANOVA de um experimento em blocos casualizados completos (um fator de
tratamento + um fator de bloco).

## Usage

``` r
rnp_dbc(resposta, tratamento, bloco, digits = 4L)
```

## Arguments

- resposta:

  Vetor numerico da resposta.

- tratamento:

  Fator de tratamento.

- bloco:

  Fator de bloco.

- digits:

  Inteiro.

## Value

tibble com a tabela ANOVA (tratamento, bloco, residuo).

## See also

Other experimental:
[`rnp_ancova()`](https://evandeilton.github.io/rnp/reference/rnp_ancova.md),
[`rnp_anova_medidas_repetidas()`](https://evandeilton.github.io/rnp/reference/rnp_anova_medidas_repetidas.md),
[`rnp_contrastes()`](https://evandeilton.github.io/rnp/reference/rnp_contrastes.md)

## Examples

``` r
set.seed(1)
resp <- rnorm(12, rep(c(5, 7, 9), 4))
trat <- factor(rep(1:3, 4)); bloco <- factor(rep(1:4, each = 3))
rnp_dbc(resp, trat, bloco)
#> # A tibble: 3 × 6
#>   fonte        gl    sq     qm estatistica_f p_valor
#>   <chr>     <dbl> <dbl>  <dbl>         <dbl>   <dbl>
#> 1 trat          2 26.1  13.1           21.1   0.0019
#> 2 bloco         3  2.02  0.672          1.08  0.425 
#> 3 Residuals     6  3.72  0.620         NA    NA     
```
