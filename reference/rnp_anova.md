# ANOVA de um fator

Realiza ANOVA de um fator com saida tidy. Inclui teste de Levene e
opcionalmente testes post-hoc.

## Usage

``` r
rnp_anova(
  x,
  g = NULL,
  post_hoc = NA_character_,
  conf = 0.95,
  digits = 4L,
  data = NULL
)
```

## Arguments

- x:

  Vetor numerico (resposta) ou formula `y ~ grupo`.

- g:

  Vetor de grupos (necessario se x nao for formula).

- post_hoc:

  String opcional: `"tukey"`, `"duncan"`, `"dunnett"`, `"scheffe"` ou
  `NA` para nenhum.

- conf:

  Nivel de confianca para post-hoc.

- digits:

  Inteiro.

- data:

  data.frame opcional (usado quando `x` e uma formula).

## Value

lista com `anova` (tibble), `levene` (tibble) e `post_hoc` (tibble ou
NULL).

## Examples

``` r
rnp_anova(mtcars$mpg, as.factor(mtcars$cyl))
#> $anova
#> # A tibble: 3 × 6
#>   fonte            gl soma_quadrados media_quadrados estatistica_F p_valor
#>   <chr>         <dbl>          <dbl>           <dbl>         <dbl>   <dbl>
#> 1 "g          "     2           825.           412.           39.7       0
#> 2 "Residuals  "    29           301.            10.4          NA        NA
#> 3 "total"          31          1126.            NA            NA        NA
#> 
#> $levene
#> # A tibble: 1 × 5
#>   estatistica   gl1   gl2 p_valor metodo
#>         <dbl> <dbl> <int>   <dbl> <chr> 
#> 1        5.51     2    29  0.0094 levene
#> 
#> $post_hoc
#> NULL
#> 
rnp_anova(mpg ~ factor(cyl), data = mtcars, post_hoc = "tukey")
#> $anova
#> # A tibble: 3 × 6
#>   fonte            gl soma_quadrados media_quadrados estatistica_F p_valor
#>   <chr>         <dbl>          <dbl>           <dbl>         <dbl>   <dbl>
#> 1 "g          "     2           825.           412.           39.7       0
#> 2 "Residuals  "    29           301.            10.4          NA        NA
#> 3 "total"          31          1126.            NA            NA        NA
#> 
#> $levene
#> # A tibble: 1 × 5
#>   estatistica   gl1   gl2 p_valor metodo
#>         <dbl> <dbl> <int>   <dbl> <chr> 
#> 1        5.51     2    29  0.0094 levene
#> 
#> $post_hoc
#> # A tibble: 3 × 6
#>   termo comparacao   diff limite_inferior limite_superior p_ajustado
#>   <chr> <chr>       <dbl>           <dbl>           <dbl>      <dbl>
#> 1 g     6-4         -6.92          -10.8           -3.07      0.0003
#> 2 g     8-4        -11.6           -14.8           -8.36      0     
#> 3 g     8-6         -4.64           -8.33          -0.958     0.0112
#> 
```
