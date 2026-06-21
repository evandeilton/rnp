# Intervalo de confianca para diferenca de medias

Intervalo de confianca para diferenca de medias

## Usage

``` r
rnp_ic_diff_medias(
  x,
  y,
  pareado = FALSE,
  var_iguais = FALSE,
  conf = 0.95,
  na.rm = TRUE,
  digits = 4L
)
```

## Arguments

- x, y:

  Vetores numericos.

- pareado:

  Logico. Amostras pareadas.

- var_iguais:

  Logico. Assumir variancias iguais (independentes).

- conf:

  Nivel de confianca.

- na.rm:

  Logico.

- digits:

  Inteiro.

## Value

tibble com `diff_medias`, `erro_padrao`, `limite_inferior`,
`limite_superior`, `gl`, `metodo`.

## Examples

``` r
rnp_ic_diff_medias(rnorm(30, 5), rnorm(30, 5.5))
#> # A tibble: 1 × 6
#>   diff_medias erro_padrao limite_inferior limite_superior    gl metodo   
#>         <dbl>       <dbl>           <dbl>           <dbl> <dbl> <chr>    
#> 1        0.14       0.231          -0.323           0.603  58.0 t (Welch)
```
