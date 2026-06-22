# Resistencia de corpos de prova de concreto (didatico)

Dados **simulados** de ensaios de compressao de corpos de prova de
concreto, para ilustrar estatistica descritiva, ANOVA e regressao.

## Usage

``` r
rnp_concreto
```

## Format

Um `data.frame` com 90 linhas e 3 variaveis:

- resistencia:

  Resistencia a compressao (MPa).

- dias_cura:

  Tempo de cura (7, 14 ou 28 dias).

- tipo_cimento:

  Tipo de cimento (`CP-II`, `CP-IV`, `CP-V`).

## Details

Dados simulados para fins didaticos; ver `data-raw/datasets.R`.

## Examples

``` r
rnp_descritiva(rnp_concreto$resistencia)
#> # A tibble: 1 × 21
#>       n n_validos n_faltantes  soma media mediana  moda desvio variancia   min
#>   <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>     <dbl> <dbl>
#> 1    90        90           0 2331.  25.9    25.8  27.2   5.29      28.0  15.6
#> # ℹ 11 more variables: q1 <dbl>, q3 <dbl>, max <dbl>, amplitude <dbl>,
#> #   iqr <dbl>, cv <dbl>, se_media <dbl>, ic_inf <dbl>, ic_sup <dbl>,
#> #   assimetria <dbl>, curtose <dbl>
rnp_anova(resistencia ~ tipo_cimento, data = rnp_concreto)
#> $anova
#> # A tibble: 3 × 6
#>   fonte            gl soma_quadrados media_quadrados estatistica_F p_valor
#>   <chr>         <dbl>          <dbl>           <dbl>         <dbl>   <dbl>
#> 1 "g          "     2           658.           329.           15.6       0
#> 2 "Residuals  "    87          1831.            21.0          NA        NA
#> 3 "total"          89          2489.            NA            NA        NA
#> 
#> $levene
#> # A tibble: 1 × 5
#>   estatistica   gl1   gl2 p_valor metodo
#>         <dbl> <dbl> <int>   <dbl> <chr> 
#> 1       0.107     2    87   0.898 levene
#> 
#> $post_hoc
#> NULL
#> 
```
