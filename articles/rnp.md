# Introdução ao rnp

## O que é o `rnp`

O `rnp` (do projeto **R NA PRÁTICA**) reúne mais de **180 funções** que
cobrem os temas tratados nos primeiros anos de um curso de estatística:
descritiva, probabilidade, inferência, regressão, análise multivariada,
séries temporais, dados categóricos e pré-processamento.

Três escolhas orientam o projeto:

- **Saídas tidy** — as funções analíticas retornam `tibble` e os
  gráficos usam `ggplot2`; documentação e mensagens em português.
- **Desempenho** — rotinas numericamente intensivas são escritas em C++
  (Rcpp/RcppArmadillo) e conferidas contra as funções equivalentes do R.
- **Poucas dependências** — apenas R base, tidyverse, tidymodels e Rcpp.

## Instalação

``` r

# install.packages("devtools")
devtools::install_github("evandeilton/rnp")
```

## Um tour rápido

Toda função analítica retorna um `tibble`, pronto para o pipe.

``` r

rnp_descritiva(airquality$Temp)
#> # A tibble: 1 × 21
#>       n n_validos n_faltantes  soma media mediana  moda desvio variancia   min
#>   <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>     <dbl> <dbl>
#> 1   153       153           0 11916  77.9      79    81   9.47      89.6    56
#> # ℹ 11 more variables: q1 <dbl>, q3 <dbl>, max <dbl>, amplitude <dbl>,
#> #   iqr <dbl>, cv <dbl>, se_media <dbl>, ic_inf <dbl>, ic_sup <dbl>,
#> #   assimetria <dbl>, curtose <dbl>
```

``` r

rnp_ic_media(airquality$Temp)
#> # A tibble: 1 × 7
#>   media erro_padrao limite_inferior limite_superior     n nivel_confianca
#>   <dbl>       <dbl>           <dbl>           <dbl> <dbl>           <dbl>
#> 1  77.9       0.765            76.4            79.4   153            0.95
#> # ℹ 1 more variable: distribuicao <chr>
rnp_teste_t(airquality$Temp, mu = 75)
#> # A tibble: 1 × 10
#>   estatistica    gl p_valor media_x media_y  diff ic_inf ic_sup hipotese_nula
#>         <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>         <dbl>
#> 1        3.77   152  0.0002    77.9      NA  2.88   76.4   79.4            75
#> # ℹ 1 more variable: alternativa <chr>
```

``` r

rnp_regressao(mpg ~ wt + hp, data = mtcars)$coeficientes
#> # A tibble: 3 × 7
#>   termo       estimativa erro_padrao estatistica_t p_valor  ic_inf  ic_sup
#>   <chr>            <dbl>       <dbl>         <dbl>   <dbl>   <dbl>   <dbl>
#> 1 (Intercept)    37.2          1.60          23.3   0      34.0    40.5   
#> 2 wt             -3.88         0.633         -6.13  0      -5.17   -2.58  
#> 3 hp             -0.0318       0.009         -3.52  0.0015 -0.0502 -0.0133
```

``` r

rnp_grafico_dispersao(mtcars, x = "wt", y = "mpg")
```

![Dispersão de consumo versus peso](rnp_files/figure-html/grafico-1.png)

## Por onde continuar

O pacote acompanha seis tutoriais que seguem a progressão típica de um
curso de estatística, com dados reais e ênfase na interpretação dos
resultados:

1.  **Estatística descritiva e análise exploratória**
2.  **Probabilidade, distribuições e os teoremas fundamentais** (Bayes,
    LGN, TCL)
3.  **Inferência estatística** (p-valor, IC, bootstrap, poder)
4.  **Regressão linear e modelagem** (projeção, pressupostos,
    regularização)
5.  **Análise multivariada** (PCA, agrupamento, LDA, Hotelling)
6.  **Dados categóricos e métodos não-paramétricos**

``` r

browseVignettes("rnp")
```

## Reportar problemas

Bugs e sugestões de novas funções são bem-vindos nas
[issues](https://github.com/evandeilton/rnp/issues) do projeto.
