# Introdução ao rnp

## O que é o `rnp`

O `rnp`, do projeto **R NA PRÁTICA**, reúne mais de **230 funções** de
estatística e ciência de dados em um único pacote, com documentação e
mensagens em português. A cobertura vai dos temas dos primeiros anos de
um curso de estatística até tópicos aplicados:

- **Descritiva e exploratória** — medidas-resumo, momentos, frequências,
  gráficos;
- **Probabilidade** — distribuições, Bayes, Lei dos Grandes Números,
  Teorema Central do Limite, simulação;
- **Inferência** — estimação, intervalos de confiança, testes de
  hipóteses, *bootstrap*, poder e tamanho de amostra;
- **Regressão e modelagem** — linear, regularizada, robusta, logística,
  GLM, modelos mistos e aditivos;
- **Multivariada** — componentes principais, agrupamento, discriminante,
  fatorial;
- **Séries temporais** — decomposição, ARIMA/SARIMA, GARCH, previsão;
- **Sobrevivência** — Kaplan-Meier, log-rank, Cox e modelos
  paramétricos;
- **Aprendizado de máquina** — fluxo com `tidymodels` e avaliação de
  modelos.

## Três escolhas de projeto

- **Saídas *tidy*** — as funções analíticas devolvem `tibble` (ou um
  objeto `rnp_resultado`, descrito adiante) e os gráficos usam
  `ggplot2`. O resultado já entra no pipe e em outras etapas da análise
  sem conversões.
- **Desempenho** — as rotinas numericamente intensivas (distâncias,
  álgebra matricial, reamostragem) são escritas em C++ com
  `Rcpp`/`RcppArmadillo` e conferidas contra as funções equivalentes do
  R.
- **Poucas dependências** — o pacote se apoia apenas no R base, no
  `tidyverse`, no `tidymodels` e no `Rcpp`, evitando uma cadeia extensa
  de pacotes externos.

## Instalação

``` r

# install.packages("devtools")
devtools::install_github("evandeilton/rnp")
```

## Um tour rápido

As funções seguem a convenção `rnp_<área>_<método>()`. Comecemos pela
descritiva de uma variável — a saída é um `tibble`:

``` r

rnp_descritiva(rnp_concreto$resistencia)
#> # A tibble: 1 × 21
#>       n n_validos n_faltantes  soma media mediana  moda desvio variancia   min
#>   <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>     <dbl> <dbl>
#> 1    90        90           0 2331.  25.9    25.8  27.2   5.29      28.0  15.6
#> # ℹ 11 more variables: q1 <dbl>, q3 <dbl>, max <dbl>, amplitude <dbl>,
#> #   iqr <dbl>, cv <dbl>, se_media <dbl>, ic_inf <dbl>, ic_sup <dbl>,
#> #   assimetria <dbl>, curtose <dbl>
```

Os modelos seguem a mesma lógica. Ajustamos uma regressão da resistência
do concreto sobre o tempo de cura e o tipo de cimento. O resultado tem
uma impressão organizada, que resume cada componente:

``` r

ajuste <- rnp_regressao(resistencia ~ dias_cura + tipo_cimento,
                        data = rnp_concreto)
ajuste
#> # A tibble: 4 × 7
#>   termo             estimativa erro_padrao estatistica_t p_valor ic_inf ic_sup
#>   <chr>                  <dbl>       <dbl>         <dbl>   <dbl>  <dbl>  <dbl>
#> 1 (Intercept)           17.8        0.689          25.8   0      16.4   19.2  
#> 2 dias_cura              0.430      0.0309         13.9   0       0.368  0.491
#> 3 tipo_cimentoCP-IV     -1.53       0.662          -2.31  0.0235 -2.84  -0.211
#> 4 tipo_cimentoCP-V       4.82       0.662           7.28  0       3.50   6.14
#> # A tibble: 1 × 7
#>      r2 r2_ajustado f_statistic f_pvalor sigma gl_residuos  nobs
#>   <dbl>       <dbl>       <dbl>    <dbl> <dbl>       <int> <int>
#> 1 0.773       0.765        97.6        0  2.56          86    90
```

Cada dia de cura adiciona cerca de 0,43 MPa à resistência, e o cimento
CP-V rende mais que os demais — o modelo explica 77% da variação
observada.

### Conversa com o `broom`

Os resultados do `rnp` implementam os genéricos
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) e
[`glance()`](https://generics.r-lib.org/reference/glance.html) do
`broom`. Assim, a tabela de coeficientes e o resumo do ajuste saem no
formato padrão do *tidymodels*, prontos para comparar modelos ou montar
relatórios:

``` r

tidy(ajuste)    # um termo por linha
#> # A tibble: 4 × 7
#>   termo             estimativa erro_padrao estatistica_t p_valor ic_inf ic_sup
#>   <chr>                  <dbl>       <dbl>         <dbl>   <dbl>  <dbl>  <dbl>
#> 1 (Intercept)           17.8        0.689          25.8   0      16.4   19.2  
#> 2 dias_cura              0.430      0.0309         13.9   0       0.368  0.491
#> 3 tipo_cimentoCP-IV     -1.53       0.662          -2.31  0.0235 -2.84  -0.211
#> 4 tipo_cimentoCP-V       4.82       0.662           7.28  0       3.50   6.14
glance(ajuste)  # uma linha de medidas-resumo do ajuste
#> # A tibble: 1 × 7
#>      r2 r2_ajustado f_statistic f_pvalor sigma gl_residuos  nobs
#>   <dbl>       <dbl>       <dbl>    <dbl> <dbl>       <int> <int>
#> 1 0.773       0.765        97.6        0  2.56          86    90
```

### Inferência e gráficos

``` r

rnp_ic_media(rnp_concreto$resistencia)
#> # A tibble: 1 × 7
#>   media erro_padrao limite_inferior limite_superior     n nivel_confianca
#>   <dbl>       <dbl>           <dbl>           <dbl> <dbl>           <dbl>
#> 1  25.9       0.558            24.8            27.0    90            0.95
#> # ℹ 1 more variable: distribuicao <chr>
rnp_teste_t(rnp_concreto$resistencia, mu = 30)
#> # A tibble: 1 × 10
#>   estatistica    gl p_valor media_x media_y  diff ic_inf ic_sup hipotese_nula
#>         <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>         <dbl>
#> 1       -7.35    89       0    25.9      NA -4.10   24.8   27.0            30
#> # ℹ 1 more variable: alternativa <chr>
```

``` r

rnp_grafico_dispersao(rnp_concreto, x = "dias_cura", y = "resistencia")
```

![Dispersão de resistência versus dias de
cura](rnp_files/figure-html/grafico-1.png)

## Conjuntos de dados didáticos

O pacote traz três conjuntos simulados, pensados para exercitar
diferentes famílias de métodos:

| Conjunto | Conteúdo | Métodos |
|----|----|----|
| `rnp_concreto` | resistência (MPa) por cura e tipo de cimento | descritiva, ANOVA, regressão |
| `rnp_defeitos` | contagem de defeitos por turno e máquina | Poisson, dados categóricos |
| `rnp_vida_util` | tempo até a falha com censura | sobrevivência, confiabilidade |

Por exemplo, a ANOVA confirma que o tipo de cimento afeta a resistência
média:

``` r

rnp_anova(resistencia ~ tipo_cimento, data = rnp_concreto)$anova
#> # A tibble: 3 × 6
#>   fonte            gl soma_quadrados media_quadrados estatistica_F p_valor
#>   <chr>         <dbl>          <dbl>           <dbl>         <dbl>   <dbl>
#> 1 "g          "     2           658.           329.           15.6       0
#> 2 "Residuals  "    87          1831.            21.0          NA        NA
#> 3 "total"          89          2489.            NA            NA        NA
```

## Como ler os resultados

Muitas funções devolvem um objeto da classe `rnp_resultado`. Ele é, na
prática, uma lista de componentes (tabelas, escalares, gráficos, o
modelo ajustado), com duas conveniências: a impressão resumida vista
acima e o acesso direto a cada parte pelo `$`. Para extrair a tabela de
coeficientes, por exemplo:

``` r

ajuste$coeficientes
#> # A tibble: 4 × 7
#>   termo             estimativa erro_padrao estatistica_t p_valor ic_inf ic_sup
#>   <chr>                  <dbl>       <dbl>         <dbl>   <dbl>  <dbl>  <dbl>
#> 1 (Intercept)           17.8        0.689          25.8   0      16.4   19.2  
#> 2 dias_cura              0.430      0.0309         13.9   0       0.368  0.491
#> 3 tipo_cimentoCP-IV     -1.53       0.662          -2.31  0.0235 -2.84  -0.211
#> 4 tipo_cimentoCP-V       4.82       0.662           7.28  0       3.50   6.14
```

Em resumo: imprima o objeto para uma visão geral, use `$` para um
componente específico e
[`tidy()`](https://generics.r-lib.org/reference/tidy.html)/[`glance()`](https://generics.r-lib.org/reference/glance.html)
para integrar com o ecossistema *tidymodels*.

## Por onde continuar

O pacote acompanha onze tutoriais que seguem a progressão típica de um
curso de estatística, com ênfase na interpretação dos resultados:

1.  Estatística descritiva e análise exploratória
2.  Probabilidade, distribuições e os teoremas fundamentais (Bayes, LGN,
    TCL)
3.  Inferência estatística (p-valor, IC, *bootstrap*, poder)
4.  Regressão linear e modelagem (pressupostos, diagnóstico,
    regularização)
5.  Análise multivariada (PCA, agrupamento, LDA, Hotelling)
6.  Dados categóricos e métodos não-paramétricos
7.  Análise de sobrevivência (Kaplan-Meier, Cox)
8.  Séries temporais: modelos ARIMA e SARIMA
9.  Modelos lineares generalizados e extensões
10. Aprendizado de máquina com `tidymodels`
11. Avaliação de modelos preditivos

Há ainda uma **referência rápida** (*cheatsheet*) com as funções por
área e uma coletânea de **soluções comentadas** dos exercícios dos
primeiros capítulos.

``` r

browseVignettes("rnp")
```

## Reportar problemas

Bugs e sugestões de novas funções são bem-vindos nas
[issues](https://github.com/evandeilton/rnp/issues) do projeto.
