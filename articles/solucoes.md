# Soluções comentadas dos exercícios (capítulos 1 e 2)

Este gabarito resolve, com o `rnp`, os exercícios das vinhetas **1
(Descritiva e exploratória)** e **2 (Probabilidade e teoremas
fundamentais)** — os dois capítulos introdutórios, alinhados a
Montgomery and Runger (2021). Cada solução traz o código e uma leitura
curta do resultado. Os números são calculados na compilação, de modo que
o que se lê aqui é exatamente o que o pacote produz.

## Capítulo 1 — Estatística descritiva

``` r

d <- mtcars
```

**1. Média, mediana e desvio-padrão de `mpg`.**

``` r

rnp_descritiva(d$mpg)[, c("media", "mediana", "desvio")]
#> # A tibble: 1 × 3
#>   media mediana desvio
#>   <dbl>   <dbl>  <dbl>
#> 1  20.1    19.2   6.03
```

A média (20.1) supera ligeiramente a mediana, primeiro sinal de leve
assimetria à direita.

**2. Assimetria e curtose de `mpg` ($`g_1`$, $`g_2`$).**

``` r

rnp_momentos(d$mpg)$resumo[, c("assimetria", "curtose_excesso")]
#> # A tibble: 1 × 2
#>   assimetria curtose_excesso
#>        <dbl>           <dbl>
#> 1      0.640          -0.200
```

$`g_1 > 0`$ confirma a cauda à direita; a curtose próxima da Normal
indica picos e caudas sem excessos.

**3. Coeficiente de variação: `mpg` vs `hp`.**

``` r

c(mpg = rnp_descritiva(d$mpg)$cv, hp = rnp_descritiva(d$hp)$cv)
#>    mpg     hp 
#> 0.3000 0.4674
```

A potência (`hp`) tem CV maior: varia mais em termos relativos que o
consumo.

**4. As quatro médias de `c(2, 4, 8, 16)`.**

``` r

rnp_medias(c(2, 4, 8, 16))
#> # A tibble: 4 × 2
#>   tipo       valor
#>   <chr>      <dbl>
#> 1 aritmetica  7.5 
#> 2 geometrica  5.66
#> 3 harmonica   4.27
#> 4 quadratica  9.22
```

Vale a desigualdade
$`\text{harmônica} \le \text{geométrica} \le \text{aritmética}
\le \text{quadrática}`$.

**5. Tabela de frequências de `cyl`.**

``` r

rnp_tabela_frequencia(d$cyl)
#> # A tibble: 3 × 5
#>   categoria    fa    fr fa_acumulada fr_acumulada
#>   <chr>     <int> <dbl>        <int>        <dbl>
#> 1 4            11 0.344           11        0.344
#> 2 6             7 0.219           18        0.562
#> 3 8            14 0.438           32        1
```

Predominam os motores de 8 cilindros.

**6. Classes de `mpg` pela regra de Sturges.**

``` r

rnp_tabela_classes(d$mpg)
#> # A tibble: 6 × 8
#>   classe      lim_inf lim_sup ponto_medio    fa     fr fa_acumulada fr_acumulada
#>   <chr>         <dbl>   <dbl>       <dbl> <int>  <dbl>        <int>        <dbl>
#> 1 [10.4,14.3]    10.4    14.3        12.4     4 0.125             4        0.125
#> 2 (14.3,18.2]    14.3    18.2        16.3    10 0.312            14        0.438
#> 3 (18.2,22.1]    18.2    22.2        20.2     9 0.281            23        0.719
#> 4 (22.1,26.1]    22.2    26.1        24.1     4 0.125            27        0.844
#> 5 (26.1,30]      26.1    30.0        28.0     1 0.0312           28        0.875
#> 6 (30,33.9]      30.0    33.9        31.9     4 0.125            32        1
```

**7. Outliers de `hp` pelo critério de Tukey (IQR).**

``` r

rnp_outliers(d$hp, method = "iqr")
#> # A tibble: 1 × 2
#>   indice valor
#>    <int> <dbl>
#> 1     31   335
```

A potência máxima (335 hp) é o único valor além da cerca superior.

**8. `trees$Volume` é Normal?**

``` r

rnp_teste_normalidade(trees$Volume)
#> # A tibble: 1 × 3
#>   estatistica p_valor metodo 
#>         <dbl>   <dbl> <chr>  
#> 1       0.888  0.0036 shapiro
rnp_grafico_qq(trees$Volume)
```

![](solucoes_files/figure-html/c1-8-1.png)

O Shapiro-Wilk não rejeita a Normalidade ao nível de 5%, e os pontos do
gráfico de probabilidade normal seguem a reta de referência.

**9. Box plot de `mpg` por número de cilindros.**

``` r

rnp_grafico_boxplot(d, "mpg", "cyl")
```

![](solucoes_files/figure-html/c1-9-1.png)

O consumo cai de forma monótona com o número de cilindros, e a dispersão
é maior nos motores de 4 cilindros.

**10. Regra empírica (68-95-99,7) para `qsec`.**

``` r

m <- mean(d$qsec); s <- sd(d$qsec)
sapply(1:3, function(k) mean(abs(d$qsec - m) <= k * s))
#> [1] 0.68750 0.96875 1.00000
```

As proporções observadas acompanham de perto 0,68 / 0,95 / 1,00,
compatível com uma distribuição aproximadamente Normal.

**11. Tabela de contingência `cyl` × `gear`.**

``` r

rnp_tabela_contingencia(d$cyl, d$gear)
#> # A tibble: 3 × 5
#>   categoria   `3`   `4`   `5` Total
#>   <chr>     <int> <int> <int> <dbl>
#> 1 4             1     8     2    11
#> 2 6             2     4     1     7
#> 3 8            12     0     2    14
```

**12. Quantas modas em `faithful$waiting`?**

``` r

rnp_grafico_histograma(faithful, "waiting")
```

![](solucoes_files/figure-html/c1-12-1.png)

O histograma é claramente bimodal: erupções curtas e longas formam dois
grupos.

**13. Momentos de `airquality$Wind` (sem NA).**

``` r

rnp_momentos(aq$Wind)$resumo[, c("assimetria", "curtose_excesso")]
#> # A tibble: 1 × 2
#>   assimetria curtose_excesso
#>        <dbl>           <dbl>
#> 1      0.456           0.281
```

Assimetria positiva pequena: a velocidade do vento é levemente
assimétrica à direita.

**14. Estrutura de `airquality`: faltantes.**

``` r

rnp_estrutura(airquality)
#> # A tibble: 6 × 5
#>   variavel classe      n n_faltantes p_faltantes
#>   <chr>    <chr>   <int>       <int>       <dbl>
#> 1 Ozone    integer   153          37      0.242 
#> 2 Solar.R  integer   153           7      0.0458
#> 3 Wind     numeric   153           0      0     
#> 4 Temp     integer   153           0      0     
#> 5 Month    integer   153           0      0     
#> 6 Day      integer   153           0      0
```

`Ozone` e `Solar.R` concentram os valores faltantes.

**15. Qual experimento de `morley` foi o mais preciso?**

``` r

rnp_descritiva_by(morley, "Speed", "Expt")[, c("Expt", "desvio")]
#> # A tibble: 5 × 2
#>    Expt desvio
#>   <int>  <dbl>
#> 1     1  105. 
#> 2     2   61.2
#> 3     3   79.1
#> 4     4   60.0
#> 5     5   54.2
```

O experimento com o menor desvio-padrão é o mais preciso (menor
dispersão das medidas em torno de seu valor central).

## Capítulo 2 — Probabilidade e inferência

**1. $`P(Z \le 2{,}5)`$ e o quantil $`z_{0{,}975}`$.**

``` r

rnp_distribuicao_normal("p", q = 2.5)     # P(Z <= 2,5)
#> [1] 0.9937903
rnp_distribuicao_normal("q", p = 0.975)   # quantil z_{0,975}
#> [1] 1.959964
```

**2. Binomial(10; 0,2): $`P(X = 3)`$ e $`P(X \le 3)`$.**

``` r

rnp_distribuicao_binomial("d", size = 10, prob = 0.2, x = 3)   # P(X = 3)
#> [1] 0.2013266
rnp_distribuicao_binomial("p", size = 10, prob = 0.2, q = 3)   # P(X <= 3)
#> [1] 0.8791261
```

**3. Poisson($`\lambda = 3`$): $`P(X \ge 2)`$.**

``` r

1 - rnp_distribuicao_poisson("p", lambda = 3, q = 1)   # P(X >= 2)
#> [1] 0.8008517
```

**4. Confiabilidade de 3 componentes ($`R = 0{,}95`$): série e
paralelo.**

``` r

R <- 0.95
c(serie = R^3, paralelo = 1 - (1 - R)^3)
#>    serie paralelo 
#> 0.857375 0.999875
```

A redundância em paralelo eleva a confiabilidade acima de cada
componente; a associação em série a reduz, pois exige que todos
funcionem.

**5. Bayes: fornecedores 60%/40%, defeitos 2%/5%.**

``` r

rnp_bayes(priori = c(0.6, 0.4), verossimilhanca = c(0.02, 0.05))
#> # A tibble: 2 × 5
#>   hipotese priori verossimilhanca conjunta posteriori
#>   <chr>     <dbl>           <dbl>    <dbl>      <dbl>
#> 1 H1          0.6            0.02    0.012      0.375
#> 2 H2          0.4            0.05    0.02       0.625
```

Embora o fornecedor 1 produza mais peças, dada uma peça defeituosa o
segundo torna-se o mais provável, por causa da sua taxa de defeito mais
alta.

**6. Exponencial com MTBF 500 h: $`P(T > 200)`$ e falta de memória.**

``` r

taxa <- 1 / 500
p_200 <- 1 - rnp_distribuicao_exponencial("p", taxa = taxa, q = 200)
p_cond <- (1 - rnp_distribuicao_exponencial("p", taxa = taxa, q = 700)) /
          (1 - rnp_distribuicao_exponencial("p", taxa = taxa, q = 500))
c(P_T_maior_200 = p_200, P_condicional = p_cond)
#> P_T_maior_200 P_condicional 
#>       0.67032       0.67032
```

$`P(T > 700 \mid T > 500) = P(T > 200)`$: a exponencial não tem memória.

**7. Weibull(forma 1,5; escala 2000): confiabilidade $`R(1000)`$.**

``` r

1 - rnp_distribuicao_weibull("p", forma = 1.5, escala = 2000, q = 1000)
#> [1] 0.7021885
```

**8. $`E[X]`$ e $`\operatorname{Var}[X]`$ de uma
Poisson($`\lambda = 5`$).**

``` r

rnp_esperanca_var("pois", lambda = 5)
#> # A tibble: 1 × 4
#>   distribuicao esperanca variancia desvio
#>   <chr>            <dbl>     <dbl>  <dbl>
#> 1 pois                 5         5   2.24
```

Na Poisson, média e variância coincidem ($`= \lambda`$).

**9. TCL a partir de uma Uniforme.**

``` r

rnp_tcl_simulacao(function(n) runif(n), n = 30, n_amostras = 1000)
```

![](solucoes_files/figure-html/c2-9-1.png)

Mesmo partindo de uma população uniforme, a distribuição das médias
amostrais se aproxima da Normal.

**10. IC de 95% para a média de `mpg`.**

``` r

rnp_ic_media(mtcars$mpg)
#> # A tibble: 1 × 7
#>   media erro_padrao limite_inferior limite_superior     n nivel_confianca
#>   <dbl>       <dbl>           <dbl>           <dbl> <dbl>           <dbl>
#> 1  20.1        1.07            17.9            22.3    32            0.95
#> # ℹ 1 more variable: distribuicao <chr>
```

**11. A média de `mpg` difere de 22?**

``` r

rnp_teste_t(mtcars$mpg, mu = 22)
#> # A tibble: 1 × 10
#>   estatistica    gl p_valor media_x media_y  diff ic_inf ic_sup hipotese_nula
#>         <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>         <dbl>
#> 1       -1.79    31  0.0829    20.1      NA -1.91   17.9   22.3            22
#> # ℹ 1 more variable: alternativa <chr>
```

O valor-p elevado não permite rejeitar $`H_0: \mu = 22`$.

**12. IC de 95% para a variância de `wt`.**

``` r

rnp_ic_variancia(mtcars$wt)
#> # A tibble: 1 × 5
#>   variancia limite_inferior limite_superior     n    gl
#>       <dbl>           <dbl>           <dbl> <int> <int>
#> 1     0.957           0.615            1.69    32    31
```

**13. IC para a proporção 18/250 (método de Wilson).**

``` r

rnp_ic_proporcao(18, 250, method = "wilson")
#> # A tibble: 1 × 5
#>   proporcao limite_inferior limite_superior metodo     n
#>       <dbl>           <dbl>           <dbl> <chr>  <dbl>
#> 1     0.072           0.046           0.111 wilson   250
```

**14. A proporção 18/250 difere de 10%?**

``` r

rnp_teste_z_proporcao(18, 250, p0 = 0.10)
#> # A tibble: 1 × 9
#>   estatistica p_valor proporcao    p0 erro_padrao ic_inf ic_sup     n
#>         <dbl>   <dbl>     <dbl> <dbl>       <dbl>  <dbl>  <dbl> <dbl>
#> 1       -1.48    0.14     0.072   0.1       0.019   0.04  0.104   250
#> # ℹ 1 more variable: alternativa <chr>
```

A proporção observada (7.2%) fica abaixo de 10%, e o teste indica se a
diferença é estatisticamente significativa.

**15. Ajuste exponencial a `faithful$eruptions`.**

``` r

rnp_ajuste_distribuicao(faithful$eruptions, "exp")$qualidade
#> # A tibble: 1 × 5
#>   log_veross   aic   bic ks_estatistica     n
#>        <dbl> <dbl> <dbl>          <dbl> <int>
#> 1      -612. 1226. 1229.          0.377   272
```

O teste de Kolmogorov-Smirnov rejeita o ajuste: `eruptions` é bimodal,
longe de uma exponencial.

**16. Tamanho de amostra para $`d = 0{,}4`$ com poder 0,90.**

``` r

rnp_tamanho_amostra_teste(efeito = 0.4, poder = 0.90)
#> # A tibble: 1 × 5
#>   efeito poder_alvo alpha     n poder_obtido
#>    <dbl>      <dbl> <dbl> <int>        <dbl>
#> 1    0.4        0.9  0.05   133        0.902
```

**17. $`\int_0^1 e^{-x^2}\,dx`$ por Monte Carlo.**

``` r

rnp_monte_carlo(function(x) exp(-x^2), c(0, 1))
#> # A tibble: 1 × 5
#>   estimativa erro_padrao ic_inf ic_sup     n
#>        <dbl>       <dbl>  <dbl>  <dbl> <int>
#> 1      0.747       0.002  0.743  0.751 10000
```

A estimativa cerca o valor exato ($`\approx 0{,}7468`$) dentro do
intervalo de confiança da simulação.

**18. Bayes em teste diagnóstico (prevalência 2%, sens. 95%, espec.
90%).**

``` r

rnp_bayes(priori = c(0.02, 0.98), verossimilhanca = c(0.95, 1 - 0.90))
#> # A tibble: 2 × 5
#>   hipotese priori verossimilhanca conjunta posteriori
#>   <chr>     <dbl>           <dbl>    <dbl>      <dbl>
#> 1 H1         0.02            0.95    0.019      0.162
#> 2 H2         0.98            0.1     0.098      0.838
```

Apesar da alta sensibilidade, a baixa prevalência faz o valor preditivo
positivo ser modesto — o paradoxo clássico dos testes de rastreamento.

## Referências

Montgomery, Douglas C., and George C. Runger. 2021. *Estatística
Aplicada e Probabilidade Para Engenheiros*. 7th ed. LTC.
