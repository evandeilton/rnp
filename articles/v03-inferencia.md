# 3. Inferência estatística

A inferência vai da amostra à população (Casella and Berger 2002;
Bolfarine and Sandoval 1998). Usamos
[`MASS::Pima.tr`](https://rdrr.io/pkg/MASS/man/Pima.tr.html): dados
reais de 200 mulheres da etnia Pima, com medidas clínicas e diagnóstico
de diabetes.

``` r

dados <- MASS::Pima.tr
```

## Estimação pontual por máxima verossimilhança

Dada uma amostra de uma densidade $`f(x;\theta)`$, a **verossimilhança**
e a **log-verossimilhança** são

``` math
L(\theta) = \prod_{i=1}^{n} f(x_i;\theta), \qquad
  \ell(\theta) = \sum_{i=1}^{n} \log f(x_i;\theta),
```

e o estimador de máxima verossimilhança é
$`\hat\theta = \arg\max_\theta \ell(\theta)`$. Para o índice de massa
corporal (IMC), supondo Normal:

``` r

x <- dados$bmi
ll <- function(th) sum(dnorm(x, th[1], th[2], log = TRUE))
rnp_emv(ll, inicio = c(30, 5), nomes = c("media", "desvio"))$estimativas
#> # A tibble: 2 × 6
#>   parametro estimativa erro_padrao     z ic_inf ic_sup
#>   <chr>          <dbl>       <dbl> <dbl>  <dbl>  <dbl>
#> 1 media          32.3        0.432  74.7  31.5   33.2 
#> 2 desvio          6.11       0.306  20.0   5.52   6.71
```

A EMV da média é 32.31. O erro-padrão vem da curvatura de $`\ell`$ no
máximo, medida pela **informação de Fisher**:

``` math
I(\theta) = -E\!\left[\frac{\partial^2 \ell}{\partial \theta^2}\right], \qquad
  \widehat{\operatorname{Var}}(\hat\theta) \approx I(\hat\theta)^{-1}.
```

Quanto mais afilada a log-verossimilhança, mais informação os dados
trazem e menor a incerteza:

``` r

rnp_log_verossimilhanca(function(mu) sum(dnorm(x, mu, sd(x), log = TRUE)),
                        intervalo = c(31, 35))
```

![Log-verossimilhança da média do
IMC](v03-inferencia_files/figure-html/logveross-1.png)

## Intervalo de confiança

Para a média de uma Normal com variância desconhecida, o IC de nível
$`1-\alpha`$ é

``` math
\bar{x} \pm t_{n-1,\,\alpha/2}\,\frac{s}{\sqrt{n}}.
```

``` r

rnp_ic_media(dados$bmi)
#> # A tibble: 1 × 7
#>   media erro_padrao limite_inferior limite_superior     n nivel_confianca
#>   <dbl>       <dbl>           <dbl>           <dbl> <dbl>           <dbl>
#> 1  32.3       0.434            31.5            33.2   200            0.95
#> # ℹ 1 more variable: distribuicao <chr>
```

O IC de 95% é $`[31{,}5;\,33{,}2]`$. A interpretação **frequentista**
correta: se o estudo fosse repetido muitas vezes, 95% dos intervalos
assim construídos conteriam a média verdadeira. A confiança está no
*procedimento*, não neste intervalo particular — não é “95% de
probabilidade de a média estar aqui”.

## Teste de hipóteses e o p-valor

O IMC médio difere de 30 (limiar de obesidade)? A estatística do teste
$`t`$ é $`t = (\bar{x} - \mu_0)/(s/\sqrt{n})`$:

``` r

rnp_teste_t(dados$bmi, mu = 30)
#> # A tibble: 1 × 10
#>   estatistica    gl p_valor media_x media_y  diff ic_inf ic_sup hipotese_nula
#>         <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>         <dbl>
#> 1        5.33   199       0    32.3      NA  2.31   31.5   33.2            30
#> # ℹ 1 more variable: alternativa <chr>
```

Com $`t = 5.33`$ e 199 graus de liberdade, $`p < 0{,}0001`$: rejeita-se
$`H_0\!:\mu = 30`$. O **p-valor** é a probabilidade de observar uma
estatística tão ou mais extrema que a obtida, *supondo $`H_0`$
verdadeira*. Ele **não** é a probabilidade de $`H_0`$ ser verdadeira,
nem mede o tamanho do efeito. Toda decisão convive com dois erros: o
**tipo I** ($`\alpha`$, rejeitar $`H_0`$ verdadeira) e o **tipo II**
($`\beta`$, não rejeitar $`H_0`$ falsa).

## Poder e tamanho de amostra

O **poder** $`1-\beta`$ é a probabilidade de detectar um efeito real.
Quantas observações são necessárias para detectar um efeito médio (d de
Cohen $`= 0{,}5`$) com 80% de poder, em duas amostras?

``` r

rnp_tamanho_amostra_teste(efeito = 0.5, poder = 0.8, tipo = "duas")
#> # A tibble: 1 × 5
#>   efeito poder_alvo alpha     n poder_obtido
#>    <dbl>      <dbl> <dbl> <int>        <dbl>
#> 1    0.5        0.8  0.05    64        0.802
```

São necessárias **64** observações por grupo. A curva de poder mostra o
compromisso entre $`n`$ e a capacidade de detecção:

``` r

rnp_poder_teste(efeito = 0.5, n = 30, tipo = "duas")$grafico
```

![Curva de poder do teste
t](v03-inferencia_files/figure-html/poder-1.png)

Calcular poder e tamanho de amostra *antes* da coleta é parte do bom
planejamento, e ajuda a evitar tanto falsos negativos quanto achados que
não se replicam.

## Bootstrap: inferência sem fórmula fechada

Para a média, o erro-padrão tem forma fechada ($`s/\sqrt{n}`$). Para a
**mediana**, não. O **bootstrap** (Efron and Tibshirani 1993) reamostra
a própria amostra com reposição e observa a variabilidade da
estatística:

``` r

rnp_ic_bootstrap(dados$bmi, estatistica = "mediana", B = 1000, tipo = "percentil")
#> # A tibble: 1 × 5
#>   estimativa limite_inferior limite_superior metodo     conf
#>        <dbl>           <dbl>           <dbl> <chr>     <dbl>
#> 1       32.8            31.6            33.8 percentil  0.95
```

O IC percentil para a mediana do IMC é $`[31{,}6;\,33{,}9]`$, obtido sem
qualquer suposição de normalidade.

## Teste de permutação

Diabéticas e não-diabéticas diferem na glicose? Em vez de confiar no
TCL, o **teste de permutação** constrói a distribuição nula embaralhando
os rótulos dos grupos sob $`H_0`$:

``` r

g1 <- dados$glu[dados$type == "Yes"]
g0 <- dados$glu[dados$type == "No"]
rnp_teste_permutacao(g1, g0, B = 2000)
#> # A tibble: 1 × 4
#>   diff_observada p_valor     B alternativa
#>            <dbl>   <dbl> <dbl> <chr>      
#> 1           32.0       0  2000 bilateral
```

A diferença observada nas médias de glicose é de cerca de **32 mg/dL**
(145 contra 113), com $`p < 0{,}001`$: a associação entre glicose e
diabetes é claramente significativa.

## Síntese

| Conceito | O que é | Erro comum |
|----|----|----|
| Verossimilhança | plausibilidade dos dados sob $`\theta`$ | confundir com probabilidade de $`\theta`$ |
| IC de 95% | 95% dos intervalos cobririam o parâmetro | “95% de chance de estar aqui” |
| p-valor | $`P(\text{dados} \mid H_0)`$ | $`P(H_0 \mid \text{dados})`$ |
| Poder | $`P(\text{detectar efeito real})`$ | ignorá-lo no planejamento |
| Bootstrap | EP por reamostragem | usar só quando há fórmula |

Mais do que calcular cada quantidade, vale entender o que ela significa
— e o que não diz.

## Referências

Bolfarine, Heleno, and Mónica C. Sandoval. 1998. *Introdução à
Inferência Estatística*. SBM.

Casella, George, and Roger L. Berger. 2002. *Statistical Inference*. 2nd
ed. Duxbury.

Efron, Bradley, and Robert J. Tibshirani. 1993. *An Introduction to the
Bootstrap*. Chapman & Hall.
