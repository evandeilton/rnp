# 2. Probabilidade, distribuições e os teoremas fundamentais

Em engenharia, a probabilidade modela a incerteza de falhas, defeitos e
variações de processo (Montgomery and Runger 2021). Esta vinheta vai dos
fundamentos — axiomas, probabilidade condicional, Teorema de Bayes — às
distribuições mais usadas em confiabilidade e controle de qualidade,
fechando com os teoremas-limite.

## Axiomas e regras

Um experimento aleatório tem um **espaço amostral** $`S`$. A
probabilidade de um evento $`A`$ satisfaz os axiomas de Kolmogorov:

``` math
0 \le P(A) \le 1, \qquad P(S) = 1, \qquad
  P(A \cup B) = P(A) + P(B) \ \text{ se } A \cap B = \varnothing.
```

A **probabilidade condicional** e a **regra da multiplicação** são

``` math
P(A \mid B) = \frac{P(A \cap B)}{P(B)}, \qquad
  P(A \cap B) = P(A \mid B)\,P(B).
```

Dois eventos são **independentes** quando $`P(A \cap B) = P(A)\,P(B)`$.

### Aplicação: confiabilidade de sistemas

Considere dois componentes independentes, cada um com confiabilidade
$`0{,}9`$. Em um sistema em **série**, ambos precisam funcionar; em
**paralelo** (redundância), basta um:

``` math
R_{\text{série}} = \prod_i R_i = 0{,}9 \times 0{,}9 = 0{,}81, \qquad
  R_{\text{paralelo}} = 1 - \prod_i (1 - R_i) = 1 - 0{,}1^2 = 0{,}99.
```

A redundância eleva a confiabilidade de 0,81 para 0,99 — o cálculo
direto da independência justifica decisões de projeto.

## Probabilidade total e Teorema de Bayes

Quando o espaço se particiona em causas $`A_1,\dots,A_k`$, a
**probabilidade total** de um evento $`B`$ é
$`P(B) = \sum_i P(B \mid A_i)\,P(A_i)`$, e o **Teorema de Bayes**
inverte a relação:

``` math
P(A_i \mid B) = \frac{P(B \mid A_i)\,P(A_i)}{\sum_j P(B \mid A_j)\,P(A_j)}.
```

Exemplo clássico de manufatura: três máquinas produzem 20%, 30% e 50%
das peças, com taxas de defeito de 5%, 3% e 1%. Uma peça saiu
**defeituosa** — de qual máquina ela provavelmente veio?

``` r

rnp_bayes(
  priori          = c(M1 = 0.20, M2 = 0.30, M3 = 0.50),
  verossimilhanca = c(0.05, 0.03, 0.01)
)
#> # A tibble: 3 × 5
#>   hipotese priori verossimilhanca conjunta posteriori
#>   <chr>     <dbl>           <dbl>    <dbl>      <dbl>
#> 1 M1          0.2            0.05    0.01       0.417
#> 2 M2          0.3            0.03    0.009      0.375
#> 3 M3          0.5            0.01    0.005      0.208
```

A probabilidade total de defeito é $`0{,}024`$ (a soma da coluna
`conjunta`). Dado o defeito, a máquina **M1** é a origem mais provável
(42%), apesar de produzir só 20% das peças — porque sua taxa de defeito
é a maior. Bayes rastreia o defeito até a fonte.

## Distribuições para contagens: Poisson

O número de defeitos por unidade de produto (falhas num fio, partículas
num wafer) segue tipicamente a **Poisson**, com
$`P(X = x) = e^{-\lambda}\lambda^x/x!`$ e a propriedade marcante
$`E[X] = \operatorname{Var}[X] = \lambda`$.

``` r

rnp_esperanca_var("pois", lambda = 4)
#> # A tibble: 1 × 4
#>   distribuicao esperanca variancia desvio
#>   <chr>            <dbl>     <dbl>  <dbl>
#> 1 pois                 4         4      2
```

Para um processo com $`\lambda = 4`$ defeitos por unidade, a
probabilidade de no máximo 2 defeitos e de pelo menos 1 são:

``` r

rnp_distribuicao_poisson("p", q = 2, lambda = 4)        # P(X <= 2)
#> [1] 0.2381033
1 - rnp_distribuicao_poisson("p", q = 0, lambda = 4)    # P(X >= 1)
#> [1] 0.9816844
```

Apenas 24% das unidades têm 2 defeitos ou menos, e 98% têm ao menos um —
um processo que precisa de melhoria.

## Distribuições para tempo de vida: exponencial e Weibull

O tempo até a falha de um componente costuma ser modelado pela
**exponencial**, cuja densidade é $`f(t) = \lambda e^{-\lambda t}`$ e a
confiabilidade $`R(t) = P(T > t) = e^{-\lambda t}`$. Para um tempo médio
entre falhas (MTBF) de 1000 h, $`\lambda = 1/1000`$:

``` r

1 - rnp_distribuicao_exponencial("p", q = 1500, taxa = 1/1000)   # P(T > 1500)
#> [1] 0.2231302
```

Há 22% de chance de o componente ultrapassar 1500 h. A exponencial é
**sem memória** ($`P(T > s+t \mid T > s) = P(T > t)`$): um componente
“não envelhece”, hipótese válida apenas para falhas puramente
aleatórias.

Para modelar **desgaste**, a **Weibull** é mais realista, pois sua taxa
de falha varia no tempo: $`R(t) = \exp\!\big[-(t/\delta)^\beta\big]`$.
Com forma $`\beta = 2`$ (taxa de falha crescente, típica de desgaste) e
escala $`\delta = 1000`$:

``` r

1 - rnp_distribuicao_weibull("p", q = 800, forma = 2, escala = 1000)  # R(800)
#> [1] 0.5272924
```

A confiabilidade em 800 h é de 53%. O parâmetro de forma distingue os
regimes: $`\beta < 1`$ (mortalidade infantil), $`\beta = 1`$ (falhas
aleatórias, equivale à exponencial) e $`\beta > 1`$ (desgaste) — a
“curva da banheira” da confiabilidade.

``` r

rnp_grafico_distribuicao("weibull", shape = 2, scale = 1000)
```

![Densidade da
Weibull](v02-probabilidade_files/figure-html/grafico-1.png)

## Lei dos Grandes Números

Estimativas de engenharia melhoram com mais dados. A LGN garante que a
média amostral converge para a média verdadeira, $`\bar{X}_n \to \mu`$:

``` r

rnp_lei_grandes_numeros(function(n) rexp(n, rate = 1/1000), media_teorica = 1000)
```

![Convergência da média
amostral](v02-probabilidade_files/figure-html/lgn-1.png)

A vida média estimada de uma amostra de componentes estabiliza em torno
do MTBF verdadeiro conforme o número de ensaios cresce.

## Teorema Central do Limite

O TCL é a razão de a Normal aparecer em tantos contextos de engenharia:
a média de muitas medições (ou a soma de muitos erros pequenos) é
aproximadamente Normal, *qualquer que seja* a distribuição de origem,

``` math
\frac{\bar{X}_n - \mu}{\sigma/\sqrt{n}} \xrightarrow{\;d\;} N(0,1).
```

``` r

rnp_tcl_simulacao(function(n) rexp(n), n = 30, n_amostras = 2000)
```

![Demonstração do Teorema Central do
Limite](v02-probabilidade_files/figure-html/tcl-1.png)

Partindo de tempos de falha exponenciais (fortemente assimétricos), o
histograma das médias adere à Normal. É esse resultado que sustenta os
intervalos de confiança e as cartas de controle da próxima vinheta.

## Da probabilidade à inferência

Os teoremas-limite abrem a porta da **inferência**: usar uma amostra
para estimar parâmetros desconhecidos da população (Montgomery and
Runger 2021). Retomamos as 100 medições de Michelson (`morley`).

``` r

v <- morley$Speed     # velocidade da luz - 299000 (km/s)
```

### Estimação pontual

Um **estimador** é uma função da amostra que aponta um valor para o
parâmetro. Um bom estimador é *não-viesado* ($`E[\hat\theta] = \theta`$)
e *eficiente* (variância mínima). O método de **máxima verossimilhança**
escolhe os parâmetros que tornam os dados observados mais prováveis:

``` r

ll <- function(th) sum(dnorm(v, th[1], th[2], log = TRUE))
rnp_emv(ll, inicio = c(800, 80), nomes = c("media", "desvio"))$estimativas
#> # A tibble: 2 × 6
#>   parametro estimativa erro_padrao     z ic_inf ic_sup
#>   <chr>          <dbl>       <dbl> <dbl>  <dbl>  <dbl>
#> 1 media          852.         7.86 108.   837.   868. 
#> 2 desvio          78.6        5.56  14.1   67.7   89.5
```

A média e o desvio estimados ($`\hat\mu = 852{,}4`$,
$`\hat\sigma = 78{,}6`$) vêm com seus erros-padrão, obtidos da
informação de Fisher.

### Intervalos de confiança

Uma estimativa pontual não comunica a incerteza; o **intervalo de
confiança** sim. Para a média de uma Normal com variância desconhecida
(usando a distribuição $`t`$):

``` math
\bar{x} \pm t_{n-1,\,\alpha/2}\,\frac{s}{\sqrt{n}}.
```

``` r

rnp_ic_media(v)
#> # A tibble: 1 × 7
#>   media erro_padrao limite_inferior limite_superior     n nivel_confianca
#>   <dbl>       <dbl>           <dbl>           <dbl> <dbl>           <dbl>
#> 1  852.        7.90            837.            868.   100            0.95
#> # ℹ 1 more variable: distribuicao <chr>
```

O IC de 95% é $`[836{,}7;\ 868{,}1]`$ km/s (acima de 299000). O valor
moderno, codificado, é $`792{,}458`$ — **fora** do intervalo, o que já
sinaliza um erro sistemático. Há ICs para outros parâmetros:

``` r

rnp_ic_variancia(v)                         # variância (qui-quadrado)
#> # A tibble: 1 × 5
#>   variancia limite_inferior limite_superior     n    gl
#>       <dbl>           <dbl>           <dbl> <int> <int>
#> 1     6243.           4812.           8424.   100    99
rnp_ic_proporcao(12, 200, method = "wilson")  # proporção: 12 defeitos em 200
#> # A tibble: 1 × 5
#>   proporcao limite_inferior limite_superior metodo     n
#>       <dbl>           <dbl>           <dbl> <chr>  <dbl>
#> 1      0.06          0.0347           0.102 wilson   200
```

### Testes de hipóteses

Um teste confronta uma afirmação ($`H_0`$) com os dados. O procedimento
de Montgomery: formular $`H_0`$ e $`H_1`$, calcular uma estatística de
teste, e decidir pelo p-valor, ciente dos erros tipo I ($`\alpha`$,
rejeitar $`H_0`$ verdadeira) e tipo II ($`\beta`$). Michelson estava
**enviesado**? Testamos $`H_0\!:\mu = 792{,}458`$ (o valor moderno) com
a estatística $`t = (\bar{x} - \mu_0)/(s/\sqrt{n})`$:

``` r

rnp_teste_t(v, mu = 792.458)
#> # A tibble: 1 × 10
#>   estatistica    gl p_valor media_x media_y  diff ic_inf ic_sup hipotese_nula
#>         <dbl> <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>  <dbl>         <dbl>
#> 1        7.59    99       0    852.      NA  59.9   837.   868.          792.
#> # ℹ 1 more variable: alternativa <chr>
```

Com $`t = 7{,}59`$ e $`p < 0{,}0001`$, rejeita-se $`H_0`$: as medições
de 1879 tinham um **viés sistemático de ~60 km/s** — um erro de
exatidão, não de acaso. Para proporções (uma linha que produz 6% de
defeitos atende à meta de no máximo 10%?):

``` r

rnp_teste_z_proporcao(12, 200, p0 = 0.10)
#> # A tibble: 1 × 9
#>   estatistica p_valor proporcao    p0 erro_padrao ic_inf ic_sup     n
#>         <dbl>   <dbl>     <dbl> <dbl>       <dbl>  <dbl>  <dbl> <dbl>
#> 1       -1.89  0.0593      0.06   0.1      0.0212 0.0271 0.0929   200
#> # ℹ 1 more variable: alternativa <chr>
```

O p-valor de $`0{,}06`$ não permite, a 5%, concluir que a taxa real está
abaixo de 10% — a amostra é pequena demais para essa decisão.

### Planejamento: poder e tamanho de amostra

Quantas medições seriam necessárias para detectar um efeito médio
($`d = 0{,}5`$) com 80% de poder? Planejar isso *antes* de coletar evita
estudos inconclusivos:

``` r

rnp_tamanho_amostra_teste(efeito = 0.5, poder = 0.8, tipo = "uma")
#> # A tibble: 1 × 5
#>   efeito poder_alvo alpha     n poder_obtido
#>    <dbl>      <dbl> <dbl> <int>        <dbl>
#> 1    0.5        0.8  0.05    34        0.808
```

## Síntese

| Fenômeno / objetivo | Ferramenta `rnp` | Conceito |
|----|----|----|
| Defeitos por unidade | `rnp_distribuicao_poisson` | Poisson |
| Tempo de vida | `rnp_distribuicao_exponencial/_weibull` | confiabilidade |
| Estimar parâmetro | `rnp_emv` | máxima verossimilhança |
| Quantificar incerteza | `rnp_ic_media/_variancia/_proporcao` | intervalo de confiança |
| Decidir sobre afirmação | `rnp_teste_t`, `rnp_teste_z_proporcao` | teste de hipótese |
| Planejar o estudo | `rnp_tamanho_amostra_teste` | poder |

Da probabilidade que descreve o mecanismo à inferência que decide a
partir de dados, o caminho é contínuo — e os teoremas-limite são a
ponte.

## Exercícios

Resolva computacionalmente com o `rnp`. Use os conjuntos indicados
(`morley`, `mtcars`, `trees`, `faithful`).

1.  Calcule $`P(Z \le 2{,}5)`$ e o quantil $`z_{0{,}975}`$ da Normal
    padrão (`rnp_distribuicao_normal`).
2.  Em 10 ensaios com $`p = 0{,}2`$, obtenha $`P(X = 3)`$ e
    $`P(X \le 3)`$ (`rnp_distribuicao_binomial`).
3.  Para uma Poisson com $`\lambda = 3`$, calcule $`P(X \ge 2)`$
    (`rnp_distribuicao_poisson`).
4.  Um sistema tem 3 componentes independentes com $`R = 0{,}95`$.
    Calcule a confiabilidade em série e em paralelo (regra da
    multiplicação).
5.  Dois fornecedores entregam 60% e 40% das peças, com 2% e 5% de
    defeito. Dada uma peça defeituosa, qual a probabilidade de cada
    fornecedor? (`rnp_bayes`).
6.  Um componente tem MTBF de 500 h (exponencial). Calcule
    $`P(T > 200)`$ e verifique a propriedade sem memória
    (`rnp_distribuicao_exponencial`).
7.  Para uma Weibull com forma $`1{,}5`$ e escala $`2000`$, obtenha a
    confiabilidade $`R(1000)`$ (`rnp_distribuicao_weibull`).
8.  Obtenha $`E[X]`$ e $`\operatorname{Var}[X]`$ de uma Poisson com
    $`\lambda = 5`$ (`rnp_esperanca_var`).
9.  Demonstre o TCL partindo de uma distribuição uniforme
    (`rnp_tcl_simulacao`).
10. Construa o IC de 95% para a média de `mtcars$mpg` (`rnp_ic_media`).
11. Teste se a média de `mtcars$mpg` difere de 22 km/L (`rnp_teste_t`).
12. Calcule o IC de 95% para a variância de `mtcars$wt`
    (`rnp_ic_variancia`).
13. Estime o IC para a proporção de 18 defeituosos em 250 peças
    (`rnp_ic_proporcao`, método de Wilson).
14. Teste se a proporção 18/250 difere de 10% (`rnp_teste_z_proporcao`).
15. Ajuste uma distribuição exponencial a `faithful$eruptions` e avalie
    o ajuste (`rnp_ajuste_distribuicao`).
16. Determine o tamanho de amostra para detectar $`d = 0{,}4`$ com poder
    de 0,90 (`rnp_tamanho_amostra_teste`).
17. Estime $`\int_0^1 e^{-x^2}\,dx`$ por Monte Carlo
    (`rnp_monte_carlo`).
18. Aplique o Teorema de Bayes (forma de partição) a um teste
    diagnóstico com prevalência 2%, sensibilidade 95% e especificidade
    90% (`rnp_bayes`).

## Referências

Montgomery, Douglas C., and George C. Runger. 2021. *Estatística
Aplicada e Probabilidade Para Engenheiros*. 7th ed. LTC.
