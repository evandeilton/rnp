# 2. Probabilidade, distribuições e os teoremas fundamentais

A probabilidade descreve a incerteza *antes* de observar os dados; a
inferência inverte o sentido, indo dos dados às causas. Esta vinheta
trata dos conceitos que sustentam essa transição: **distribuições**, o
**Teorema de Bayes** e os dois teoremas-limite (Ross 2010; Magalhães and
Lima 2015).

## Distribuições e seus momentos

Uma variável aleatória $`X`$ é descrita por sua distribuição. A
esperança e a variância — primeiro momento e segundo momento central —
resumem centro e dispersão:

``` math
E[X] = \sum_x x\,p(x) \;\; \text{(discreta)}, \qquad
  \operatorname{Var}[X] = E[X^2] - (E[X])^2.
```

As funções `rnp_distribuicao_*` oferecem os quatro verbos usuais: `d`
(densidade/massa), `p` (acumulada), `q` (quantil) e `r` (amostra).

``` r

rnp_distribuicao_normal("p", q = 1.96)   # P(Z <= 1.96)
#> [1] 0.9750021
```

O valor 0.975 é a área acumulada até $`1{,}96`$ na Normal padrão, que
delimita 95% da massa central. Para distribuições nomeadas, a esperança
e a variância saem dos parâmetros:

``` r

rnp_esperanca_var("binom", size = 10, prob = 0.3)
#> # A tibble: 1 × 4
#>   distribuicao esperanca variancia desvio
#>   <chr>            <dbl>     <dbl>  <dbl>
#> 1 binom                3       2.1   1.45
```

Confere com as fórmulas da Binomial, $`E[X] = np = 3`$ e
$`\operatorname{Var}[X] = np(1-p) = 2{,}1`$. Cada distribuição modela um
mecanismo: a Binomial conta sucessos em $`n`$ ensaios; a Poisson,
ocorrências num intervalo; a Exponencial, tempos de espera; a Normal, a
soma de muitos efeitos pequenos.

``` r

rnp_grafico_distribuicao("norm", mean = 0, sd = 1)
```

![Densidade e acumulada da Normal
padrão](v02-probabilidade_files/figure-html/grafico-dist-1.png)

## Teorema de Bayes e a falácia da taxa-base

Para hipóteses $`H_1,\dots,H_k`$ mutuamente exclusivas e exaustivas, e
uma evidência $`E`$, a probabilidade *a posteriori* é

``` math
P(H_i \mid E) = \frac{P(E \mid H_i)\,P(H_i)}{\sum_{j} P(E \mid H_j)\,P(H_j)}.
```

Considere um teste para uma doença rara: prevalência de 1%,
sensibilidade de 99% e especificidade de 95% (logo, 5% de falsos
positivos). Uma pessoa testa positivo — qual a probabilidade de estar
doente?

``` r

rnp_bayes(
  priori          = c(doente = 0.01, sadio = 0.99),
  verossimilhanca = c(0.99, 0.05)
)
#> # A tibble: 2 × 5
#>   hipotese priori verossimilhanca conjunta posteriori
#>   <chr>     <dbl>           <dbl>    <dbl>      <dbl>
#> 1 doente     0.01            0.99   0.0099      0.167
#> 2 sadio      0.99            0.05   0.0495      0.833
```

Apenas **16,7%**. A intuição que aponta “99%” ignora a *taxa-base*: como
99% da população é saudável, seus 5% de falsos positivos superam, em
número, os verdadeiros positivos. Bayes obriga a combinar a evidência (o
teste) com o conhecimento prévio (a prevalência) — um raciocínio que
vale da medicina ao direito.

## Lei dos Grandes Números

A LGN garante que a média amostral converge para a média populacional,
$`\bar{X}_n \to \mu`$, à medida que $`n`$ cresce. Simulando o lançamento
de um dado honesto ($`\mu = 3{,}5`$):

``` r

rnp_lei_grandes_numeros(function(n) sample(1:6, n, TRUE), media_teorica = 3.5)
```

![Convergência da média
amostral](v02-probabilidade_files/figure-html/lgn-1.png)

A média acumulada oscila no início e estabiliza em torno de $`3{,}5`$. É
esse resultado que torna previsíveis, no agregado, fenômenos
individualmente aleatórios.

## Teorema Central do Limite

Se a LGN diz *para onde* a média vai, o TCL diz **como** ela chega lá: a
média amostral padronizada converge em distribuição para a Normal
padrão, *independentemente* da distribuição de origem (com variância
finita),

``` math
\frac{\bar{X}_n - \mu}{\sigma/\sqrt{n}} \xrightarrow{\;d\;} N(0,1).
```

Partindo de uma Exponencial (assimétrica), observamos a média de 2000
amostras de tamanho 30:

``` r

rnp_tcl_simulacao(function(n) rexp(n), n = 30, n_amostras = 2000)
```

![Demonstração do Teorema Central do
Limite](v02-probabilidade_files/figure-html/tcl-1.png)

O histograma das médias adere à curva Normal sobreposta. Esse é o
resultado que autoriza tratar a média amostral como aproximadamente
Normal — base de quase todos os intervalos de confiança da próxima
vinheta.

## Integração por Monte Carlo

Quando a integral é difícil de obter analiticamente, estima-se por
amostragem: para $`U_i \sim \text{Unif}(a,b)`$,

``` math
\int_a^b g(x)\,dx \approx (b - a)\,\frac{1}{n}\sum_{i=1}^{n} g(U_i).
```

``` r

rnp_monte_carlo(function(x) x^2, limites = c(0, 1), n = 1e5)
#> # A tibble: 1 × 5
#>   estimativa erro_padrao ic_inf ic_sup      n
#>        <dbl>       <dbl>  <dbl>  <dbl>  <dbl>
#> 1      0.334      0.0009  0.333  0.336 100000
```

A estimativa cerca o valor exato
$`\int_0^1 x^2\,dx = 1/3 \approx 0{,}3333`$, e o método ainda fornece um
erro-padrão e um intervalo de confiança — ou seja, uma medida da própria
incerteza.

## Ajustando uma distribuição a dados reais

O conjunto `faithful` registra tempos de espera entre erupções do gêiser
Old Faithful. Ajustamos uma Normal por máxima verossimilhança e
avaliamos o ajuste pela estatística de Kolmogorov-Smirnov:

``` r

rnp_ajuste_distribuicao(faithful$waiting, dist = "norm")$qualidade
#> # A tibble: 1 × 5
#>   log_veross   aic   bic ks_estatistica     n
#>        <dbl> <dbl> <dbl>          <dbl> <int>
#> 1     -1095. 2195. 2202.          0.152   272
```

A estatística KS de 0.15 é alta: a espera do Old Faithful é, na verdade,
**bimodal** (erupções curtas e longas), e uma única Normal não captura
isso. O exemplo lembra que ajustar é sempre *confrontar* o modelo com os
dados, nunca confiar cegamente num número.

## Síntese

- **Distribuições** modelam mecanismos geradores; conheça o mecanismo
  antes de escolher a distribuição.
- **Bayes** combina evidência e conhecimento prévio; ignorar a taxa-base
  engana.
- **LGN** garante que médias estabilizam; o **TCL** garante que elas se
  tornam aproximadamente Normais — a fundação da inferência (Casella and
  Berger 2002).

## Referências

Casella, George, and Roger L. Berger. 2002. *Statistical Inference*. 2nd
ed. Duxbury.

Magalhães, Marcos N., and Antonio C. P. Lima. 2015. *Noções de
Probabilidade e Estatística*. 7th ed. Edusp.

Ross, Sheldon M. 2010. *A First Course in Probability*. 8th ed. Pearson.
