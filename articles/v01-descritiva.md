# 1. Estatística descritiva e análise exploratória

A análise descritiva resume um conjunto de dados por meio de poucas
medidas e de gráficos, antes de qualquer modelagem. Mais do que um
preâmbulo, é a etapa em que se reconhece a forma, o centro, a dispersão
e as anomalias de cada variável (Tukey 1977; Bussab and Morettin 2017).
Usamos o conjunto `airquality` — medições diárias reais de qualidade do
ar em Nova York no verão de 1973.

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

As colunas `Ozone` e `Solar.R` têm dados faltantes; `Ozone` perde 37 das
153 observações. Reportar essa lacuna é parte de qualquer análise
honesta.

## Medidas de posição

A **média aritmética** e a **mediana** estimam o “centro”, mas respondem
de formas diferentes a valores extremos:

``` math
\bar{x} = \frac{1}{n}\sum_{i=1}^{n} x_i, \qquad
  \text{med}(x) = x_{(\lceil n/2 \rceil)}.
```

``` r

rnp_descritiva(airquality$Ozone)
#> # A tibble: 1 × 21
#>       n n_validos n_faltantes  soma media mediana  moda desvio variancia   min
#>   <dbl>     <dbl>       <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>     <dbl> <dbl>
#> 1   153       116          37  4887  42.1    31.5    23   33.0     1088.     1
#> # ℹ 11 more variables: q1 <dbl>, q3 <dbl>, max <dbl>, amplitude <dbl>,
#> #   iqr <dbl>, cv <dbl>, se_media <dbl>, ic_inf <dbl>, ic_sup <dbl>,
#> #   assimetria <dbl>, curtose <dbl>
```

Para o ozônio, a média (42.1) supera a mediana (31.5). Esse descompasso
é a assinatura de uma distribuição **assimétrica à direita**: alguns
dias de ozônio muito alto puxam a média, enquanto a mediana, por ser um
quantil, resiste. A regra prática segue daí: em distribuições
assimétricas ou com *outliers*, a mediana descreve melhor o valor típico
— é por isso que se reporta a renda *mediana* de uma população, não a
média.

### As três médias clássicas

Para dados positivos valem três médias, com aplicações distintas:

``` math
\bar{x}_g = \Big(\textstyle\prod_{i=1}^{n} x_i\Big)^{1/n}, \qquad
  \bar{x}_h = \frac{n}{\sum_{i=1}^{n} 1/x_i},
```

e sempre se ordenam por $`\bar{x}_h \le \bar{x}_g \le \bar{x}`$(Bussab
and Morettin 2017).

``` r

rnp_medias(c(2, 8, 32))
#> # A tibble: 4 × 2
#>   tipo       valor
#>   <chr>      <dbl>
#> 1 aritmetica 14   
#> 2 geometrica  8   
#> 3 harmonica   4.57
#> 4 quadratica 19.1
```

A média **geométrica** é a correta para fatores multiplicativos: um
investimento que rende $`+100\%`$ e depois $`-50\%`$ tem média
geométrica nula, não $`+25\%`$. A **harmônica** vale para razões
(velocidade média de um percurso, em km/h).

## Medidas de dispersão

O centro não descreve a variabilidade. A **variância amostral** e o
**desvio-padrão** medem o espalhamento na escala dos dados; o
**coeficiente de variação** mede a dispersão *relativa* e adimensional:

``` math
s^2 = \frac{1}{n-1}\sum_{i=1}^{n}(x_i - \bar{x})^2, \qquad
  \text{CV} = \frac{s}{\bar{x}}.
```

``` r

rnp_descritiva(airquality$Wind)[c("desvio", "iqr", "cv")]
#> # A tibble: 1 × 3
#>   desvio   iqr    cv
#>    <dbl> <dbl> <dbl>
#> 1   3.52   4.1 0.354
```

O vento tem $`\text{CV} \approx 0.35`$, isto é, o desvio-padrão equivale
a cerca de 35% da média. O CV só faz sentido para variáveis de razão com
média positiva — nunca para temperatura em graus Celsius, cujo zero é
arbitrário. O **IQR** $`= Q_3 - Q_1`$ é uma medida de dispersão
*robusta*, imune a extremos.

## Forma da distribuição: assimetria e curtose

Os momentos centrais de ordem 3 e 4 descrevem a forma. Com
$`m_k = \frac{1}{n}\sum (x_i-\bar x)^k`$, definem-se o coeficiente de
**assimetria** e a **curtose em excesso**:

``` math
g_1 = \frac{m_3}{m_2^{3/2}}, \qquad g_2 = \frac{m_4}{m_2^{2}} - 3.
```

``` r

rnp_momentos(airquality$Temp)$resumo
#> # A tibble: 1 × 6
#>   media variancia desvio_padrao assimetria curtose_excesso     n
#>   <dbl>     <dbl>         <dbl>      <dbl>           <dbl> <dbl>
#> 1  77.9      89.6          9.47     -0.374          -0.429   153
```

A temperatura tem $`g_1 = -0.37`$ (quase simétrica, leve cauda à
esquerda) e $`g_2 = -0.43`$ (**platicúrtica**: caudas mais leves que a
Normal). Já o ozônio é fortemente assimétrico ($`g_1 \approx 1{,}24`$) e
**leptocúrtico** ($`g_2 \approx 1{,}29`$), coerente com a presença de
valores extremos. A curtose mede o **peso das caudas** em relação à
Normal ($`g_2 = 0`$), e não o quão “pontuda” é a distribuição — um
equívoco comum.

## Tabelas de frequência

Para uma variável discreta, a tabela de frequências organiza contagens
absolutas e relativas, simples e acumuladas:

``` r

rnp_tabela_frequencia(airquality$Month)
#> # A tibble: 5 × 5
#>   categoria    fa    fr fa_acumulada fr_acumulada
#>   <chr>     <int> <dbl>        <int>        <dbl>
#> 1 5            31 0.203           31        0.203
#> 2 6            30 0.196           61        0.399
#> 3 7            31 0.203           92        0.601
#> 4 8            31 0.203          123        0.804
#> 5 9            30 0.196          153        1
```

Os cinco meses (maio a setembro) têm 30 ou 31 dias, distribuídos de
forma quase uniforme. Para variáveis contínuas, agrupamos em classes; o
número de classes pela regra de Sturges é $`k = 1 + \log_2 n`$(Sturges
1926):

``` r

head(rnp_tabela_classes(airquality$Temp, regra = "sturges"), 3)
#> # A tibble: 3 × 8
#>   classe      lim_inf lim_sup ponto_medio    fa     fr fa_acumulada fr_acumulada
#>   <chr>         <dbl>   <dbl>       <dbl> <int>  <dbl>        <int>        <dbl>
#> 1 [56,60.6]      56      60.6        58.3     8 0.0523            8       0.0523
#> 2 (60.6,65.1]    60.6    65.1        62.8    10 0.0654           18       0.118 
#> 3 (65.1,69.7]    65.1    69.7        67.4    14 0.0915           32       0.209
```

## Detecção de *outliers*

Dois critérios, com filosofias opostas. O de **Tukey** marca como
*outlier* o que cai fora das cercas
$`[\,Q_1 - 1{,}5\,\text{IQR},\; Q_3 + 1{,}5\,\text{IQR}\,]`$; é robusto,
pois usa quantis. O do **escore-z** marca $`|z_i| > k`$, com
$`z_i = (x_i - \bar{x})/s`$; é frágil, porque média e desvio já foram
inflados pelos próprios extremos.

``` r

rnp_outliers(airquality$Ozone, method = "iqr")
#> # A tibble: 2 × 2
#>   indice valor
#>    <int> <int>
#> 1     62   135
#> 2    117   168
```

O critério de Tukey acusa os dias nas posições 62 e 117, com ozônio de
135 e 168 — exatamente os responsáveis pela assimetria observada. Em uma
distribuição tão assimétrica, o critério do IQR é o mais defensável.

## A exploração gráfica

Os números resumem; os gráficos revelam. O histograma exibe a forma, o
boxplot sintetiza os cinco números de Tukey por grupo, e o gráfico Q-Q
confronta os dados com a Normal teórica.

``` r

rnp_grafico_histograma(airquality, "Ozone", bins = 20)
```

![Histograma do ozônio](v01-descritiva_files/figure-html/hist-1.png)

``` r

rnp_grafico_qq(airquality$Ozone)
```

![Gráfico quantil-quantil do
ozônio](v01-descritiva_files/figure-html/qq-1.png)

A curvatura sistemática no Q-Q confirma o afastamento da normalidade já
diagnosticado pelos números — justificativa, mais adiante, para
transformar a variável ou recorrer a métodos robustos.

## Síntese

| Pergunta | Função | Medida |
|----|----|----|
| Qual o centro típico? | `rnp_descritiva`, `rnp_medias` | média, mediana |
| Quanto varia? | `rnp_descritiva` | desvio, IQR, CV |
| Qual a forma? | `rnp_momentos` | $`g_1`$, $`g_2`$ |
| Há anomalias? | `rnp_outliers` | cercas de Tukey |
| Como é a distribuição? | `rnp_grafico_*` | histograma, boxplot, Q-Q |

Entender *por que* a média amostral se comporta como observamos conduz à
**probabilidade**, tema da próxima vinheta.

## Referências

Bussab, Wilton O., and Pedro A. Morettin. 2017. *Estatística básica*.
9th ed. Saraiva.

Sturges, Herbert A. 1926. “The Choice of a Class Interval.” *Journal of
the American Statistical Association* 21 (153): 65–66.

Tukey, John W. 1977. *Exploratory Data Analysis*. Addison-Wesley.
