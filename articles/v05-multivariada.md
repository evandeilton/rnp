# 5. Análise multivariada

A análise multivariada estuda a estrutura *conjunta* de várias variáveis
— correlações, agrupamentos e direções de maior variação. É onde a
álgebra linear (autovalores, autovetores, projeções) vira ferramenta
estatística (Johnson and Wichern 2007; Mingoti 2005). Usamos o clássico
`iris`: quatro medidas de 150 flores de três espécies, coletadas por
Anderson e analisadas por Fisher (1936).

``` r

X <- iris[, 1:4]
rnp_matriz_correlacao(X)$matriz
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length       1.0000     -0.1176       0.8718      0.8179
#> Sepal.Width       -0.1176      1.0000      -0.4284     -0.3661
#> Petal.Length       0.8718     -0.4284       1.0000      0.9629
#> Petal.Width        0.8179     -0.3661       0.9629      1.0000
```

As pétalas (comprimento e largura) têm correlação de $`0{,}96`$ —
carregam informação quase redundante. É essa redundância que a PCA
explora.

## Componentes principais

A PCA encontra direções ortogonais que maximizam a variância. São os
**autovetores** da matriz de covariância $`\Sigma`$, e os
**autovalores** são as variâncias ao longo delas:

``` math
\Sigma v_j = \lambda_j v_j, \qquad
  \text{proporção explicada por } j = \frac{\lambda_j}{\sum_k \lambda_k}.
```

``` r

rnp_pca(X)$variancia
#> # A tibble: 4 × 4
#>   componente variancia percentual acumulada
#>   <chr>          <dbl>      <dbl>     <dbl>
#> 1 PC1           2.92       0.730      0.730
#> 2 PC2           0.914      0.228      0.958
#> 3 PC3           0.147      0.0367     0.995
#> 4 PC4           0.0207     0.0052     1
```

Os dois primeiros componentes explicam **95,8%** da variação
($`72{,}3\%`$ e $`22{,}9\%`$): reduzimos de quatro para duas dimensões
perdendo quase nada. O **biplot** sobrepõe observações (pontos) e
variáveis (vetores):

``` r

rnp_biplot(rnp_pca(X))
```

![Biplot da PCA de
iris](v05-multivariada_files/figure-html/biplot-1.png)

Vetores na mesma direção indicam variáveis correlacionadas; o
comprimento mede o peso da variável nos componentes.

## Agrupamento

O **k-médias** particiona as observações minimizando a soma de quadrados
intragrupo,

``` math
\min_{C_1,\dots,C_k} \sum_{j=1}^{k} \sum_{i \in C_j} \lVert x_i - \mu_j \rVert^2.
```

``` r

km <- rnp_kmeans(X, k = 3)
km$metricas
#> # A tibble: 1 × 5
#>   wss_total between_ss ratio_ss     k  nobs
#>       <dbl>      <dbl>    <dbl> <dbl> <int>
#> 1      139.       457.    0.767     3   150
```

A razão $`\text{between\_ss}/\text{total\_ss} = 0.767`$ indica que 77%
da variação total está *entre* os grupos — uma separação boa.

O **agrupamento hierárquico** não exige fixar $`k`$ de antemão: ele
constrói uma árvore de fusões sucessivas, o **dendrograma**, em que a
altura de cada junção mede a dissimilaridade. Cortá-lo a uma altura
define os grupos.

``` r

ch <- rnp_cluster_hierarquico(X, k = 3)
rnp_grafico_dendrograma(ch)
```

![Dendrograma de agrupamento
hierárquico](v05-multivariada_files/figure-html/dendro-1.png)

O k-medoids (PAM) é uma alternativa robusta a *outliers*, por usar
observações reais como centros em vez de médias.

### Validação pela silhueta

Escolher o número de grupos é delicado. A **silhueta** compara, para
cada ponto, a dissimilaridade ao próprio grupo ($`a_i`$) e ao vizinho
mais próximo ($`b_i`$):

``` math
s(i) = \frac{b_i - a_i}{\max(a_i,\,b_i)} \in [-1, 1].
```

``` r

rnp_silhueta(X, km$clusters$cluster)$media
#> [1] 0.5062
```

A silhueta média de 0.51 indica grupos razoavelmente coesos e separados
— um critério *interno*, que dispensa os rótulos verdadeiros.

## Classificação supervisionada: análise discriminante

Quando os rótulos são conhecidos, a **LDA** busca as combinações
lineares que maximizam a separação entre classes relativa à variação
interna (a razão de Fisher). Ao contrário da PCA, que maximiza a
variância total, a LDA maximiza a *separabilidade*:

``` r

rnp_lda(Species ~ ., iris)$acuracia
#> [1] 0.98
```

Com as quatro medidas, a LDA classifica as três espécies com **98%** de
acerto no conjunto de treino.

## Testes de médias multivariadas

Para comparar o *vetor* de médias de dois grupos, o **$`T^2`$ de
Hotelling** generaliza o teste $`t`$:

``` math
T^2 = \frac{n_1 n_2}{n_1 + n_2}\,(\bar{x}_1 - \bar{x}_2)^\top S_p^{-1} (\bar{x}_1 - \bar{x}_2).
```

``` r

rnp_hotelling(iris[1:50, 1:4], iris[51:100, 1:4])
#> # A tibble: 1 × 5
#>      t2 estatistica_f   gl1   gl2 p_valor
#>   <dbl>         <dbl> <int> <dbl>   <dbl>
#> 1 2581.          625.     4    95       0
```

O $`T^2`$ enorme ($`p < 0{,}001`$) confirma que setosa e versicolor
diferem nas médias. Para mais de dois grupos, a **MANOVA** estende a
ANOVA (estatísticas de Wilks e Pillai):

``` r

rnp_manova(cbind(Sepal.Length, Petal.Length) ~ Species, iris)
#> # A tibble: 2 × 4
#>   teste  estatistica aprox_f p_valor
#>   <chr>        <dbl>   <dbl>   <dbl>
#> 1 Wilks       0.0399   293.        0
#> 2 Pillai      0.988     71.8       0
```

O lambda de Wilks de $`0{,}04`$ (próximo de zero, $`p < 0{,}001`$)
indica forte separação entre as espécies. Fazer um teste multivariado,
em vez de vários univariados, controla o erro tipo I global e aproveita
as correlações entre as respostas.

## Síntese

| Objetivo | Função | Ideia central |
|----|----|----|
| Reduzir dimensão | `rnp_pca`, `rnp_biplot` | autovetores de $`\Sigma`$ |
| Achar grupos | `rnp_kmeans`, `rnp_cluster_hierarquico` | minimizar dissimilaridade interna |
| Validar grupos | `rnp_silhueta` | coesão versus separação |
| Classificar | `rnp_lda` | razão de Fisher |
| Comparar médias | `rnp_hotelling`, `rnp_manova` | generalizam $`t`$ e ANOVA |

Muitos desses métodos repousam sobre a mesma base — autovalores,
autovetores e projeções —, o que ajuda a enxergar as semelhanças entre
eles.

## Exercícios

Resolva com o `rnp`, usando `iris`, `mtcars`, `USArrests` e `swiss`.

1.  Calcule a matriz de correlação de `mtcars` e identifique os pares
    mais associados (`rnp_matriz_correlacao`).
2.  Construa o correlograma correspondente (`rnp_grafico_correlograma`).
3.  Faça a PCA de `USArrests`: quantos componentes explicam 90% da
    variância? (`rnp_pca`).
4.  Construa o biplot da PCA e interprete as direções das variáveis
    (`rnp_biplot`).
5.  Agrupe `USArrests` em $`k = 4`$ por k-médias e avalie a razão de
    separação (`rnp_kmeans`).
6.  Construa o dendrograma do agrupamento hierárquico dos mesmos dados
    (`rnp_grafico_dendrograma`).
7.  Compare a silhueta média para $`k = 2, 3, 4`$ e escolha o melhor
    $`k`$ (`rnp_silhueta`).
8.  Aplique o k-medoids e compare os grupos com os do k-médias
    (`rnp_kmedoids`).
9.  Calcule a matriz de distâncias de Mahalanobis das primeiras 10
    flores de `iris` (`rnp_distancia`).
10. Faça o escalonamento multidimensional (MDS) de uma matriz de
    distâncias e visualize em 2D (`rnp_mds`).
11. Ajuste uma LDA para classificar as espécies de `iris` e obtenha a
    acurácia (`rnp_lda`).
12. Faça uma análise fatorial de `swiss` com 2 fatores
    (`rnp_analise_fatorial`).
13. Teste a igualdade dos vetores de média de duas espécies de `iris`
    pelo $`T^2`$ de Hotelling (`rnp_hotelling`).
14. Aplique a MANOVA comparando as três espécies (`rnp_manova`).
15. Teste a normalidade multivariada das quatro medidas de `iris`
    (`rnp_normalidade_multivariada`).
16. Calcule a correlação canônica entre as medidas de sépala e de pétala
    (`rnp_correlacao_canonica`).

## Referências

Johnson, Richard A., and Dean W. Wichern. 2007. *Applied Multivariate
Statistical Analysis*. 6th ed. Pearson.

Mingoti, Sueli A. 2005. *Análise de Dados Através de métodos de
Estatística Multivariada*. Editora UFMG.
