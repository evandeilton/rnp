# 10. Aprendizado de máquina com tidymodels

O aprendizado de máquina supervisionado busca uma função $`\hat{f}`$ que
prevê bem *dados novos*. O desafio central é a **generalização**: um
modelo flexível pode decorar o ruído do treino (sobreajuste) e falhar
fora dele. As funções `rnp_ml_*` encapsulam o fluxo do `tidymodels`
(Kuhn and Silge 2022) em uma interface enxuta, em português. Usamos
`iris` (classificar a espécie a partir de quatro medidas).

## Partição treino/teste

A regra de ouro: o desempenho deve ser medido em dados **nunca vistos**
no treino. Separamos uma fração para teste, estratificando pela resposta
para preservar as proporções das classes:

``` r

sp <- rnp_ml_particao(iris, prop = 0.75, estrato = "Species")
sp
#> <Training/Testing/Total>
#> <111/39/150>
```

## Pré-processamento (receita)

A `recipe` declara as transformações, *aprendidas no treino* e aplicadas
igualmente ao teste — evitando vazamento de informação:

``` r

rnp_ml_receita(Species ~ ., iris, passos = c("normalizar"))
```

## Ajuste e avaliação

Ajustamos uma árvore de decisão no treino e medimos no teste:

``` r

fit <- rnp_ml_ajustar(rnp_ml_arvore("classificacao"), Species ~ ., sp)
fit$metricas
#> # A tibble: 3 × 4
#>   .metric     .estimator .estimate .config        
#>   <chr>       <chr>          <dbl> <chr>          
#> 1 accuracy    multiclass    0.923  pre0_mod0_post0
#> 2 roc_auc     hand_till     0.942  pre0_mod0_post0
#> 3 brier_class multiclass    0.0771 pre0_mod0_post0
```

A acurácia de teste é de **92%** e a AUC de **0,94** — desempenho
honesto, porque calculado fora da amostra de treino. A **importância das
variáveis** mostra o que o modelo usou:

``` r

rnp_ml_importancia(fit$modelo)
#> # A tibble: 4 × 2
#>   variavel     importancia
#>   <chr>              <dbl>
#> 1 Petal.Width         70.1
#> 2 Petal.Length        66.7
#> 3 Sepal.Length        43.2
#> 4 Sepal.Width         29.7
```

As medidas de **pétala** dominam, coerente com a estrutura conhecida de
`iris`.

## Validação cruzada e comparação de modelos

Uma única partição é ruidosa. A **validação cruzada** $`k`$-fold reparte
os dados em $`k`$ blocos, treina em $`k-1`$ e valida no restante,
repetindo $`k`$ vezes:

``` math
\widehat{\text{Err}}_{\text{CV}} = \frac{1}{k}\sum_{i=1}^{k} L\big(y_i, \hat{f}^{-i}(x_i)\big).
```

Assim comparam-se modelos de forma mais estável:

``` r

cv <- rnp_ml_cv(iris, v = 5, estrato = "Species")
specs <- list(
  arvore_rasa = rnp_ml_arvore("classificacao", profundidade_max = 2),
  arvore_cheia = rnp_ml_arvore("classificacao"))
rnp_ml_comparar(specs, Species ~ ., cv)$tabela
#> # A tibble: 6 × 7
#>   .metric     .estimator   mean     n std_err .config         modelo      
#>   <chr>       <chr>       <dbl> <dbl>   <dbl> <chr>           <chr>       
#> 1 accuracy    multiclass 0.94       5  0.0194 pre0_mod0_post0 arvore_rasa 
#> 2 brier_class multiclass 0.0567     5  0.0181 pre0_mod0_post0 arvore_rasa 
#> 3 roc_auc     hand_till  0.968      5  0.0093 pre0_mod0_post0 arvore_rasa 
#> 4 accuracy    multiclass 0.947      5  0.017  pre0_mod0_post0 arvore_cheia
#> 5 brier_class multiclass 0.0522     5  0.0162 pre0_mod0_post0 arvore_cheia
#> 6 roc_auc     hand_till  0.964      5  0.0105 pre0_mod0_post0 arvore_cheia
```

A acurácia média por validação cruzada (com erro-padrão) permite
escolher entre modelos sem depender da sorte de uma única divisão.

## Tunagem de hiperparâmetros

Hiperparâmetros (como o custo de complexidade da poda) controlam o
equilíbrio viés-variância. A **tunagem** os otimiza por validação
cruzada. Marca-se o parâmetro com
[`hardhat::tune()`](https://hardhat.tidymodels.org/reference/tune.html):

``` r

arvore_tune <- rnp_ml_arvore("classificacao",
                             custo_complexidade = hardhat::tune())
rnp_ml_tunagem(arvore_tune, Species ~ ., cv, grade = 5)$melhores
#> # A tibble: 5 × 7
#>   cost_complexity .metric  .estimator  mean     n std_err .config        
#>             <dbl> <chr>    <chr>      <dbl> <dbl>   <dbl> <chr>          
#> 1          0      accuracy multiclass 0.947     5  0.017  pre0_mod1_post0
#> 2          0      accuracy multiclass 0.947     5  0.017  pre0_mod2_post0
#> 3          0      accuracy multiclass 0.947     5  0.017  pre0_mod3_post0
#> 4          0.0006 accuracy multiclass 0.947     5  0.017  pre0_mod4_post0
#> 5          0.0976 accuracy multiclass 0.94      5  0.0194 pre0_mod5_post0
```

O melhor custo de complexidade maximiza a acurácia média de validação.
Com ele, treina-se o modelo final em todos os dados de treino e
avalia-se uma única vez no teste.

## Outros algoritmos

A mesma interface serve a vários algoritmos (engines instalados sob
demanda):

| Função                | Algoritmo                   | Engine    |
|-----------------------|-----------------------------|-----------|
| `rnp_ml_arvore`       | árvore de decisão           | `rpart`   |
| `rnp_ml_floresta`     | floresta aleatória          | `ranger`  |
| `rnp_ml_boosting`     | gradient boosting           | `xgboost` |
| `rnp_ml_knn`          | k-vizinhos                  | `kknn`    |
| `rnp_ml_svm`          | máquina de vetor de suporte | `kernlab` |
| `rnp_ml_regularizada` | lasso/ridge/elastic net     | `glmnet`  |

Todas retornam especificações que entram em `rnp_ml_ajustar`,
`rnp_ml_tunagem` ou `rnp_ml_comparar`.

## Síntese

O fluxo de modelagem preditiva é sempre o mesmo (Kuhn and Johnson 2013):
**separar** treino/teste, **pré-processar** sem vazamento, **ajustar**,
**validar** por CV, **tunar** hiperparâmetros e, só ao final,
**avaliar** no teste. As funções `rnp_ml_*` tornam esse ciclo direto,
preservando o rigor metodológico.

O fio condutor é o **compromisso viés-variância**: a árvore rasa do
exemplo anterior é simples mas pode subajustar; a árvore cheia é
flexível mas pode sobreajustar. A validação cruzada e a tunagem existem
justamente para encontrar o ponto de equilíbrio que generaliza melhor.

## Exercícios

Resolva com o `rnp` (requer os pacotes do tidymodels), usando `iris`,
`palmerpenguins`/`mtcars` e
[`MASS::Pima.tr`](https://rdrr.io/pkg/MASS/man/Pima.tr.html).

1.  Particione `iris` em 70% treino e 30% teste, estratificando por
    espécie (`rnp_ml_particao`).
2.  Crie reamostras de validação cruzada com 5 folds (`rnp_ml_cv`).
3.  Monte uma receita que normalize os preditores (`rnp_ml_receita`).
4.  Ajuste uma árvore de decisão e avalie a acurácia no teste
    (`rnp_ml_arvore`, `rnp_ml_ajustar`).
5.  Extraia a importância das variáveis da árvore
    (`rnp_ml_importancia`).
6.  Tune o custo de complexidade da árvore por validação cruzada
    (`rnp_ml_tunagem`).
7.  Ajuste uma floresta aleatória e compare a acurácia com a da árvore
    (`rnp_ml_floresta`).
8.  Ajuste um modelo regularizado (glmnet) para regressão em `mtcars`
    (`rnp_ml_regularizada`).
9.  Compare árvore, floresta e regularizado por validação cruzada
    (`rnp_ml_comparar`).
10. Faça predições para novos dados com o melhor modelo
    (`rnp_ml_prever`).
11. Em [`MASS::Pima.tr`](https://rdrr.io/pkg/MASS/man/Pima.tr.html),
    treine um classificador para diabetes e avalie a AUC no teste.
12. Varie a profundidade da árvore (2, 5, 30) e observe o efeito sobre o
    desempenho de treino e teste (sub/sobreajuste).

## Referências

Kuhn, Max, and Kjell Johnson. 2013. *Applied Predictive Modeling*.
Springer.

Kuhn, Max, and Julia Silge. 2022. *Tidy Modeling with r*. O’Reilly.
