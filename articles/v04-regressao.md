# 4. Regressao Linear e Modelagem

## Regressao e geometria

Uma forma util de enxergar a regressao linear e como **projecao
ortogonal**. O vetor de respostas $`y`$ vive num espaco de $`n`$
dimensoes; o modelo projeta $`y`$ sobre o subespaco gerado pelas colunas
de $`X`$. Os coeficientes de minimos quadrados sao as coordenadas dessa
projecao, e os residuos sao a parte de $`y`$ ortogonal ao subespaco. E
por isso que os residuos sao nao-correlacionados com os preditores: nao
por acaso, mas por construcao geometrica.

Usaremos [`MASS::Boston`](https://rdrr.io/pkg/MASS/man/Boston.html): 506
bairros de Boston com o valor mediano dos imoveis (`medv`) e 13
caracteristicas reais.

``` r

dados <- MASS::Boston
```

## Ajuste e leitura dos coeficientes

``` r

fit <- rnp_regressao(medv ~ rm + lstat + crim, data = dados)
fit$coeficientes
#> # A tibble: 4 × 7
#>   termo       estimativa erro_padrao estatistica_t p_valor ic_inf ic_sup
#>   <chr>            <dbl>       <dbl>         <dbl>   <dbl>  <dbl>  <dbl>
#> 1 (Intercept)     -2.56       3.17          -0.809  0.419  -8.78   3.66 
#> 2 rm               5.22       0.442         11.8    0       4.35   6.09 
#> 3 lstat           -0.578      0.0477       -12.1    0      -0.672 -0.485
#> 4 crim            -0.103      0.032         -3.21   0.0014 -0.166 -0.04
```

Cada coeficiente e o efeito **parcial** do preditor: a variacao esperada
em `medv` por unidade do preditor, *mantidos os demais constantes*. Por
exemplo, o coeficiente de `rm` (numero medio de comodos) diz quanto o
valor sobe por comodo adicional, controlando criminalidade e status
socioeconomico. Esse “controle” e exatamente o que a regressao multipla
oferece e a correlacao simples nao.

``` r

fit$modelo
#> # A tibble: 1 × 7
#>      r2 r2_ajustado f_statistic f_pvalor sigma gl_residuos  nobs
#>   <dbl>       <dbl>       <dbl>    <dbl> <dbl>       <int> <int>
#> 1 0.646       0.644        305.        0  5.49         502   506
```

O **R-quadrado** e a fracao da variancia de `medv` explicada pelo modelo
— geometricamente, o quanto da “energia” de $`y`$ caiu dentro do
subespaco. O **R-ajustado** penaliza preditores inuteis: ao contrario do
R2 cru, ele *pode cair* quando se adiciona ruido. A estatistica F testa
o modelo como um todo contra o modelo nulo (so o intercepto).

## Pressupostos: o modelo so vale se eles valerem

Regressao linear nao e magica; ela repousa sobre quatro pilares.
**Violar os pressupostos invalida os erros-padrao, os ICs e os
p-valores** — os coeficientes podem ate continuar uteis, mas a
inferencia sobre eles desaba. Os diagnosticos graficos sao o exame
medico do modelo:

``` r

g <- rnp_grafico_residuos(lm(medv ~ rm + lstat + crim, dados))
g$residuo_ajustado
```

![Painel de diagnostico de
residuos](v04-regressao_files/figure-html/diag-1.png)

- **Residuos vs ajustados** (acima): testa **linearidade** e
  **homocedasticidade**. Queremos uma nuvem sem padrao em torno de zero.
  Um funil indica variancia nao-constante; uma curva indica
  nao-linearidade.
- **Q-Q dos residuos**: testa a **normalidade** dos residuos (necessaria
  para os ICs em amostras pequenas).
- **Escala-locacao**: reforca o exame da homocedasticidade.
- **Residuos vs leverage**: identifica pontos **influentes** que
  sozinhos podem dominar o ajuste.

``` r

rnp_regressao_diagnosticos(lm(medv ~ rm + lstat, dados))$testes
#> # A tibble: 3 × 4
#>   teste                   estatistica p_valor interpretacao                   
#>   <chr>                         <dbl>   <dbl> <chr>                           
#> 1 shapiro-wilk (residuos)       0.910       0 Rejeita normalidade             
#> 2 breusch-pagan                75.6         0 Heterocedasticidade             
#> 3 durbin-watson                 0.834      NA Possivel autocorrelacao positiva
```

Os testes formais (Shapiro nos residuos, Breusch-Pagan para
heterocedasticidade, Durbin-Watson para autocorrelacao) complementam o
olho.

## Multicolinearidade: quando os preditores brigam

Se dois preditores carregam quase a mesma informacao, o modelo nao
consegue separar seus efeitos — os erros-padrao explodem e os
coeficientes ficam instaveis. O **VIF** (fator de inflacao de variancia)
detecta isso:

``` r

rnp_vif(lm(medv ~ rm + lstat + tax + rad, dados))
#> # A tibble: 4 × 3
#>   termo   vif interpretacao
#>   <chr> <dbl> <chr>        
#> 1 rm     1.65 baixa        
#> 2 lstat  2.10 baixa        
#> 3 tax    6.38 moderada     
#> 4 rad    5.98 moderada
```

Regra pratica: VIF \> 5 e preocupante, \> 10 e grave. `tax` e `rad`
(acesso a rodovias e imposto predial) costumam ser colineares em Boston
— faz sentido economico.

## O trade-off vies-variancia: regressao penalizada

Quando ha muitos preditores ou multicolinearidade, abrir mao de um pouco
de imparcialidade pode reduzir muito a variancia — e melhorar a
predicao. Essa e a ideia da **regularizacao**, e o trade-off
vies-variancia em acao.

- **Ridge** (L2) encolhe todos os coeficientes em direcao a zero,
  domando a multicolinearidade sem zerar ninguem.
- **Lasso** (L1) zera coeficientes irrelevantes, fazendo **selecao de
  variaveis**.

``` r

rnp_regressao_ridge(medv ~ rm + lstat + crim + tax + rad, dados, lambda = 1)
#> # A tibble: 6 × 2
#>   termo       estimativa
#>   <chr>            <dbl>
#> 1 (Intercept)     1.07  
#> 2 rm              5.09  
#> 3 lstat          -0.536 
#> 4 crim           -0.0871
#> 5 tax            -0.0123
#> 6 rad             0.169
```

``` r

rnp_regressao_lasso(medv ~ rm + lstat + crim + tax + rad, dados, lambda = 0.5)
#> # A tibble: 6 × 2
#>   termo       estimativa
#>   <chr>            <dbl>
#> 1 (Intercept)     1.03  
#> 2 rm              4.75  
#> 3 lstat          -0.529 
#> 4 crim           -0.0289
#> 5 tax            -0.0038
#> 6 rad             0
```

Note como o lasso zera preditores fracos — algo que o minimos quadrados
comum jamais faz. O parametro $`\lambda`$ controla a intensidade da
penalidade: e o dial do trade-off.

## Robustez a outliers

Minimos quadrados eleva os residuos ao quadrado — logo, um unico outlier
pode sequestrar a reta. A **regressao robusta** (M-estimadores via IRLS)
reduz o peso de observacoes aberrantes:

``` r

rnp_regressao_robusta(medv ~ rm + lstat, dados, metodo = "huber")$coeficientes
#> # A tibble: 3 × 2
#>   termo       estimativa
#>   <chr>            <dbl>
#> 1 (Intercept)     -5.68 
#> 2 rm               5.61 
#> 3 lstat           -0.604
```

## Quando a resposta e binaria: regressao logistica

Nem toda resposta e continua. Para classificar (doente/sadio,
aprovado/reprovado) usa-se a **regressao logistica**, que modela a
*chance* (odds) do evento. Voltando aos dados de diabetes:

``` r

pima <- MASS::Pima.tr
log_fit <- rnp_logistic(type ~ glu + bmi + age, data = pima)
log_fit$coeficientes
#> # A tibble: 4 × 8
#>   termo    estimativa erro_padrao estatistica_z p_valor odds_ratio ic_inf ic_sup
#>   <chr>         <dbl>       <dbl>         <dbl>   <dbl>      <dbl>  <dbl>  <dbl>
#> 1 (Interc…    -9.41        1.48           -6.37  0          0.0001   0    0.0015
#> 2 glu          0.0309      0.0064          4.78  0          1.03     1.02 1.04  
#> 3 bmi          0.0919      0.0322          2.85  0.0044     1.10     1.03 1.17  
#> 4 age          0.0526      0.017           3.10  0.002      1.05     1.02 1.09
```

O `odds_ratio` (= $`e^\beta`$) e a leitura natural: um OR de 1,03 para
`glu` significa que cada unidade de glicose multiplica a chance de
diabetes por 1,03. A qualidade da classificacao se mede pela **curva
ROC** e sua area (AUC):

``` r

prob <- predict(glm(type ~ glu + bmi + age, pima, family = binomial()),
                type = "response")
roc <- rnp_curva_roc(pima$type, prob, positivo = "Yes")
roc$auc
#> [1] 0.8355
```

A **AUC** e a probabilidade de o modelo ranquear um caso positivo acima
de um negativo escolhidos ao acaso. AUC = 0,5 e moeda jogada; 1,0 e
perfeito.

## Sintese

| Etapa | Ferramenta | Pergunta |
|----|----|----|
| Ajustar | `rnp_regressao` | os coeficientes fazem sentido teorico? |
| Diagnosticar | `rnp_grafico_residuos` | os pressupostos valem? |
| Colinearidade | `rnp_vif` | os preditores sao redundantes? |
| Regularizar | `rnp_regressao_ridge/_lasso` | vale trocar vies por variancia? |
| Classificar | `rnp_logistic` + `rnp_curva_roc` | quao bem separa as classes? |

O ajuste e apenas o comeco: a parte mais importante e examinar se os
pressupostos se sustentam e se o modelo descreve bem os dados.
