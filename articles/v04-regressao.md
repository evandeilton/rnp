# 4. Regressão linear e modelagem

A regressão linear modela a esperança de uma resposta como função de
preditores. Geometricamente, ela é a **projeção ortogonal** de $`y`$
sobre o espaço gerado pelas colunas de $`X`$: a solução de mínimos
quadrados e os valores ajustados são

``` math
\hat{\beta} = (X^\top X)^{-1} X^\top y, \qquad \hat{y} = X\hat{\beta},
```

e os resíduos $`e = y - \hat{y}`$ ficam ortogonais a $`X`$ por
construção — daí serem não-correlacionados com os preditores (Montgomery
et al. 2012). Usamos
[`MASS::Boston`](https://rdrr.io/pkg/MASS/man/Boston.html): 506 bairros
de Boston, com o valor mediano dos imóveis (`medv`, em milhares de
dólares).

``` r

dados <- MASS::Boston
```

## Ajuste e interpretação dos coeficientes

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

Cada coeficiente é o efeito **parcial** do preditor, mantidos os demais
constantes. O coeficiente de `rm` $`\approx 5{,}22`$ indica que cada
cômodo adicional eleva o valor mediano em cerca de US\$ 5 200,
controlando criminalidade e status socioeconômico; `lstat`
$`\approx -0{,}58`$ mostra que bairros com mais população de baixa renda
valem menos. Esse “controle” é o que a regressão múltipla oferece e a
correlação simples não.

``` r

fit$modelo
#> # A tibble: 1 × 7
#>      r2 r2_ajustado f_statistic f_pvalor sigma gl_residuos  nobs
#>   <dbl>       <dbl>       <dbl>    <dbl> <dbl>       <int> <int>
#> 1 0.646       0.644        305.        0  5.49         502   506
```

O **coeficiente de determinação**
$`R^2 = 1 - \text{SQ}_{\text{res}}/\text{SQ}_{\text{tot}}`$ vale
$`0.646`$: o modelo explica cerca de 65% da variância de `medv`. O
$`R^2`$ ajustado penaliza preditores inúteis, e a estatística $`F`$
testa o modelo contra o nulo (só intercepto).

## Pressupostos e diagnóstico

A inferência (erros-padrão, ICs, p-valores) depende de quatro
pressupostos: linearidade, homocedasticidade, normalidade dos resíduos e
independência. **Violá-los invalida a inferência**, ainda que os
coeficientes sigam úteis. Os gráficos de diagnóstico são o exame do
modelo:

``` r

g <- rnp_grafico_residuos(lm(medv ~ rm + lstat + crim, dados))
g$residuo_ajustado
```

![Resíduos versus valores
ajustados](v04-regressao_files/figure-html/diag-1.png)

Queremos uma nuvem sem padrão em torno de zero. Um formato de funil
indica **heterocedasticidade**; uma curvatura, **não-linearidade**. Os
testes formais complementam o exame visual:

``` r

rnp_regressao_diagnosticos(lm(medv ~ rm + lstat, dados))$testes
#> # A tibble: 3 × 4
#>   teste                   estatistica p_valor interpretacao                   
#>   <chr>                         <dbl>   <dbl> <chr>                           
#> 1 shapiro-wilk (residuos)       0.910       0 Rejeita normalidade             
#> 2 breusch-pagan                75.6         0 Heterocedasticidade             
#> 3 durbin-watson                 0.834      NA Possivel autocorrelacao positiva
```

## Multicolinearidade

Quando preditores carregam informação redundante, os erros-padrão se
inflam. O **fator de inflação de variância** mede isso a partir do
$`R_j^2`$ da regressão do preditor $`j`$ sobre os demais:

``` math
\text{VIF}_j = \frac{1}{1 - R_j^2}.
```

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

`tax` e `rad` (imposto predial e acesso a rodovias) têm VIF próximo de 6
(“moderada”) — são, de fato, correlacionados em Boston. Como regra, VIF
$`> 5`$ merece atenção e VIF $`> 10`$ é grave.

## Regularização: o compromisso viés-variância

Com muitos preditores ou colinearidade, aceitar um pequeno viés pode
reduzir muito a variância e melhorar a predição. A **ridge** (L2)
encolhe os coeficientes; o **lasso** (L1) ainda zera os irrelevantes,
fazendo seleção de variáveis (Hastie et al. 2009):

``` math
\hat\beta_{\text{ridge}} = (X^\top X + \lambda I)^{-1} X^\top y, \qquad
  \hat\beta_{\text{lasso}} = \arg\min_\beta \|y - X\beta\|^2 + \lambda \|\beta\|_1.
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

Com $`\lambda = 0{,}5`$, o lasso **zera** o coeficiente de `rad`,
descartando-o do modelo — algo que os mínimos quadrados nunca fazem. O
parâmetro $`\lambda`$ é o controle do compromisso entre viés e
variância.

## Resposta binária: regressão logística

Para classificar, modela-se a *chance* (odds) do evento pela função
logito, $`\operatorname{logit}(p) = \log\frac{p}{1-p} = X\beta`$, de
modo que $`e^{\beta_j}`$ é a **razão de chances**. Voltando aos dados de
diabetes:

``` r

pima <- MASS::Pima.tr
rnp_logistic(type ~ glu + bmi + age, data = pima)$coeficientes
#> # A tibble: 4 × 8
#>   termo    estimativa erro_padrao estatistica_z p_valor odds_ratio ic_inf ic_sup
#>   <chr>         <dbl>       <dbl>         <dbl>   <dbl>      <dbl>  <dbl>  <dbl>
#> 1 (Interc…    -9.41        1.48           -6.37  0          0.0001   0    0.0015
#> 2 glu          0.0309      0.0064          4.78  0          1.03     1.02 1.04  
#> 3 bmi          0.0919      0.0322          2.85  0.0044     1.10     1.03 1.17  
#> 4 age          0.0526      0.017           3.10  0.002      1.05     1.02 1.09
```

A razão de chances de `glu` $`\approx 1{,}03`$ significa que cada
unidade de glicose multiplica a chance de diabetes por 1,03, controlando
IMC e idade. A qualidade da classificação é medida pela área sob a curva
ROC:

``` r

prob <- predict(glm(type ~ glu + bmi + age, pima, family = binomial()),
                type = "response")
rnp_curva_roc(pima$type, prob, positivo = "Yes")$auc
#> [1] 0.8355
```

A **AUC** de 0.84 é a probabilidade de o modelo ranquear um caso
positivo acima de um negativo tomados ao acaso (0,5 = aleatório; 1 =
perfeito) — uma discriminação boa.

## Predição: intervalo de confiança versus de predição

Há duas perguntas distintas ao prever. *Qual o valor médio de `medv`
para todos os bairros com $`rm = 6`$ e $`lstat = 10`$?* — responde-se
com o **intervalo de confiança** da média. *Qual será o valor de **um**
bairro específico?* — pede o **intervalo de predição**, mais largo, pois
inclui também a variabilidade irredutível $`\sigma`$ em torno da média:

``` r

fit <- lm(medv ~ rm + lstat, data = MASS::Boston)
novos <- data.frame(rm = c(6, 7), lstat = c(10, 5))
rnp_predicao(fit, novos, tipo = "confianca")   # média da resposta
#> # A tibble: 2 × 3
#>   ajuste limite_inferior limite_superior
#>    <dbl>           <dbl>           <dbl>
#> 1   22.8            22.1            23.4
#> 2   31.1            30.4            31.8
rnp_predicao(fit, novos, tipo = "predicao")     # nova observação
#> # A tibble: 2 × 3
#>   ajuste limite_inferior limite_superior
#>    <dbl>           <dbl>           <dbl>
#> 1   22.8            11.9            33.7
#> 2   31.1            20.2            42.0
```

Para o primeiro bairro, o IC da média é estreito
($`[22{,}1;\,23{,}4]`$), mas o de predição é amplo
($`[11{,}9;\,33{,}7]`$). Confundir os dois — usar o IC da média para
prever um caso individual — subestima gravemente a incerteza, um erro
comum em engenharia preditiva.

## Síntese

| Etapa | Função | Pergunta |
|----|----|----|
| Ajustar | `rnp_regressao` | os coeficientes fazem sentido teórico? |
| Diagnosticar | `rnp_grafico_residuos` | os pressupostos valem? |
| Colinearidade | `rnp_vif` | os preditores são redundantes? |
| Regularizar | `rnp_regressao_ridge/_lasso` | vale trocar viés por variância? |
| Prever | `rnp_predicao` | IC da média ou intervalo de predição? |
| Classificar | `rnp_logistic`, `rnp_curva_roc` | quão bem separa as classes? |

O ajuste é o começo; o essencial é examinar se os pressupostos se
sustentam.

## Exercícios

Resolva com o `rnp`, usando
[`MASS::Boston`](https://rdrr.io/pkg/MASS/man/Boston.html), `mtcars`,
`cars` e `trees`.

1.  Ajuste `medv ~ rm + lstat + crim` e interprete o coeficiente de `rm`
    (`rnp_regressao`).
2.  Qual a fração da variância explicada ($`R^2`$)? Compare com o
    $`R^2`$ ajustado.
3.  Avalie os pressupostos do modelo pelos gráficos de resíduos
    (`rnp_grafico_residuos`).
4.  Aplique os testes formais de normalidade, homocedasticidade e
    autocorrelação (`rnp_regressao_diagnosticos`).
5.  Calcule o VIF de um modelo com `rm`, `tax`, `rad` e `age`; há
    colinearidade? (`rnp_vif`).
6.  Ajuste uma regressão ridge e uma lasso ao mesmo modelo; o lasso zera
    algum coeficiente? (`rnp_regressao_ridge`, `rnp_regressao_lasso`).
7.  Encontre o $`\lambda`$ ótimo variando a penalização e observando os
    coeficientes.
8.  Em `cars`, ajuste `dist ~ speed` linear e polinomial de grau 2; qual
    ajusta melhor? (`rnp_regressao_polinomial`).
9.  Aplique a transformação de Box-Cox a `medv ~ rm + lstat`; qual o
    $`\lambda`$ ótimo? (`rnp_box_cox`).
10. Para um bairro com `rm = 6.5` e `lstat = 8`, calcule o IC da média e
    o intervalo de predição (`rnp_predicao`).
11. Ajuste uma regressão de Poisson a uma variável de contagem
    (`rnp_regressao_poisson`).
12. Ajuste a regressão logística `am ~ wt + hp` em `mtcars` e interprete
    as razões de chances (`rnp_logistic`).
13. Trace a curva ROC e calcule a AUC do modelo anterior
    (`rnp_curva_roc`).
14. Construa a matriz de confusão para um limiar de 0,5
    (`rnp_matriz_confusao`).
15. Ajuste uma regressão robusta a dados com um outlier inserido e
    compare com os mínimos quadrados (`rnp_regressao_robusta`).

## Referências

Hastie, Trevor, Robert Tibshirani, and Jerome Friedman. 2009. *The
Elements of Statistical Learning*. 2nd ed. Springer.

Montgomery, Douglas C., Elizabeth A. Peck, and G. Geoffrey Vining. 2012.
*Introduction to Linear Regression Analysis*. 5th ed. Wiley.
