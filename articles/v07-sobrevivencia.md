# 7. Análise de sobrevivência

A análise de sobrevivência estuda o **tempo até a ocorrência de um
evento** (morte, falha, recidiva). Seu traço distintivo é a **censura**:
para alguns indivíduos, o estudo termina antes do evento, e só se sabe
que o tempo de falha *excede* o observado. Ignorar essa informação
parcial enviesaria as estimativas (Colosimo and Giolo 2006; Klein and
Moeschberger 2003). Usamos
[`survival::lung`](https://rdrr.io/pkg/survival/man/lung.html): 228
pacientes com câncer de pulmão avançado, com tempo de seguimento e
estado vital.

``` r

dados <- survival::lung
dados$sexo <- factor(dados$sex, 1:2, c("Masculino", "Feminino"))
dados$evento <- dados$status == 2          # 1 = óbito; 0 = censura
```

## Conceitos fundamentais

Seja $`T`$ o tempo até o evento. As três funções que o descrevem são

``` math
S(t) = P(T > t), \qquad
  h(t) = \frac{f(t)}{S(t)}, \qquad
  H(t) = \int_0^t h(u)\,du = -\log S(t),
```

a **sobrevivência** $`S`$, o **risco instantâneo** (hazard) $`h`$ e o
**risco acumulado** $`H`$. Conhecida uma delas, obtêm-se as demais.

## Estimador de Kaplan-Meier

Na presença de censura, $`S(t)`$ é estimada não-parametricamente pelo
produto limite de Kaplan-Meier, onde $`d_i`$ é o número de eventos e
$`n_i`$ o número em risco em cada tempo de falha $`t_i`$:

``` math
\hat{S}(t) = \prod_{t_i \le t} \left(1 - \frac{d_i}{n_i}\right).
```

``` r

km <- rnp_kaplan_meier(dados$time, dados$evento, grupo = dados$sexo)
km$mediana
#> # A tibble: 2 × 6
#>   grupo         n eventos mediana ic_inf ic_sup
#>   <chr>     <int>   <int>   <dbl>  <dbl>  <dbl>
#> 1 Masculino   138     112     270    212    310
#> 2 Feminino     90      53     426    348    550
```

A sobrevida **mediana** — tempo em que $`\hat{S}(t) = 0{,}5`$ — é de
**270 dias** para homens e **426 dias** para mulheres, uma diferença
expressiva. A curva torna isso visível:

``` r

rnp_grafico_sobrevivencia(km)
```

![Curvas de Kaplan-Meier por
sexo](v07-sobrevivencia_files/figure-html/km-grafico-1.png)

## Comparando grupos: teste log-rank

O **teste log-rank** confronta o número de eventos observados e
esperados em cada grupo sob $`H_0`$ de curvas iguais, acumulando as
diferenças sobre os tempos de falha:

``` r

rnp_log_rank(dados$time, dados$evento, dados$sexo)
#> # A tibble: 1 × 4
#>   estatistica    gl p_valor metodo  
#>         <dbl> <int>   <dbl> <chr>   
#> 1        10.3     1  0.0013 log-rank
```

Com $`\chi^2 = 10{,}3`$ e $`p = 0{,}001`$, rejeita-se a igualdade: a
sobrevivência das mulheres é significativamente maior.

## Modelo de Cox de riscos proporcionais

O modelo de Cox relaciona o risco às covariáveis sem especificar a forma
de $`h_0(t)`$ (o risco basal), sendo por isso **semiparamétrico**:

``` math
h(t \mid x) = h_0(t)\,\exp(x_1\beta_1 + \dots + x_p\beta_p).
```

O efeito de cada covariável é a **razão de riscos** (hazard ratio),
$`\text{HR}_j = e^{\beta_j}`$(Therneau and Grambsch 2000).

``` r

fit <- rnp_cox(Surv(time, status) ~ age + sexo + ph.ecog, data = dados)
fit$coeficientes
#> # A tibble: 3 × 8
#>   termo           coef hazard_ratio erro_padrao     z p_valor ic_inf ic_sup
#>   <chr>          <dbl>        <dbl>       <dbl> <dbl>   <dbl>  <dbl>  <dbl>
#> 1 age           0.0111        1.01       0.0093  1.19   0.232  0.993  1.03 
#> 2 sexoFeminino -0.553         0.575      0.168  -3.29   0.001  0.414  0.799
#> 3 ph.ecog       0.464         1.59       0.114   4.08   0      1.27   1.99
```

Interpretação: ser do sexo feminino multiplica o risco por
$`\text{HR} = 0{,}58`$ (redução de 42%, $`p = 0{,}001`$); cada ponto na
escala de incapacidade `ph.ecog` multiplica o risco por $`1{,}59`$
($`p < 0{,}001`$); a idade não é significativa ($`p = 0{,}23`$). A
**concordância** mede o poder discriminante do modelo:

``` r

fit$modelo
#> # A tibble: 1 × 5
#>   concordancia   aic     n eventos p_razao_veross
#>          <dbl> <dbl> <int>   <dbl>          <dbl>
#> 1        0.637 1464.   227     164              0
```

A concordância de $`0{,}64`$ indica capacidade preditiva moderada (0,5 =
acaso).

### Verificando o pressuposto de proporcionalidade

O modelo de Cox supõe que as razões de risco são **constantes no
tempo**. O teste baseado nos resíduos de Schoenfeld avalia isso:

``` r

rnp_cox_diagnosticos(fit)$teste
#> # A tibble: 4 × 5
#>   termo   chisq    gl p_valor interpretacao   
#>   <chr>   <dbl> <dbl>   <dbl> <chr>           
#> 1 age     0.188     1   0.665 PH nao rejeitada
#> 2 sexo    2.31      1   0.129 PH nao rejeitada
#> 3 ph.ecog 2.05      1   0.152 PH nao rejeitada
#> 4 GLOBAL  4.46      3   0.216 PH nao rejeitada
```

Nenhuma covariável viola o pressuposto (global $`p = 0{,}22`$): o modelo
de Cox é adequado aqui.

## Modelo paramétrico (AFT)

Quando se quer modelar diretamente o tempo, os modelos de **tempo de
falha acelerado** assumem uma distribuição para $`T`$ (Weibull,
lognormal, …):

``` r

rnp_sobrevivencia_parametrica(Surv(time, status) ~ age + sexo, dados,
                              dist = "weibull")$coeficientes
#> # A tibble: 4 × 5
#>   termo        estimativa erro_padrao     z p_valor
#>   <chr>             <dbl>       <dbl> <dbl>   <dbl>
#> 1 (Intercept)      6.66        0.448  14.9   0     
#> 2 age             -0.0123      0.007  -1.76  0.0781
#> 3 sexoFeminino     0.382       0.128   3.00  0.0027
#> 4 Log(scale)      -0.282       0.0619 -4.56  0
```

O coeficiente positivo de `sexoFeminino` ($`0{,}38`$, $`p = 0{,}003`$)
indica que ser mulher *acelera* (prolonga) o tempo de sobrevida —
coerente com o achado de Cox.

Qual distribuição descreve melhor os tempos? Comparamos pelo **AIC**
(menor é melhor):

``` r

dists <- c("weibull", "lognormal", "loglogistic", "exponential")
sapply(dists, function(d)
  rnp_sobrevivencia_parametrica(Surv(time, status) ~ age + sexo, dados, d)$aic)
#>     weibull   lognormal loglogistic exponential 
#>    2302.109    2325.500    2313.794    2318.198
```

A **Weibull** tem o menor AIC (2302), sendo a escolha preferível entre
as quatro — resultado típico em confiabilidade, pois sua taxa de falha
flexível acomoda o desgaste.

## Síntese

| Objetivo | Função | Conceito |
|----|----|----|
| Estimar $`S(t)`$ | `rnp_kaplan_meier` | produto limite |
| Visualizar | `rnp_grafico_sobrevivencia` | curva com IC |
| Comparar grupos | `rnp_log_rank` | observados vs esperados |
| Risco acumulado | `rnp_nelson_aalen` | $`\hat{H}(t)`$ |
| Efeito de covariáveis | `rnp_cox` | razão de riscos |
| Validar Cox | `rnp_cox_diagnosticos` | resíduos de Schoenfeld |
| Modelo paramétrico | `rnp_sobrevivencia_parametrica` | AFT |

A censura é o que distingue esta área: toda a maquinaria existe para
extrair informação de observações *incompletas*.

## Exercícios

Resolva com o `rnp`, usando
[`survival::lung`](https://rdrr.io/pkg/survival/man/lung.html),
[`survival::veteran`](https://rdrr.io/pkg/survival/man/veteran.html) e
[`survival::ovarian`](https://rdrr.io/pkg/survival/man/ovarian.html).

1.  Estime a curva de Kaplan-Meier global de `lung` e obtenha a
    sobrevida mediana (`rnp_kaplan_meier`).
2.  Estratifique a curva pelo estado funcional `ph.ecog` e visualize-a
    (`rnp_grafico_sobrevivencia`).
3.  Compare as curvas por sexo com o teste log-rank (`rnp_log_rank`).
4.  Refaça a comparação com o teste de Gehan-Wilcoxon (`rho = 1`) e veja
    se a conclusão muda.
5.  Estime o risco acumulado de Nelson-Aalen e relacione-o com
    $`-\log \hat{S}(t)`$ (`rnp_nelson_aalen`).
6.  Ajuste um modelo de Cox com `age`, `sex` e `ph.karno`; interprete as
    razões de risco (`rnp_cox`).
7.  Verifique a hipótese de riscos proporcionais
    (`rnp_cox_diagnosticos`).
8.  Calcule o risco relativo predito para perfis de 50 e 70 anos
    (`rnp_cox_risco_relativo`).
9.  Ajuste um modelo AFT Weibull e compare o AIC com lognormal e
    exponencial (`rnp_sobrevivencia_parametrica`).
10. Construa a tábua de vida atuarial de `lung` em intervalos de 200
    dias (`rnp_tabela_vida`).
11. Em `veteran`, compare a sobrevida entre os tipos de tratamento
    (`trt`) por log-rank.
12. Ajuste um Cox em `veteran` com `karno` e `celltype` e identifique o
    fator de maior risco.
13. Em `ovarian`, estime a curva de Kaplan-Meier por grupo de tratamento
    (`rx`).
14. Compare a concordância (C de Harrell) de dois modelos de Cox com
    diferentes covariáveis (`rnp_cox`).

## Referências

Colosimo, Enrico A., and Suely R. Giolo. 2006. *Análise de Sobrevivência
Aplicada*. Edgard Blücher.

Klein, John P., and Melvin L. Moeschberger. 2003. *Survival Analysis:
Techniques for Censored and Truncated Data*. 2nd ed. Springer.

Therneau, Terry M., and Patricia M. Grambsch. 2000. *Modeling Survival
Data: Extending the Cox Model*. Springer.
