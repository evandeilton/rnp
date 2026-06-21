# 6. Dados categóricos e métodos não-paramétricos

Muitos dados são **categóricos** (sexo, fumante/não-fumante) ou têm
distribuições que invalidam o teste $`t`$. Esta vinheta cobre a análise
de tabelas de contingência (Agresti 2013) e os métodos
**não-paramétricos** (Conover 1999), que trocam suposições de
distribuição por raciocínio sobre postos e contagens. Usamos
[`MASS::birthwt`](https://rdrr.io/pkg/MASS/man/birthwt.html): 189
nascimentos reais, com peso do bebê e fatores como tabagismo da mãe.

``` r

dados <- MASS::birthwt
dados$fumante    <- factor(ifelse(dados$smoke == 1, "Sim", "Nao"), c("Sim", "Nao"))
dados$baixo_peso <- factor(ifelse(dados$low == 1, "Sim", "Nao"), c("Sim", "Nao"))
```

## Tabela de contingência

A frequência relativa *por linha* responde à pergunta de interesse:
dentre as fumantes, que proporção teve bebê de baixo peso?

``` r

rnp_tabela_contingencia(dados$fumante, dados$baixo_peso, tipo = "fr_linha")
#> # A tibble: 2 × 4
#>   categoria   Sim   Nao Total
#>   <chr>     <dbl> <dbl> <dbl>
#> 1 Sim       0.405 0.595     1
#> 2 Nao       0.252 0.748     1
```

Entre as fumantes, 40,5% tiveram bebê de baixo peso; entre as
não-fumantes, apenas 25,2%. Resta saber se essa diferença é
estatisticamente sustentável.

## Teste qui-quadrado

O teste de independência compara as contagens observadas $`O_{ij}`$ com
as esperadas sob independência, $`E_{ij} = n_{i\cdot}\,n_{\cdot j}/n`$:

``` math
\chi^2 = \sum_{i,j} \frac{(O_{ij} - E_{ij})^2}{E_{ij}}, \qquad
  V = \sqrt{\frac{\chi^2}{n\,(\min(r,c) - 1)}}.
```

``` r

rnp_teste_qui_quadrado(dados$fumante, dados$baixo_peso)
#> # A tibble: 1 × 5
#>   estatistica    gl p_valor v_cramer metodo       
#>         <dbl> <int>   <dbl>    <dbl> <chr>        
#> 1        4.92     1  0.0265    0.161 independencia
```

Há associação ($`p = 0.026`$), mas o **V de Cramér** de $`0{,}16`$
revela que ela é *fraca*. Vale a distinção: significância estatística
não é o mesmo que força da associação — com amostras grandes,
associações triviais ficam significativas.

## Teste exato de Fisher

Quando alguma frequência esperada é pequena ($`< 5`$), a aproximação do
qui-quadrado falha, e o **teste exato de Fisher** calcula a
probabilidade exata pela distribuição hipergeométrica:

``` r

tab <- table(dados$fumante, dados$baixo_peso)
rnp_teste_fisher(tab)
#> # A tibble: 1 × 4
#>   p_valor odds_ratio ic_inf ic_sup
#>     <dbl>      <dbl>  <dbl>  <dbl>
#> 1  0.0362       2.01   1.03   3.96
```

## Razão de chances versus risco relativo

Para uma tabela $`2\times2`$ com células $`a, b, c, d`$, definem-se

``` math
\text{OR} = \frac{a\,d}{b\,c}, \qquad
  \text{RR} = \frac{a/(a+b)}{c/(c+d)}.
```

``` r

rnp_odds_ratio(tab)
#> # A tibble: 1 × 5
#>   odds_ratio ic_inf ic_sup log_or ep_log
#>        <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1       2.02   1.08   3.78  0.704  0.320
rnp_risco_relativo(tab)
#> # A tibble: 1 × 5
#>   risco_relativo ic_inf ic_sup risco_expostos risco_nao_expostos
#>            <dbl>  <dbl>  <dbl>          <dbl>              <dbl>
#> 1           1.61   1.06   2.44          0.405              0.252
```

A **razão de chances** é $`2{,}0`$ e o **risco relativo**, $`1{,}6`$.
Eis uma das distinções mais atropeladas da epidemiologia: como o
desfecho é **comum** (baixo peso ocorre em ~31% dos casos), o OR
*exagera* o RR. Reportar “OR $`= 2`$” como “o dobro do risco” seria
incorreto — o risco relativo correto é de 1,6 vez. (Para desfechos
raros, OR $`\approx`$ RR.) O RR só é diretamente estimável em estudos
prospectivos; o OR é o que sai de estudos caso-controle e da regressão
logística.

## Concordância: Kappa de Cohen

A concordância bruta entre dois avaliadores engana, pois parte dela
ocorre por acaso. O **Kappa de Cohen** desconta o acaso (Cohen 1960):

``` math
\kappa = \frac{p_o - p_e}{1 - p_e},
```

onde $`p_o`$ é a concordância observada e $`p_e`$, a esperada por acaso.

``` r

a1 <- c("normal", "alterado", "normal", "alterado", "normal", "alterado", "normal")
a2 <- c("normal", "alterado", "normal", "normal",   "normal", "alterado", "alterado")
rnp_kappa(a1, a2)
#> # A tibble: 1 × 3
#>   kappa concordancia_observada concordancia_esperada
#>   <dbl>                  <dbl>                 <dbl>
#> 1 0.417                  0.714                 0.510
```

$`\kappa = 1`$ é concordância perfeita; $`0`$ é o nível do acaso; acima
de $`0{,}6`$ costuma ser considerada “boa”.

## Métodos não-paramétricos

Quando a resposta numérica não é normal (assimétrica, ordinal, com
*outliers*), o teste $`t`$ e a ANOVA ficam suspeitos. Os métodos
não-paramétricos trabalham com **postos**, dispensando a normalidade.
Vale checar antes:

``` r

rnp_teste_normalidade(dados$bwt, metodo = "shapiro")
#> # A tibble: 1 × 3
#>   estatistica p_valor metodo 
#>         <dbl>   <dbl> <chr>  
#> 1       0.992   0.435 shapiro
```

Aqui o peso ao nascer é aproximadamente normal ($`p = 0.44`$, não se
rejeita a normalidade), de modo que o teste $`t`$ seria válido. Ainda
assim, o **teste de Mann-Whitney** (postos) é uma alternativa robusta
para comparar dois grupos independentes:

``` r

rnp_mann_whitney(dados$bwt[dados$fumante == "Sim"],
                 dados$bwt[dados$fumante == "Nao"])
#> # A tibble: 1 × 4
#>   estatistica p_valor metodo                                         alternativa
#>         <dbl>   <dbl> <chr>                                          <chr>      
#> 1       3260.  0.0068 Wilcoxon rank sum test with continuity correc… bilateral
```

Com $`p = 0.007`$, confirma-se que filhos de fumantes pesam menos (média
de 2772 g contra 3056 g). Para comparar **mais de dois** grupos (peso
por etnia), o **Kruskal-Wallis** generaliza o Mann-Whitney:

``` r

rnp_kruskal(dados$bwt, factor(dados$race))
#> # A tibble: 1 × 4
#>   estatistica    gl p_valor metodo        
#>         <dbl> <int>   <dbl> <chr>         
#> 1        8.52     2  0.0141 kruskal-wallis
```

## Síntese

| Situação | Função | Cuidado |
|----|----|----|
| Associação entre categóricas | `rnp_teste_qui_quadrado` | significância $`\ne`$ força (V de Cramér) |
| Tabela $`2\times2`$, $`n`$ pequeno | `rnp_teste_fisher` | esperadas $`< 5`$ |
| Medir risco | `rnp_odds_ratio`, `rnp_risco_relativo` | OR $`\ne`$ RR em desfechos comuns |
| Concordância | `rnp_kappa` | descontar o acaso |
| Comparar 2 grupos | `rnp_mann_whitney` | menos poder que o $`t`$ |
| Comparar $`k`$ grupos | `rnp_kruskal` | ANOVA dos postos |

A escolha entre métodos paramétricos e não-paramétricos é um
compromisso: os primeiros têm mais poder quando os pressupostos valem;
os segundos são mais robustos quando a normalidade é duvidosa.

## Referências

Agresti, Alan. 2013. *Categorical Data Analysis*. 3rd ed. Wiley.

Cohen, Jacob. 1960. “A Coefficient of Agreement for Nominal Scales.”
*Educational and Psychological Measurement* 20 (1): 37–46.

Conover, William J. 1999. *Practical Nonparametric Statistics*. 3rd ed.
Wiley.
