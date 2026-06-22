## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.2,
                      fig.align = "center",
                      message = FALSE, warning = FALSE)
library(rnp)


## ----instalar, eval = FALSE---------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("evandeilton/rnp")


## ----descritiva---------------------------------------------------------------
rnp_descritiva(rnp_concreto$resistencia)


## ----regressao----------------------------------------------------------------
ajuste <- rnp_regressao(resistencia ~ dias_cura + tipo_cimento,
                        data = rnp_concreto)
ajuste


## ----broom--------------------------------------------------------------------
tidy(ajuste)    # um termo por linha
glance(ajuste)  # uma linha de medidas-resumo do ajuste


## ----inferencia---------------------------------------------------------------
rnp_ic_media(rnp_concreto$resistencia)
rnp_teste_t(rnp_concreto$resistencia, mu = 30)


## ----grafico, fig.alt="Dispersão de resistência versus dias de cura"----------
rnp_grafico_dispersao(rnp_concreto, x = "dias_cura", y = "resistencia")


## ----anova--------------------------------------------------------------------
rnp_anova(resistencia ~ tipo_cimento, data = rnp_concreto)$anova


## ----componentes--------------------------------------------------------------
ajuste$coeficientes


## ----vinhetas, eval = FALSE---------------------------------------------------
# browseVignettes("rnp")

