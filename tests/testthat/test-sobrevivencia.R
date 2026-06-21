lung <- survival::lung

test_that("rnp_kaplan_meier reproduz survfit (mediana e sobrevivencia)", {
  km <- rnp_kaplan_meier(lung$time, lung$status == 2)
  # mediana de referencia (survival) para lung sem estratos = 310
  expect_equal(km$mediana$mediana, 310)
  expect_true(all(diff(km$tabela$sobrevivencia) <= 1e-8))  # nao-crescente
  expect_equal(km$tabela$sobrevivencia[1], 1, tolerance = 0.02)
})

test_that("rnp_kaplan_meier estratificado e grafico", {
  km <- rnp_kaplan_meier(lung$time, lung$status == 2, grupo = lung$sex)
  expect_equal(nrow(km$mediana), 2L)
  expect_s3_class(rnp_grafico_sobrevivencia(km), "ggplot")
})

test_that("rnp_log_rank bate com survdiff", {
  r <- rnp_log_rank(lung$time, lung$status == 2, lung$sex)
  ref <- survival::survdiff(survival::Surv(time, status) ~ sex, lung)
  expect_equal(r$estatistica, round(ref$chisq, 4))
  expect_equal(r$gl, 1)
  expect_lt(r$p_valor, 0.05)
})

test_that("rnp_nelson_aalen e crescente e bate com cumhaz", {
  na <- rnp_nelson_aalen(lung$time, lung$status == 2)
  expect_true(all(diff(na$risco_acumulado) >= -1e-8))
  ref <- survival::survfit(survival::Surv(time, status) ~ 1, lung)$cumhaz
  expect_equal(na$risco_acumulado, round(ref, 4))
})

test_that("rnp_cox reproduz coxph", {
  r <- rnp_cox(survival::Surv(time, status) ~ age + sex + ph.ecog, lung)
  ref <- survival::coxph(survival::Surv(time, status) ~ age + sex + ph.ecog, lung)
  expect_equal(r$coeficientes$hazard_ratio,
               round(unname(exp(coef(ref))), 4), tolerance = 1e-3)
  expect_equal(r$modelo$concordancia,
               round(unname(summary(ref)$concordance[1]), 4))
  expect_lt(r$coeficientes$p_valor[r$coeficientes$termo == "sex"], 0.05)
})

test_that("rnp_cox_diagnosticos roda cox.zph", {
  fit <- rnp_cox(survival::Surv(time, status) ~ age + sex, lung)
  d <- rnp_cox_diagnosticos(fit, grafico = TRUE)
  expect_true("GLOBAL" %in% d$teste$termo)
  expect_s3_class(d$grafico, "ggplot")
})

test_that("rnp_sobrevivencia_parametrica ajusta AFT", {
  r <- rnp_sobrevivencia_parametrica(survival::Surv(time, status) ~ age + sex,
                                     lung, dist = "weibull")
  ref <- survival::survreg(survival::Surv(time, status) ~ age + sex, lung,
                           dist = "weibull")
  expect_equal(r$coeficientes$estimativa[1], round(unname(coef(ref))[1], 4),
               tolerance = 1e-3)
  expect_true(is.finite(r$aic))
})

test_that("rnp_tabela_vida produz sobrevivencia nao-crescente", {
  tv <- rnp_tabela_vida(lung$time, lung$status == 2,
                        intervalos = seq(0, 1000, by = 200))
  expect_true(all(diff(tv$sobrevivencia_acumulada) <= 1e-8))
  expect_true(all(tv$prob_evento >= 0 & tv$prob_evento <= 1))
})

test_that("rnp_cox_risco_relativo prediz risco por perfil", {
  fit <- rnp_cox(survival::Surv(time, status) ~ age + sex, lung)
  rr <- rnp_cox_risco_relativo(fit, data.frame(age = c(50, 70), sex = c(1, 1)))
  expect_equal(nrow(rr), 2L)
  expect_true(all(rr$risco_relativo > 0))
  # idade maior => risco relativo maior (coef de age > 0)
  expect_gt(rr$risco_relativo[2], rr$risco_relativo[1])
})
