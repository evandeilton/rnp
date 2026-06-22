# Gera os conjuntos de dados didaticos do rnp (simulados, com sementes fixas).
# Execute com: source("data-raw/datasets.R")
set.seed(2024)

# 1) Resistencia de corpos de prova de concreto (engenharia de materiais).
#    Resistencia a compressao (MPa) em funcao do tempo de cura e do tipo de
#    cimento; util para descritiva, ANOVA e regressao.
n <- 90L
tipo_cimento <- factor(rep(c("CP-II", "CP-IV", "CP-V"), each = 30L))
dias_cura    <- rep(c(7L, 14L, 28L), times = 30L)
base_mpa     <- c("CP-II" = 18, "CP-IV" = 16, "CP-V" = 22)[as.character(tipo_cimento)]
resistencia  <- round(base_mpa + 0.45 * dias_cura +
                        rnorm(n, 0, 2.5), 1)
rnp_concreto <- data.frame(
  resistencia  = resistencia,    # MPa
  dias_cura    = dias_cura,      # dias
  tipo_cimento = tipo_cimento
)

# 2) Defeitos por lote de producao (controle de qualidade / contagens).
#    Numero de defeitos por lote, por turno e maquina; Poisson/superdispersao.
m <- 120L
turno   <- factor(sample(c("manha", "tarde", "noite"), m, TRUE))
maquina <- factor(sample(c("A", "B", "C"), m, TRUE))
lambda  <- exp(0.7 + c(manha = 0, tarde = 0.2, noite = 0.5)[as.character(turno)] +
                 c(A = 0, B = 0.3, C = -0.2)[as.character(maquina)])
rnp_defeitos <- data.frame(
  defeitos = rpois(m, lambda),
  turno    = turno,
  maquina  = maquina,
  tamanho_lote = sample(c(100L, 200L, 500L), m, TRUE)
)

# 3) Tempo de vida de componentes (confiabilidade / sobrevivencia).
#    Tempo ate a falha (horas) com censura a direita, por fornecedor.
k <- 150L
fornecedor <- factor(sample(c("Nacional", "Importado"), k, TRUE))
escala     <- c(Nacional = 800, Importado = 1200)[as.character(fornecedor)]
tempo_falha <- round(rweibull(k, shape = 1.8, scale = escala))
censura_em  <- 1500L                       # fim do estudo
evento      <- as.integer(tempo_falha <= censura_em)
tempo       <- pmin(tempo_falha, censura_em)
rnp_vida_util <- data.frame(
  tempo      = tempo,        # horas observadas
  evento     = evento,       # 1 = falhou; 0 = censurado
  fornecedor = fornecedor,
  temperatura = round(rnorm(k, 60, 8), 1)  # temperatura de operacao (C)
)

usethis::use_data(rnp_concreto, rnp_defeitos, rnp_vida_util,
                  overwrite = TRUE, compress = "xz")
