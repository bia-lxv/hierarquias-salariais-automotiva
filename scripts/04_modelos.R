# =============================================================================
# 04_modelos.R
# Equações mincerianas — OLS (HC3), interações, WLS e Pooled OLS
# (Tabela 3 do artigo)
# -----------------------------------------------------------------------------
# Entrada : data/processed/dados_final.rds
# Saída   : output/tables/tabela3_regressoes.txt   (stargazer, todos os modelos)
#           output/tables/tabela3_regressoes.html  (para colar no Word)
#           output/tables/diagnosticos.txt
#           output/figures/diagnostico_*.png
# =============================================================================

source("scripts/00_config.R")

dados <- readRDS(file.path(DIR_PROCESSED, "dados_final.rds"))

# Garantias de tipagem e categoria de referência (occup: ref = "supp")
dados <- dados %>%
  mutate(
    educ   = as.numeric(educ),
    exper  = as.numeric(exper),
    exper2 = as.numeric(exper2),
    occup  = fct_relevel(occup, NIVEIS_OCCUP)
  )
options(contrasts = c("contr.treatment", "contr.poly"))

# Erros-padrão robustos HC3 a partir de um modelo lm
se_hc3 <- function(m) sqrt(diag(vcovHC(m, type = "HC3")))

## ---- MODELO 1: OLS base ----------------------------------------------------
modelo1 <- lm(
  log_wage ~ educ + higher_educ + race + exper + exper2 + occup + city + female,
  data = dados
)

## ---- MODELO 1.1: interação educ * race -------------------------------------
modelo1.1 <- lm(
  log_wage ~ educ * race + higher_educ + exper + exper2 + occup + city + female,
  data = dados
)

## ---- MODELO 1.2: interação exper * occup -----------------------------------
modelo1.2 <- lm(
  log_wage ~ educ * race + higher_educ + exper2 + exper * occup + city + female,
  data = dados
)

## ---- MODELO 1.3: interação exper * city ------------------------------------
modelo1.3 <- lm(
  log_wage ~ educ + female + race + higher_educ + exper2 + occup + exper * city,
  data = dados
)

## ---- MODELO WLS (robustez) -------------------------------------------------
# Pesos inversamente proporcionais ao quadrado dos resíduos do OLS base
residuos <- residuals(modelo1)
pesos    <- 1 / (residuos^2 + 1e-6)

modelo_wls <- lm(
  log_wage ~ educ + higher_educ + race + exper + exper2 + occup + city + female,
  data = dados, weights = pesos
)

## ---- MODELO POOLED OLS (plm) -----------------------------------------------
dados_pooled <- dados %>%
  mutate(
    date = as.Date(paste0(`competênciamov`, "01"), format = "%Y%m%d"),
    id   = row_number()
  )

modelo_pooled <- plm(
  log_wage ~ educ + higher_educ + race + exper + exper2 + occup + city +
    female + date,
  data  = dados_pooled,
  index = c("id", "date"),
  model = "pooling"
)
se_pooled_hc3 <- sqrt(diag(vcovHC(modelo_pooled, type = "HC3")))

## ---- Tabela 3: todos os modelos com SE HC3 ---------------------------------
lista_modelos <- list(modelo1, modelo1.1, modelo1.2, modelo1.3, modelo_wls)
lista_se <- c(lapply(lista_modelos, se_hc3), list(se_pooled_hc3))
lista_modelos <- c(lista_modelos, list(modelo_pooled))
rotulos <- c("OLS", "OLS educ×race", "OLS exper×occup",
             "OLS exper×city", "WLS", "Pooled OLS")

# Versão texto (conferência rápida no console e no repo)
stargazer(
  lista_modelos,
  type = "text",
  se = lista_se,
  column.labels = rotulos,
  title = "Tabela 3: Resultados das regressões estimadas (erros robustos HC3)",
  star.cutoffs = c(0.05, 0.01, 0.001),
  keep.stat = c("n", "rsq", "adj.rsq", "f", "ser", "ll"),
  out = file.path(DIR_TABLES, "tabela3_regressoes.txt")
)

# Versão HTML (abrir no navegador e copiar/colar no Word mantendo a tabela)
stargazer(
  lista_modelos,
  type = "html",
  se = lista_se,
  column.labels = rotulos,
  title = "Tabela 3: Resultados das regressões estimadas (erros robustos HC3)",
  star.cutoffs = c(0.05, 0.01, 0.001),
  keep.stat = c("n", "rsq", "adj.rsq", "f", "ser", "ll"),
  out = file.path(DIR_TABLES, "tabela3_regressoes.html")
)

## ---- Diagnósticos do modelo base -------------------------------------------
sink(file.path(DIR_TABLES, "diagnosticos.txt"))

cat("=== Heterocedasticidade (Breusch-Pagan) ===\n")
print(bptest(modelo1))

cat("\n=== Multicolinearidade (VIF) ===\n")
print(vif(modelo1))

cat("\n=== Normalidade dos resíduos (Shapiro-Wilk, amostra de até 5000) ===\n")
set.seed(123)
print(shapiro.test(sample(residuals(modelo1),
                          min(5000, length(residuals(modelo1))))))

cat("\n=== Autocorrelação (Durbin-Watson) ===\n")
print(dwtest(modelo1))

cat("\n=== Especificação (RESET) ===\n")
print(resettest(modelo1))

sink()

# Gráficos de diagnóstico
png(file.path(DIR_FIGURES, "diagnostico_qqplot.png"),
    width = 1600, height = 1200, res = 200)
qqnorm(residuals(modelo1)); qqline(residuals(modelo1), col = "red")
dev.off()

png(file.path(DIR_FIGURES, "diagnostico_cooks.png"),
    width = 1600, height = 1200, res = 200)
plot(cooks.distance(modelo1), type = "h", main = "Distância de Cook")
dev.off()

png(file.path(DIR_FIGURES, "diagnostico_leverage.png"),
    width = 1600, height = 1200, res = 200)
plot(hatvalues(modelo1), main = "Leverage")
dev.off()

message("Modelos concluídos -> output/tables/tabela3_regressoes.{txt,html}")
