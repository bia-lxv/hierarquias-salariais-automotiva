# =============================================================================
# 03_estatisticas_descritivas.R
# Estatísticas descritivas e testes t (Tabelas 1 e 2 do artigo)
# -----------------------------------------------------------------------------
# Entrada : data/processed/dados_final.rds
# Saída   : output/tables/tabela1_descritivas_continuas.xlsx
#           output/tables/tabela1_descritivas_categoricas.xlsx
#           output/tables/tabela2_educ_exper_por_cidade.xlsx
#           output/tables/testes_t.txt
# =============================================================================

source("scripts/00_config.R")

dados <- readRDS(file.path(DIR_PROCESSED, "dados_final.rds"))

## ---- 1. Variáveis contínuas (Tabela 1, parte contínua) ---------------------
resumo_continua <- function(x, nome) {
  tibble(
    variavel = nome,
    media    = mean(x, na.rm = TRUE),
    mediana  = median(x, na.rm = TRUE),
    dp       = sd(x, na.rm = TRUE),
    min      = min(x, na.rm = TRUE),
    max      = max(x, na.rm = TRUE),
    n        = sum(!is.na(x))
  )
}

tab1_continuas <- bind_rows(
  resumo_continua(dados$wage_44h, "wage_44h"),
  resumo_continua(dados$log_wage, "log_wage"),
  resumo_continua(dados$educ,     "educ"),
  resumo_continua(dados$exper,    "exper"),
  resumo_continua(dados$exper2,   "exper2")
)
print(tab1_continuas)
write_xlsx(tab1_continuas,
           file.path(DIR_TABLES, "tabela1_descritivas_continuas.xlsx"))

## ---- 2. Variáveis categóricas: salário médio por grupo ---------------------
resumo_categorica <- function(df, var) {
  df %>%
    group_by(grupo = .data[[var]]) %>%
    summarise(
      n        = n(),
      pct      = n() / nrow(df) * 100,
      media    = mean(wage_44h, na.rm = TRUE),
      mediana  = median(wage_44h, na.rm = TRUE),
      dp       = sd(wage_44h, na.rm = TRUE),
      min      = min(wage_44h, na.rm = TRUE),
      max      = max(wage_44h, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(variavel = var, .before = 1)
}

tab1_categoricas <- bind_rows(
  resumo_categorica(dados, "gender"),
  resumo_categorica(dados, "race_label"),
  resumo_categorica(dados, "occup"),
  resumo_categorica(dados, "city")
)
print(tab1_categoricas, n = Inf)
write_xlsx(tab1_categoricas,
           file.path(DIR_TABLES, "tabela1_descritivas_categoricas.xlsx"))

## ---- 3. Composição da amostra (concentração ocupacional) -------------------
# Contexto da discussão de heterocedasticidade no artigo
prop_ocupacoes <- dados %>%
  count(name_ocupacao, sort = TRUE) %>%
  mutate(pct = n / sum(n) * 100)
print(head(prop_ocupacoes, 10))

## ---- 4. Testes t (unilaterais) ---------------------------------------------
# Todos os testes reportados nas Tabelas 1 e 2 do artigo, gravados em .txt
sink(file.path(DIR_TABLES, "testes_t.txt"))

cat("=======================================================\n")
cat("TESTES t UNILATERAIS (alternative = 'less')\n")
cat("=======================================================\n\n")

cat("--- Salário: mulheres < homens ---\n")
print(t.test(dados$wage_44h[dados$female == 1],
             dados$wage_44h[dados$female == 0],
             alternative = "less"))

cat("\n--- Salário: não brancos < brancos ---\n")
print(t.test(dados$wage_44h[dados$race == 2],
             dados$wage_44h[dados$race == 1],
             alternative = "less"))

cat("\n--- Salário: ope < supp ---\n")
print(t.test(dados$wage_44h[dados$occup == "ope"],
             dados$wage_44h[dados$occup == "supp"],
             alternative = "less"))

cat("\n--- Salário: Porto Real < Betim ---\n")
print(t.test(dados$wage_44h[dados$city == "Porto Real (RJ)"],
             dados$wage_44h[dados$city == "Betim (MG)"],
             alternative = "less"))

cat("\n--- Salário: Goiana < Betim ---\n")
print(t.test(dados$wage_44h[dados$city == "Goiana (PE)"],
             dados$wage_44h[dados$city == "Betim (MG)"],
             alternative = "less"))

cat("\n=======================================================\n")
cat("HIPÓTESES 6 e 7: educação e experiência por cidade\n")
cat("=======================================================\n\n")

cat("--- Médias por cidade ---\n")
print(aggregate(educ  ~ city, data = dados, FUN = mean))
print(aggregate(exper ~ city, data = dados, FUN = mean))

cat("\n--- Educação: Goiana < Betim ---\n")
print(t.test(dados$educ[dados$city == "Goiana (PE)"],
             dados$educ[dados$city == "Betim (MG)"],
             alternative = "less"))

cat("\n--- Educação: Porto Real < Betim ---\n")
print(t.test(dados$educ[dados$city == "Porto Real (RJ)"],
             dados$educ[dados$city == "Betim (MG)"],
             alternative = "less"))

cat("\n--- Experiência: Goiana < Betim ---\n")
print(t.test(dados$exper[dados$city == "Goiana (PE)"],
             dados$exper[dados$city == "Betim (MG)"],
             alternative = "less"))

cat("\n--- Experiência: Porto Real < Betim ---\n")
print(t.test(dados$exper[dados$city == "Porto Real (RJ)"],
             dados$exper[dados$city == "Betim (MG)"],
             alternative = "less"))

sink()

## ---- 5. Tabela 2: educação e experiência médias por cidade -----------------
tab2 <- dados %>%
  group_by(city) %>%
  summarise(
    media_exper = mean(exper, na.rm = TRUE),
    media_educ  = mean(educ,  na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(tab2)
write_xlsx(tab2, file.path(DIR_TABLES, "tabela2_educ_exper_por_cidade.xlsx"))

## ---- 6. Correlações --------------------------------------------------------
sink(file.path(DIR_TABLES, "correlacoes.txt"))

cat("--- Correlação geral: educ x log_wage ---\n")
print(cor.test(dados$educ, dados$log_wage))

cat("\n--- Por raça ---\n")
print(cor.test(dados$educ[dados$race_label == "White"],
               dados$log_wage[dados$race_label == "White"]))
print(cor.test(dados$educ[dados$race_label == "Non-White"],
               dados$log_wage[dados$race_label == "Non-White"]))

cat("\n--- Por gênero ---\n")
print(cor.test(dados$educ[dados$gender == "Male"],
               dados$log_wage[dados$gender == "Male"]))
print(cor.test(dados$educ[dados$gender == "Female"],
               dados$log_wage[dados$gender == "Female"]))

cat("\n--- Por cidade ---\n")
for (cid in levels(dados$city)) {
  cat("\n>>", cid, "\n")
  print(cor.test(dados$educ[dados$city == cid],
                 dados$log_wage[dados$city == cid]))
}

cat("\n--- Matriz de correlação (log_wage, educ, exper) ---\n")
print(cor(dados[, c("log_wage", "educ", "exper")],
          use = "complete.obs", method = "pearson"))

sink()

message("Estatísticas descritivas concluídas -> output/tables/")
