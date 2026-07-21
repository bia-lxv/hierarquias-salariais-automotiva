# =============================================================================
# 05_graficos.R
# Figuras do artigo (Gráficos 1 a 5) — todas salvas em output/figures/
# -----------------------------------------------------------------------------
# Entrada : data/processed/dados_final.rds
# Saída   : output/figures/grafico*.png
# =============================================================================

source("scripts/00_config.R")

dados <- readRDS(file.path(DIR_PROCESSED, "dados_final.rds"))

CORES_GENERO <- c("Male" = "steelblue", "Female" = "pink")
CORES_RACA   <- c("White" = "#B0B0B0", "Non-White" = "darkred")
CORES_CIDADE <- c("Betim (MG)" = "tomato3",
                  "Goiana (PE)" = "seagreen3",
                  "Porto Real (RJ)" = "steelblue2")

## ---- Função: média + IC 95% por grupo --------------------------------------
resumo_ic <- function(df, var_grupo) {
  df %>%
    group_by(grupo = .data[[var_grupo]]) %>%
    summarise(
      media   = mean(wage_44h, na.rm = TRUE),
      sd_wage = sd(wage_44h, na.rm = TRUE),
      n       = sum(!is.na(wage_44h)),
      IC_lower = media - qt(0.975, df = n - 1) * (sd_wage / sqrt(n)),
      IC_upper = media + qt(0.975, df = n - 1) * (sd_wage / sqrt(n)),
      .groups = "drop"
    ) %>%
    filter(n >= 2)
}

## ---- Gráfico 1: média salarial por grupo de ocupação (IC) ------------------
resumo_occup <- resumo_ic(dados, "occup")

g1 <- ggplot(resumo_occup, aes(x = reorder(grupo, media), y = media)) +
  geom_point(size = 4, shape = 21, fill = "blue", color = "black") +
  geom_errorbar(aes(ymin = IC_lower, ymax = IC_upper),
                width = 0.2, color = "black") +
  labs(
    title = "Graph 1. Confidence Interval of Average Wage by Occupation",
    x = "Group of Occupation",
    y = "Average Wage (R$)"
  ) +
  scale_y_continuous(
    breaks = seq(500, ceiling(max(resumo_occup$IC_upper)), by = 1000),
    labels = scales::comma,
    limits = c(1500, ceiling(max(resumo_occup$IC_upper)))
  ) +
  tema_artigo() +
  theme(legend.position = "none")

salvar_figura(g1, "grafico1_ic_salario_por_ocupacao")

## ---- Gráfico (complementar): média salarial por cidade (IC) ----------------
resumo_cidade <- resumo_ic(dados, "city")

g_cidade <- ggplot(resumo_cidade, aes(x = reorder(grupo, media), y = media)) +
  geom_point(size = 4, shape = 21, fill = "red", color = "black") +
  geom_errorbar(aes(ymin = IC_lower, ymax = IC_upper),
                width = 0.2, color = "black") +
  labs(
    title = "Confidence Interval of Average Wage by City",
    x = "City",
    y = "Average Wage (R$)"
  ) +
  scale_y_continuous(
    breaks = seq(100, ceiling(max(resumo_cidade$IC_upper)), by = 200),
    labels = scales::comma,
    limits = c(1750, ceiling(max(resumo_cidade$IC_upper)))
  ) +
  tema_artigo() +
  theme(legend.position = "none")

salvar_figura(g_cidade, "grafico_ic_salario_por_cidade")

## ---- Boxplot: distribuição salarial por cidade -----------------------------
limite_p90 <- quantile(dados$wage_44h, probs = 0.90, na.rm = TRUE)

g_box <- ggplot(dados, aes(x = city, y = wage_44h, color = city)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(1400, limite_p90)) +
  scale_color_manual(values = CORES_CIDADE) +
  labs(
    title = "Wage Distribution by City (up to 90th percentile)",
    x = "City",
    y = "Wage (R$)"
  ) +
  tema_artigo() +
  theme(legend.position = "none")

salvar_figura(g_box, "grafico_boxplot_salario_por_cidade")

## ---- Função: barras empilhadas 100% de composição --------------------------
grafico_composicao <- function(df, var_x, var_fill, cores, titulo, rotulo_x) {
  df_prop <- df %>%
    group_by(.data[[var_x]], .data[[var_fill]]) %>%
    summarise(count = n(), .groups = "drop")

  ggplot(df_prop,
         aes(x = .data[[var_x]], y = count, fill = .data[[var_fill]])) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    scale_fill_manual(values = cores) +
    labs(title = titulo, x = rotulo_x, y = "Percentage (%)", fill = NULL) +
    tema_artigo() +
    theme(legend.position = "right")
}

## ---- Gráfico 2: gênero por cidade (a) e por ocupação (b) -------------------
g2a <- grafico_composicao(dados, "city", "gender", CORES_GENERO,
                          "Gender Distribution by City", "City")
salvar_figura(g2a, "grafico2a_genero_por_cidade")

g2b <- grafico_composicao(dados, "occup", "gender", CORES_GENERO,
                          "Gender Distribution by Occupation", "Occupation")
salvar_figura(g2b, "grafico2b_genero_por_ocupacao")

## ---- Gráfico 3: raça por cidade (a) e por ocupação (b) ---------------------
g3a <- grafico_composicao(dados, "city", "race_label", CORES_RACA,
                          "Race Distribution by City", "City")
salvar_figura(g3a, "grafico3a_raca_por_cidade")

g3b <- grafico_composicao(dados, "occup", "race_label", CORES_RACA,
                          "Race Distribution by Occupation", "Occupation")
salvar_figura(g3b, "grafico3b_raca_por_ocupacao")

## ---- Gráfico 4: dispersão educ x log_wage (geral) --------------------------
cor_geral <- cor(dados$educ, dados$log_wage, use = "complete.obs")

g4 <- ggplot(dados, aes(x = educ, y = log_wage)) +
  geom_point(color = "darkgrey", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Scatter Plot of Education vs. Log Wage",
    x = "Years of Education (educ)",
    y = "Log Wage (log_wage)"
  ) +
  scale_x_continuous(breaks = seq(min(dados$educ), max(dados$educ), by = 1)) +
  scale_y_continuous(breaks = seq(floor(min(dados$log_wage)),
                                  ceiling(max(dados$log_wage)), by = 0.5)) +
  annotate("text",
           x = max(dados$educ) - 1.5,
           y = max(dados$log_wage) - 0.2,
           label = paste("Correlation:", round(cor_geral, 2)),
           size = 4, color = "black", family = FONTE_BASE) +
  tema_artigo() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

salvar_figura(g4, "grafico4_dispersao_educ_logwage")

## ---- Função: dispersão educ x log_wage por grupo, com correlações ----------
grafico_dispersao_grupo <- function(df, var_grupo, cores, titulo,
                                    rotulo_legenda) {
  correlacoes <- df %>%
    group_by(grupo = .data[[var_grupo]]) %>%
    summarise(r = cor(educ, log_wage, use = "complete.obs"), .groups = "drop")

  rotulos <- paste0(correlacoes$grupo, " correlation: ",
                    round(correlacoes$r, 2))

  ggplot(df, aes(x = educ, y = log_wage, color = .data[[var_grupo]])) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    scale_color_manual(values = cores) +
    labs(
      title = titulo,
      x = "Years of Education (educ)",
      y = "Log Wage (log_wage)",
      color = rotulo_legenda,
      caption = paste(rotulos, collapse = "  |  ")
    ) +
    scale_x_continuous(breaks = seq(min(df$educ), max(df$educ), by = 1)) +
    scale_y_continuous(breaks = seq(floor(min(df$log_wage)),
                                    ceiling(max(df$log_wage)), by = 0.5)) +
    tema_artigo() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "right",
      plot.caption = element_text(family = FONTE_BASE, size = 10, hjust = 0)
    )
}

## ---- Gráfico 5a: dispersão por raça ----------------------------------------
g5a <- grafico_dispersao_grupo(
  dados, "race_label", CORES_RACA,
  "Scatter plot of years of education vs. log wage by Race", "Race"
)
salvar_figura(g5a, "grafico5a_dispersao_por_raca")

## ---- Gráfico 5b: dispersão por cidade --------------------------------------
g5b <- grafico_dispersao_grupo(
  dados, "city", CORES_CIDADE,
  "Scatter plot of years of education vs. log wage by City", "City"
)
salvar_figura(g5b, "grafico5b_dispersao_por_cidade")

## ---- Matriz de correlação (corrplot) ---------------------------------------
correlacao <- cor(dados[, c("log_wage", "educ", "exper")],
                  use = "complete.obs", method = "pearson")

png(file.path(DIR_FIGURES, "matriz_correlacao.png"),
    width = 1400, height = 1400, res = 220)
corrplot(correlacao, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
dev.off()

message("Figuras concluídas -> output/figures/")
