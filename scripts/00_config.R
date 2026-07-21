# =============================================================================
# 00_config.R
# Configurações gerais do projeto
# Artigo: Hierarquias salariais, território e desigualdade na indústria
#         automotiva (Stellantis: Betim, Porto Real e Goiana) — Novo CAGED
# =============================================================================
# Este script é carregado no início de todos os demais via source().
# Rode os scripts sempre a partir da raiz do projeto (abra o .Rproj no RStudio).
# =============================================================================

## ---- Pacotes ---------------------------------------------------------------
pacotes <- c(
  "tidyverse",   # dplyr, ggplot2, readr, purrr, forcats etc.
  "readxl", "writexl",
  "psych",       # estatísticas descritivas (describe)
  "lmtest", "sandwich", "car", "stargazer", "plm",
  "corrplot",
  "scales"
)

instalar <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(pacotes, library, character.only = TRUE))

## ---- Caminhos --------------------------------------------------------------
# ATENÇÃO: ajuste apenas DIR_CAGED para a pasta onde estão os microdados
# baixados do site do MTE (arquivos CAGEDMOV{AAAAMM}.txt).
# Fonte: https://www.gov.br/trabalho-e-emprego/pt-br/acesso-a-informacao/
#        acoes-e-programas/programas-projetos-acoes-obras-e-atividades/
#        estatisticas-trabalho/microdados-rais-e-caged
DIR_CAGED <- "~/Desktop/BRAIN/DADOS"      # <<< AJUSTAR AQUI (fora do repo)

DIR_AUX       <- "data/aux"               # arquivos auxiliares (CBO, classificação)
DIR_PROCESSED <- "data/processed"         # bases intermediárias e final
DIR_FIGURES   <- "output/figures"
DIR_TABLES    <- "output/tables"

ARQ_CBO           <- file.path(DIR_AUX, "CBO2002 - Ocupacao.csv")
ARQ_CLASSIFICACAO <- file.path(DIR_AUX, "classificacao_ocupacoes.xlsx")

## ---- Constantes do estudo --------------------------------------------------
# Municípios (códigos IBGE de 6 dígitos usados no Novo CAGED)
MUNICIPIOS <- c(
  "Betim (MG)"      = 310670,
  "Porto Real (RJ)" = 330411,
  "Goiana (PE)"     = 260620
)

# CNAE subclasse: 2910-7/01 — Fabricação de automóveis, camionetas e utilitários
SUBCLASSE_CNAE <- 2910701

# Período de análise
ANOS <- 2021:2024

# Ordem dos grupos ocupacionais (referência do modelo = "supp")
NIVEIS_OCCUP <- c("supp", "ope", "tec", "adm", "R_D", "mgmt")

# Ocupações excluídas da análise (difícil classificação — ver seção Dados)
OCUPACOES_REMOVER <- c(
  "Médico do trabalho",
  "Enfermeiro do trabalho",
  "Enfermeiro",
  "Técnico de enfermagem do trabalho",
  "Técnico de enfermagem",
  "Bombeiro civil"
)

## ---- Tema padrão dos gráficos ----------------------------------------------
# Times New Roman: no Windows, rode uma única vez:
#   install.packages("extrafont"); library(extrafont)
#   font_import(); loadfonts(device = "win")
FONTE_BASE <- "Times New Roman"

tema_artigo <- function(base_size = 12) {
  theme_light(base_family = FONTE_BASE, base_size = base_size) +
    theme(
      plot.title  = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12, color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    )
}

# Função auxiliar para salvar figuras com padrão único
salvar_figura <- function(plot, nome, largura = 8, altura = 5.5, dpi = 300) {
  ggsave(
    filename = file.path(DIR_FIGURES, paste0(nome, ".png")),
    plot = plot, width = largura, height = altura, dpi = dpi, bg = "white"
  )
}

message("Config carregada: ", length(MUNICIPIOS), " municípios | subclasse ",
        SUBCLASSE_CNAE, " | período ", min(ANOS), "-", max(ANOS))
