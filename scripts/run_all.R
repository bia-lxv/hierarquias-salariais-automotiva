# =============================================================================
# run_all.R — Roda o pipeline completo, na ordem
# =============================================================================
# Pré-requisitos:
#   1. Microdados CAGEDMOV{AAAAMM}.txt em DIR_CAGED (ver 00_config.R)
#   2. data/aux/CBO2002 - Ocupacao.csv
#   3. data/aux/classificacao_ocupacoes.xlsx (gerado/preenchido na 1ª execução)
# =============================================================================

source("scripts/01_extracao.R")
source("scripts/02_limpeza.R")
source("scripts/03_estatisticas_descritivas.R")
source("scripts/04_modelos.R")
source("scripts/05_graficos.R")

message("\n=== Pipeline completo executado com sucesso ===")
