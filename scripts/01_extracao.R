# =============================================================================
# 01_extracao.R
# Extração dos microdados do Novo CAGED (CAGEDMOV)
# -----------------------------------------------------------------------------
# Entrada : arquivos CAGEDMOV{AAAAMM}.txt em DIR_CAGED (baixados do MTE)
# Saída   : data/processed/caged_bruto_filtrado.rds  (+ .xlsx de conferência)
# -----------------------------------------------------------------------------
# Substitui os 48 blocos mensais repetidos por UMA função aplicada a todos
# os meses de 2021 a 2024 com purrr::map_dfr().
# =============================================================================

source("scripts/00_config.R")

## ---- 1. Lista de arquivos esperados ----------------------------------------
competencias <- as.vector(outer(ANOS, sprintf("%02d", 1:12), paste0)) |> sort()
arquivos <- file.path(DIR_CAGED, paste0("CAGEDMOV", competencias, ".txt"))

existe <- file.exists(arquivos)
if (any(!existe)) {
  warning(
    "Arquivos não encontrados (serão ignorados):\n",
    paste(basename(arquivos[!existe]), collapse = "\n")
  )
}
arquivos <- arquivos[existe]
message(length(arquivos), " arquivos mensais encontrados em ", DIR_CAGED)

## ---- 2. Função de leitura + filtro de um mês -------------------------------
ler_caged_mes <- function(caminho) {
  message("Lendo: ", basename(caminho))
  read_csv2(
    caminho,
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    filter(
      município %in% MUNICIPIOS,          # Betim, Porto Real, Goiana
      subclasse == SUBCLASSE_CNAE         # 2910-7/01 (fabricação de automóveis)
    ) %>%
    select(
      competênciamov,
      município,
      subclasse,
      saldomovimentação,
      cbo2002ocupação,
      idade,
      horascontratuais,
      raçacor,
      sexo,
      graudeinstrução,
      salário
    )
}

## ---- 3. Ler todos os meses e empilhar --------------------------------------
caged_bruto <- map_dfr(arquivos, ler_caged_mes)

message("Total de registros extraídos: ", nrow(caged_bruto))
count(caged_bruto, competênciamov) |> print(n = Inf)

## ---- 4. Nome dos municípios ------------------------------------------------
lookup_municipios <- tibble(
  município      = unname(MUNICIPIOS),
  name_municipio = names(MUNICIPIOS)
)

caged_bruto <- caged_bruto %>% left_join(lookup_municipios, by = "município")
table(caged_bruto$name_municipio, useNA = "ifany")

## ---- 5. Salvar -------------------------------------------------------------
# .rds preserva tipos e acentos; .xlsx apenas para conferência visual
saveRDS(caged_bruto, file.path(DIR_PROCESSED, "caged_bruto_filtrado.rds"))
write_xlsx(caged_bruto, file.path(DIR_PROCESSED, "caged_bruto_filtrado.xlsx"))

message("Extração concluída -> data/processed/caged_bruto_filtrado.rds")
