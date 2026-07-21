# =============================================================================
# 02_limpeza.R
# Limpeza e construção das variáveis do modelo
# -----------------------------------------------------------------------------
# Entrada : data/processed/caged_bruto_filtrado.rds
#           data/aux/CBO2002 - Ocupacao.csv         (dicionário de ocupações)
#           data/aux/classificacao_ocupacoes.xlsx   (classificação manual:
#                                                    cbo2002ocupação, occup, level)
# Saída   : data/processed/dados_final.rds / .xlsx
# -----------------------------------------------------------------------------
# =============================================================================

source("scripts/00_config.R")

dados <- readRDS(file.path(DIR_PROCESSED, "caged_bruto_filtrado.rds"))

## ---- 1. Recorte: trabalhadores contratados ---------------------------------
# O artigo analisa admissões (saldomovimentação == 1); desligamentos são
# preservados em arquivo separado caso sejam úteis em outra análise.
dados_desligados <- dados %>% filter(saldomovimentação == -1)
saveRDS(dados_desligados, file.path(DIR_PROCESSED, "dados_desligados.rds"))

dados <- dados %>% filter(saldomovimentação == 1)
message("Contratados: ", nrow(dados), " observações")

## ---- 2. Nome das ocupações (dicionário CBO 2002) ---------------------------
cbo <- read_delim(
  ARQ_CBO,
  delim = ";",
  locale = locale(encoding = "latin1"),   # arquivo original vem em latin1
  show_col_types = FALSE
) %>%
  rename(name_ocupacao = TITULO, cbo2002ocupação = CODIGO) %>%
  mutate(cbo2002ocupação = as.numeric(cbo2002ocupação))

dados <- dados %>% left_join(cbo, by = "cbo2002ocupação")

# Conferência: ocupações sem título no dicionário
sem_titulo <- dados %>% filter(is.na(name_ocupacao)) %>% distinct(cbo2002ocupação)
if (nrow(sem_titulo) > 0) {
  warning("Ocupações sem título no dicionário CBO: ",
          paste(sem_titulo$cbo2002ocupação, collapse = ", "))
}

## ---- 3. Classificação manual em grupos ocupacionais (occup / level) --------
if (!file.exists(ARQ_CLASSIFICACAO)) {
  ocupacoes_unicas <- dados %>%
    distinct(cbo2002ocupação, name_ocupacao) %>%
    arrange(name_ocupacao) %>%
    mutate(occup = NA_character_, level = NA_character_)

  write_xlsx(ocupacoes_unicas,
             file.path(DIR_AUX, "ocupacoes_para_classificar.xlsx"))
  stop(
    "Arquivo de classificação não encontrado.\n",
    "Foi gerado 'data/aux/ocupacoes_para_classificar.xlsx' com as ocupações únicas.\n",
    "Preencha as colunas 'occup' (supp/ope/tec/adm/R_D/mgmt) e 'level',\n",
    "salve como 'data/aux/classificacao_ocupacoes.xlsx' e rode este script de novo."
  )
}

classificacao <- read_excel(ARQ_CLASSIFICACAO) %>%
  select(cbo2002ocupação, occup, level) %>%
  mutate(cbo2002ocupação = as.numeric(cbo2002ocupação))

dados <- dados %>% left_join(classificacao, by = "cbo2002ocupação")

## ---- 4. Exclusões (idênticas às descritas na seção Dados do artigo) --------
# 4a. Ocupações de difícil classificação (saúde ocupacional e bombeiro)
dados <- dados %>% filter(!name_ocupacao %in% OCUPACOES_REMOVER)

# 4b. Raça/cor não informada (código 6) — 168 obs (~1,5% da amostra)
dados <- dados %>% filter(raçacor != 6)

# 4c. Grau de instrução não identificado (código 80)
dados <- dados %>% filter(graudeinstrução != 80)

message("Após exclusões: ", nrow(dados), " observações")

## ---- 5. Variáveis derivadas ------------------------------------------------
dados <- dados %>%
  mutate(
    ## Anos de estudo a partir do grau de instrução (Novo CAGED)
    educ = case_when(
      graudeinstrução == 1  ~ 0,    # Analfabeto
      graudeinstrução == 2  ~ 3,    # Até 5ª incompleto
      graudeinstrução == 3  ~ 5,    # 5ª completo fundamental
      graudeinstrução == 4  ~ 7,    # 6ª a 9ª fundamental
      graudeinstrução == 5  ~ 9,    # Fundamental completo
      graudeinstrução == 6  ~ 10,   # Médio incompleto
      graudeinstrução == 7  ~ 12,   # Médio completo
      graudeinstrução == 8  ~ 13,   # Superior incompleto
      graudeinstrução == 9  ~ 16,   # Superior completo
      graudeinstrução == 10 ~ 18,   # Mestrado
      graudeinstrução == 11 ~ 22,   # Doutorado
      TRUE ~ NA_real_
    ),

    ## Experiência potencial (Mincer):
    ## - educ < 12  -> idade - 18
    ## - educ >= 12 -> idade - educ - 6
    ## truncada em zero
    exper  = ifelse(educ < 12, idade - 18, idade - educ - 6),
    exper  = pmax(exper, 0),
    exper2 = exper^2,

    ## Dummies sociodemográficas
    race   = ifelse(raçacor == 1, 1, 2),         # 1 = branco; 2 = não branco
    female = case_when(sexo == 1 ~ 0,            # homem
                       sexo == 3 ~ 1,            # mulher
                       TRUE ~ NA_real_),

    ## Salário padronizado para jornada de 44h semanais e log
    salário  = as.numeric(salário),
    wage_44h = round((salário / horascontratuais) * 44, 2),
    log_wage = log(wage_44h),

    ## Dummy de ensino superior completo
    higher_educ = ifelse(educ >= 16, 1, 0),

    ## Rótulos para gráficos
    gender     = ifelse(female == 1, "Female", "Male"),
    race_label = ifelse(race == 1, "White", "Non-White")
  ) %>%
  rename(city = name_municipio)

## ---- 6. Imputação documentada de NA em educ/exper --------------------------
# No código original, 1 observação com educ NA foi imputada manualmente pela
# posição da linha (linha 264: educ = 12, exper = 18 — perfil técnico).
# Aqui a mesma decisão é aplicada POR REGRA, independente da ordem das linhas.
n_na_educ <- sum(is.na(dados$educ))
if (n_na_educ > 0) {
  message("Imputando educ = 12 (perfil técnico) em ", n_na_educ, " observação(ões) com NA")
  dados <- dados %>%
    mutate(
      exper = ifelse(is.na(educ), pmax(idade - 12 - 6, 0), exper),
      exper2 = exper^2,
      educ  = ifelse(is.na(educ), 12, educ)
    )
}

## ---- 7. Tipagem final ------------------------------------------------------
dados <- dados %>%
  mutate(
    female = as.factor(female),
    race   = as.factor(race),
    city   = as.factor(city),
    level  = as.factor(level),
    occup  = factor(occup, levels = NIVEIS_OCCUP)   # referência = "supp"
  )

# Conferência de valores ausentes nas variáveis do modelo
vars_modelo <- c("log_wage", "educ", "exper", "exper2", "race",
                 "female", "occup", "city", "higher_educ")
colSums(is.na(dados[vars_modelo])) |> print()

## ---- 8. Salvar base final --------------------------------------------------
saveRDS(dados, file.path(DIR_PROCESSED, "dados_final.rds"))
write_xlsx(dados, file.path(DIR_PROCESSED, "dados_final.xlsx"))

message("Limpeza concluída -> data/processed/dados_final.rds (",
        nrow(dados), " observações)")
