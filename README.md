# Hierarquias salariais, território e desigualdade na indústria automotiva

Código em R do artigo sobre hierarquias salariais nas plantas da Stellantis em
**Betim (MG)**, **Porto Real (RJ)** e **Goiana (PE)**, com microdados do
**Novo CAGED** (2021–2024) e equações mincerianas com erros robustos (HC3).

## Estrutura do repositório

```
├── scripts/
│   ├── 00_config.R                    # caminhos, pacotes, constantes, tema dos gráficos
│   ├── 01_extracao.R                  # leitura dos CAGEDMOV mensais + filtro (municípios, CNAE 2910-7/01)
│   ├── 02_limpeza.R                   # CBO, grupos ocupacionais, educ/exper, dummies, salário 44h
│   ├── 03_estatisticas_descritivas.R  # Tabelas 1 e 2, testes t, correlações
│   ├── 04_modelos.R                   # OLS (HC3), interações, WLS, Pooled OLS, diagnósticos
│   ├── 05_graficos.R                  # Gráficos 1 a 5 do artigo
│   └── run_all.R                      # roda tudo na ordem
├── data/
│   ├── raw/         # (vazio no repo) microdados brutos — ver "Dados" abaixo
│   ├── aux/         # dicionário CBO 2002 + classificação manual das ocupações
│   └── processed/   # bases intermediárias e final (geradas pelos scripts)
└── output/
    ├── figures/     # figuras em .png (300 dpi)
    └── tables/      # tabelas (.xlsx), regressões (stargazer) e testes (.txt)
```

## Dados

Os microdados do Novo CAGED (arquivos `CAGEDMOV{AAAAMM}.txt`) **não são
versionados** por tamanho. Baixe-os do site do Ministério do Trabalho e
Emprego:

> https://www.gov.br/trabalho-e-emprego/pt-br/acesso-a-informacao/acoes-e-programas/programas-projetos-acoes-obras-e-atividades/estatisticas-trabalho/microdados-rais-e-caged

Coloque os arquivos mensais de 2021 a 2024 em uma pasta local e ajuste
`DIR_CAGED` em `scripts/00_config.R`.

Arquivos auxiliares (em `data/aux/`):

- `CBO2002 - Ocupacao.csv` — dicionário oficial de ocupações da CBO 2002;
- `classificacao_ocupacoes.xlsx` — classificação manual das 89 ocupações em
  grupos ocupacionais (`occup`: supp, ope, tec, adm, R_D, mgmt) e níveis
  (`level`), baseada na CBO/ISCO-88 e em Mintzberg (1979). Se o arquivo não
  existir, `02_limpeza.R` gera `ocupacoes_para_classificar.xlsx` para
  preenchimento único.

## Como reproduzir

```r
# na raiz do projeto (abra o .Rproj no RStudio)
source("scripts/run_all.R")
```

Ou rode os scripts individualmente na ordem `01` → `05`. Cada script carrega
`00_config.R` e lê/grava apenas em `data/processed/` e `output/`, então é
possível re-rodar qualquer etapa isoladamente sem repetir a extração.

## Decisões metodológicas (resumo)

- Amostra: admissões (`saldomovimentação == 1`) na subclasse CNAE 2910-7/01,
  2021–2024, nos três municípios;
- Exclusões: ocupações de saúde ocupacional e bombeiro civil, raça/cor não
  informada e grau de instrução não identificado (código 80);
- `educ` mapeada do grau de instrução para anos de estudo (0–22);
- `exper` = experiência potencial minceriana, truncada em zero;
- `wage_44h` = salário padronizado para jornada de 44 h; variável dependente
  = `log(wage_44h)`;
- Erros-padrão robustos HC3 em todos os modelos; WLS e Pooled OLS como
  checagens de robustez.

## Requisitos

R ≥ 4.2 e os pacotes listados em `scripts/00_config.R` (instalados
automaticamente na primeira execução). Para os gráficos em Times New Roman no
Windows, rode uma vez: `extrafont::font_import(); extrafont::loadfonts("win")`.
