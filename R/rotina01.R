library(tidyverse)
library(abjutils)
library(janitor)
library(writexl)
library(readxl)

# ESSA ROTINA MONTA E TRATA TODAS AS TABELAS DO INFOPEN E OUTRAS NECESSARIAS AS ANALISES
# O INTUITO EH CENTRALIZAR O BANCO DE DADOS FORMADO PELO TRATAMENTO DAS TABELAS ORIGINAIS

## VETORES --------------------------
vetor_filtro_monitoramento <- c("Monitoramento|Eletrônic(o|a)|Eletronic(o|a)|C(e|é)lula|Monitorado|Monitorados|Monitora(c|ç)(a|ã)o")
vetor_filtro_domiciliar <- c("Domiciliar|P/Domiciliar")


## CICLO 10 - SEMESTRE 01 - 2021 ----------------------

base_ciclo10_2021 <-
  read_xlsx(
    path = "../data_raw/ciclo10.xlsx"
  ) |>
  janitor::clean_names() |>
  filter(
    situacao_de_preenchimento == "Validado"
  )|>
  mutate_if(
    is.character,
    str_to_title
  ) |>
  mutate_if(
    is.character,
    str_trim
  ) |>
  mutate(
    ciclo = 10,
    semestre = 1,
    ano = 2021,

    monitoramento = case_when(
      str_detect(nome_do_estabelecimento,regex(vetor_filtro_monitoramento, ignore_case=TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    domiciliar = case_when(
      str_detect(nome_do_estabelecimento, regex(vetor_filtro_domiciliar, ignore_case =TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    modalidade = case_when(
      monitoramento == "Sim" & domiciliar == "Sim" ~ "Domiciliar monitorado eletronicamente",
      monitoramento == "Sim" & domiciliar == "Não" ~ "Monitoração eletrônica não domiciliar",
      monitoramento == "Não" & domiciliar == "Sim" ~ "Domiciliar não monitorado eletronicamente",
      monitoramento == "Não" & domiciliar == "Não" ~ "Custódia em unidade prisional",
      TRUE ~ "Erro"
    ),

    x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total,

  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  ) |>
  relocate(x1_3_capacidade_do_estabelecimento_total, .after = x1_3_capacidade_do_estabelecimento_feminino_total)



## CICLO 11 - SEMESTRE 02 - 2021 ----------------------

base_ciclo11_2021 <-
  read_xlsx(
    path = "../data_raw/ciclo11.xlsx"
  ) |>
  janitor::clean_names() |>
  filter(
    situacao_de_preenchimento == "Validado"
  )|>
  mutate_if(
    is.character,
    str_to_title
  ) |>
  mutate_if(
    is.character,
    str_trim
  ) |>
  mutate(
    ciclo = 11,
    semestre = 2,
    ano = 2021,

    monitoramento = case_when(
      str_detect(nome_do_estabelecimento,regex(vetor_filtro_monitoramento, ignore_case=TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    domiciliar = case_when(
      str_detect(nome_do_estabelecimento, regex(vetor_filtro_domiciliar, ignore_case =TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    modalidade = case_when(
      monitoramento == "Sim" & domiciliar == "Sim" ~ "Domiciliar monitorado eletronicamente",
      monitoramento == "Sim" & domiciliar == "Não" ~ "Monitoração eletrônica não domiciliar",
      monitoramento == "Não" & domiciliar == "Sim" ~ "Domiciliar não monitorado eletronicamente",
      monitoramento == "Não" & domiciliar == "Não" ~ "Custódia em unidade prisional",
      TRUE ~ "Erro"
    ),

    x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total
  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  ) |>
  relocate(x1_3_capacidade_do_estabelecimento_total, .after = x1_3_capacidade_do_estabelecimento_feminino_total)

## CICLO 12 - SEMESTRE 01 - 2022 ----------------------

base_ciclo12_2022 <-
  read_xlsx(
    path = "../data_raw/ciclo12.xlsx"
  ) |>
  janitor::clean_names() |>
  filter(
    situacao_de_preenchimento == "Validado"
  )|>
  mutate_if(
    is.character,
    str_to_title
  ) |>
  mutate_if(
    is.character,
    str_trim
  ) |>
  mutate(
    ciclo = 12,
    semestre = 1,
    ano = 2022,

    monitoramento = case_when(
      str_detect(nome_do_estabelecimento,regex(vetor_filtro_monitoramento, ignore_case=TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    domiciliar = case_when(
      str_detect(nome_do_estabelecimento, regex(vetor_filtro_domiciliar, ignore_case =TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    modalidade = case_when(
      monitoramento == "Sim" ~ "Domiciliar monitorado eletronicamente",
      monitoramento == "Não" & domiciliar == "Sim" ~ "Domiciliar não monitorado eletronicamente",
      monitoramento == "Não" & domiciliar == "Não" ~ "Custódia em unidade prisional"
    ),

    x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total
  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  ) |>
  relocate(x1_3_capacidade_do_estabelecimento_total, .after = x1_3_capacidade_do_estabelecimento_feminino_total)

## CICLO 13 - SEMESTRE 02 - 2022 ----------------------

base_ciclo13_2022 <-
  read_xlsx(
    path = "../data_raw/ciclo13.xlsx"
  ) |>
  janitor::clean_names() |>
  filter(
    situacao_de_preenchimento == "Validado"
  )|>
  mutate_if(
    is.character,
    str_to_title
  ) |>
  mutate_if(
    is.character,
    str_trim
  ) |>
  mutate(
    ciclo = 13,
    semestre = 2,
    ano = 2022,

    monitoramento = case_when(
      str_detect(nome_do_estabelecimento,regex(vetor_filtro_monitoramento, ignore_case=TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    domiciliar = case_when(
      str_detect(nome_do_estabelecimento, regex(vetor_filtro_domiciliar, ignore_case =TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    modalidade = case_when(
      monitoramento == "Sim" ~ "Domiciliar monitorado eletronicamente",
      monitoramento == "Não" & domiciliar == "Sim" ~ "Domiciliar não monitorado eletronicamente",
      monitoramento == "Não" & domiciliar == "Não" ~ "Custódia em unidade prisional"
    ),

    x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total,

  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  ) |>
  relocate(x1_3_capacidade_do_estabelecimento_total, .after = x1_3_capacidade_do_estabelecimento_feminino_total)

## EMPILHANDO AS TABELAS  ------------------

base_geral <- bind_rows(
  base_ciclo10_2021,
  base_ciclo11_2021,
  base_ciclo12_2022,
  base_ciclo13_2022
  ) |>
  mutate(
    uf = str_to_upper(uf)
  )

## SALVANDO AS TABELAS NA PASTA DE RELATORIOS DA DIRPP -------------
### TABELA GERAL -------------

write_rds(
  base_geral,
  file = "../data/data_rds/base_geral_infopen.rds"
)

write_xlsx(
  base_geral,
  path = "../data/data_xlsx/base_geral_infopen.xlsx"
)






## TESTES PARA VERIFICACAO DO MONITORAMENTO ELETRONICO POR CICLO -----

teste <-
  base_geral|>
  filter(ciclo == 13) |>
  group_by(
    modalidade
  ) |>
  summarise(
    soma = sum(x4_1_populacao_prisional_total, na.rm = TRUE)
  )

teste2 <-
  base_geral|>
  filter(ciclo == 13) |>
  group_by(
    ano
  ) |>
  summarise(
    soma = sum(x4_1_populacao_prisional_total, na.rm = TRUE)
  )

