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

vetor_filtro_monitoramento <- c("Monitoramento","Eletrônico","Eletronico")
vetor_filtro_domiciliar    <- c("Domiciliar")

base_ciclo10_2021 <-
  read.csv2(
    file = "../data_raw/ciclo10.csv",
    fileEncoding = "UTF-8",
    quote = "",
    row.names = NULL,
    stringsAsFactors = FALSE
  ) |>
  janitor::clean_names() |>
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
    ano = 2021
  ) |>
  relocate(
    ciclo, .before = situacao_de_preenchimento,
  ) |>
  relocate(
    semestre, .after = ciclo
  ) |>
  relocate(
    ano, .after = semestre,
  )

## CICLO 11 - SEMESTRE 02 - 2021 ----------------------

base_ciclo11_2021 <-
  read.csv2(
    file = "../data_raw/ciclo11.csv",
    fileEncoding = "UTF-8",
    quote = "",
    row.names = NULL,
    stringsAsFactors = FALSE
  ) |>
  janitor::clean_names() |>
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
    ano = 2021
  ) |>
  relocate(
    ciclo, .before = situacao_de_preenchimento,
  ) |>
  relocate(
    semestre, .after = ciclo
  ) |>
  relocate(
    ano, .after = semestre,
  )

## CICLO 12 - SEMESTRE 01 - 2022 ----------------------

base_ciclo12_2022 <-
  read.csv2(
    file = "../data_raw/ciclo12.csv",
    sep = ";",
    fileEncoding = "UTF-8",
    quote = "",
    row.names = NULL,
    stringsAsFactors = FALSE
  ) |>
  janitor::clean_names() |>
  filter(situacao_de_preenchimento == "Validado") |>
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
      monitoramento == "Sim" ~ "Domiciliar monitorado",
      monitoramento == "Sim" & domiciliar == "Sim" ~ "Domiciliar monitorado",
      monitoramento == "Não" & domiciliar == "Sim" ~ "Domiciliar não monitorado",
      monitoramento == "Não" & domiciliar == "Não" ~ "Presencial",
    ),

    x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total
  ) |>
  relocate(
    ciclo, .before = situacao_de_preenchimento,
  ) |>
  relocate(
    semestre, .after = ciclo
  ) |>
  relocate(
    ano, .after = semestre,
  )|>
  relocate(
    modalidade, .after = nome_do_estabelecimento,
  )|>
  relocate(
    monitoramento, .after = modalidade,
  )|>
  relocate(
    domiciliar, .after = monitoramento,
  )


base_ciclo12_excel_sisdepen <-
  read_xlsx(
    path = "../data_raw/ciclo12_excel_sisdepen.xlsx"
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
      monitoramento == "Sim" ~ "Domiciliar monitorado",
      monitoramento == "Não" & domiciliar == "Sim" ~ "Domiciliar não monitorado",
      monitoramento == "Não" & domiciliar == "Não" ~ "Presencial"
    ),

    x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total
  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade,everything()
  ) |>
  relocate(x1_3_capacidade_do_estabelecimento_total, .after = x1_3_capacidade_do_estabelecimento_feminino_total)

teste <-
  base_ciclo12_excel_sisdepen |>
  group_by(
    modalidade
  ) |>
  summarise(
    soma = sum(x4_1_populacao_prisional_total, na.rm = TRUE)
  )

teste2 <- base_ciclo12_excel_sisdepen |>
  group_by(
    ano
  ) |>
  summarise(
    soma = sum(x4_1_populacao_prisional_total, na.rm = TRUE)
  )

teste3 <-
  base_ciclo12_excel_sisdepen |>
  filter(is.na(x4_1_populacao_prisional_total))

busca_unidade1 <-
  base_ciclo12_excel_sisdepen |>
  filter(
    situacao_de_preenchimento == "Validado",
    str_detect(modalidade, "Domiciliar")
  )|>
  select(nome_do_estabelecimento,x4_1_populacao_prisional_total)

busca_unidade2 <-
  base_ciclo12_2022|>
  filter(
    situacao_de_preenchimento == "Validado",
    str_detect(modalidade, "Domiciliar")
  )|>
  select(nome_do_estabelecimento,x4_1_populacao_prisional_total)

unidade <-
  full_join(x = busca_unidade1, y = busca_unidade2, by = "nome_do_estabelecimento",relationship = "many-to-many" ) |>
  mutate(
    sim = case_when(
      x4_1_populacao_prisional_total.x != x4_1_populacao_prisional_total.y ~ "Diferente",
      TRUE ~ "Igual"
    )
  )



for (i in 1:NROW(busca_unidade)){
  for(j in 1:NROW(busca_unidade)){
  if(busca_unidade$x4_1_populacao_prisional_total[i]+busca_unidade$populacao2[j] == 124){
    print(busca_unidade$nome_do_estabelecimento[i])
    print(busca_unidade$nome_do_estabelecimento[j])
  }
}
}





## EMPILHANDO AS TABELAS  ------------------




base_geral <- bind_rows(
  base_ciclo10_2021,
  base_ciclo11_2021,
  base_ciclo12_2022
) |>
mutate(
  nome_do_estabelecimento = abjutils::rm_accent(nome_do_estabelecimento),

  monitoramento = case_when(
      str_detect(nome_do_estabelecimento,regex(vetor_filtro_monitoramento, ignore_case=TRUE)) ~ "Sim",
      TRUE ~ "Não"
  ),

    domiciliar = case_when(
      str_detect(nome_do_estabelecimento, regex(vetor_filtro_domiciliar, ignore_case =TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

  x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total,

  cela_fisica = case_when(
    monitoramento == "Não" & domiciliar == "Não" ~ "Sim",
    TRUE ~ "Não"
  ),

  domiciliar_com_monitoramento = case_when(
    monitoramento == "Sim" & domiciliar == "Sim" ~ "Sim",
    TRUE ~ "Não"
  ),

  domiciliar_sem_monitoramento = case_when(
    monitoramento == "Não" & domiciliar == "Sim" ~ "Sim",
    TRUE ~ "Não"
  ),

  contagem = n()

) |>
  relocate(
    cela_fisica, .after =  nome_do_estabelecimento
) |>
  relocate(
    domiciliar_com_monitoramento, .after =  cela_fisica
)|>
  relocate(
    domiciliar_sem_monitoramento, .after =  domiciliar_com_monitoramento
  )|>
  relocate(
   monitoramento, .after =  domiciliar_sem_monitoramento
  )|>
  relocate(
    domiciliar, .after =  monitoramento
  )|>
  relocate(
    x1_3_capacidade_do_estabelecimento_total, .after =  x1_3_capacidade_do_estabelecimento_feminino_total
)

## SALVANDO AS TABELAS NA PASTA DESTE PROJETO -------------

write_rds(
  base_geral,
  file = "data/base_geral_infopen.rds"
)

## SALVANDO TABELA NO RELATORIO DE INDICADORES -------------

write_rds(
  base_geral,
  file = "../relatorio_indicadores/data_raw/base_geral_infopen.rds"
)

mon_ciclo12 <-
  base_geral |>
  filter(
    ciclo ==12,
    situacao_de_preenchimento == "Validado"
  ) |>
  group_by(
    ciclo, domiciliar_com_monitoramento,domiciliar_sem_monitoramento
  ) |>
  summarise(
    pop = sum(x4_1_populacao_prisional_total, na.rm = TRUE),
    contagem = n()
  )

teste_pop <- base_geral |>
  filter(
    ciclo == 12,
    x4_1_populacao_prisional_total == 123                              )

comp_ciclo12 <-
  read.csv2(
    file = "data_raw/csv_site_ciclo12.csv",
    fileEncoding = "UTF-8",
    quote = "",
    row.names = NULL,
    stringsAsFactors = FALSE
  ) |>
  janitor::clean_names() |>
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
    ano = 2021
  ) |>
  relocate(
    ciclo, .before = situacao_de_preenchimento,
  ) |>
  relocate(
    semestre, .after = ciclo
  ) |>
  relocate(
    ano, .after = semestre,
  )
comp_ciclo12 <- comp_ciclo12 |>
  mutate(
    nome_do_estabelecimento = abjutils::rm_accent(nome_do_estabelecimento),

    monitoramento = case_when(
      str_detect(nome_do_estabelecimento,regex(vetor_filtro_monitoramento, ignore_case=TRUE)) ~ "Sim",
      TRUE ~ "Não"
    ),

    domiciliar = case_when(
      str_detect(nome_do_estabelecimento, regex(vetor_filtro_domiciliar, ignore_case =TRUE)) & monitoramento == "Não" ~ "Sim",
      TRUE ~ "Não"
    ),

    x1_3_capacidade_do_estabelecimento_total = x1_3_capacidade_do_estabelecimento_masculino_total+x1_3_capacidade_do_estabelecimento_feminino_total,

    monitoramento_domiciliar = case_when(
      monitoramento == "Sim" & domiciliar == "Sim" ~ "Sim",
      TRUE ~ "Não"
    )

  ) |>
  relocate(
    monitoramento, .after =  nome_do_estabelecimento
  ) |>
  relocate(
    domiciliar, .after =  monitoramento
  )|>
  relocate(
    x1_3_capacidade_do_estabelecimento_total, .after =  x1_3_capacidade_do_estabelecimento_feminino_total
  )|>
  relocate(
    monitoramento_domiciliar, .after =  domiciliar
  ) |>
  select(
    monitoramento,monitoramento_domiciliar,domiciliar,nome_do_estabelecimento,x4_1_populacao_prisional_total,situacao_de_preenchimento
  ) |>
  filter()
