library(tidyverse)
library(abjutils)
library(janitor)
library(writexl)
library(readxl)

## ESSA ROTINA TRATA AS TABELAS DA PESQUISA SOBRE TRABALHO PRISIONAL

tabela_alagoas <-
  read_xlsx(
      path = "../data_raw/trabalho_informacoes_alagoas.xlsx"
  ) |>
  janitor::clean_names() |>
  mutate(
    uf = "AL"
  ) |>
  relocate(
    uf, .before = unidade_prisional
  ) |>
  mutate(
    atividade = case_when(
      str_detect(oficina_atividade, regex(c("(blocos)|(concreto)|(bloquetes)|(cimento)"),ignore_case=TRUE)) ~ "Artefatos de concreto",
      str_detect(produto_produzido, regex(c("(blocos)|(concreto)|(bloquetes)|(cimento)"),ignore_case=TRUE)) ~ "Artefatos de concreto",
      TRUE ~ "Outros"
    )
  ) |>
  select(uf, unidade_prisional, atividade)

tabela_parana <-
  read_xlsx(
    path = "../data_raw/trabalho_informacoes_parana.xlsx",
    skip = 0
  ) |>
  janitor::clean_names()
names(tabela_parana) <- c("empresa","atividade","num_ppp","remuneracao","destinacao")
tabela_parana <- tabela_parana |>
  filter(
    empresa != "EMPRESA"
  ) |>
  mutate(
    unidade_prisional = empresa
  ) |>
  relocate(
    unidade_prisional, .before = empresa
  )

tabela_parana2 <-
  read_xlsx(
    path = "../data_raw/tabela_trabalho_parana.xlsx",
    skip = 0
  ) |>
  filter(
    !is.na(atividade)
  ) |>
  separate(
    col = remuneracao,
    into = c("rem_minima","rem_maxima"),
    sep = "a"
  ) |>
  mutate(
    rem_minima = str_sub(rem_minima, start = 4),
    rem_minima = str_replace_all(rem_minima, ",00",""),
    rem_maxima = str_sub(rem_maxima, start = 5),
    rem_maxima = str_replace_all(rem_maxima, ",00",""),
    uf = "PR"
  ) |>
  relocate(
    uf, .before = unidade_prisional
  )|>
  select(uf, unidade_prisional, atividade)

tabela_santa_catarina <-
  read_xlsx(
    path = "../data_raw/trabalho_informacoes_santa_catarina.xlsx",
    skip = 1
  ) |>
  janitor::clean_names() |>
  mutate(
    uf = "SC",
  ) |>
  select(
    uf,
    unidade_prisional = unidade,
    empresa = nome_estabelecimento_empresa_convenio,
    atividades_exercidas,
  ) |>
  mutate(
    atividade = case_when(
      str_detect(atividades_exercidas, regex(c("(blocos)|(concreto)|(bloquetes)|(cimento)"),ignore_case=TRUE)) ~ "Artefatos de concreto",
      TRUE ~ "Outros"
    )
  )|>
  select(uf, unidade_prisional, atividade)

tabela_forms <-
  read_xlsx(
    path = "../data_raw/trabalho_forms_geral.xlsx",
    skip = 0
  ) |>
  janitor::clean_names() |>
  select(-carimbo_de_data_hora)
names(tabela_forms) <- c("uf1","unidade_prisional","forma_financiamento","atividade_laboral","caracteristicas_materiais")

tabela_forms <- tabela_forms|>
  filter(
    !uf1 %in% c("Paraná","Santa Catarina","ALAGOAS FEITO - FALTA COMPLEMENTAR INFORMAÇÕES")
  ) |>
  mutate(
    uf = case_when(
      str_detect(uf1, regex(c("(rora(i|í)ma)|(RORAIMA)"),ignore_case=TRUE)) ~ "RR",
      str_detect(uf1, regex(c("(Rio Grande do Sul)"),ignore_case=TRUE)) ~ "RS",
      str_detect(uf1, regex(c("(Mato Grosso do Su)"),ignore_case=TRUE)) ~ "MS",
      str_detect(uf1, regex(c("(AMAP(Á|A))"),ignore_case=TRUE)) ~ "AP",
      str_detect(uf1, regex(c("(São Paulo)"),ignore_case=TRUE)) ~ "SP",
      str_detect(uf1, regex(c("(Rondônia)"),ignore_case=TRUE)) ~ "RO",
      str_detect(uf1, regex(c("(Mato Grosso)"),ignore_case=TRUE)) ~ "MT",
      str_detect(uf1, regex(c("(AMAZONAS)"),ignore_case=TRUE)) ~ "AM",
      str_detect(uf1, regex(c("(Sergipe)"),ignore_case=TRUE)) ~ "SE",
      str_detect(uf1, regex(c("(GOIAS)"),ignore_case=TRUE)) ~ "GO",
      str_detect(uf1, regex(c("(Bahia)"),ignore_case=TRUE)) ~ "BA",
      str_detect(uf1, regex(c("(Maranhão)"),ignore_case=TRUE)) ~ "MA",
      str_detect(uf1, regex(c("(Rio Grande do Norte)"),ignore_case=TRUE)) ~ "RN",
      str_detect(uf1, regex(c("(PERNAMBUCO)"),ignore_case=TRUE)) ~ "PE",
      str_detect(uf1, regex(c("(Para(í|i)ba)"),ignore_case=TRUE)) ~ "PB",
      str_detect(uf1, regex(c("(ESP(Í|I)RITO SANTO)"),ignore_case=TRUE)) ~ "ES",
      str_detect(uf1, regex(c("(ACRE)"),ignore_case=TRUE)) ~ "AC",
      str_detect(uf1, regex(c("(Minas Gerais)"),ignore_case=TRUE)) ~ "MG",
      str_detect(uf1, regex(c("(Distrito Federal)"),ignore_case=TRUE)) ~ "DF",
      str_detect(uf1, regex(c("(ALAGOAS)"),ignore_case=TRUE)) ~ "AL",
      TRUE ~ "Erro"
      ),

     atividade = case_when(
      str_detect(unidade_prisional, regex(c("(blocos)|(concreto)|(bloquetes)|(cimento)"),ignore_case=TRUE)) ~ "Artefatos de concreto",
      str_detect(caracteristicas_materiais, regex(c("(blocos)|(concreto)|(bloquetes)|(cimento)"),ignore_case=TRUE)) ~ "Artefatos de concreto",
      TRUE ~ "Outros"
    )
  ) |>
  relocate(
    uf, .before = uf1
  )|>
  select(uf, unidade_prisional, atividade) |>
  filter(
    atividade == "Artefatos de concreto"
  )

write_xlsx(
  tabela_forms,
  path = "../data_raw/trabalho_tabela_forms.xlsx"
)

###########################
# VETOR PARA FILTRAR AS LINHAS QUE NAO POSSUEM UNIDADES PRISIONAIS

vetor_filtro_unidades <-
  c("(TRABALHADORES|75%|90%|TRABALHADOR|CEK|COOPERACAO|ARTESANATO|REMUNERADOS|SEJUS|GRINGS|HORTA|POLICIA MILITAR)")
vetor_filtro_unidades2 <-
  c("GRINGS","PEA","CAPEPI","CRVDG","PEJTAA","LAVA","PEMS","PEMR","PMP","CAPFARI","CRJF","A ","CRJF","MIRIM","CDFGUM",
    "PRAMC")

rel01_artefatos_concreto <-
  bind_rows(
    tabela_alagoas,
    tabela_forms,
    tabela_parana2,
    tabela_santa_catarina
  ) |>
  filter(
    atividade == "Artefatos de concreto"
  ) |>
  mutate_if(
    is.character,
    abjutils::rm_accent
  )|>
  mutate_if(
    is.character,
    str_to_upper
  )

rel01_artefatos_concreto2<-
  rel01_artefatos_concreto |>
  separate_rows(
    unidade_prisional,
    sep = "[;.:-]"
  ) |>
  mutate_if(
    is.character,
    str_trim
  ) |>
 mutate(
   filtro_unidade  = case_when(
     str_detect(unidade_prisional, regex(vetor_filtro_unidades,ignore_case=TRUE)) ~ "NAO",
     TRUE ~ "SIM"
   )
 ) |>
  filter(
    filtro_unidade == "SIM",
    !unidade_prisional %in% vetor_filtro_unidades2
  )
