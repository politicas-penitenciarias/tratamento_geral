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
  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  )



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
    )
  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  )

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
    )
  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  )

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
    )
  ) |>
  select(
    ciclo,semestre,ano,situacao_de_preenchimento,nome_do_estabelecimento,
    modalidade, everything()
  )

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

## CARCERAGENS DE DELEGACIAS E BATALHOES ------
### A PRIMEIRA ANALISE CONSIDERA A BASE DE DADOS BRUTA DA CNISP.-----

carceragens_ciclo10_2021 <-
  read_xlsx(
    path = "../data_raw/carceragens_ciclo10.xlsx"
  ) |>
  janitor::clean_names() |>
  select(
    ano = ano_de_referencia,
    instituicao = informacoes_referentes_a,
    tipo_de_estabelecimento_de_custodia,
    uf = unidade_federativa,
    qtd_estabelecimentos = quantidade_de_estabelecimentos_destinados_a_custodia_de_pessoas,
    qtd_presos_masculino = quantidade_de_custodiados_sexo_masculino_em_30_06_2021,
    qtd_presos_feminino = quantidade_de_custodiados_sexo_feminino_em_30_06_2021,
    qtd_vagas_masculino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_masculino,
    qtd_vagas_feminino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_feminino,
    qtd_presos_90dias_masculino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_masculino,
    qtd_presos_90dias_feminino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_feminino,
    observacoes_gerais
  ) |>
  mutate(
    ciclo = 10,
    semestre = 1
  ) |>
  relocate(
    ciclo, .after = ano
  ) |>
  relocate(
    semestre, .after = ano
  )

carceragens_ciclo11_2021 <-
  read_xlsx(
    path = "../data_raw/carceragens_ciclo11.xlsx"
  ) |>
  janitor::clean_names() |>
  select(
    ano = ano_de_referencia,
    instituicao = informacoes_referentes_a,
    tipo_de_estabelecimento_de_custodia,
    uf = unidade_federativa_uf,
    qtd_estabelecimentos = quantidade_de_estabelecimentos_destinados_a_custodia_de_pessoas,
    qtd_presos_masculino = quantidade_de_custodiados_sexo_masculino_em_31_12_2021,
    qtd_presos_feminino = quantidade_de_custodiados_sexo_feminino_em_31_12_2021,
    qtd_vagas_masculino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_masculino,
    qtd_vagas_feminino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_feminino,
    qtd_presos_90dias_masculino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_masculino,
    qtd_presos_90dias_feminino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_feminino,
    observacoes_gerais
  ) |>
  mutate(
    ciclo = 11,
    semestre = 2
  ) |>
  relocate(
    ciclo, .after = ano
  ) |>
  relocate(
    semestre, .after = ano
  )

carceragens_ciclo12_2022 <-
  read_xlsx(
    path = "../data_raw/carceragens_ciclo12.xlsx"
  ) |>
  janitor::clean_names() |>
  select(
    ano = ano_de_referencia,
    instituicao = informacoes_referentes_a,
    tipo_de_estabelecimento_de_custodia,
    uf = unidade_federativa_uf,
    qtd_estabelecimentos = quantidade_de_estabelecimentos_destinados_a_custodia_de_pessoas,
    qtd_presos_masculino = quantidade_de_custodiados_sexo_masculino_em_30_06_2022,
    qtd_presos_feminino = quantidade_de_custodiados_sexo_feminino_em_30_06_2022,
    qtd_vagas_masculino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_masculino,
    qtd_vagas_feminino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_feminino,
    qtd_presos_90dias_masculino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_masculino,
    qtd_presos_90dias_feminino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_feminino,
    observacoes_gerais
  ) |>
  mutate(
    ciclo = 12,
    semestre = 1
  ) |>
  relocate(
    ciclo, .after = ano
  ) |>
  relocate(
    semestre, .after = ano
  )

carceragens_ciclo13_2022 <-
  read_xlsx(
    path = "../data_raw/carceragens_ciclo13.xlsx"
  ) |>
  janitor::clean_names() |>
  select(
    ano = ano_de_referencia,
    instituicao = informacoes_referente_a,
    tipo_de_estabelecimento_de_custodia,
    uf = unidade_federativa_uf,
    qtd_estabelecimentos = quantidade_de_estabelecimentos_destinados_a_custodia_de_pessoas,
    qtd_presos_masculino = quantidade_de_custodiados_sexo_masculino_em_31_12_2022,
    qtd_presos_feminino = quantidade_de_custodiados_sexo_feminino_em_31_12_2022,
    qtd_vagas_masculino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_masculino,
    qtd_vagas_feminino = quantidade_de_vagas_destinadas_as_pessoas_do_sexo_feminino,
    qtd_presos_90dias_masculino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_masculino,
    qtd_presos_90dias_feminino = quantidade_de_custodiados_com_mais_de_90_dias_de_prisao_sexo_feminino,
    observacoes_gerais
  ) |>
  mutate(
    ciclo = 13,
    semestre = 2
  ) |>
  relocate(
    ciclo, .after = ano
  ) |>
  relocate(
    semestre, .after = ano
  )

base_dados_carceragens <-
  bind_rows(
    carceragens_ciclo10_2021,
    carceragens_ciclo11_2021,
    carceragens_ciclo12_2022,
    carceragens_ciclo13_2022
  ) |>
  filter(
    qtd_presos_masculino != "1.1"
  ) |>
  mutate(
    instituicao = case_when(
      instituicao == "Corpo de Bombeiro Militar" ~ "Corpo de Bombeiro Militar (CBM)",
      str_detect(instituicao, "Polícia Federal") ~ "Departamento de Polícia Federal (DPF)",
      str_detect(instituicao, "Polícia Civil") ~ "Polícia Civil (PC)",
      str_detect(instituicao, "Polícia Militar") ~ "Polícia Militar (PM)",
      TRUE ~ instituicao
    ),

    qtd_estabelecimentos = str_to_lower(qtd_estabelecimentos),
    qtd_estabelecimentos = case_when(
      str_detect(qtd_estabelecimentos, "em alguns casos utilizamos os alojamentos das unidades") ~ "0",
      str_detect(qtd_estabelecimentos, "são duas pequenas celas") ~ "0",
      str_detect(qtd_estabelecimentos, "prejudicado") ~ "0",
      str_detect(qtd_estabelecimentos, "são duas pequenas") ~ "0",
      str_detect(qtd_estabelecimentos, "não existe local") ~ "0",
      str_detect(qtd_estabelecimentos, "não há serviço") ~ "0",
      str_detect(qtd_estabelecimentos, "não possuímos") ~ "0",
      str_detect(qtd_estabelecimentos, "não temos") ~ "0",
      str_detect(qtd_estabelecimentos, "não possui") ~ "0",
      str_detect(qtd_estabelecimentos, "não realiza") ~ "0",
      str_detect(qtd_estabelecimentos, "apenas celas de custódia") ~ "0",
      str_detect(qtd_estabelecimentos, "casos utilizamos os") ~ "0",
      str_detect(qtd_estabelecimentos, "inexistente") ~ "0",
      str_detect(qtd_estabelecimentos, "nenhum") ~ "0",
      str_detect(qtd_estabelecimentos, "zero") ~ "0",
      str_detect(qtd_estabelecimentos, "apenas um local") ~ "1",
      str_detect(qtd_estabelecimentos, "apenas o pmrg, ou seja 01") ~ "1",
      str_detect(qtd_estabelecimentos, "apenas 01 (um) unidade") ~ "1",
      str_detect(qtd_estabelecimentos, "1 no comando do corpo de bombeiros") ~ "1",
      str_detect(qtd_estabelecimentos, "1 setor de custódia provisória") ~ "1",
      str_detect(qtd_estabelecimentos, "1 (com um anexo distinto)") ~ "1",
      str_detect(qtd_estabelecimentos, "01 estabelecimento") ~ "1",
      str_detect(qtd_estabelecimentos, "presídio militar") ~ "1",
      str_detect(qtd_estabelecimentos, "Um") ~ "1",
      str_detect(qtd_estabelecimentos, "9 celas") ~ "1",
      str_detect(qtd_estabelecimentos, "hum") ~ "1",
      str_detect(qtd_estabelecimentos, "um") ~ "1",
      str_detect(qtd_estabelecimentos, "uma") ~ "1",
      str_detect(qtd_estabelecimentos, "01 no comando") ~ "2",
      str_detect(qtd_estabelecimentos, "dois") ~ "2",
      str_detect(qtd_estabelecimentos, "quatro") ~ "2",
      str_detect(qtd_estabelecimentos, "14 celas") ~ "1",
      str_detect(qtd_estabelecimentos, "nº 28") ~ "28",
      str_detect(qtd_estabelecimentos, "32 delegacias de polícia metropolitanas") ~ "32",
      str_detect(qtd_estabelecimentos, "34 unidades militares prisionais") ~ "34",
      str_detect(qtd_estabelecimentos, "sede e anexo") ~ "1",
      str_detect(qtd_estabelecimentos, "1 estabelecimento") ~ "1",
      TRUE ~ qtd_estabelecimentos
      ),
   qtd_estabelecimentos = as.integer(qtd_estabelecimentos),

   qtd_presos_masculino = case_when(
      str_detect(qtd_presos_masculino, "INEXISTENTE") ~ "0",
      str_detect(qtd_presos_masculino, "Cento e cinquenta e três") ~ "153",
      str_detect(qtd_presos_masculino, "zero") ~ "0",
      str_detect(qtd_presos_masculino, "28 presos") ~ "28",
      str_detect(qtd_presos_masculino, "nenhum custodiado") ~ "0",
      str_detect(qtd_presos_masculino, "catorze") ~ "14",
      str_detect(qtd_presos_masculino, "não se aplica à PCRO") ~ "0",
      str_detect(qtd_presos_masculino, "Um Militar do CBMRO") ~ "1",
      str_detect(qtd_presos_masculino, "Não há custodiados") ~ "0",
      str_detect(qtd_presos_masculino, "Prejudicado") ~ "0",
      str_detect(qtd_presos_masculino, "Trinta e dois") ~ "0",
      str_detect(qtd_presos_masculino, "1.125") ~ "1125",
      str_detect(qtd_presos_masculino, "dois") ~ "2",
      str_detect(qtd_presos_masculino, "dezoito") ~ "18",
      str_detect(qtd_presos_masculino, "23 custodiados") ~ "23",
      str_detect(qtd_presos_masculino, "36 militares custodiados") ~ "36",
      str_detect(qtd_presos_masculino, "Nenhum") ~ "0",
      str_detect(qtd_presos_masculino, "ZERO") ~ "0",
      str_detect(qtd_presos_masculino, "Zero") ~ "0",
      str_detect(qtd_presos_masculino, "Um") ~ "1",
      str_detect(qtd_presos_masculino, "01 preso") ~ "1",
      str_detect(qtd_presos_masculino, "2.097") ~ "2097",
      TRUE ~ qtd_presos_masculino
    ),
    qtd_presos_masculino = as.integer(qtd_presos_masculino),

   qtd_presos_feminino = case_when(
     str_detect(qtd_presos_feminino, "INEXISTENTE") ~ "0",
     str_detect(qtd_presos_feminino, "Duas") ~ "2",
     str_detect(qtd_presos_feminino, "zero") ~ "0",
     str_detect(qtd_presos_feminino, "7 mulheres") ~ "7",
     str_detect(qtd_presos_feminino, "nenhum custodiado") ~ "0",
     str_detect(qtd_presos_feminino, "Inexistente") ~ "0",
     str_detect(qtd_presos_feminino, "Nada Consta") ~ "0",
     str_detect(qtd_presos_feminino, "nenhuma custodiada") ~ "0",
     str_detect(qtd_presos_feminino, "não se aplica à PCRO") ~ "0",
     str_detect(qtd_presos_feminino, "Prejudicado") ~ "0",
     str_detect(qtd_presos_feminino, "Trinta e dois") ~ "0",
     str_detect(qtd_presos_feminino, "1.125") ~ "1125",
     str_detect(qtd_presos_feminino, "dois") ~ "2",
     str_detect(qtd_presos_feminino, "dezoito") ~ "18",
     str_detect(qtd_presos_feminino, "23 custodiados") ~ "23",
     str_detect(qtd_presos_feminino, "28 presos") ~ "28",
     str_detect(qtd_presos_feminino, "36 militares custodiados") ~ "36",
     str_detect(qtd_presos_feminino, "Nenhum") ~ "0",
     str_detect(qtd_vagas_masculino, "ZERO") ~ "0",
     str_detect(qtd_presos_feminino, "Zero") ~ "0",
     str_detect(qtd_presos_feminino, "Um") ~ "1",
     str_detect(qtd_presos_feminino, "Não há custodiados") ~ "0",
     TRUE ~ qtd_presos_feminino
   ),
   qtd_presos_feminino = as.integer(qtd_presos_feminino),

   qtd_vagas_masculino = case_when(
     str_detect(qtd_vagas_masculino, "INEXISTENTE") ~ "0",
     str_detect(qtd_vagas_masculino, "Duas") ~ "2",
     str_detect(qtd_vagas_masculino, "regime semiaberto") ~ "40",
     str_detect(qtd_vagas_masculino, "Setenta e cinco") ~ "75",
     str_detect(qtd_vagas_masculino, "trinta e seis") ~ "36",
     str_detect(qtd_vagas_masculino, "36 vagas") ~ "36",
     str_detect(qtd_vagas_masculino, "Cinquenta e Oito") ~ "58",
     str_detect(qtd_vagas_masculino, "70 vagas") ~ "70",
     str_detect(qtd_vagas_masculino, "não se aplica à PCRO") ~ "0",
     str_detect(qtd_vagas_masculino, "APROXIMADAMENTE 70") ~ "70",
     str_detect(qtd_vagas_masculino, "zero") ~ "0",
     str_detect(qtd_vagas_masculino, "quatro") ~ "4",
     str_detect(qtd_vagas_masculino, "128 vagas") ~ "128",
     str_detect(qtd_vagas_masculino, "Inexistente") ~ "0",
     str_detect(qtd_vagas_masculino, "Nenhuma") ~ "0",
     str_detect(qtd_vagas_masculino, "nenhuma") ~ "0",
     str_detect(qtd_vagas_masculino, "Prejudicado") ~ "0",
     str_detect(qtd_vagas_masculino, "ZERO") ~ "0",
     str_detect(qtd_vagas_masculino, "A PMSC não possui unidade") ~ "0",
     TRUE ~ qtd_vagas_masculino
   ),
   qtd_vagas_masculino = as.integer(qtd_vagas_masculino),

   qtd_vagas_feminino = case_when(
     str_detect(qtd_vagas_feminino, "INEXISTENTE") ~ "0",
     str_detect(qtd_vagas_feminino, "Quatro") ~ "4",
     str_detect(qtd_vagas_feminino, "zero") ~ "0",
     str_detect(qtd_vagas_feminino, "128 vagas") ~ "128",
     str_detect(qtd_vagas_feminino, "20 vagas") ~ "20",
     str_detect(qtd_vagas_feminino, "Três") ~ "3",
     str_detect(qtd_vagas_feminino, "APROXIMADAMENTE 05") ~ "5",
     str_detect(qtd_vagas_feminino, "Inexistente") ~ "0",
     str_detect(qtd_vagas_feminino, "não se aplica à PCRO") ~ "0",
     str_detect(qtd_vagas_feminino, "Nenhuma") ~ "0",
     str_detect(qtd_vagas_feminino, "nenhuma") ~ "0",
     str_detect(qtd_vagas_feminino, "ZERO") ~ "0",
     str_detect(qtd_vagas_feminino, "Em reforma, no momento 0") ~ "0",
     str_detect(qtd_vagas_feminino, "Prejudicado") ~ "0",
     TRUE ~ qtd_vagas_feminino
   ),
   qtd_vagas_feminino = as.integer(qtd_vagas_feminino),

   qtd_presos_90dias_masculino = case_when(
     str_detect(qtd_presos_90dias_masculino, "INEXISTENTE") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Quatro") ~ "4",
     str_detect(qtd_presos_90dias_masculino, "zero") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "128 vagas") ~ "128",
     str_detect(qtd_presos_90dias_masculino, "20 vagas") ~ "20",
     str_detect(qtd_presos_90dias_masculino, "Três") ~ "3",
     str_detect(qtd_presos_90dias_masculino, "APROXIMADAMENTE 05") ~ "5",
     str_detect(qtd_presos_90dias_masculino, "Inexistente") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "não se aplica à PCRO") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Nenhuma") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "nenhuma") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "ZERO") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Em reforma, no momento 0") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Prejudicado") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Zero") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Unidades de Trânsito") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "dois") ~ "2",
     str_detect(qtd_presos_90dias_masculino, "nove") ~ "9",
     str_detect(qtd_presos_90dias_masculino, "dez") ~ "10",
     str_detect(qtd_presos_90dias_masculino, "22 presos") ~ "22",
     str_detect(qtd_presos_90dias_masculino, "2.097") ~ "2097",
     str_detect(qtd_presos_90dias_masculino, "Trinta") ~ "30",
     str_detect(qtd_presos_90dias_masculino, "61") ~ "61",
     str_detect(qtd_presos_90dias_masculino, "Nenhum") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Não há custodiados") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Não há permanência") ~ "0",
     str_detect(qtd_presos_90dias_masculino, "Um") ~ "1",
     str_detect(qtd_presos_90dias_masculino, "nenhum") ~ "0",
     TRUE ~ qtd_presos_90dias_masculino
   ),
   qtd_presos_90dias_masculino = as.integer(qtd_presos_90dias_masculino),

   qtd_presos_90dias_feminino = case_when(
     str_detect(qtd_presos_90dias_feminino, "INEXISTENTE") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Quatro") ~ "4",
     str_detect(qtd_presos_90dias_feminino, "zero") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "128 vagas") ~ "128",
     str_detect(qtd_presos_90dias_feminino, "20 vagas") ~ "20",
     str_detect(qtd_presos_90dias_feminino, "Três") ~ "3",
     str_detect(qtd_presos_90dias_feminino, "APROXIMADAMENTE 05") ~ "5",
     str_detect(qtd_presos_90dias_feminino, "Inexistente") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "não se aplica à PCRO") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Nenhuma") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "nenhuma") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "ZERO") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Em reforma, no momento 0") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Prejudicado") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Zero") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Unidades de Trânsito") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "dois") ~ "2",
     str_detect(qtd_presos_90dias_feminino, "nove") ~ "9",
     str_detect(qtd_presos_90dias_feminino, "dez") ~ "10",
     str_detect(qtd_presos_90dias_feminino, "22 presos") ~ "22",
     str_detect(qtd_presos_90dias_feminino, "2.097") ~ "2097",
     str_detect(qtd_presos_90dias_feminino, "Trinta") ~ "30",
     str_detect(qtd_presos_90dias_feminino, "61") ~ "61",
     str_detect(qtd_presos_90dias_feminino, "Nenhum") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Não há custodiados") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Não há permanência") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Um") ~ "1",
     str_detect(qtd_presos_90dias_feminino, "nenhum") ~ "0",
     str_detect(qtd_presos_90dias_feminino, "Presídio da Polícia Civil") ~ "1",
     TRUE ~ qtd_presos_90dias_feminino
   ),
   qtd_presos_90dias_feminino = as.integer(qtd_presos_90dias_feminino)
  )

write_rds(
  base_dados_carceragens,
  file = "../data/data_rds/base_dados_carceragens.rds"
)

write_xlsx(
  base_dados_carceragens,
  path = "../data/data_xlsx/base_dados_carceragens.xlsx"
)


### A SEGUNDA ANALISE CONSIDERA A BASE DE DADOS TRATADA DA CNISP.-----
# NESSE CASO, HA MUITA PERDA DE INFORMACAO E OS DADOS INICIAIS NAO BATEM COM A PRIMEIRA ANALISE

base_dados_carceragens2 <-
  read_xlsx(
    path = "../data_raw/carceragens_geral.xlsx",
    sheet = "Planilha2"
  )

write_rds(
  base_dados_carceragens2,
  file = "../data/data_rds/base_dados_carceragens2.rds"
)

write_xlsx(
  base_dados_carceragens2,
  path = "../data/data_xlsx/base_dados_carceragens2.xlsx"
)




