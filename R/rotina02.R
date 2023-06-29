library(tidyverse)
library(abjutils)
library(janitor)
library(writexl)
library(readxl)

# ESSA ROTINA TRATA A TABELA DO INFOPEN E A DIVIDE EM SUB-TABELAS
# EM GERAL, AS TABELAS PRINCIPAIS (RELATORIOS) SAO CRIADAS TENDO AS SEGUINTES VARIAVEIS:
# - CICLO;
# - ANO;
# - SEMESTRE;
# - UF;
# - NOME DO ESTABELECIMENTO;
# - MODALIDADE;
# - AMBITO;
# - VARIAVEL;
# - REGIME;
# - SEXO;
# - QTD;


## LE A TABELA DO INFOPEN ---------------------------

dados_gerais <-
  readRDS(
    file = "../data/data_rds/base_geral_infopen.rds"
  )

## CAPACIDADE DAS UNIDADES  -------------
### RELATORIO 01 - CAPACIDADE 01 - CAPACIDADE GERAL ----------------
# ESSA ROTINA CONTEM OS DADOS DA CAPACIDADE DAS UNIDADES POR REGIME E SEXO
# ELA TRATA OS ITENS X1_3 DA TABELA "dados gerais"

rel01_capacidade01 <-
  dados_gerais |>
  select(
    ciclo,
    ano,
    semestre,
    uf,
    nome_do_estabelecimento,
    modalidade,
    ambito,
    Capacidade_semAcondenação_masculino    = x1_3_capacidade_do_estabelecimento_presos_provisorios_masculino,
    Capacidade_semAcondenação_feminino     = x1_3_capacidade_do_estabelecimento_presos_provisorios_feminino,
    Capacidade_fechado_masculino           = x1_3_capacidade_do_estabelecimento_regime_fechado_masculino,
    Capacidade_fechado_feminino            = x1_3_capacidade_do_estabelecimento_regime_fechado_feminino,
    Capacidade_semiaberto_masculino        = x1_3_capacidade_do_estabelecimento_regime_semiaberto_masculino,
    Capacidade_semiaberto_feminino         = x1_3_capacidade_do_estabelecimento_regime_semiaberto_feminino,
    Capacidade_aberto_masculino            = x1_3_capacidade_do_estabelecimento_regime_aberto_masculino,
    Capacidade_aberto_feminino             = x1_3_capacidade_do_estabelecimento_regime_aberto_feminino,
    Capacidade_medidaAdeAsegurança_masculino = x1_3_capacidade_do_estabelecimento_medidas_de_seguranca_de_internacao_masculino,
    Capacidade_medidaAdeAsegurança_feminino  = x1_3_capacidade_do_estabelecimento_medidas_de_seguranca_de_internacao_feminino,
    Capacidade_rdd_masculino               = x1_3_capacidade_do_estabelecimento_regime_disciplinar_diferenciado_rdd_masculino,
    Capacidade_rdd_feminino                = x1_3_capacidade_do_estabelecimento_regime_disciplinar_diferenciado_rdd_feminino,
    Capacidade_outros_masculino            = x1_3_capacidade_do_estabelecimento_outro_s_qual_is_masculino,
    Capacidade_outros_feminino             = x1_3_capacidade_do_estabelecimento_outro_s_qual_is_feminino
  ) |>
  pivot_longer(
    cols = starts_with("capacidade"),names_to = "variavel",values_to = "qtd"
  ) |>
  separate(col = "variavel", into=c("variavel","regime","sexo"),sep = "_") |>
  mutate(
    qtd = if_else(is.na(qtd),0,qtd),
    regime = str_to_sentence(str_replace_all(regime, "A", " ")),
    sexo = str_to_sentence(sexo)
  )

# GRAVA O RELATORIO 01 - CAPACIDADE

write_rds(
  rel01_capacidade01,
  file = "../data/data_rds/rel01_capacidade01.rds"
)

write_xlsx(
  rel01_capacidade01,
  path = "../data/data_xlsx/rel01_capacidade01.xlsx"
)

write_rds(
  rel01_capacidade01,
  file = "../relatorio_indicadores/data/data_rds/rel01_capacidade01.rds"
)

write_xlsx(
  rel01_capacidade01,
  path = "../relatorio_indicadores/data/data_xlsx/rel01_capacidade01.xlsx"
)

### RELATORIO 01 - CAPACIDADE 02 -----------------
# ESSA TABELA ARRUMA O REGIME "RDD" PARA COMPATIBILIZACAO COM A TABELA DA POPULACAO
rel01_capacidade02 <-
  rel01_capacidade01 |>
  mutate(
    regime = str_to_sentence(if_else(regime == "Rdd", "fechado",regime)),
  ) |>
  group_by(ciclo,ano,semestre,uf,nome_do_estabelecimento, modalidade,ambito,variavel,regime,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

write_rds(
  rel01_capacidade02,
  file = "../data/data_rds/rel01_capacidade02.rds"
)

write_xlsx(
  rel01_capacidade02,
  path = "../data/data_xlsx/rel01_capacidade02.xlsx"
)

write_rds(
  rel01_capacidade02,
  file = "../relatorio_indicadores/data/data_rds/rel01_capacidade02.rds"
)

write_xlsx(
  rel01_capacidade02,
  path = "../relatorio_indicadores/data/data_xlsx/rel01_capacidade02.xlsx"
)

### RELATORIO 01 - CAPACIDADE 03 -----------------
# ESSA TABELA AJUSTA A TABELA COM A CAPACIDADE DAS CARCERAGENS

rel01_capacidade03<-
  readRDS(file = "../data/data_rds/base_dados_carceragens.rds") |>
  select(
    ciclo,
    ano,
    semestre,
    uf,
    qtd_vagas_masculino,
    qtd_vagas_feminino
  ) |>
  pivot_longer(
    cols = qtd_vagas_masculino:qtd_vagas_feminino,
    names_to = "variavel",
    values_to = "qtd"
  ) |>
  mutate(
    regime = "Sem condenação",
    ambito = "Estadual",
    ambito_origem = "Justiça estadual",
    sexo = case_when(
      str_detect(variavel, regex(c("(masculino)"), ignore_case = TRUE)) ~ "Masculino",
      TRUE ~ "Feminino"
    ),
    variavel = "Capacidade",
    modalidade = "Custódia em carceragens"
  ) |>
  group_by(
    ciclo,ano,semestre,
    modalidade,ambito,uf,
    variavel,regime,
    ambito_origem,sexo
  ) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

## POPULACAO DAS UNIDADES -------------

### RELATORIO 02 - POPULACAO 01 - POP. POR MODALIDADE -----------------

rel02_populacao01 <-
  dados_gerais |>
  select(
    ciclo,
    ano,
    semestre,
    modalidade,
    ambito,
    uf,
    nome_do_estabelecimento,
    modalidade,
    população_semAcondenação_justiçaAestadual_masculino = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_estadual_masculino,
    população_semAcondenação_justiçaAestadual_feminino  = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_estadual_feminino,
    população_semAcondenação_justiçaAfederal_masculino = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_federal_masculino,
    população_semAcondenação_justiçaAfederal_feminino  = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_federal_feminino,
    população_semAcondenação_outros_masculino = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_outros_just_trab_civel_masculino,
    população_semAcondenação_outros_feminino  = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_outros_just_trab_civel_feminino,

    população_fechado_justiçaAestadual_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_estadual_masculino,
    população_fechado_justiçaAestadual_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_estadual_feminino,
    população_fechado_justiçaAfederal_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_federal_masculino,
    população_fechado_justiçaAfederal_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_federal_feminino,
    população_fechado_outros_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_outros_just_trab_civel_masculino,
    população_fechado_outros_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_outros_just_trab_civel_feminino,

    população_semiaberto_justiçaAestadual_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_justica_estadual_masculino,
    população_semiaberto_justiçaAestadual_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_justica_estadual_feminino,
    população_semiaberto_justiçaAfederal_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_justica_federal_masculino,
    população_semiaberto_justiçaAfederal_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_justica_federal_feminino,
    população_semiaberto_outros_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_outros_just_trab_civel_masculino,
    população_semiaberto_outros_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_semiaberto_outros_just_trab_civel_feminino,

    população_aberto_justiçaAestadual_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_justica_estadual_masculino,
    população_aberto_justiçaAestadual_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_justica_estadual_feminino,
    população_aberto_justiçaAfederal_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_justica_federal_masculino,
    população_aberto_justiçaAfederal_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_justica_federal_feminino,
    população_aberto_outros_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_outros_just_trab_civel_masculino,
    população_aberto_outros_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_aberto_outros_just_trab_civel_feminino,

    população_medidaAdeAsegurançaAinternação_justiçaAestadual_masculino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_estadual_masculino,
    população_medidaAdeAsegurançaAinternação_justiçaAestadual_feminino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_estadual_feminino,
    população_medidaAdeAsegurançaAinternação_justiçaAfederal_masculino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_federal_masculino,
    população_medidaAdeAsegurançaAinternação_justiçaAfederal_feminino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_federal_feminino,
    população_medidaAdeAsegurançaAinternação_outros_masculino = x4_1_populacao_prisional_medida_de_seguranca_internacao_outros_just_trab_civel_masculino,
    população_medidaAdeAsegurançaAinternação_outros_feminino = x4_1_populacao_prisional_medida_de_seguranca_internacao_outros_just_trab_civel_feminino,

    população_medidaAdeAsegurançaAtratamentoAambulatorial_justiçaAestadual_masculino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_estadual_masculino,
    população_medidaAdeAsegurançaAtratamentoAambulatorial_justiçaAestadual_feminino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_estadual_feminino,
    população_medidaAdeAsegurançaAtratamentoAambulatorial_justiçaAfederal_masculino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_federal_masculino,
    população_medidaAdeAsegurançaAtratamentoAambulatorial_justiçaAfederal_feminino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_federal_feminino,
    população_medidaAdeAsegurançaAtratamentoAambulatorial_outros_masculino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_outros_just_trab_civel_masculino,
    população_medidaAdeAsegurançaAtratamentoAambulatorial_outros_feminino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_outros_just_trab_civel_feminino
  ) |>
  pivot_longer(
    cols = starts_with("população"), names_to = "variavel", values_to =  "qtd"
  ) |>
  separate(col = "variavel", into=c("variavel","regime","ambito_origem","sexo"),sep = "_") |>
  mutate(
    qtd = if_else(is.na(qtd),0,qtd),
    variavel = str_to_sentence(variavel),
    regime = str_to_sentence(str_replace_all(regime, "A", " ")),
    ambito_origem = str_to_sentence(str_replace_all(ambito_origem, "A", " ")),
    sexo = str_to_sentence(sexo)
  )

write_rds(
  rel02_populacao01,
  file = "../data/data_rds/rel02_populacao01.rds"
)

write_xlsx(
  rel02_populacao01,
  path = "../data/data_xlsx/rel02_populacao01.xlsx"
)

write_rds(
  rel02_populacao01,
  file = "../relatorio_indicadores/data/data_rds/rel02_populacao01.rds"
)

write_xlsx(
  rel02_populacao01,
  path = "../relatorio_indicadores/data/data_xlsx/rel02_populacao01.xlsx"
)


### RELATORIO 02 - POPULACAO 02 ---------------
# ESSA TABELA ARRUMA O REGIME "MEDIDA DE SEGURANCA" PARA COMPATIBILIZACAO COM A TABELA DA CAPACIDADE

rel02_populacao02<-
  rel02_populacao01 |>
  mutate(
    regime = if_else(str_detect(regime,regex("Medida de segurança", ignore_case=TRUE)),"Medida de segurança", regime),
  ) |>
  group_by(ciclo,ano,semestre,uf,nome_do_estabelecimento,modalidade,ambito,variavel,regime,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

write_rds(
  rel02_populacao02,
  file = "../data/data_rds/rel02_populacao02.rds"
)

write_xlsx(
  rel02_populacao02,
  path = "../data/data_xlsx/rel02_populacao02.xlsx"
)


write_rds(
  rel02_populacao02,
  file = "../relatorio_indicadores/data/data_rds/rel02_populacao02.rds"
)

write_xlsx(
  rel02_populacao02,
  path = "../relatorio_indicadores/data/data_xlsx/rel02_populacao02.xlsx"
)

### RELATORIO 02 - POPULACAO 03 - CARCERAGENS  -----------------
# ESSE RELATORIO ASSOCIA AS TABELAS COM PESSOAS PRESAS EM CARCERAGENS DE DELEGACIAS,
# BATALHOES DE PM E BOMBEIRO

rel02_populacao03<-
  readRDS(file = "../data/data_rds/base_dados_carceragens.rds") |>
  select(
    ciclo,
    ano,
    semestre,
    uf,
    qtd_presos_masculino,
    qtd_presos_feminino
  ) |>
  pivot_longer(
    cols = qtd_presos_masculino:qtd_presos_feminino,
    names_to = "variavel",
    values_to = "qtd"
  ) |>
  mutate(
    regime = "Sem condenação",
    ambito = "Estadual",
    ambito_origem = "Justiça estadual",
    sexo = case_when(
      str_detect(variavel, regex(c("(masculino)"), ignore_case = TRUE)) ~ "Masculino",
      TRUE ~ "Feminino"
    ),
    variavel = "População",
    modalidade = "Custódia em carceragens ou batalhões"
  ) |>
  group_by(
    ciclo,ano,semestre,
    modalidade,ambito,uf,
    variavel,regime,
    ambito_origem,sexo
  ) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

### RELATORIO 02 - POPULACAO 04 - CARCERAGENS UTILIZANDO A TABELA SIMPLIFICADA DA CNISP -----------------
# ESSE RELATORIO ASSOCIA AS TABELAS COM PESSOAS PRESAS EM CARCERAGENS DE DELEGACIAS,
# BATALHOES DE PM E BOMBEIRO BASEADA NA TABELA SIMPLIFICADA DA CNISP

rel02_populacao04 <-
  readRDS("../data/data_rds/base_dados_carceragens2.rds") |>
  select(-total) |>
  pivot_longer(
    cols = c("Masculino","Feminino"),
    names_to = "sexo",
    values_to = "qtd"
  ) |>
  relocate(uf, .after = "semestre")


### RELATORIO 02 - POPULACAO 05 - ADEQUA A TABELA DE POPULACAO A TABELA DAS CARCERAGENS -----------------

rel02_populacao05 <-
  rel02_populacao02 |>
  group_by(ciclo, ano, semestre, uf, modalidade,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

### RELATORIO 02 - POPULACAO 06 - INTEGRA AS TABELAS DE CARCERAGEM COM O INFOPEN -----------------

rel02_populacao06 <-
  bind_rows(rel02_populacao05,rel02_populacao04) |>
  group_by(ciclo, ano, semestre, uf, modalidade,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

write_rds(
  rel02_populacao06,
  file = "../data/data_rds/rel02_populacao06.rds"
)

write_xlsx(
  rel02_populacao06,
  path = "../data/data_xlsx/rel02_populacao06.xlsx"
)


write_rds(
  rel02_populacao06,
  file = "../relatorio_indicadores/data/data_rds/rel02_populacao06.rds"
)

write_xlsx(
  rel02_populacao06,
  path = "../relatorio_indicadores/data/data_xlsx/rel02_populacao06.xlsx"
)

## OCUPACAO - EMPILHAMENTO CAPACIDADE X POPULACAO ---------------
# ESSAS TABELA EMPILHA TODOS OS DADOS SOBRE CAPACIDADE E POPULACAO E CALCULA TAXA DE OCUPACAO

### RELATORIO 03 - OCUPACAO 01 - EMPILHA AS TABELAS ----

rel03_ocupacao01 <-
  bind_rows(
    rel01_capacidade02,
    rel02_populacao02
  )

### RELATORIO 03 - OCUPACAO 02 - TRATA A CAPACIDADE E POPULACAO EM COLUNAS SEPARADAS -----

rel03_ocupacao02 <-
  rel03_ocupacao01 |>
  pivot_wider(
    names_from = variavel,
    values_from = qtd
  )

### RELATORIO 03 - OCUPACAO 03 - CALCULA A TAXA DE OCUPACAO PARA UNIDADES FISICAS ------
# TRANSFORMA A CAPACIDADE "OUTROS" EM "SEM CONDENACAO"
# EM ANALISE A TABELA BRUTA, FOI VERIFICADO QUE A GRANDE MAIORIA DA CAPACIDADE "OUTROS" EM UNIDADES
# FISICAS SAO ADVINDAS DE VAGAS DE TRIAGEM, OU SEJA, PESSOAS "SEM CONDENACAO" PRINCIPALMENTE NA
# BAHIA

rel03_ocupacao03 <-
  rel03_ocupacao02 |>
  filter(modalidade == "Custódia em unidade prisional") |>
  mutate(
    regime = case_when(
      regime == "Outros" ~ "Sem condenação",
      TRUE ~ regime
    )
  ) |>
  group_by(
    ciclo,ano,semestre,uf,nome_do_estabelecimento, modalidade, ambito, regime, sexo
  ) |>
  summarise(
    Capacidade = sum(Capacidade, na.rm = TRUE),
    População = sum(População, na.rm = TRUE),
    taxa_ocupacao = round((População/Capacidade)*100, digits = 2)
  )

### RELATORIO 03 - OCUPACAO 04 - CALCULA A QUANTIDADE DE EQUIPAMENTO DE MONITORACAO DISPONIVEL -----

rel03_ocupacao04 <-
  rel03_ocupacao02 |>
  filter(modalidade != "Custódia em unidade prisional") |>
  group_by(
    ciclo,ano,semestre,uf,nome_do_estabelecimento, modalidade, ambito, regime, sexo
  ) |>
  summarise(
    Capacidade = sum(Capacidade, na.rm = TRUE),
    População = sum(População, na.rm = TRUE),
    deficit_superavit_equip = as.integer(População-Capacidade)
  )



## IBGE - POPULACAO --------------

# TABELA RETIRADA DO SIDRA NO LINK
# https://sidra.ibge.gov.br/tabela/5917
# A METODOLOGIA DO IBGE ARREDONDA A ESTIMATIVA NA ORDEM DE 1000 PESSOAS
# ESSE CALCULO TODOS OS APENADOS - INCLUSIVE EM PRISAO DOMICILIAR MONITORADOS OU NAO
# ESSE CALCULO TODOS OS APENADOS - EM AMBITOS ESTADUAL E FEDERAL

### RELATORIO 04 - CICLO 13 -----

rel04_ibge_trimestre4_2022_ciclo13 <-
  read_xlsx(
    path = "../data_raw/populacao_ibge_pnad_continua_trimestre4_2022_ciclo13.xlsx",
    skip = 4
  ) |>
  filter(!is.na(Homens))
names(rel04_ibge_trimestre4_2022_ciclo13) <- c("uf","Masculino","Feminino")

rel04_ibge_trimestre4_2022_ciclo13 <-
  rel04_ibge_trimestre4_2022_ciclo13 |>
  mutate(
    uf = case_when(
      uf == "Rondônia" ~ "RO",
      uf == "Acre"     ~ "AC",
      uf == "Amazonas" ~ "AM",
      uf == "Roraima" ~ "RR",
      uf == "Pará"     ~ "PA",
      uf == "Amapá"    ~ "AP",
      uf == "Tocantins"~ "TO",
      uf == "Maranhão" ~ "MA",
      uf == "Piauí"    ~ "PI",
      uf == "Ceará"    ~ "CE",
      uf == "Rio Grande do Norte" ~ "RN",
      uf == "Paraíba"  ~ "PB",
      uf == "Pernambuco" ~ "PE",
      uf == "Alagoas"  ~ "AL",
      uf == "Sergipe"  ~ "SE",
      uf == "Bahia"    ~ "BA",
      uf == "Minas Gerais" ~ "MG",
      uf == "Espírito Santo" ~ "ES",
      uf == "Rio de Janeiro" ~ "RJ",
      uf == "São Paulo" ~ "SP",
      uf == "Paraná" ~ "PR",
      uf == "Santa Catarina" ~ "SC",
      uf == "Rio Grande do Sul" ~ "RS",
      uf == "Mato Grosso" ~ "MT",
      uf == "Mato Grosso do Sul" ~ "MS",
      uf == "Goiás" ~ "GO",
      uf == "Distrito Federal" ~ "DF",
      TRUE ~ "Erro"
    ),

    Masculino = Masculino * 1000,
    Feminino = Feminino * 1000,
    ciclo = 13,
    ano = 2022,
    semestre = 2
  ) |>
  arrange(uf) |>
  pivot_longer(
    cols = c("Feminino","Masculino"),
    names_to = "sexo",
    values_to = "populacao_ibge"
  )

### RELATORIO 04 - CICLO 12 -----

rel04_ibge_trimestre2_2022_ciclo12 <-
  read_xlsx(
    path = "../data_raw/populacao_ibge_pnad_continua_trimestre2_2022_ciclo12.xlsx",
    skip = 4
  ) |>
  filter(!is.na(Homens))
names(rel04_ibge_trimestre2_2022_ciclo12) <- c("uf","Masculino","Feminino")

rel04_ibge_trimestre2_2022_ciclo12 <-
  rel04_ibge_trimestre2_2022_ciclo12 |>
  mutate(
    uf = case_when(
      uf == "Rondônia" ~ "RO",
      uf == "Acre"     ~ "AC",
      uf == "Amazonas" ~ "AM",
      uf == "Roraima" ~ "RR",
      uf == "Pará"     ~ "PA",
      uf == "Amapá"    ~ "AP",
      uf == "Tocantins"~ "TO",
      uf == "Maranhão" ~ "MA",
      uf == "Piauí"    ~ "PI",
      uf == "Ceará"    ~ "CE",
      uf == "Rio Grande do Norte" ~ "RN",
      uf == "Paraíba"  ~ "PB",
      uf == "Pernambuco" ~ "PE",
      uf == "Alagoas"  ~ "AL",
      uf == "Sergipe"  ~ "SE",
      uf == "Bahia"    ~ "BA",
      uf == "Minas Gerais" ~ "MG",
      uf == "Espírito Santo" ~ "ES",
      uf == "Rio de Janeiro" ~ "RJ",
      uf == "São Paulo" ~ "SP",
      uf == "Paraná" ~ "PR",
      uf == "Santa Catarina" ~ "SC",
      uf == "Rio Grande do Sul" ~ "RS",
      uf == "Mato Grosso" ~ "MT",
      uf == "Mato Grosso do Sul" ~ "MS",
      uf == "Goiás" ~ "GO",
      uf == "Distrito Federal" ~ "DF",
      TRUE ~ "Erro"
    ),

    Masculino = Masculino * 1000,
    Feminino = Feminino * 1000,
    ciclo = 12,
    ano = 2022,
    semestre = 1
  ) |>
  arrange(uf) |>
  pivot_longer(
    cols = c("Feminino","Masculino"),
    names_to = "sexo",
    values_to = "populacao_ibge"
  )

### RELATORIO 04 - CICLO 11 -----

rel04_ibge_trimestre4_2021_ciclo11 <-
  read_xlsx(
    path = "../data_raw/populacao_ibge_pnad_continua_trimestre4_2021_ciclo11.xlsx",
    skip = 5
  ) |>
  filter(!is.na(Mulheres)) |>
  mutate(
    uf = "Brasil",
    ciclo = 11,
    ano = 2021,
    semestre = 2,
    Masculino = as.numeric(Homens) * 1000,
    Feminino = as.numeric(Mulheres) * 1000
  ) |>
  select(-Homens, -Mulheres) |>
  pivot_longer(
    cols = c("Feminino","Masculino"),
    names_to = "sexo",
    values_to = "populacao_ibge"
  )

### RELATORIO 04 - CICLO 10 -----

rel04_ibge_trimestre2_2021_ciclo10 <-
  read_xlsx(
    path = "../data_raw/populacao_ibge_pnad_continua_trimestre2_2021_ciclo10.xlsx",
    skip = 5
  ) |>
  filter(!is.na(Mulheres)) |>
  mutate(
    uf = "Brasil",
    ciclo = 10,
    ano = 2021,
    semestre = 1,
    Masculino = as.numeric(Homens) * 1000,
    Feminino = as.numeric(Mulheres) * 1000
  ) |>
  select(-Homens, -Mulheres) |>
  pivot_longer(
    cols = c("Feminino","Masculino"),
    names_to = "sexo",
    values_to = "populacao_ibge"
  )


### RELATORIO 04 - IBGE 01 - EMPILHA AS TABELAS DO IBGE POR ESTADO NOS CICLO 12 E 13 -----

rel04_ibge01_empilhamento_ciclo_12_13 <-
  bind_rows(
    rel04_ibge_trimestre2_2022_ciclo12,
    rel04_ibge_trimestre4_2022_ciclo13
  )

### RELATORIO 04 - IBGE 02 - EMPILHANDO OS CICLO 12 E 13 E SOMANDO A POPULACAO POR PAIS ----

rel04_ibge02_ciclo_12_13 <-
  rel04_ibge01_empilhamento_ciclo_12_13 |>
  group_by(
    ciclo,ano,semestre,sexo
  ) |>
  summarise(
    populacao_ibge = sum(populacao_ibge, na.rm = TRUE)
  ) |>
  mutate(
    uf = "Brasil"
  ) |>
  relocate(uf, .before = ciclo)


### RELATORIO 04 - IBGE 03 - EMPILHANDO AS TABELAS DOS CICLOS 10 E 11 ----

rel04_ibge03_ciclo_10_11 <-
  bind_rows(
    rel04_ibge_trimestre2_2021_ciclo10,
    rel04_ibge_trimestre4_2021_ciclo11
  )

### RELATORIO 04 - IBGE 04 - EMPILHANDO TODAS AS TABEAS DO IBGE ------

rel04_ibge04_completa <-
  bind_rows(
    rel04_ibge03_ciclo_10_11,
    rel04_ibge02_ciclo_12_13
  )

## APRISIONAMENTO - INTEGRA AS TABELAS DO INFOPEN E IBGE COM CALCULO DA TAXA DE APRISIONAMENTO ----
# A TAXA DE APRISIONAMENTO EH CALCULAR A CONSIDERANDO AS PESSOAS QUE CUMPREM PENA NOS ESTABELECIMENTOS
# PRISIONAIS FISICAMENTE.

### RELATORIO 05 - APRIS. 01 - SOMA A POPULACAO POR UF PARA INTEGRACAO COM IBGE ----

rel05_aprisionamento01_soma_populacao <-
  rel02_populacao01 |>
  filter(modalidade == "Custódia em unidade prisional") |>
  group_by(
    uf, ciclo, ano, semestre, sexo
  ) |>
 summarise(
    populacao_prisional = sum(qtd, na.rm = TRUE)
  )


### RELATORIO 05 - APRIS. 02 - INTEGRA TABELAS INFOPEN E IBGE BRASIL PARA OS CICLOS 12 E 13 ----

rel05_aprisionamento02_integracao_filtro <-
  rel05_aprisionamento01_soma_populacao |>
  filter(!ciclo %in% c(10,11))

rel05_aprisionamento02_integracao <-
  full_join(rel05_aprisionamento02_integracao_filtro, rel04_ibge01_empilhamento_ciclo_12_13) |>
  group_by(uf, ciclo, ano, sexo, semestre) |>
  summarise(
    populacao_prisional = as.integer(sum(populacao_prisional, na.rm = TRUE)),
    populacao_ibge = as.integer(sum(populacao_ibge,na.rm = TRUE)),
    tx_aprisionamento = round((sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000, digits = 2)
  )


### RELATORIO 05 - APRIS. 03 - SOMA A POPULACAO PRISIONAL PARA INTEGRACAO COM O IBGE ----

rel05_aprisionamento03_soma_populacao <-
  rel02_populacao01 |>
  filter(modalidade == "Custódia em unidade prisional") |>
  group_by(
    ciclo, ano, semestre, sexo
  ) |>
  summarise(
    populacao_prisional = sum(qtd, na.rm = TRUE)
  )

### RELATORIO 05 - APRIS. 04 - INTEGRA TABELAS INFOPEN E IBGE BRASIL PARA TODOS OS CICLOS ----

rel05_aprisionamento04_integracao <-
  full_join(rel05_aprisionamento03_soma_populacao, rel04_ibge04_completa) |>
  group_by(uf, ciclo, ano, sexo, semestre) |>
  summarise(
    populacao_prisional = as.integer(sum(populacao_prisional, na.rm = TRUE)),
    populacao_ibge = as.integer(sum(populacao_ibge,na.rm = TRUE)),
    tx_aprisionamento = round((sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000, digits = 2)
  )

## PENALIZACAO - INTEGRA AS TABELAS DO INFOPEN E IBGE COM CALCULO DA TAXA DE PENALIZACAO ----
# A TAXA DE PENALIZACAO CONSIDERA TODOS AS PESSOAS CUMPRINDO ALGUMA PENA PUNITIVA, OU SEJA,
# CONSIDERA PESSOAS EM MONITORAMENTO ELETRONICO, PRISAO DOMICILIAR ENTRE OUTRAS MEDIDAS
# EXECUTADAS PELO PODE EXECUTIVO.

### RELATORIO 06 - PENAL. 01 - SOMA A POPULACAO POR UF PARA INTEGRACAO COM IBGE ----

rel06_penalizacao01_soma_populacao <-
  rel02_populacao01 |>
  group_by(
    uf, ciclo, ano, semestre, sexo
  ) |>
  summarise(
    populacao_prisional = sum(qtd, na.rm = TRUE)
  )


### RELATORIO 06 - PENAL. 02 - INTEGRA TABELAS INFOPEN E IBGE BRASIL PARA OS CICLOS 12 E 13 ----

rel06_penalizacao02_integracao_filtro <-
  rel06_penalizacao01_soma_populacao |>
  filter(!ciclo %in% c(10,11))

rel06_penalizacao02_integracao <-
  full_join(rel06_penalizacao02_integracao_filtro, rel04_ibge01_empilhamento_ciclo_12_13) |>
  group_by(uf, ciclo, ano, sexo, semestre) |>
  summarise(
    populacao_prisional = as.integer(sum(populacao_prisional, na.rm = TRUE)),
    populacao_ibge = as.integer(sum(populacao_ibge,na.rm = TRUE)),
    tx_penalizacao = round((sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000, digits = 2)
  )

### RELATORIO 06 - PENAL. 03 - SOMA A POPULACAO PRISIONAL PARA INTEGRACAO COM O IBGE ----

rel06_penalizacao03_soma_populacao <-
  rel02_populacao01 |>
  group_by(
    ciclo, ano, semestre, sexo
  ) |>
  summarise(
    populacao_prisional = sum(qtd, na.rm = TRUE)
  )

### RELATORIO 06 - PENAL. 04 - INTEGRA TABELAS INFOPEN E IBGE BRASIL PARA TODOS OS CICLOS ----

rel06_penalizacao04_integracao <-
  full_join(rel06_penalizacao03_soma_populacao, rel04_ibge04_completa) |>
  group_by(uf, ciclo, ano, sexo, semestre) |>
  summarise(
    populacao_prisional = as.integer(sum(populacao_prisional, na.rm = TRUE)),
    populacao_ibge = as.integer(sum(populacao_ibge,na.rm = TRUE)),
    tx_penalizacao = round((sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000, digits = 2)
  )

## INTEGRACAO DE TAXAS - INTEGRA AS TABELAS COM TAXAS DE APRISIONAMENTO E PENALIZACAO ----
# AGRUPA AS TAXAS CITADAS EM UMA MESMA TABELA, QUANDO POSSIVEL.
# LEMBRANDO:
#   - AS TAXAS DE OCUPACAO E APRISIONAMENTO EXCLUEM AS PESSOAS EM PRISAO DOMICILIAR MONITORADAS OU NAO;
#   - A TAXA DE PENALIZACAO INCLUI TODO OS SISTEMA PRISIONAL ADMINISTRADO PELO PODER EXECUTIVO.
#   - AS VARIAVEIS "populacao_prisinal.x" e "populacao_prisinal.y" SAO REFERENTES AS POPULACOES COM FILTRO DE MONITORAMENTO OU NAO.

### RELATORIO 07 - TAXAS 01 - INTEGRA AS TAXAS EM UMA UNICA TABELA ----
rel07_taxas01 <-
  full_join(
    rel05_aprisionamento02_integracao,
    rel06_penalizacao02_integracao,
    by = c("uf","ciclo","ano","sexo","semestre","populacao_ibge")
  )

## MEDIDA DE SEGURANCA - ANALISA A SITUACAO DOS PRESOS EM TRATAMENTO ----

### RELATORIO 08 - MEDIDA01 -----

rel08_medida01 <-
  dados_gerais |>
  select(
    ciclo,
    x4_1_populacao_prisional_medida_de_seguranca_internacao_total,
    x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_total,
    x1_2_tipo_de_estabelecimento_originalmente_destinado,
    nome_do_estabelecimento
  ) |>
  filter(
    ciclo == 13,
    x4_1_populacao_prisional_medida_de_seguranca_internacao_total > 0,
    x1_2_tipo_de_estabelecimento_originalmente_destinado != "Estabelecimento Destinado Ao Cumprimento De Medida De Segurança De Internação Ou Tratamento Ambulatorial"
  )

write_xlsx(
  rel08_medida01,
  path = "../data/data_xlsx/rel08_medida01.xlsx"
)




