library(tidyverse)
library(abjutils)
library(janitor)
library(writexl)
library(readxl)

# ESSA ROTINA TRATA A TABELA DO INFOPEN E A DIVIDE EM SUB-TABELAS

##  LE A TABELA DO INFOPEN ---------------------------

dados_gerais <-
  readRDS(
    file = "../data/data_rds/base_geral_infopen.rds"
  )

## CAPACIDADE DAS UNIDADES  -------------
### ENTIDADE 01 - CAPACIDADE 01 ----------------
# ESSA ROTINA CONTEM OS DADOS DA CAPACIDADE DAS UNIDADES POR REGIME E SEXO
# ELA TRATA OS ITENS X1_3 DA TABELA "dados gerais"

entidade01_capacidade01 <-
  dados_gerais |>
  select(
    ciclo,
    ano,
    semestre,
    uf,
    nome_do_estabelecimento,
    modalidade,
    capacidade_semAcondenação_masculino    = x1_3_capacidade_do_estabelecimento_presos_provisorios_masculino,
    capacidade_semAcondenação_feminino     = x1_3_capacidade_do_estabelecimento_presos_provisorios_feminino,
    capacidade_fechado_masculino           = x1_3_capacidade_do_estabelecimento_regime_fechado_masculino,
    capacidade_fechado_feminino            = x1_3_capacidade_do_estabelecimento_regime_fechado_feminino,
    capacidade_semiaberto_masculino        = x1_3_capacidade_do_estabelecimento_regime_semiaberto_masculino,
    capacidade_semiaberto_feminino         = x1_3_capacidade_do_estabelecimento_regime_semiaberto_feminino,
    capacidade_aberto_masculino            = x1_3_capacidade_do_estabelecimento_regime_aberto_masculino,
    capacidade_aberto_feminino             = x1_3_capacidade_do_estabelecimento_regime_aberto_feminino,
    capacidade_medidaAdeAsegurança_masculino = x1_3_capacidade_do_estabelecimento_medidas_de_seguranca_de_internacao_masculino,
    capacidade_medidaAdeAsegurança_feminino  = x1_3_capacidade_do_estabelecimento_medidas_de_seguranca_de_internacao_feminino,
    capacidade_rdd_masculino               = x1_3_capacidade_do_estabelecimento_regime_disciplinar_diferenciado_rdd_masculino,
    capacidade_rdd_feminino                = x1_3_capacidade_do_estabelecimento_regime_disciplinar_diferenciado_rdd_feminino,
    capacidade_outros_masculino            = x1_3_capacidade_do_estabelecimento_outro_s_qual_is_masculino,
    capacidade_outros_feminino             = x1_3_capacidade_do_estabelecimento_outro_s_qual_is_feminino
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

### SALVA NA TABELA DE DADOS TRATADOS GERAL ----
write_rds(
  entidade01_capacidade01,
  file = "../data/data_rds/entidade01_capacidade01.rds"
)

write_xlsx(
  entidade01_capacidade01,
  path = "../data/data_xlsx/entidade01_capacidade01.xlsx"
)

### SALVA NA TABELA DE DADOS TRATADOS NO RELATORIO DE INDICADORES
write_rds(
  entidade01_capacidade01,
  file = "../relatorio_indicadores/data/data_rds/entidade01_capacidade01.rds"
)

write_xlsx(
  entidade01_capacidade01,
  path = "../relatorio_indicadores/data/data_xlsx/entidade01_capacidade01.xlsx"
)

### ENTIDADE 01 - CAPACIDADE 02 - CAPACIDADE POR CICLO, REGIME E SEXO -------------

entidade01_capacidade02 <-
  entidade01_capacidade01 |>
  group_by(ciclo, ano, semestre,uf, modalidade, variavel, regime, sexo) |>
  summarise(
    qtd = sum(qtd, na.rm = TRUE)
  )

write_rds(
  entidade01_capacidade02,
  file = "../data/data_rds/entidade01_capacidade02.rds"
)

write_xlsx(
  entidade01_capacidade02,
  path = "../data/data_xlsx/entidade01_capacidade02.xlsx"
)

write_rds(
  entidade01_capacidade02,
  file = "../relatorio_indicadores/data/data_rds/entidade01_capacidade02.rds"
)

write_xlsx(
  entidade01_capacidade02,
  path = "../relatorio_indicadores/data/data_xlsx/entidade01_capacidade02.xlsx"
)

## POPULACAO DAS UNIDADES -------------

### ENTIDADE 02 - POP. POR MODALIDADE

entidade02_populacao01 <-
  dados_gerais |>
  select(
    ciclo,
    ano,
    semestre,
    modalidade,
    uf,
    nome_do_estabelecimento,
    modalidade,
    população_semAcondenação_justiçaAestadual_masculino = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_estadual_masculino,
    população_semAcondenação_justiçaAestadual_feminino  = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_estadual_feminino,
    população_semAcondenação_justicaAfederal_masculino = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_federal_masculino,
    população_semAcondenação_justicaAfederal_feminino  = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_justica_federal_feminino,
    população_semAcondenação_outros_masculino = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_outros_just_trab_civel_masculino,
    população_semAcondenação_outros_feminino  = x4_1_populacao_prisional_presos_provisorios_sem_condenacao_outros_just_trab_civel_feminino,

    população_fechado_justiçaAestadual_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_estadual_masculino,
    população_fechado_justiçaAestadual_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_estadual_feminino,
    população_fechado_justicaAfederal_masculino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_federal_masculino,
    população_fechado_justicaAfederal_feminino = x4_1_populacao_prisional_presos_sentenciados_regime_fechado_justica_federal_feminino,
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

    população_medidaAdeAsegurancaAinternacao_justiçaAestadual_masculino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_estadual_masculino,
    população_medidaAdeAsegurancaAinternacao_justiçaAestadual_feminino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_estadual_feminino,
    população_medidaAdeAsegurancaAinternacao_justiçaAfederal_masculino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_federal_masculino,
    população_medidaAdeAsegurancaAinternacao_justiçaAfederal_feminino = x4_1_populacao_prisional_medida_de_seguranca_internacao_justica_federal_feminino,
    população_medidaAdeAsegurancaAinternacao_outros_masculino = x4_1_populacao_prisional_medida_de_seguranca_internacao_outros_just_trab_civel_masculino,
    população_medidaAdeAsegurancaAinternacao_outros_feminino = x4_1_populacao_prisional_medida_de_seguranca_internacao_outros_just_trab_civel_feminino,

    população_medidaAdeAsegurancaAtratamentoAambulatorial_justiçaAestadual_masculino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_estadual_masculino,
    população_medidaAdeAsegurancaAtratamentoAambulatorial_justiçaAestadual_feminino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_estadual_feminino,
    população_medidaAdeAsegurancaAtratamentoAambulatorial_justiçaAfederal_masculino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_federal_masculino,
    população_medidaAdeAsegurancaAtratamentoAambulatorial_justiçaAfederal_feminino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_justica_federal_feminino,
    população_medidaAdeAsegurancaAtratamentoAambulatorial_outros_masculino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_outros_just_trab_civel_masculino,
    população_medidaAdeAsegurancaAtratamentoAambulatorial_outros_feminino = x4_1_populacao_prisional_medida_de_seguranca_tratamento_ambulatorial_outros_just_trab_civel_feminino
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


## TAXA DE OCUPACAO --------------
# SEPARA A CAPACIDADE POR MODALIDADE, UF E REGIME

taxa_ocupacao_capacidade <-
  entidade01_capacidade02 |>
  filter(modalidade == "Custódia em unidade prisional") |>
  group_by(ciclo,ano,semestre,uf,variavel,regime,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  ) |>
  mutate(
    regime = str_to_sentence(if_else(regime == "Rdd", "fechado",regime)),
  )

taxa_ocupacao_populacao <-
  entidade02_populacao01 |>
  filter(modalidade == "Custódia em unidade prisional") |>
  group_by(ciclo,ano,semestre,uf,variavel,regime,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  ) |>
  mutate(
    regime = str_to_sentence(if_else(str_detect(regex("Medida de segurança",ignore_case=TRUE)), "Medida de segurança",regime)),
  )





