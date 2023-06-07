library(tidyverse)
library(abjutils)
library(janitor)
library(writexl)
library(readxl)

# ESSA ROTINA TRATA A TABELA DO INFOPEN E A DIVIDE EM SUB-TABELAS
# TODAS AS TABELAS PRINCIPAIS (RELATORIOS) SAO CRIADOS TENDO AS SEGUINTES VARIAVEIS FILTROS
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
### RELATORIO 01 - CAPACIDADE GERAL ----------------
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


## RELATORIO 03 - OCUPACAO - EMPILHAMENTO CAPACIDADE X POPULACAO ---------------
# ESSAS TABELA EMPILHA TODOS OS DADOS SOBRE CAPACIDADE E POPULACAO E CALCULA TAXA DE OCUPACAO

### RELATORIO 03 - OCUPACAO 01 - EMPILHA AS TABELAS

rel03_ocupacao01 <-
  bind_rows(
    rel01_capacidade02,
    rel02_populacao02
  )

### RELATORIO 03 - OCUPACAO 02 - TRATA A CAPACIDADE E POPULACAO EM COLUNAS SEPARADAS

rel03_ocupacao02 <-
  rel03_ocupacao01 |>
  pivot_wider(
    names_from = variavel,
    values_from = qtd
  )

### RELATORIO 03 - OCUPACAO 03 - CALCULA A TAXA DE OCUPACAO PARA UNIDADES FISICAS
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



populacao_sexo <-
  entidade02_populacao01 |>
  group_by(ciclo,ano,semestre,variavel, sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  ) |>
  pivot_wider(
      names_from = sexo,
      values_from = qtd
  ) |>
  mutate(
    total = Feminino + Masculino
  )


## CAPACIDADE E POPULACAO GERAL - SEM FILTRO DE MONITORAMENTO ----------------




populacao_geral <-
  entidade02_populacao01 |>
  mutate(
    regime = if_else(str_detect(regime,regex("Medida de segurança", ignore_case=TRUE)),"Medida de segurança", regime),
  ) |>
  group_by(ciclo,ano,semestre,modalidade,ambito, uf,variavel,regime,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

capacidade_populacao_geral <-
  bind_rows(capacidade_geral,populacao_geral)

write_rds(
  capacidade_populacao_geral,
  file = "../data/data_rds/capacidade_populacao_geral.rds"
)

write_xlsx(
  capacidade_populacao_geral,
  path = "../data/data_xlsx/capacidade_populacao_geral.xlsx"
)

write_rds(
  capacidade_populacao_geral,
  file = "../relatorio_indicadores/data/data_rds/capacidade_populacao_geral.rds"
)

write_xlsx(
  capacidade_populacao_geral,
  path = "../relatorio_indicadores/data/data_xlsx/capacidade_populacao_geral.xlsx"
)

capacidade_populacao_geral2 <-
  capacidade_populacao_geral |>
  group_by(ciclo,ano,semestre,modalidade,variavel,ambito,sexo) |>
  summarise(
    qtd = sum(qtd, na.rm = TRUE)
  )

deficit_vagas <-
  capacidade_populacao_geral2 |>
  pivot_wider(
    names_from = variavel,
    values_from = qtd
  ) |>
  mutate(
    deficit = as.integer(round(Capacidade - População))
  )

## CAPACIDADE E POPULACAO --------------
# SEPARA A CAPACIDADE POR MODALIDADE, UF E REGIME


taxa_ocupacao_capacidade <-
  entidade01_capacidade02 |>
  filter(
    modalidade == "Custódia em unidade prisional"
  ) |>
  mutate(
    regime = str_to_sentence(if_else(regime == "Rdd", "fechado",regime)),
  ) |>
  group_by(ciclo,ano,semestre,ambito,uf,variavel,regime,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )


taxa_ocupacao_populacao <-
  entidade02_populacao01 |>
  mutate(
    regime = if_else(str_detect(regime,regex("Medida de segurança", ignore_case=TRUE)),"Medida de segurança", regime),
  ) |>
  filter(
    modalidade == "Custódia em unidade prisional"
  ) |>
  group_by(ciclo,ano,semestre,ambito, uf,variavel,regime,sexo) |>
  summarise(
    qtd = sum(qtd,na.rm = TRUE)
  )

tabela_capacidade_populacao <-
  bind_rows(taxa_ocupacao_capacidade,taxa_ocupacao_populacao)

write_rds(
  tabela_capacidade_populacao,
  file = "../data/data_rds/tabela_capacidade_populacao.rds"
)

write_xlsx(
  tabela_capacidade_populacao,
  path = "../data/data_xlsx/tabela_capacidade_populacao.xlsx"
)

write_rds(
  tabela_capacidade_populacao,
  file = "../relatorio_indicadores/data/data_rds/tabela_capacidade_populacao.rds"
)

write_xlsx(
  tabela_capacidade_populacao,
  path = "../relatorio_indicadores/data/data_xlsx/tabela_capacidade_populacao.xlsx"
)


## TAXA DE OCUPACAO E DEFICIT DE VAGAS -----

taxa_ocupacao_geral <-
  tabela_capacidade_populacao |>
  pivot_wider(
    names_from = variavel,
    values_from = qtd
  )

taxa_ocupacao_uf_estadual <-
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  group_by(ciclo, ano, semestre, uf) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

taxa_ocupacao_sexo_uf_estadual <-
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  group_by(ciclo, ano, semestre, uf, sexo) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

taxa_ocupacao_regime_uf_estadual <-
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  group_by(ciclo, ano, semestre, uf, regime) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  ) |>
  filter(
    taxa_ocupacao > 0
  )

taxa_ocupacao_brasil_estadual <-
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  group_by(ciclo, ano, semestre) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

taxa_ocupacao_brasil2_estadual <- # SOMENTE REGIMES FECHADO, SEM CONDENACAO E MEDIDA DE SEGURANCAO
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  filter(
    regime %in% c("Fechado","Medida de segurança","Sem condenação")
  ) |>
  group_by(ciclo, ano, semestre, regime) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

taxa_ocupacao_brasil3_estadual <- # SEPARADA POR AMBITO
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  filter(
    regime %in% c("Fechado","Medida de segurança","Sem condenação")
  ) |>
  group_by(ciclo, ano, ambito, semestre, regime) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

taxa_ocupacao_sexo_brasil_estadual <-
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  group_by(ciclo, ano, semestre, sexo) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

taxa_ocupacao_regime_brasil_estadual <-
  taxa_ocupacao_geral |>
  filter(ambito == "Estadual") |>
  group_by(ciclo, ano, semestre, regime) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

taxa_ocupacao_sexo_brasil_federal <-
  taxa_ocupacao_geral |>
  filter(ambito == "Federal") |>
  group_by(ciclo, ano, semestre, sexo) |>
  summarise(
    taxa_ocupacao = round(sum(População,na.rm = TRUE)/sum(Capacidade, na.rm = TRUE)*100,digits = 2),
    deficit_vagas = sum(Capacidade, na.rm = TRUE) - sum(População,na.rm = TRUE)
  )

## APRISIONAMENTO --------------

# TABELA RETIRADA DO SIDRA NO LINK
# https://sidra.ibge.gov.br/tabela/5917
# A METODOLOGIA DO IBGE ARREDONDA A ESTIMATIVA NA ORDEM DE 1000 PESSOAS
# ESSE CALCULO TODOS OS APENADOS - INCLUSIVE EM PRISAO DOMICILIAR MONITORADOS OU NAO
# ESSE CALCULO TODOS OS APENADOS - EM AMBITOS ESTADUAL E FEDERAL

tabela_ibge_trimestre4_2022_ciclo13 <-
  read_xlsx(
    path = "../data_raw/populacao_ibge_pnad_continua_trimestre4_2022_ciclo13.xlsx",
    skip = 4
  ) |>
  filter(!is.na(Homens))
names(tabela_ibge_trimestre4_2022_ciclo13) <- c("uf","Masculino","Feminino")

tabela_ibge_trimestre4_2022_ciclo13<-
  tabela_ibge_trimestre4_2022_ciclo13 |>
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


tabela_ibge_trimestre2_2022_ciclo12 <-
  read_xlsx(
    path = "../data_raw/populacao_ibge_pnad_continua_trimestre2_2022_ciclo12.xlsx",
    skip = 4
  ) |>
  filter(!is.na(Homens))
names(tabela_ibge_trimestre2_2022_ciclo12) <- c("uf","Masculino","Feminino")

tabela_ibge_trimestre2_2022_ciclo12 <-
  tabela_ibge_trimestre2_2022_ciclo12 |>
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


tabela_ibge_trimestre4_2021_ciclo11 <-
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


tabela_ibge_trimestre2_2021_ciclo10 <-
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

populacao_ibge_ciclo_12_13 <-
  bind_rows(
    tabela_ibge_trimestre2_2022_ciclo12,
    tabela_ibge_trimestre4_2022_ciclo13,
  )

soma_populacao_ibge_ciclo_12_13 <-
  populacao_ibge_ciclo_12_13 |>
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

soma_populacao_ibge_ciclo_10_11 <-
  bind_rows(
    tabela_ibge_trimestre2_2021_ciclo10,
    tabela_ibge_trimestre4_2021_ciclo11
  )

soma_populacao_ibge_completa <-
  bind_rows(
    soma_populacao_ibge_ciclo_10_11,
    soma_populacao_ibge_ciclo_12_13
  )


# TABELA COM DADOS POPULACIONAIS

aprisionamento_populacao <-
  entidade02_populacao01 |>
  filter(modalidade == "Custódia em unidade prisional") |>
  group_by(
    uf, ciclo, ano, semestre, sexo
  ) |>
 summarise(
    populacao_prisional = sum(qtd, na.rm = TRUE)
  )

soma_populacao_prisional_completa <-
  aprisionamento_populacao |>
  group_by(
    ciclo,ano,semestre,sexo
  ) |>
  summarise(
    populacao_prisional = sum(populacao_prisional,na.rm = TRUE)
  )

taxa_aprisionamento_geral_brasil <-
  left_join(soma_populacao_ibge_completa,soma_populacao_prisional_completa) |>
  group_by(ciclo, ano, semestre) |>
  summarise(
    populacao_prisional = sum(populacao_prisional, na.rm = TRUE),
    populacao_ibge = sum(populacao_ibge,na.rm = TRUE),
    tx_aprisionamento = (sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000
  )

taxa_aprisionamento_geral_brasil_sexo <-
  left_join(soma_populacao_ibge_completa,soma_populacao_prisional_completa) |>
  group_by(ciclo, ano, semestre, sexo) |>
  summarise(
    populacao_prisional = sum(populacao_prisional, na.rm = TRUE),
    populacao_ibge = sum(populacao_ibge,na.rm = TRUE),
    tx_aprisionamento = (sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000
  )


taxa_aprisionamento_geral_uf_sexo <-
  left_join(
    aprisionamento_populacao,
    populacao_ibge_ciclo_12_13
  ) |>
  filter(!ciclo %in% c(10,11)) |>
  mutate(
    tx_aprisionamento = (sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000
  )


taxa_aprisionamento_geral_uf <-
  taxa_aprisionamento_geral_uf_sexo |>
  group_by(
    uf, ciclo, ano, semestre
  ) |>
  summarise(
    tx_aprisionamento = (sum(populacao_prisional, na.rm = TRUE) / sum(populacao_ibge,na.rm = TRUE))*100000
  )




