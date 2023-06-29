library(tidyverse)
library(abjutils)
library(janitor)
library(writexl)
library(readxl)


tab2017 <- read_xlsx(
  path = "../data_raw/prestacao_contas_faf.xlsx",
  sheet = 7,
  skip = 9
  ) |>
  janitor::clean_names() |>
  select(uf:observacoes) |>
  filter(
    data_das_retiradas_da_conta != "TOTAL"
  ) |>
  mutate(
    data_das_retiradas_da_conta = case_when(
      nchar(data_das_retiradas_da_conta) == 5 ~  openxlsx::convertToDate(data_das_retiradas_da_conta),
      nchar(data_das_retiradas_da_conta) == 8 ~ readr::parse_date(data_das_retiradas_da_conta, format =  c("%d/%m/%y")),
      nchar(data_das_retiradas_da_conta) == 10 ~ readr::parse_date(data_das_retiradas_da_conta, format =  c("%d/%m/%y"))
    )
  ) |>
  filter(
    is.na(data_das_retiradas_da_conta)
  )

