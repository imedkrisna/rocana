setwd('C:/github/rocana')

library(WDI)
library(tidyverse)
library(scales)
library(readxl)
library(writexl)

read_excel("data/map.xlsx",sheet="pma") |>
  pivot_longer(!sektorbps,values_to="investasi",names_to = "year") |>
  write_xlsx("data/pma.xlsx")

read_excel("data/map.xlsx",sheet="pmdn") |>
  pivot_longer(!sektorbps,values_to="investasi",names_to = "year") |>
  write_xlsx("data/pmdn.xlsx")

read_excel("data/pmafix.xlsx")|>
  pivot_wider(names_from = "year",values_from = "inves") |>
  write_xlsx("data/pmafix2.xlsx")

read_excel("data/pmdnfix.xlsx")|>
  pivot_wider(names_from = "year",values_from = "inves") |>
  write_xlsx("data/pmdnfix2.xlsx")