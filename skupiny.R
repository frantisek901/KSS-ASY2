#### Skript na rozd�len� 17 student� do 3 skupin A, B, C

## Encoding: windows-1250
## Vytvo�il: 2021-09-20 FrK
## Upravil:  2021-09-20 FrK

## NOTES:
#  Zat�m nic...


library(readxl)
library(writexl)
library(dplyr)
library(tibble)

## Nejd��v na�tu seznam studuj�c�ch a vezmu jen jm�no a st. ��slo:
studujici = read_xls("seznamStudujicich.xls") %>% select(1:3)

## Pak si vytvo��m marker pro skupiny
skupiny = c(rep("A", 6), rep("B", 6), rep("C", 5)) %>% sample(., length(.))
# skupiny

## Vytvo��m si seznam studuj�c�ch v�etn� studijn� skupiny:
seznam = tibble(studujici, skupina = skupiny) %>% arrange(skupina, prijmeni)

## Ulo�en� seznamu:
write_xlsx(seznam, "seznamStudujicich.xlsx")
