#### Skript na rozdìlení 17 studentù do 3 skupin A, B, C

## Encoding: windows-1250
## Vytvoøil: 2021-09-20 FrK
## Upravil:  2021-09-20 FrK

## NOTES:
#  Zatím nic...


library(readxl)
library(writexl)
library(dplyr)
library(tibble)

## Nejdøív naètu seznam studujících a vezmu jen jméno a st. èíslo:
studujici = read_xls("seznamStudujicich.xls") %>% select(1:3)

## Pak si vytvoøím marker pro skupiny
skupiny = c(rep("A", 6), rep("B", 6), rep("C", 5)) %>% sample(., length(.))
# skupiny

## Vytvoøím si seznam studujících vèetnì studijní skupiny:
seznam = tibble(studujici, skupina = skupiny) %>% arrange(skupina, prijmeni)

## Uložení seznamu:
write_xlsx(seznam, "seznamStudujicich.xlsx")
