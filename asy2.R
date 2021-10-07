#### Práce v kurzu KSS/ASY2


## Encoding: windows-1250
## Vytvoøil: 2021-10-07 FrK
## Upravil:  2021-10-07 FrK

# Poznámky:
# =========
# 1) Cílem je vybrat z vyèištìných dat promìnné potøebné pro jednotlivé skupiny.
# 2) Vyøešit pøevedení tabulky na tibble a uložení do Excelu
# 3) Pøíprava promìnných pro analýzu
#


# ## Hlavièka -------------------------------------------------------------

# Smazání pamìti
rm(list=ls())

# Naètení packagí
library(dplyr)
library(tidyr)
library(tibble)
library(sjmisc)
library(stringr)
library(forcats)
library(ggplot2)
library(stargazer)
library(readxl)
library(writexl)
# source("D:/owncloud2/mujskript.R")  # Funguje pouze mnì, studenty tím nebudeme trápit.

# Naètení dat
# load("bezDomova2021.RData")  # Také funguje pouze mnì, studenty tím nebudu trápit.


# Vlastní funkce ----------------------------------------------------------

# Nejprve potøebujeme vlastní funkci na pøejmenování, která zaèlení 'names()' do 'tidyverse'
prejmenuj = function(data, pos, newNames){
  names(data)[pos] = newNames
  data
}


# Funkce, která ze dvou promìnných udìlá frekvenèní tabulku tøídìní 2. stupnì a
# výsledek vrátí jako tibble
table_as_tibble = function(data, var1, var2) {

  # Aby funkce pracovala, potøebuje naèíst package 'tidyr'
  library(tidyr)

  # Jádro, vytvoøení vlastní tabulky:
  table(data[[var1]], data[[var2]], useNA = "ifany") %>%

    # Pøevedení tabulky na data.frame, protože s tibble to není úplnì jednoduché:
    as.data.frame() %>%

    # Tak a teï víme, že jedna promìnná je "Var1", druhá "Var2", máme i jejich pùvodní jména
    # v objektech 'var1' a 'var2' a koneènì, víme, že hodnoty jsou ve "Freq".
    # Mùžeme to tedy celé v klidu reshapovat pomocí 'pivot_wider()'
    pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq,
                names_prefix = paste0(var2, "::")) %>%  # Zajistí, aby se pøed sloupeèky

    # Nakonec první sloupeèek tabulky pøejmenujeme pomocí vlastní funkce 'prejmenuj()' tak,
    # aby nesl název pùvodní první promìnné:
    prejmenuj(1, var1)
}


# Tým Lucie ---------------------------------------------------------------

# Dluhy --> Pracovní morálka

library(dplyr)
library(readxl)
library(writexl)

Lucie = select(dfb, Nízkopráh.Kde, Dluhy, Pøehled, starts_with("Pracoval"),
               ends_with(".Za80"), ends_with(".Výdrž"), Telefon.Sdìlil)

write_xlsx(Lucie, "dataC.xlsx")
dfl = read_xlsx("dataC.xlsx")

table(dfl$`Práce-Stavba.Výdrž`, dfl$Dluhy, useNA = "ifany")
tb = table_as_tibble(data = dfl, var1 = "Práce-Stavba.Výdrž", var2 = "Dluhy")
write_xlsx(tb, "tab1.xlsx")



# Tým Vojta ---------------------------------------------------------------

# Sociální kapitál --> Alkohol

# Sociální kapitál = Poèty pøátel na ulici a v bydlení
# Alkohol = Kolik + Jak èasto
# Kontext: Doba, Vìk, Partner

library(dplyr)
library(readxl)
library(writexl)


Vojta = select(dfb, Vìk, Pije, Pije.Kolik, Prevalence, Partner, Pøátelé.BezDomova, starts_with("Pøátelé.Byd"),
               starts_with("Kontakt."), Pohlaví, Doba)

write_xlsx(Vojta, "dataA.xlsx")
dfv = read_xlsx("dataA.xlsx")

table(dfv$Pøátelé.BezDomova, dfv$Prevalence, useNA = "ifany")
tb = table_as_tibble(dfv,  "Pøátelé.BezDomova", "Prevalence")
write_xlsx(tb, "tab2.xlsx")



# Tým Lukáš ---------------------------------------------------------------

# Alkohol + Drogy --> Dùvìra(?) a Kontakt(?)
# Sociální kapitál = Nocování + Kdo jim pomáhá
# Drogy = Co všechno bere


library(dplyr)
library(readxl)
library(writexl)


Lukáš = select(dfb, Pije, Pije.Kolik, Prevalence, Sebevnímání.Látky, ends_with(".Užívá"),
               ends_with(".Nejèastìji"), starts_with("Pomoc."), -ends_with(".Výpis"), Nocování)

write_xlsx(Lukáš, "dataB.xlsx")
dfl = read_xlsx("dataB.xlsx")

table(dfl$Pije, dfl$Sebevnímání.Látky, useNA = "ifany")
tb = table_as_tibble(dfl, "Sebevnímání.Látky", "Pije")
write_xlsx(tb, "tab3.xlsx")



