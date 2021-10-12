#### Práce v kurzu KSS/ASY2


## Encoding: windows-1250
## Vytvoøil: 2021-10-07 FrK
## Upravil:  2021-10-07 FrK

# Poznámky:
# =========
# 1) Cílem je vybrat z vyèištìných dat promìnné potøebné pro jednotlivé skupiny. DONE!
# 2) Vyøešit pøevedení tabulky na tibble a uložení do Excelu. DONE!
# 3) Pøíprava promìnných pro analýzu ... ještì jsme nezaèali :-)
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

  # Jádro, vytvoøení vlastní tabulky:
  table(data[[var1]], data[[var2]], useNA = "ifany") %>%

    # Pøevedení tabulky na data.frame, protože s tibble to není úplnì jednoduché:
    as.data.frame() %>%

    # Tak a teï víme, že jedna promìnná je "Var1", druhá "Var2", máme i jejich pùvodní jména
    # v objektech 'var1' a 'var2' a koneènì, víme, že hodnoty jsou ve "Freq".
    # Mùžeme to tedy celé v klidu reshapovat pomocí 'pivot_wider()'
    pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq,
                names_prefix = paste0(var2, "::")) %>%  # Zajistí, aby se pøed kategorie v názvu sloupeèku napsal i název pùvodní druhé promìnné.

    # Nakonec první sloupeèek tabulky pøejmenujeme pomocí vlastní funkce 'prejmenuj()' tak,
    # aby nesl název pùvodní první promìnné:
    prejmenuj(1, var1)
}


# Tým Lucie ---------------------------------------------------------------

# Dluhy --> Pracovní morálka



# Zakomentovaný kód nefunguje, protože by k nìmu byla tøeba plná data a ta nedáme z ruky:
# Lucie = select(dfb, Nízkopráh.Kde, Dluhy, Pøehled, starts_with("Pracoval"), starts_with("Vyhaz"),
#                ends_with(".Za80"), ends_with(".Výdrž"), Telefon.Sdìlil)
# write_xlsx(Lucie, "dataC.xlsx")

# Naètení vyselektovaných dat:
dfl = read_xlsx("dataC.xlsx")

# Klasická nehezká tabulka:
table(dfl$`Práce-Stavba.Výdrž`, dfl$Dluhy, useNA = "ifany")

# Pìknìjší tabulka vytvoøená novou funkcí:
tb = table_as_tibble(data = dfl, var1 = "Práce-Stavba.Výdrž", var2 = "Dluhy")
tb

# Uložení tabulky do Excelu
write_xlsx(tb, "tab1.xlsx")



# Tým Vojta ---------------------------------------------------------------

# Sociální kapitál --> Alkohol

# Sociální kapitál = Poèty pøátel na ulici a v bydlení
# Alkohol = Kolik + Jak èasto
# Kontext: Doba, Vìk, Partner



# Zakomentovaný kód nefunguje, protože by k nìmu byla tøeba plná data a ta nedáme z ruky:
# Vojta = select(dfb, Vìk, Pije, Pije.Kolik, Prevalence, Partner, Pøátelé.BezDomova, starts_with("Pøátelé.Byd"),
#                starts_with("Kontakt."), Pohlaví, Doba)
# write_xlsx(Vojta, "dataA.xlsx")

# Naètení vybraných promìnných:
dfv = read_xlsx("dataA.xlsx")

# Klasická nehezká tabulka:
table(dfv$Pøátelé.BezDomova, dfv$Prevalence, useNA = "ifany")

# Pìknìjší tabulka jako 'pajplajna' s pomocí vlastní funkce:
dfv %>% table_as_tibble("Pøátelé.BezDomova", "Prevalence")

# Uložení tabulky do Excelu v jedné 'pajplajnì':
dfv %>% table_as_tibble("Pøátelé.BezDomova", "Prevalence") %>%
write_xlsx("tab2.xlsx")



# Tým Lukáš ---------------------------------------------------------------

# Alkohol + Drogy --> Dùvìra(?) a Kontakt(?)
# Sociální kapitál = Nocování + Kdo jim pomáhá
# Drogy = Co všechno bere



# Zakomentovaný kód nefunguje, protože by k nìmu byla tøeba plná data a ta nedáme z ruky:
# Lukáš = select(dfb, Pije, Pije.Kolik, Prevalence, Sebevnímání.Látky, ends_with(".Užívá"),
#                ends_with(".Nejèastìji"), starts_with("Pomoc."), -ends_with(".Výpis"), Nocování)
# write_xlsx(Lukáš, "dataB.xlsx")

# Naètení vybraných promìnných:
dfl = read_xlsx("dataB.xlsx")

# Klasická tabulka:
table(dfl$Pije, dfl$Sebevnímání.Látky, useNA = "ifany")

# Tibble tabulka pomocí vlastní funkce a 'pajplajny':
dfl %>% table_as_tibble("Sebevnímání.Látky", "Pije")

# Export tabulky do Excelu pomocí pajplajny a vlastní funkce:
dfl %>% table_as_tibble("Sebevnímání.Látky", "Pije") %>%
write_xlsx("tab3.xlsx")



# Tabulky všech kombinací promìnných --------------------------------------

# Na nejmenším tibble 'dfv' si ukážeme, jak by bylo možné si vygenerovat
# do Excelu všechny dvojice promìnných. Každá dvojice bude mít svùj soubor, Bohu žel...
# zatím nedokážu vyøešit, jak uložit všechny tabulky na jednotlivé listy jednoho Excelovského sešitu.
# Resp. tuším jak by to šlo, ale je to buï složité, nebo je na to potøeba další package,
# nebo to bude nesrozumitelné...

# Napíšu si na to funkci. Staèí pak do funkce jen 'vložit' tibble a
# funkce udìlá otrocky tabulku pro každou dvojici promìnných.
# BACHA! Nechte v tibble jen ty promìnné, které má smysl zkoumat tabulkou, napø. ID pøípadu dejte pryè.
# BACHA! Defaultnì bude nastavený export do Excelu, ale lze nastavit jen vypsání na obrazovku,
# když zadáte 'Excel = FALSE', nebo jen dáte druhý argument 'FALSE'.

vsechny_tabulky = function(data, Excel = TRUE) {
  pp = length(data)  # Do 'pp' si uložíme poèet promìnných.
  jména = names(data)  # Do 'jména' si uložíme všechna jména promìnných.
  k = 1000  # 'k' bude obsahovat poøadové èíslo poslední tabulky, zaèneme èíslovat od 1000.

  # Budeme využívat toho, že do funkce 'table_as_tibble()' zadáváme název promìnné jako string!

  # Vše vyøešíme dvìma cykly:
  for (i in 1:(pp - 1)) {  # První projede promìnnými od první po pøedposlední.
    for (j in (i+1):pp) {  # Druhý projede uvnitø od 'í' plus první po poslední.
      tb = table_as_tibble(data, jména[i], jména[j])  # Uložíme si tibble tabulku do objektu 'tb'
      k = k + 1  # Posuneme poøadové èíslo tabulky o 1.
      ifelse(Excel,  # Podle toho, zda je v argumentu funkce uloženo TRUE, nebo FALSE...
             write_xlsx(tb, paste0("tab", k, ".xlsx")),   #... buï uložíme tabulku do Excelu...
             print(tb))  # ...nebo jí jen vytiskneme na obrazovku.
    }
  }
}

# Toto jen vypíše tabulky na obrazovku:
vsechny_tabulky(dfv, FALSE)

# Toto tabulky rovnou uloží do Excelu:
vsechny_tabulky(dfv, TRUE)
# Všech 91 tabulek se uložilo jako jednotlivé soubory.



# Nyní si to mohou vyzkoušet i další skupiny, ale BACHA!
# U týmu Lucie to bude dìlat 14 * 27 = 378 souborù.
# U týmu Lukáš to bude neuvìøitelných 91 * 46 = 4186 souborù.
# Pøipravte si na to hard disky :-D
# A nebo pracujte na menších èástech souborù, kde má smysl vše se vším srovnat...


