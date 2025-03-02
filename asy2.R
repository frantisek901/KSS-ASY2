#### Pr�ce v kurzu KSS/ASY2


## Encoding: windows-1250
## Vytvo�il: 2021-10-07 FrK
## Upravil:  2021-10-07 FrK

# Pozn�mky:
# =========
# 1) C�lem je vybrat z vy�i�t�n�ch dat prom�nn� pot�ebn� pro jednotliv� skupiny. DONE!
# 2) Vy�e�it p�eveden� tabulky na tibble a ulo�en� do Excelu. DONE!
# 3) P��prava prom�nn�ch pro anal�zu ... je�t� jsme neza�ali :-)
#


# ## Hlavi�ka -------------------------------------------------------------

# Smaz�n� pam�ti
rm(list=ls())

# Na�ten� packag�
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
# source("D:/owncloud2/mujskript.R")  # Funguje pouze mn�, studenty t�m nebudeme tr�pit.

# Na�ten� dat
# load("bezDomova2021.RData")  # Tak� funguje pouze mn�, studenty t�m nebudu tr�pit.


# Vlastn� funkce ----------------------------------------------------------

# Nejprve pot�ebujeme vlastn� funkci na p�ejmenov�n�, kter� za�len� 'names()' do 'tidyverse'
prejmenuj = function(data, pos, newNames){
  names(data)[pos] = newNames
  data
}


# Funkce, kter� ze dvou prom�nn�ch ud�l� frekven�n� tabulku t��d�n� 2. stupn� a
# v�sledek vr�t� jako tibble
table_as_tibble = function(data, var1, var2) {

  # J�dro, vytvo�en� vlastn� tabulky:
  table(data[[var1]], data[[var2]], useNA = "ifany") %>%

    # P�eveden� tabulky na data.frame, proto�e s tibble to nen� �pln� jednoduch�:
    as.data.frame() %>%

    # Tak a te� v�me, �e jedna prom�nn� je "Var1", druh� "Var2", m�me i jejich p�vodn� jm�na
    # v objektech 'var1' a 'var2' a kone�n�, v�me, �e hodnoty jsou ve "Freq".
    # M��eme to tedy cel� v klidu reshapovat pomoc� 'pivot_wider()'
    pivot_wider(id_cols = Var1, names_from = Var2, values_from = Freq,
                names_prefix = paste0(var2, "::")) %>%  # Zajist�, aby se p�ed kategorie v n�zvu sloupe�ku napsal i n�zev p�vodn� druh� prom�nn�.

    # Nakonec prvn� sloupe�ek tabulky p�ejmenujeme pomoc� vlastn� funkce 'prejmenuj()' tak,
    # aby nesl n�zev p�vodn� prvn� prom�nn�:
    prejmenuj(1, var1)
}


# T�m Lucie ---------------------------------------------------------------

# Dluhy --> Pracovn� mor�lka



# Zakomentovan� k�d nefunguje, proto�e by k n�mu byla t�eba pln� data a ta ned�me z ruky:
# Lucie = select(dfb, N�zkopr�h.Kde, Dluhy, P�ehled, starts_with("Pracoval"), starts_with("Vyhaz"),
#                ends_with(".Za80"), ends_with(".V�dr�"), Telefon.Sd�lil)
# write_xlsx(Lucie, "dataC.xlsx")

# Na�ten� vyselektovan�ch dat:
dfl = read_xlsx("dataC.xlsx")

# Klasick� nehezk� tabulka:
table(dfl$`Pr�ce-Stavba.V�dr�`, dfl$Dluhy, useNA = "ifany")

# P�kn�j�� tabulka vytvo�en� novou funkc�:
tb = table_as_tibble(data = dfl, var1 = "Pr�ce-Stavba.V�dr�", var2 = "Dluhy")
tb

# Ulo�en� tabulky do Excelu
write_xlsx(tb, "tab1.xlsx")



# T�m Vojta ---------------------------------------------------------------

# Soci�ln� kapit�l --> Alkohol

# Soci�ln� kapit�l = Po�ty p��tel na ulici a v bydlen�
# Alkohol = Kolik + Jak �asto
# Kontext: Doba, V�k, Partner



# Zakomentovan� k�d nefunguje, proto�e by k n�mu byla t�eba pln� data a ta ned�me z ruky:
# Vojta = select(dfb, V�k, Pije, Pije.Kolik, Prevalence, Partner, P��tel�.BezDomova, starts_with("P��tel�.Byd"),
#                starts_with("Kontakt."), Pohlav�, Doba)
# write_xlsx(Vojta, "dataA.xlsx")

# Na�ten� vybran�ch prom�nn�ch:
dfv = read_xlsx("dataA.xlsx")

# Klasick� nehezk� tabulka:
table(dfv$P��tel�.BezDomova, dfv$Prevalence, useNA = "ifany")

# P�kn�j�� tabulka jako 'pajplajna' s pomoc� vlastn� funkce:
dfv %>% table_as_tibble("P��tel�.BezDomova", "Prevalence")

# Ulo�en� tabulky do Excelu v jedn� 'pajplajn�':
dfv %>% table_as_tibble("P��tel�.BezDomova", "Prevalence") %>%
write_xlsx("tab2.xlsx")


dfx = select(dfb, Kontakt.Nikdo, P��tel�.BezDomova, P��tel�.Bydl�, Partner) %>%
  mutate(
    Samot�� = if_else(
      # Kontakt.Nikdo == "Ano" &
      (P��tel�.BezDomova == "0" | P��tel�.BezDomova == "1-2") &
      (P��tel�.Bydl� == "0" | P��tel�.Bydl� == "1-2"), "Ano", "Ne"),
    Bez = if_else(P��tel�.BezDomova == "0" | P��tel�.BezDomova == "1-2", "Ano", "Ne"),
    Byd = if_else(P��tel�.Bydl� == "0" | P��tel�.Bydl� == "1-2", "Ano", "Ne")
  )

frq(dfx$Samot��)
frq(dfx$P��tel�.BezDomova)
frq(dfx$P��tel�.Bydl�)
frq(dfx$Kontakt.Nikdo)
table_as_tibble(dfx, "Kontakt.Nikdo", "Partner")

# T�m Luk� ---------------------------------------------------------------

# Alkohol + Drogy --> D�v�ra(?) a Kontakt(?)
# Soci�ln� kapit�l = Nocov�n� + Kdo jim pom�h�
# Drogy = Co v�echno bere



# Zakomentovan� k�d nefunguje, proto�e by k n�mu byla t�eba pln� data a ta ned�me z ruky:
# Luk� = select(dfb, Pije, Pije.Kolik, Prevalence, Sebevn�m�n�.L�tky, ends_with(".U��v�"),
#                ends_with(".Nej�ast�ji"), starts_with("Pomoc."), -ends_with(".V�pis"), Nocov�n�)
# write_xlsx(Luk�, "dataB.xlsx")

# Na�ten� vybran�ch prom�nn�ch:
dfl = read_xlsx("dataB.xlsx")

# Klasick� tabulka:
table(dfl$Pije, dfl$Sebevn�m�n�.L�tky, useNA = "ifany")

# Tibble tabulka pomoc� vlastn� funkce a 'pajplajny':
dfl %>% table_as_tibble("Sebevn�m�n�.L�tky", "Pije")

# Export tabulky do Excelu pomoc� pajplajny a vlastn� funkce:
dfl %>% table_as_tibble("Sebevn�m�n�.L�tky", "Pije") %>%
write_xlsx("tab3.xlsx")



# Tabulky v�ech kombinac� prom�nn�ch --------------------------------------

# Na nejmen��m tibble 'dfv' si uk�eme, jak by bylo mo�n� si vygenerovat
# do Excelu v�echny dvojice prom�nn�ch. Ka�d� dvojice bude m�t sv�j soubor, Bohu �el...
# zat�m nedok�u vy�e�it, jak ulo�it v�echny tabulky na jednotliv� listy jednoho Excelovsk�ho se�itu.
# Resp. tu��m jak by to �lo, ale je to bu� slo�it�, nebo je na to pot�eba dal�� package,
# nebo to bude nesrozumiteln�...

# Nap�u si na to funkci. Sta�� pak do funkce jen 'vlo�it' tibble a
# funkce ud�l� otrocky tabulku pro ka�dou dvojici prom�nn�ch.
# BACHA! Nechte v tibble jen ty prom�nn�, kter� m� smysl zkoumat tabulkou, nap�. ID p��padu dejte pry�.
# BACHA! Defaultn� bude nastaven� export do Excelu, ale lze nastavit jen vyps�n� na obrazovku,
# kdy� zad�te 'Excel = FALSE', nebo jen d�te druh� argument 'FALSE'.

vsechny_tabulky = function(data, Excel = TRUE) {
  pp = length(data)  # Do 'pp' si ulo��me po�et prom�nn�ch.
  jm�na = names(data)  # Do 'jm�na' si ulo��me v�echna jm�na prom�nn�ch.
  k = 1000  # 'k' bude obsahovat po�adov� ��slo posledn� tabulky, za�neme ��slovat od 1000.

  # Budeme vyu��vat toho, �e do funkce 'table_as_tibble()' zad�v�me n�zev prom�nn� jako string!

  # V�e vy�e��me dv�ma cykly:
  for (i in 1:(pp - 1)) {  # Prvn� projede prom�nn�mi od prvn� po p�edposledn�.
    for (j in (i+1):pp) {  # Druh� projede uvnit� od '�' plus prvn� po posledn�.
      tb = table_as_tibble(data, jm�na[i], jm�na[j])  # Ulo��me si tibble tabulku do objektu 'tb'
      k = k + 1  # Posuneme po�adov� ��slo tabulky o 1.
      ifelse(Excel,  # Podle toho, zda je v argumentu funkce ulo�eno TRUE, nebo FALSE...
             write_xlsx(tb, paste0("tab", k, ".xlsx")),   #... bu� ulo��me tabulku do Excelu...
             print(tb))  # ...nebo j� jen vytiskneme na obrazovku.
    }
  }
}

# Toto jen vyp�e tabulky na obrazovku:
vsechny_tabulky(dfv, FALSE)

# Toto tabulky rovnou ulo�� do Excelu:
vsechny_tabulky(dfv, TRUE)
# V�ech 91 tabulek se ulo�ilo jako jednotliv� soubory.



# Nyn� si to mohou vyzkou�et i dal�� skupiny, ale BACHA!
# U t�mu Lucie to bude d�lat 14 * 27 = 378 soubor�.
# U t�mu Luk� to bude neuv��iteln�ch 91 * 46 = 4186 soubor�.
# P�ipravte si na to hard disky :-D
# A nebo pracujte na men��ch ��stech soubor�, kde m� smysl v�e se v��m srovnat...


