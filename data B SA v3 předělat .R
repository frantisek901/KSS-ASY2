library(tidyverse)
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
library(rlang)
library(rstatix)

# Rozd�len�: (1.) M�n� a (2.) V�ce rizikov�ch drog; a 3 kategorie: (1.) Rekrea�n� u�ivatel, (2.) Neu�ivatel, (3.) Z�visl�

dataB = readRDS("dataB.rds")

# Stratifikace u�ivatel�
data <-
  dataB %>%
  mutate(Uzivatel_sebevnimani = case_when(Neu��v�.��dnou.l�tku.U��v� == "Ano" ~ "Neu�ivatel",
                                          Sebevn�m�n�.L�tky == "Nem�m pot�e" |
                                          Sebevn�m�n�.L�tky  == "Bez l�tky je to hor��" ~ "Rekrea�n� u�ivatel",
                                          Sebevn�m�n�.L�tky == "M�m pot�e" |
                                          Sebevn�m�n�.L�tky  == "Z�vislost" ~ "Z�visl�")) %>%
  mutate(Uzivatel_droga = case_when(
                                    Opi�ty.injek�n�.Nej�ast�ji == "Ano" |
                                    Opi�ty.jinak.Nej�ast�ji  == "Ano" |
                                    Pervitin.injek�n�.Nej�ast�ji =="Ano" |
                                    Pervitin.jinak.Nej�ast�ji == "Ano" |
                                    �edidla.Nej�ast�ji == "Ano" ~ "U�ivatel v�ce rizikov�ch drog",
                                    THC.Nej�ast�ji == "Ano" |
                                    Halucinogeny.Nej�ast�ji  == "Ano" ~ "U�ivatel m�n� rizikov�ch drog",
                                    Neu��v�.��dnou.l�tku.U��v� == "Ano" ~ "Neu�ivatel")
         )

  # Zde se sna��me kv�li probl�mu p�ebyte�n�ch hodnot sebevn�m�n� u u�ivatel� alkoholu (a z�rove� neu�ivatel� drog) p�idat hodnotu neu�ivatel. Syntax u casewhen je takto ale �patn�.

# Varianta B, kter� je tak� nefunk�n�
# data <-
#   dataB %>%
#   if (`Neu��v� ��dnou l�tku.U��v�` == "Ano") {
#     mutate(Uzivatel_sebevnimani = "Neu�ivatel") %>%
#     mutate(Uzivatel_droga = "Neu�ivatel")
#   } else {
#     mutate(Uzivatel_sebevnimani = case_when(Sebevn�m�n�.L�tky == "Nem�m pot�e" |
#                                             Sebevn�m�n�.L�tky  == "Bez l�tky je to hor��" ~ "Rekrea�n� u�ivatel",
#                                             Sebevn�m�n�.L�tky == "M�m pot�e" |
#                                             Sebevn�m�n�.L�tky  == "Z�vislost" ~ "Z�visl�")) %>%
#     mutate(Uzivatel_droga = case_when(THC.Nej�ast�ji == "Ano" |
#                                       Halucinogeny.Nej�ast�ji  == "Ano" ~ "U�ivatel m�n� rizikov�ch drog",
#                                       `Opi�ty injek�n�.Nej�ast�ji` == "Ano" |
#                                       `Opi�ty jinak.Nej�ast�ji`  == "Ano" |
#                                       `Pervitin injek�n�.Nej�ast�ji` =="Ano" |
#                                       `Pervitin jinak.Nej�ast�ji` == "Ano" |
#                                       �edidla.Nej�ast�ji == "Ano" ~ "U�ivatel v�ce rizikov�ch drog",
#                                       `Neu��v� ��dnou l�tku.U��v�` == "Ano" ~ "Neu�ivatel"))
#   }

# Se�azen� prom�nn�ch
data <-
  data %>%
  mutate(ID = row_number()) %>%
  select(ID, Uzivatel_sebevnimani, Uzivatel_droga, everything())

# Sk�re soc. kapit�lu
# Rekodov�n� Ano a Ne na numerick� hodnoty

data <-
  data %>%
  mutate(across(.cols = ends_with("Rodina"),
                .fns = ~ifelse(test = .x=="Ano", yes =5, no = 0))) %>%
  mutate(across(.cols = ends_with("Ulice"),
                .fns = ~ifelse(test = .x=="Ano", yes =1, no = 0))) %>%
  mutate(across(.cols = ends_with("Jin�"),
                .fns = ~ifelse(test = .x=="Ano", yes =1, no = 0))) %>%
  mutate(across(.cols = ends_with("Nikdo"),
                .fns = ~ifelse(test = .x=="Ano", yes =0, no = 0))) %>%
  mutate(Skore_nocovani = case_when(Nocov�n� == "S�m" ~ 0,
                                    Nocov�n� == "Dvojice" ~ 1,
                                    Nocov�n� == "3-5 lid�" ~ 2,
                                    Nocov�n� == "6-10 lid�" ~ 3,
                                    Nocov�n� == "11 a v�ce" ~ 4))

# rowwise (s��t�n� ��dk�/skore)
data <-
  data %>%
  select(-contains("Organizace")) %>%
  rowwise() %>%
  mutate(Soc_skore1 = sum(across(contains("Pomoc")))) %>%
  mutate(Soc_skore = Soc_skore1 + Skore_nocovani) %>%
  select(-Soc_skore1) %>%
  ungroup()






# Popisn� statistika
data %>%
  group_by(Uzivatel_sebevnimani) %>%
  get_summary_stats(Soc_skore, type = "mean_sd")

data %>%
  group_by(Uzivatel_droga) %>%
  drop_na() %>%
  get_summary_stats(Soc_skore, type = "mean_sd")

# Soc. skore X Uzivatel podle sebevn�m�n�
data %>%
  ggplot(aes(x = Soc_skore,y = Uzivatel_sebevnimani)) +
  geom_boxplot()

# Soc. skore X Uzivatel podle drogy
data %>%
  drop_na() %>%
  ggplot(aes(x = Soc_skore, y = Uzivatel_droga)) +
  geom_boxplot()

# Pije X Soc. skore
data %>%
  drop_na() %>%
  ggplot(aes(x = Soc_skore, fill = Pije)) +
  geom_bar()

data %>%
  drop_na() %>%
  ggplot(aes(x = Soc_skore, fill = Pije)) +
  geom_bar(position = position_fill())

# U�ivatel podle sebevn�m�n� X Nocov�n�
data %>%
  drop_na() %>%
  ggplot(aes(x = Nocov�n�, fill = Uzivatel_sebevnimani)) +
  geom_bar()

data %>%
  drop_na() %>%
  ggplot(aes(x = Nocov�n�, fill = Uzivatel_sebevnimani)) +
  geom_bar(position = position_fill())

# U�ivatel podle drog X Nocov�n�
data %>%
  drop_na() %>%
  ggplot(aes(x = Nocov�n�, fill = Uzivatel_droga)) +
  geom_bar()

data %>%
  drop_na() %>%
  ggplot(aes(x = Nocov�n�, fill = Uzivatel_droga)) +
  geom_bar(position = position_fill())

# Pije X Nocov�n�
data %>%
  drop_na() %>%
  ggplot(aes(x = Nocov�n�, fill = Pije)) +
  geom_bar()

data %>%
  drop_na() %>%
  ggplot(aes(x = Nocov�n�, fill = Pije)) +
  geom_bar(position = position_fill())



