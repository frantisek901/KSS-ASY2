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

# Rozdìlení: (1.) Ménì a (2.) Více rizikových drog; a 3 kategorie: (1.) Rekreaèní uživatel, (2.) Neuživatel, (3.) Závislý

# Stratifikace uživatelù
data <-
  dataB %>%
  mutate(Uzivatel_sebevnimani = case_when(Sebevnímání.Látky == "Nemám potíže" |
                                          Sebevnímání.Látky  == "Bez látky je to horší" ~ "Rekreaèní uživatel",
                                          Sebevnímání.Látky == "Mám potíže" |
                                          Sebevnímání.Látky  == "Závislost" ~ "Závislý")) %>%
  mutate(Uzivatel_droga = case_when(THC.Nejèastìji == "Ano" |
                                    Halucinogeny.Nejèastìji  == "Ano" ~ "Uživatel ménì rizikových drog",
                                    Opiáty.injekènì.Nejèastìji == "Ano" |
                                    Opiáty.jinak.Nejèastìji  == "Ano" |
                                    Pervitin.injekènì.Nejèastìji =="Ano" |
                                    Pervitin.jinak.Nejèastìji == "Ano" |
                                    Øedidla.Nejèastìji == "Ano" ~ "Uživatel více rizikových drog",
                                    Neužívá.žádnou.látku.Užívá == "Ano" ~ "Neuživatel")) %>% 
  case_when(Neužívá.žádnou.látku.Užívá == "Ano" ~ Uzivatel_sebevnimani = "Neuživatel")
  # Zde se snažíme kvùli problému pøebyteèných hodnot sebevnímání u uživatelù alkoholu (a zároveò neuživatelù drog) pøidat hodnotu neuživatel. Syntax u casewhen je takto ale špatnì.

# Varianta B, která je také nefunkèní
# data <-
#   dataB %>%
#   if (`Neužívá žádnou látku.Užívá` == "Ano") {
#     mutate(Uzivatel_sebevnimani = "Neuživatel") %>% 
#     mutate(Uzivatel_droga = "Neuživatel")
#   } else {
#     mutate(Uzivatel_sebevnimani = case_when(Sebevnímání.Látky == "Nemám potíže" |
#                                             Sebevnímání.Látky  == "Bez látky je to horší" ~ "Rekreaèní uživatel",
#                                             Sebevnímání.Látky == "Mám potíže" |
#                                             Sebevnímání.Látky  == "Závislost" ~ "Závislý")) %>%
#     mutate(Uzivatel_droga = case_when(THC.Nejèastìji == "Ano" |
#                                       Halucinogeny.Nejèastìji  == "Ano" ~ "Uživatel ménì rizikových drog",
#                                       `Opiáty injekènì.Nejèastìji` == "Ano" |
#                                       `Opiáty jinak.Nejèastìji`  == "Ano" |
#                                       `Pervitin injekènì.Nejèastìji` =="Ano" |
#                                       `Pervitin jinak.Nejèastìji` == "Ano" |
#                                       Øedidla.Nejèastìji == "Ano" ~ "Uživatel více rizikových drog",
#                                       `Neužívá žádnou látku.Užívá` == "Ano" ~ "Neuživatel"))
#   }

# Seøazení promìnných
data <- 
  data %>%
  mutate(ID = row_number()) %>% 
  select(ID, Uzivatel_sebevnimani, Uzivatel_droga, everything())

# Skóre soc. kapitálu
# Rekodování Ano a Ne na numerické hodnoty

data <- 
  data %>% 
  mutate(across(.cols = ends_with("Rodina"), 
                .fns = ~ifelse(test = .x=="Ano", yes =5, no = 0))) %>% 
  mutate(across(.cols = ends_with("Ulice"), 
                .fns = ~ifelse(test = .x=="Ano", yes =1, no = 0))) %>% 
  mutate(across(.cols = ends_with("Jiný"), 
                .fns = ~ifelse(test = .x=="Ano", yes =1, no = 0))) %>% 
  mutate(across(.cols = ends_with("Nikdo"), 
                .fns = ~ifelse(test = .x=="Ano", yes =0, no = 0))) %>% 
  mutate(Skore_nocovani = case_when(Nocování == "Sám" ~ 0, 
                                    Nocování == "Dvojice" ~ 1, 
                                    Nocování == "3-5 lidí" ~ 2, 
                                    Nocování == "6-10 lidí" ~ 3, 
                                    Nocování == "11 a více" ~ 4))

# rowwise (sèítání øádkù/skore)
data <- 
  data %>%
  select(-contains("Organizace")) %>% 
  rowwise() %>%
  mutate(Soc_skore1 = sum(across(contains("Pomoc")))) %>%
  mutate(Soc_skore = Soc_skore1 + Skore_nocovani) %>%
  select(-Soc_skore1) %>% 
  ungroup()






# Popisná statistika
data %>% 
  group_by(Uzivatel_sebevnimani) %>% 
  get_summary_stats(Soc_skore, type = "mean_sd")

data %>% 
  group_by(Uzivatel_droga) %>% 
  drop_na() %>% 
  get_summary_stats(Soc_skore, type = "mean_sd")

# Soc. skore X Uzivatel podle sebevnímání
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

# Uživatel podle sebevnímání X Nocování
data %>%
  drop_na() %>% 
  ggplot(aes(x = Nocování, fill = Uzivatel_sebevnimani)) +
  geom_bar()

data %>%
  drop_na() %>% 
  ggplot(aes(x = Nocování, fill = Uzivatel_sebevnimani)) +
  geom_bar(position = position_fill())

# Uživatel podle drog X Nocování
data %>%
  drop_na() %>% 
  ggplot(aes(x = Nocování, fill = Uzivatel_droga)) +
  geom_bar()

data %>%
  drop_na() %>% 
  ggplot(aes(x = Nocování, fill = Uzivatel_droga)) +
  geom_bar(position = position_fill())

# Pije X Nocování
data %>%
  drop_na() %>% 
  ggplot(aes(x = Nocování, fill = Pije)) +
  geom_bar()

data %>%
  drop_na() %>% 
  ggplot(aes(x = Nocování, fill = Pije)) +
  geom_bar(position = position_fill())


  
