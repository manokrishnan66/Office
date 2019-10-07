library(tidyverse)
library(dplyr)
library("readxl")

dirpath <- getwd()
filename <- 'iplallseasons_refined.xlsx'
fullpath <- file.path(dirpath,filename)

ipl <- read_excel("D:/Mine/Data Science/IPL VIZ/Data/iplallseasons_refined.xlsx")

head(iplyr$month-date)

ipl %>% str_split(ipl$Match_date, ",")
ipl %>% strsplit(as.character(Match_date),',')

iplyr <- ipl %>%separate(col = Match_date, into = c("monthdate", "year"), sep = ",")

iplyr <- iplyr %>% mutate(year = as.numeric(year))

iplyr <- iplyr %>%separate(col = monthdate, into = c("month", "date"), sep = " ")