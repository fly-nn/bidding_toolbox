library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(lubridate)
library(glue)
library(ggtext)
library(DT)

### Imoort Vacation Spreadsheet ###

vacation_award_file <- read_excel("~/OneDrive - NJASAP/_Action NJASAP/2023 - 2024 Pilot Vacation Awards 7.21.23.xlsx", 
                                                       sheet = "tVacaAward")
vacation_award <- vacation_award_file %>% 
  rename_with(~tolower(gsub(" ","_", .x))) %>% 
  mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

rm(vacation_award_file)

vacation_award %>% 
  select(-end_date, -week_number) %>% 
  datatable(
    colnames = c("Name", "Senioirty", "Fleet", "Seat", "Week", "Start Date", "No. Days"),
    rownames = F,
    filter = "top",
    options = list(
      # paging = TRUE,
      pageLength = 25,
      autoWidth = TRUE,
      #    scrollY="100vh",
      scrollCollapse = FALSE
    ),
    caption = "2023-2024 NJASAP Vacation Award"
  )