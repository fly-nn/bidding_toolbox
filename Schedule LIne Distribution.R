library(tidyverse)
library(readxl)
library(ggplot2)
library("scales")
library(lubridate)
library(glue)

tjune_ops_sched <- read_excel("~/Library/CloudStorage/OneDrive-NJASAP/Documents/Operational Schedules/Ops Schedules Merge PowerQuery/2021/2024-06 All Fleets June  2024 Operational Schedule PQ.xlsx", 
                              sheet = "Sheet1")

ops_sched_file <- dir(path = "~/OneDrive - NJASAP/Documents/Operational Schedules/Ops Schedules Merge PowerQuery/2021",
                       full.names = T,
                       pattern = "2024-06 All Fleets.*\\.xlsx$")

tjune_ops_sched <- read_excel(ops_sched_file,
                          sheet = "Sheet1",
                          range = cell_cols(1:6)
)

tjune_ops_sched <- tjune_ops_sched %>% 
  rename_with(~tolower(gsub(" ","_", .x)))

tjune_ops_sched %>% 
  select(schedule_type) %>% 
  filter(str_detect(schedule_type, "^7")) %>% 
  unique()
              

tjune_bid_dist <- tjune_ops_sched %>% 
  rename_with(~tolower(gsub(" ","_", .x))) %>% 
  filter(str_detect(schedule_type, "^7|^8")) %>% 
  mutate(across(schedule_type, ~gsub("7 & 7 - ", "7&7_", .x)),
         across(schedule_type, ~gsub("8&6 - ", "8&6_", .x)),
         across(schedule_type, ~gsub(" W/Travel - ", "_", .x)),
         across(schedule_type, ~gsub(" TSP - ", "_", .x))
         )%>% 
  count(cmi, fleet, position, schedule_type)

View(tjune_bid_dist)

tjune_bid_dist %>% 
  filter(str_detect(schedule_type, "^7")) %>% 
  count(schedule_type, name = "count") %>% 
  ggplot(aes(schedule_type, count))+
  geom_col()+
  geom_text(aes(label = count), vjust = -0.5)

            