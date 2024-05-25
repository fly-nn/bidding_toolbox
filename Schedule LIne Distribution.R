library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(lubridate)
library(glue)
library(ggtext)

### Ops Schedule Import & Clean ###
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
  select(position) %>% 
  unique()
              

tjune_bid_clean <- tjune_ops_sched %>% 
  rename_with(~tolower(gsub(" ","_", .x))) %>% 
  filter(str_detect(schedule_type, "^7|^8"),
         ! position %in% c("FA", "FACA", "FIOE")) %>% 
  mutate(across(schedule_type, ~gsub("7 & 7 - ", "7&7_", .x)),
         across(schedule_type, ~gsub("8&6 - ", "8&6_", .x)),
         across(schedule_type, ~gsub(" W/Travel - ", "_", .x)),
         across(schedule_type, ~gsub(" TSP - ", "_", .x)),
         line_num = as.double(str_extract(schedule_type, "\\d{1,2}$")),
         weekend = case_when(line_num == 2 ~ "Weekend",
                             line_num == 3 ~ "Weekend",
                             line_num == 9 ~ "Weekend",
                             line_num == 10 ~ "Weekend",
                             TRUE ~ "Weekday"
                              )
         )

View(tjune_bid_clean)

### Seiority List Import and Clean ###

seniority_file <- dir(path = "~/OneDrive - NJASAP/Documents/Seniority Related/Seniority List - Union/2024",
                      full.names = T,
                      pattern = "2024-05.*\\.xlsx$")

tseniority <- read_excel(seniority_file,
                         sheet = "UNION_EXCEL_FILE",
                         range = cell_cols(1:16)
)

tseniority <- tseniority %>% 
  rename_with(~tolower(gsub(" ","_",.x)))

### Join to Add Seniority ###

tjune_bid_clean <- tjune_bid_clean %>% 
  left_join(tseniority, by = c("cmi" = "cmi")) %>% 
  select(1:8, union_seniority) %>% 
  rename(name = name.x)
  

### Plot Construction ###

#t7n7_all <- 
tjune_bid_clean %>% 
  filter(str_detect(schedule_type, "^7")) %>% 
  count(line_num, schedule_type, weekend, name = "count") %>% 
  mutate(schedule_type = fct_reorder(schedule_type, line_num),
         percent = label_percent(accuracy = 0.1)(count / sum(count))) %>%
  ggplot(aes(schedule_type, count))+
  geom_col(aes(fill = weekend))+
  geom_text(aes(label = glue("{count} ({percent})")), vjust = -0.5)+ #fill = "steelblue", color = "#2C5171"
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = "7&7 Line Distribution",
       subtitle = "*All Fllets and Seats*",
       fill = ""
       )+
  scale_fill_manual(values = c("steelblue", "#2C5171"))+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown()
        )
  

t8n6_all <- tjune_bid_clean %>% 
  filter(str_detect(schedule_type, "^8")) %>% 
  count(line_num, schedule_type, name = "count") %>% 
  mutate(schedule_type = fct_reorder(schedule_type, line_num)) %>% 
  ggplot(aes(schedule_type, count))+
  geom_col(fill = "steelblue", color = "#2C5171")+
  geom_text(aes(label = count), vjust = -0.5)+
  theme_bw()+
  labs(x = "",
       y = "Count",
       title = "8&6 Line Distribution",
       subtitle = "*All Fllets and Seats*"
  )+
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

            