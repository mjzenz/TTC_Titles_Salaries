##Lognitudinal salary analysis

library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
filenames <- list.files("SalaryFiles", pattern="*.xlsx", full.names=TRUE)

for(i in 1:length(filenames)){
  #i <- 5
  x <- read_excel(filenames[i]) |>
        janitor::clean_names() |>
        mutate(Date = str_extract(filenames[i], "(\\d)+-\\d+")) 
  if(i > 1){
    salaries <- bind_rows(x, salaries)
  }else{
    salaries <- x
  }
}

salaries <-   salaries|>
  select(Date, first_name, last_name, division, department, 
         title, full_time_equivalent, annual_fte_adjusted_salary,
         current_annual_contracted_salary) |>
  mutate(annual_fte_adjusted_salary = ifelse(is.na(annual_fte_adjusted_salary), 
          current_annual_contracted_salary * full_time_equivalent, 
          annual_fte_adjusted_salary)) |>
  select(-current_annual_contracted_salary) 


#AIM salaries as a percentage of SAA salaries
salaries |>
  filter(division == "College of Letters & Science") |>
  filter(department == "Admin:Student Academic Affairs") |>
  mutate(AIM = last_name %in% c("ZENZ", "MCMILLAN", "SCHUTH", "PFLIEGER"), 
         Non_Manager_AsstDean = title == "Assistant Dean" & 
           !(last_name %in% c("ALBRINCK", 
                  "PFLIEGER", "LEE", "KLATT")) , 
          Group = ifelse(AIM, "AIM",
                         ifelse(Non_Manager_AsstDean, 
                          "AsstDean", 
                          "Other")))   |>
  
  group_by(Date, Group) |>
  summarize(sum_salaries = sum(annual_fte_adjusted_salary, na.rm = TRUE)) |>
  group_by(Date) |>
  mutate(prop_SAA = sum_salaries/sum(sum_salaries))|>
  filter(Group != "Other") |>
  pivot_wider(names_from = Group, values_from = c("sum_salaries", "prop_SAA"))




