##Lognitudinal salary analysis

library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
library(zoo)
library(quantmod)

job.groups <-  read_csv("Title_Groups.csv")

getSymbols("CPIAUCSL", src='FRED') 
inflation.index <- data.frame(Date=zoo::index(CPIAUCSL), zoo::coredata(CPIAUCSL))


filenames <- list.files("SalaryFiles", pattern="*.xlsx", full.names=TRUE)


#Read in and clean salary data
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
rm(x)

person.ids <- salaries |>
  select(last_name, first_name) |>
  unique() |>
  mutate(ID = str_pad(1:n(), width = 5, side = "left", pad = "0"))


salary.data <-   salaries|>
  left_join(person.ids, by = c("last_name", "first_name")) |>
  mutate(job_code = ifelse(is.na(job_code), jobcode, job_code),
         employee_category = ifelse(employee_category == "AS", 
                                    "Academic Staff", employee_category),
         employee_category = ifelse(employee_category == "FA",
                                    "Faculty", employee_category),
         employee_category = ifelse(employee_category %in%
                                      c("CJ", "CL", "CP"), 
                                    "University Staff", employee_category), 
         employee_category = ifelse(employee_category %in%
                                  c("ET1", "ET2", "ET3", "ET4"), 
                                    "Employee-In-Training", employee_category),
         employee_category = ifelse(employee_category %in%
                                      c("LI"), 
                                    "Limited Appointee", employee_category),
         employee_category = ifelse(employee_category %in%
                                      c("OT1", "OT2", "OT3", "OT4", "OT5", "OT6"), 
                                    "Other", employee_category)) |>
  select(Date, ID, first_name, last_name, division, department, 
         title,job_code,employee_category,
         flsa_status, date_of_hire, salary_grade, 
         full_time_equivalent, annual_fte_adjusted_salary,
         current_annual_contracted_salary) |>
  mutate(Date = as.Date(paste(Date, "-01", sep = "")),
          annual_fte_adjusted_salary = ifelse(is.na(annual_fte_adjusted_salary), 
          current_annual_contracted_salary * full_time_equivalent, 
          annual_fte_adjusted_salary), 
         FullTime = full_time_equivalent == 1, 
         JobGroup = substr(job_code, 1, 2), 
         JobNumber = substr(job_code, 3, 5)) |>
  left_join(inflation.index, by = c("Date")) |>
  mutate(`2021 Index` = inflation.index[which(inflation.index$Date=="2021-11-01"),]$CPIAUCSL / CPIAUCSL,
         `FTE Adjusted Salary (2021 Dollars)` = annual_fte_adjusted_salary * `2021 Index`) |>
  select(-current_annual_contracted_salary) 

data.timespan <- c(min(salary.data$Date), max(salary.data$Date))  



#Write out clean consolidated salary data
#write.csv(salaries.data, "CleanUW_Salaries.csv", row.names = FALSE)

#Add variables to salaries
salary.change <- salary.data |>
  filter(Date %in% data.timespan) |>
  filter(FullTime) |>
  filter(annual_fte_adjusted_salary > 2000) |>
  filter(employee_category %in% 
           c("Academic Staff", "University Staff", 
             "Faculty", "Limited Appointee")) |>
  mutate(`Fac-Staff-Limited` = ifelse(employee_category %in% 
                                        c("Academic Staff", "University Staff"),
                                      "Staff", 
                                      ifelse(employee_category == "Limited Appointee", 
                                      "Limited", "Faculty"))) |>
  arrange(ID, Date) |>
  group_by(ID,`Fac-Staff-Limited`, JobGroup) |>
  mutate(`Real Salary Change` = `FTE Adjusted Salary (2021 Dollars)`- 
                                   lag(`FTE Adjusted Salary (2021 Dollars)`),
         `Real % Salary Change` = round((`Real Salary Change`/lag(`FTE Adjusted Salary (2021 Dollars)`)) * 100,2)) |>
    filter(!(is.na(`Real % Salary Change`)))

        


### graph of % change by groups

salary.change |>
  #filter(`Real % Salary Change` < 50) |>
  ggplot(aes(x = employee_category, y = `Real % Salary Change`)) +
  geom_boxplot()


### graph of % change by L&S area.
salary.change |>
  filter(division == "College of Letters & Science") |>
  mutate(`L&S Area` = ifelse(department == "Administration", 
                             "Admin", 
                             ifelse(department %in% 
                              c("Admin:Student Academic Affairs", 
                                "L&S Career Init & Svcs",
                                "L&S Honors Program"),
                              "Central Student Services",
                              "Department/Other"))) |>
  ggplot(aes(x = `L&S Area`, y = `Real % Salary Change`,
             Group = employee_category)) + 
  geom_boxplot(aes(fill = employee_category))


###Admin Limited salary changes
ls.admin.limited <- salary.change |>
  filter(division == "College of Letters & Science") |>
  filter(department == "Administration") # |>
  filter(employee_category == "Limited Appointee")


###SAA Limited salary changes
ls.saa.limited <- salary.change |>
  filter(division == "College of Letters & Science") |>
  filter(department == "Admin:Student Academic Affairs") |>
  filter(employee_category == "Limited Appointee")

