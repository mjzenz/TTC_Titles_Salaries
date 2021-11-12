##This examines data about TTC titles and salaries
library(dplyr)
library(readr)

Title_Salary_Data <- read_csv("~/Downloads/All Faculty and Staff Title and Salary Information - Post-TTC and Pre-TTC.xlsx - Post-Nov.7, 2021 (Post-TTC).csv")



title.salary.dist <- Title_Salary_Data %>%
                    group_by(Title, `Salary Grade`) %>%
                    filter(`Full-time Equivalent` > .25) %>%
                    summarize(min = round(min(`Current Annual Contracted Salary`), 2),
                              first = round(quantile(`Current Annual Contracted Salary`, .25), 2),
                              median = round(median(`Current Annual Contracted Salary`), 2),
                              third = round(quantile(`Current Annual Contracted Salary`, .75), 2),
                              max = round(max(`Current Annual Contracted Salary`), 2),
                              n = n_distinct(paste(`First Name`, `Last Name`)))

write.csv(title.salary.dist, file = "Title_Salary_Quartiles.csv", row.names = FALSE)

                    