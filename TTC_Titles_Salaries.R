##This examines data about TTC titles and salaries
library(dplyr)
library(readr)

Title_Salary_Data <- read_csv("Salary_2022-08.csv")

Title_Salary_Data$YearsOfService <- time_length(difftime(Sys.Date(), 
                                              as.Date(Title_Salary_Data$`Date of Hire`, 
                                            format = "%d%b%Y %H:%M:%S")), "years") 



title.salary.dist <- Title_Salary_Data %>%
                    group_by(Title, `Salary Grade`) %>%
                    filter(`Full-time Equivalent` > .25) %>%
                    summarize(min = round(min(`Current Annual Contracted Salary`), 2),
                              first = round(quantile(`Current Annual Contracted Salary`, .25), 2),
                              median = round(median(`Current Annual Contracted Salary`), 2),
                              third = round(quantile(`Current Annual Contracted Salary`, .75), 2),
                              max = round(max(`Current Annual Contracted Salary`), 2),
                              n = n_distinct(paste(`First Name`, `Last Name`)),
                              years_min = round(min(YearsOfService), 2),
                              years_first = round(quantile(YearsOfService, .25), 2),
                              years_median = round(median(YearsOfService), 2),
                              years_third = round(quantile(YearsOfService, .75), 2),
                              years_max = round(max(YearsOfService), 2))



write.csv(title.salary.dist, file = "Title_Salary_Years_Quartiles.csv", row.names = FALSE)



Title_Salary_Data %>% filter(`Job Code` %in% c("FN008", "FN007", "FN006")) %>%
  ggplot(aes(x = YearsOfService, y = `Current Annual Contracted Salary`)) +
    geom_point(aes(color = Title)) +
  geom_smooth(method=lm)
  
fs_lm <- Title_Salary_Data %>% filter(`Job Code` %in% c("FN008", "FN007", "FN006")) %>%
        lm(`Current Annual Contracted Salary` ~ YearsOfService + `Job Code`, data = .)
summary(fs_lm)
                    
