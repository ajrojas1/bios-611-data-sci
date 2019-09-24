# Alfredo Rojas
# BIOS 611: UMD data tidy
# 9.21.19

# """
# This code takes the Urban Ministries data and uses tidyr
# to tidy the dataset for analysis
# """

library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

# read in data
UMD_data =  read_tsv("UMD_data.tsv")
UMD_data

# Check the `Field1 - 3` variables, are they empty?
# from: https://www.quora.com/How-do-I-get-a-frequency-count-based-on-two-columns-variables-in-an-R-dataframe
summarise(group_by(UMD_data, `Field1`, `Field2`, `Field3`), count = n())

# Remove Field1, Field2, and Field3 since they are all NAs
UMD_data2 <- UMD_data %>%
  select(-`Client File Merge`, -`Field1`, -`Field2`, -`Field3`)

# # create new variable names
# new_names <- UMD_data2 %>%
#   colnames() %>% 
#   str_remove_all("\\(\\)") %>%
#   str_replace_all(" ", "_")

# change date format
UMD_data2$Date <- as.Date(UMD_data2$Date, "%m/%d/%Y")

UMD_food <- UMD_data2 %>% 
  select(Date, `Client File Number`, `Food Provided for`, `Food Pounds`) %>% 
  drop_na(`Food Provided for`,`Food Pounds`) %>%
  filter(Date >= "2000-01-01", Date <= "2019-12-31", `Food Provided for` < 30, `Food Pounds` <= 25) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  group_by(Month, Year, Day) %>%
  summarise(count = n(),
            lbs_per_prsn = sum(`Food Pounds`) / sum(`Food Provided for`),
            food_pounds_avg = mean(`Food Pounds`, na.rm = TRUE),
            ppl_avg = mean(`Food Provided for`, na.rm = TRUE))

# help from: https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/
UMD_food %>%
  #mutate(month2 = as.Date(paste0("2019-", Month, "-01"), "%Y-%m-%d")) %>% # 2019 and 01 are dummy numbers, just trying to use format for month
  ggplot(aes(x = ppl_avg, y = food_pounds_avg)) +
    geom_point(color = "darkorchid4") +
  facet_wrap(~ Year, ncol = 4) +
    labs(title = "Average Pounds of Food per Month, 2000 - 2019",
         subtitle = "Data plotted by year", 
         x = "Month",
         y = "Average Pounds of Food Given") + theme_bw(base_size = 15) #+
    #scale_x_date(date_labels = "%b")



  # separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  # filter(!is.na(`Food Provided for`) & !is.na(`Food Pounds`)) %>%
  # filter((`Food Provided for` >= 0) & (`Food Pounds` >= 0)) 
  # filter_at(.vars = vars(`Client File Number`, `Food Provided for`), .vars_predicate = any_vars(!is.na(.)))



  
  
  
  
  
  
  


