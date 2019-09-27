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
# install.packages("GGally")
library(GGally)
# install.packages("gridExtra")
library(gridExtra)

# read in data
UMD_data =  read_tsv("data/UMD_Services_Provided_20190719.tsv")
UMD_data

# Check the `Field 1 - 3` variables, are they empty?
# from: https://www.quora.com/How-do-I-get-a-frequency-count-based-on-two-columns-variables-in-an-R-dataframe
summarise(group_by(UMD_data, `Field1`, `Field2`, `Field3`), count = n())

# Remove Field1, Field2, and Field3 since they are all NAs
UMD_data2 <- UMD_data %>%
  select(-`Client File Merge`, -`Field1`, -`Field2`, -`Field3`)


# change date format so R can interpret it
UMD_data2$Date <- as.Date(UMD_data2$Date, "%m/%d/%Y")

# clean data, select target variables, drop NAs, filter for 2000 - 2019, and group_by and summarise
UMD_food <- UMD_data2 %>% 
  select(Date, `Client File Number`, `Food Provided for`, `Food Pounds`, `Clothing Items`) %>% 
  drop_na(`Food Provided for`,`Food Pounds`, `Clothing Items`) %>%
  filter(Date >= "2000-01-01", Date <= "2019-12-31", `Food Provided for` < 30, `Food Pounds` <= 25) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  group_by(Month, Year, Day) %>%
  summarise(count = n(),
            lbs_per_prsn = sum(`Food Pounds`) / sum(`Food Provided for`),
            food_pounds_sum = sum(`Food Pounds`, na.rm = TRUE),
            ppl_avg = mean(`Food Provided for`, na.rm = TRUE),
            clothes_sum = sum(`Clothing Items`), na.rm= TRUE) %>%
  filter(lbs_per_prsn > 0, food_pounds_sum > 0, ppl_avg > 0, clothes_sum > 0)



# Plotting yearly food lbs per year on a monthly basis, using bar graph
# help from: 
# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/
UMD_food %>% # create new monthly variable for bar graph
  mutate(month2 = as.Date(paste0("2019-", Month, "-01"), "%Y-%m-%d")) %>% # 2019 and 01 are dummy numbers, just trying to use format for month
  ggplot(aes(x = month2, y = food_pounds_sum)) +
  geom_bar(stat = "identity", fill = "darkslateblue") +
  facet_wrap(~ Year, ncol = 4) +
  labs(title = "Monthly Pounds of Food, 2000 - 2019",
       subtitle = "Data plotted by year", 
       x = "Month",
       y = "Food Pounds") + theme_bw(base_size = 15) +
  scale_x_date(date_labels = "%b")

# now explore relationship between clothes items and food pounds on a daily basis
# find correlation coefficient, rounded to nearest 3 decimals
round(cor(UMD_food$clothes_sum, UMD_food$food_pounds_sum, method = "pearson"), 3)

# plot sums of clothes and food pounds
p1 <- UMD_food %>% 
  ggplot(aes(x = clothes_sum, y = food_pounds_sum)) +
    geom_point(color = "darkslateblue", alpha = 1/3) +
    geom_smooth(se = FALSE, color = "deeppink3") +
    labs(title = "Daily Sums of Clothes Items\n and Food lbs, 2000 - 2019",
         x = "Sum of clothes Items per day",
         y = "Sum of food lbs per day") +
    annotate("text", x = 150, y = 50, label = "r = 0.786")

# correlation coefficient, log transformed
round(cor(log2(UMD_food$clothes_sum),log2(UMD_food$food_pounds_sum)), 3)

# same variables, but log transformed
p2  <- UMD_food %>% 
  ggplot(aes(x = log2(clothes_sum), y = log2(food_pounds_sum))) +
  geom_point(color = "darkslateblue", alpha = 1/3) +
  geom_smooth(se = FALSE, color = "deeppink3") +
  labs(title = "Sums of Clothes Items and Food lbs \n(Log Transformed), 2000 - 2019",
       x = "Log of daily summed clothes items", 
       y = "Log of daily summed food lbs") +
  annotate("text", x = 5, y = 2, label = "r = 0.769")

grid.arrange(p1, p2, nrow = 1)

# Explore the relationship between clothes and food lbs on a yearly basis, notice positive trend
# for most years
UMD_food %>%
  ggplot(aes(x = log2(clothes_sum), y = log2(food_pounds_sum))) +
  geom_point(color = "darkslateblue") +
  geom_smooth(se = FALSE, color = "deeppink3") +
  facet_wrap(~ Year, ncol = 4) +
  labs(title = "Logged Clothing Items & Food Pounds per day, 2000 - 2019",
       subtitle = "Data plotted by year", 
       x = "Log of sum of food lbs per day",
       y = "Log of clothing items per day") + theme_bw(base_size = 15) 




  
  
  
  
  
  
  


