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
library(GGally)
library(gridExtra)

#""""
# Function for normalization
# From: https://datasharkie.com/how-to-normalize-data-in-r/
#""""

normalize <- function(x) {
  return((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

# read in data
UMD_data =  read_tsv("data/UMD_Services_Provided_20190719.tsv")
head(UMD_data)

# Check the `Field 1 - 3` variables, are they empty?
# from: https://www.quora.com/How-do-I-get-a-frequency-count-based-on-two-columns-variables-in-an-R-dataframe
summarise(group_by(UMD_data, `Field1`, `Field2`, `Field3`), count = n())

# Remove Field1, Field2, and Field3 since they are all NAs
UMD_data2 <- UMD_data %>%
  select(-`Client File Merge`, -`Field1`, -`Field2`, -`Field3`)

# change date format so R can interpret it
UMD_data2$Date <- as.Date(UMD_data2$Date, "%m/%d/%Y")

# search for outliers and remove
outliers <- boxplot(UMD_data2$`Food Pounds`)$out

# food data, select target variables, drop NAs, filter for 2000 - 2019, change date format
UMD_food <- UMD_data2 %>% 
  select(Date, `Client File Number`, `Food Provided for`, `Food Pounds`) %>% 
  drop_na(`Food Provided for`,`Food Pounds`) %>%
  filter(Date >= "2000-01-01", Date <= "2019-12-31", `Food Pounds` < 100, `Food Provided for` < 100) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-")

# summarize data by group, count gets number of observations per day
food_summary <- UMD_food %>%
  group_by(Year, Month, Day) %>%
  summarise(
    count = n(),
    lbs_per_prsn = sum(`Food Pounds`) / sum(`Food Provided for`),
    food_pounds_sum = sum(`Food Pounds`, na.rm = TRUE),
    ppl_sum = sum(`Food Provided for`, na.rm = TRUE),
    ppl_avg = mean(`Food Provided for`, na.rm = TRUE)
    )

# clothes data, select relevant variables, change date format
clths_smmry <- UMD_data2 %>%
  select(Date, `Client File Number`, `Clothing Items`) %>%
  drop_na(`Client File Number`, `Clothing Items`) %>%
  filter(Date >= "2000-01-01", Date <= "2019-12-31") %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  group_by(Year, Month) %>%
  summarise(
    count = n(),
    sum_clths = sum(`Clothing Items`)
  )


############################# 
# Plotting yearly food lbs per year on a monthly basis, using bar graph
# Also plotting yearly clothing items on a montly basis, using bar graph
# help from: 
# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/
# create new monthly variable for bar graph
# 2019 and 01 are dummy numbers, just trying to use format for month
#############################

# plot food pounds annually in a bar graph
food_summary %>% 
  mutate(month2 = as.Date(paste0("2019-", Month, "-01"), "%Y-%m-%d")) %>%
  ggplot(mapping = aes(x = month2, y = food_pounds_sum)) +
    geom_bar(stat = "identity", fill = "darkseagreen4") + 
    facet_wrap(~ Year, ncol = 4) +
    labs(title = "Monthly Pounds of Food, 2000 - 2019",
         subtitle = "Data plotted by year", 
         x = "Month",
         y = "Food Pounds") + theme_bw(base_size = 15) +
    scale_x_date(date_labels = "%b") 

# plot clothing items annually in a bar graph
clths_smmry %>%
  mutate(month2 = as.Date(paste0("2019-", Month, "-01"), "%Y-%m-%d")) %>%
  ggplot(mapping = aes(x = month2, y = sum_clths)) +
    geom_bar(stat = "identity", fill = "darkslateblue") + 
    facet_wrap(~ Year, ncol = 4) +
    labs(title = "Clothes Items Provided, 2000 - 2019 (Monthly)",
         subtitle = "Data plotted by year", 
         x = "Month",
         y = "Clothes Items") + theme_bw(base_size = 15) +
    scale_x_date(date_labels = "%b")

###########################
# COMPARE CLOTHES and FOOD
# create data frame containing food and clothes data together
###########################
food_clths <- UMD_data2 %>% 
  select(Date, `Client File Number`, `Clothing Items`, `Food Pounds`, `Food Provided for`) %>%
  drop_na(`Client File Number`, `Clothing Items`, `Food Pounds`, `Food Provided for`) %>%
  filter(Date >= "2000-01-01", Date <= "2019-12-31", `Food Pounds` < 100) 

# Summary table of food_clths grouped by Date
food_clths_smry <- food_clths %>%
  group_by(Date) %>%
  summarise(
    count = n(),
    sum_food = sum(`Food Pounds`),
    sum_clths = sum(`Clothing Items`),
    sum_ppl_food = sum(`Food Provided for`),
    food_per_prsn = sum_food / sum_ppl_food,
    clths_per_vist = sum_clths / count
  )

# now explore relationship between clothes items and food pounds on a daily basis
# find correlation coefficient, rounded to nearest 3 decimals
round(cor(food_clths_smry$sum_clths, food_clths_smry$sum_food, method = "pearson"), 3)

# plot sums of clothes and food pounds
p1 <- food_clths_smry %>% 
  ggplot(aes(x = sum_clths, y = sum_food)) +
    geom_point(color = "darkslateblue", alpha = 1/3, position = "jitter") +
    geom_smooth(se = FALSE, color = "deeppink3") +
    labs(title = "Daily Sums of Clothes Items\nand Food lbs, 2000 - 2019",
         subtitle = "Each observation is one day", 
         x = "Sum of clothes items",
         y = "Sum of food lbs.") +
     annotate("text", x = 150, y = 30, label = "r = 0.786")

# Normalize the variables

# correlation coefficient of food/person and clothes/visit
round(cor(food_clths_smry$food_per_prsn, food_clths_smry$clths_per_vist, 
          method = "pearson", use = "complete.obs"), 3)

# plot food per person & and clothes per visit
p2  <- food_clths_smry %>% 
  ggplot(aes(x = clths_per_vist, y = food_per_prsn)) +
    geom_point(color = "darkslateblue", alpha = 1/3) +
    geom_smooth(se = FALSE, color = "deeppink3") +
    labs(title = "Daily Clothing Items and Food Pounds \nper Person, 2000 - 2019",
         subtitle = "Each observation is for one day",
         x = "Sum of clothing items per daily visits \n(multiple clients)", 
         y = "Food lbs. provided per person") +
    annotate("text", x = 30, y = 2.5, label = "r = - 0.484")

# side-by-side plot
grid.arrange(p1, p2, nrow = 1)

# Summary of food_clths table grouped by Client
client_smry <- food_clths %>%
  group_by(`Client File Number`) %>%
  summarise(
    count = n(),
    sum_food = sum(`Food Pounds`),
    sum_clths = sum(`Clothing Items`),
    sum_ppl_food = sum(`Food Provided for`),
    food_per_prsn = sum_food / sum_ppl_food,
    clths_per_clnt = sum_clths / count
  )

# find correlation coefficient for sum of clothes and food per client 
round(cor(client_smry$sum_clths, client_smry$sum_food, 
          method = "pearson", use = "complete.obs"), 3)

p3 <- client_smry %>% 
  ggplot(aes(x = sum_clths, y = sum_food)) +
  geom_point(color = "darkslateblue", alpha = 1/3, position = "jitter") +
  geom_smooth(se = FALSE, color = "deeppink3") +
  labs(title = "Sums of Clothes Items and Food lbs \nper Client Number, 2000 - 2019",
       subtitle = "Each observation is one client", 
       x = "Sum of clothes items",
       y = "Sum of food lbs.") +
  annotate("text", x = 1000, y = 100, label = "r = 0.924")

# Normalize the variables

# correlation coefficient, log transformed
round(cor(client_smry$clths_per_clnt, client_smry$food_per_prsn, 
          method = "pearson", use = "complete.obs"), 3)

# same variables, but log transformed
p4  <- client_smry %>% 
  ggplot(aes(x = clths_per_clnt, y = food_per_prsn)) +
  geom_point(color = "darkslateblue", alpha = 1/3) +
  geom_smooth(se = FALSE, color = "deeppink3") +
  labs(title = "Clothing Items and Food lbs per Person \for each Client Number, 2000 - 2019",
       subtitle = "Each observation is for one client",
       x = "Sum of clothing items per visit \n(for one client)", 
       y = "Food lbs. provided per person")  +
  annotate("text", x = 30, y = 12, label = "r = - 0.419")

# side-by-side plot
grid.arrange(p3, p4, nrow = 1)

# Explore the relationship between clothes and food lbs on a yearly basis, notice positive trend
# for most years

food_clths %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  ggplot(aes(x = food_per_prsn, y = sum_food)) +
  geom_point(color = "darkslateblue", alpha = 1/3) +
  geom_smooth(se = FALSE, color = "deeppink3") +
  facet_wrap(~ Year, ncol = 4) +
  labs(title = "Logged Clothing Items & Food Pounds per day, 2000 - 2019",
       subtitle = "Data plotted by year", 
       x = "Clothing items per day",
       y = "Food pounds per day") + theme_bw(base_size = 15) 
