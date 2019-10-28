# Alfredo Rojas
# BIOS 611: Project 2
# Helper Functions, Shiny app

# path: "data/UMD_Services_Provided_20190719.tsv"
load_data = function(x) {
  
  umd_data <- read_tsv(x)
  
  umd_data2 <- umd_data %>%
    select(-`Client File Merge`, -`Field1`, -`Field2`, -`Field3`)
  
  umd_data2$Date <- as.Date(umd_data2$Date, "%m/%d/%Y") 
  
  # food data, select target variables, drop NAs, filter for 2000 - 2019, change date format
  umd_food <- umd_data2 %>% 
    select(Date, `Client File Number`, `Food Provided for`, `Food Pounds`) %>% 
    drop_na(`Food Provided for`,`Food Pounds`) %>%
    filter(Date >= "2000-01-01", Date <= "2019-12-31", `Food Pounds` < 100, `Food Provided for` < 100) %>%
    separate(Date, into = c("Year", "Month", "Day"), sep = "-")
  
  food_summary <- umd_food %>%
    group_by(Year, Month) %>%
    summarise(
      count = n(),
      lbs_per_prsn = sum(`Food Pounds`) / sum(`Food Provided for`),
      food_pounds_sum = sum(`Food Pounds`, na.rm = TRUE),
      ppl_sum = sum(`Food Provided for`, na.rm = TRUE),
      ppl_avg = mean(`Food Provided for`, na.rm = TRUE)
    )
  
  return(food_summary)
  
}

food_plot = function(x, y) {
  
  # plot food pounds annually in a bar graph
  x %>% 
    mutate(month2 = as.Date(paste0("2019-", Month, "-01"), "%Y-%m-%d")) %>%
    ggplot(mapping = aes(x = month2, y = food_pounds_sum)) +
    geom_bar(stat = "identity", fill = "purple") + 
    labs(title = paste0("Monthly Pounds of Food: ", y),
         subtitle = "Data plotted by year", 
         x = "Month",
         y = "Food Pounds") + theme_bw(base_size = 15) +
    scale_x_date(
      limits = c(as.Date("2019-01-01"), as.Date("2019-12-01")), 
      date_minor_breaks = "1 month", 
      date_labels = "%b") # this last function creates the missing months for visual purposes
  
}
visits_plot <- function(x, y){
  x %>%
    mutate(month2 = as.Date(paste0("2019-", Month, "-01"), "%Y-%m-%d")) %>%
    ggplot(mapping = aes(x = month2, y = count)) +
    geom_point() + 
    geom_line() +
    labs(title = paste0("Visits per month: ", y),
         subtitle = "Data plotted by year", 
         x = "Month",
         y = "Visits per Month") + theme_bw(base_size = 15) +
    scale_x_date(
      limits = c(as.Date("2019-01-01"), as.Date("2019-12-01")), 
      date_minor_breaks = "1 month", 
      date_labels = "%b") # this last function creates the missing months for visual purposes
} 
