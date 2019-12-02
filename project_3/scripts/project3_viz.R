# Alfredo Rojas
# BIOS 611: Project 3 
# 11/18/2019

library(tidyverse)
library(ggplot2)

client_data = read.csv("scripts/client_data.csv")
head(client_data)

# Client Primary Race, export as JPEG
jpeg("client_primary_race.jpg", res = 300, units = "in", width = 7, height = 5)
client_data %>% ggplot(mapping = aes(x = Client.Primary.Race)) +
  geom_bar() +
  labs(title = "Client Primary Race Bar Graph",
       x = "Client Primary Race",
       y = "Count") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Client Ethnicity, export as JPEG
jpeg("client_ethnicity.jpg", res = 300, units = "in", width = 5, height = 5)
client_data %>% ggplot(mapping = aes(x = Client.Ethnicity)) +
  geom_bar() +
  labs(title = "Client Ethnicity Bar Graph",
       x = "Client Primary Race",
       y = "Count") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

entry_exit = read.csv("scripts/entry_exit.csv")
head(entry_exit)
data_merge = left_join(client_data, entry_exit, by = "Client.Unique.ID")

data_merge$Entry.Date.Format = as.Date(data_merge$Entry.Date.Format) 
data_merge$Exit.Date.Format = as.Date(data_merge$Exit.Date.Format) 

data_merge2 = data_merge %>%
  mutate(difference = difftime( Exit.Date.Format, Entry.Date.Format, units = "days")) %>%
  group_by(difference, Client.Primary.Race) %>%
  summarise(
    count = n()
  )

data_merge3 = data_merge2 %>% 
  filter(Client.Primary.Race != "Client refused (HUD)", 
         is.na(Client.Primary.Race) != TRUE,
         Client.Primary.Race != "",
         Client.Primary.Race != "Client doesn't know (HUD)",
         Client.Primary.Race != "Data not collected (HUD)") %>%
  filter(count < 100, difference < 750)

jpeg("entry_exit.jpg", res = 300, units = "in", width = 7, height = 6)
data_merge3 %>% ggplot(mapping = aes(x = difference, y = count)) +
  geom_point(aes(color = Client.Primary.Race)) +
  labs(
    title = "From Entry to Exit by Client Primary Race",
    x = "Amount of Days from Entry to Exit",
    y = "Count",
    fill = "Client Primary Race"
  ) 
dev.off()

summary(data_merge3)
