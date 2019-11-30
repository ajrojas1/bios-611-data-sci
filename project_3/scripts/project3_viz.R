# Alfredo Rojas
# BIOS 611: Project 3 
# 11/18/2019

library(tidyverse)
library(ggplot2)

client_data = read.csv("scripts/client_data.csv")
head(client_data)

client_data %>% ggplot(mapping = aes(x = Client.Primary.Race)) +
  geom_bar() +
  labs(title = "Client Primary Race Bar Graph",
       x = "Client Primary Race",
       y = "Count") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1))

client_data %>% ggplot(mapping = aes(x = Client.Ethnicity)) +
  geom_bar() +
  labs(title = "Client Ethnicity Bar Graph",
       x = "Client Primary Race",
       y = "Count") +
  theme(text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1))
