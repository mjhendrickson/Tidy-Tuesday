# ===== Introduction =====
# Project: Tidy Tuesday - Week 1 - Average Tuition
# By: Matthew Hendrickson
# Twitter: @mjhendrickson
# GitHub: https://github.com/mjhendrickson/Tidy-Tuesday

# ===== Install needed packages =====
install.packages("tidyverse")
install.packages("readxl")
install.packages("scales")
install.packages("viridis")
#install.packages("RColorBrewer")


# ===== Load needed packages =====
library(tidyverse)
library(readxl)
library(scales)
library(viridis)
#library(RColorBrewer)


# ===== Load & review data =====
average_tuition <- read_excel("us_avg_tuition.xlsx")
View(average_tuition)
glimpse(average_tuition)


# ===== Data prep =====
average_tuition <- average_tuition %>% # append new fields to df
  rename(state = State) %>%  # adhere to tidy naming convention
  mutate(state_abb = factor(state.abb))  # state abbreviation

avg_tuition <- average_tuition %>% # create new df
  gather(year, tuition, `2004-05`:`2015-16`) # reshape data wide to long

avg_tuit <- average_tuition %>% # append new fields to df
  mutate(tuition_5yr_chg = `2015-16` - `2010-11`) %>% # 5 year tuition change
  mutate(tuition_5yr_pct_chg = (`2015-16` - `2010-11`) / `2010-11` * 100) %>% # 5 year % change
  gather(year, tuition, `2004-05`:`2015-16`) %>% # reshape data wide to long
  select(state, state_abb, year, tuition, tuition_5yr_chg, tuition_5yr_pct_chg) %>% # select needed fields
  filter(year == "2015-16")  # keep only last year



# ===== Create plots =====
# Average Tuition - Box
avg_tuition %>% 
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "State", 
       y = "Tuition Range",
       title = "College Tuition Ranges by State - Academic Years 2004-2005 - 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")


# Average Tuition - Point
avg_tuition %>% 
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition, color = year)) +
  geom_point() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "State", 
       y = "Tuition",
       color = "Academic Year", 
       title = "College Tuition by State",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")


# Average Tuition & 5 Year Change - Point
avg_tuit %>% 
  ggplot(aes(x = fct_reorder(state_abb, tuition), 
             y = tuition, 
             color = tuition_5yr_pct_chg, 
             size = tuition_5yr_pct_chg)) +
  geom_point() +
  #coord_flip() +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_viridis() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) +
  labs(x = "State", 
       y = "Average Tuition",
       size = "5 yr change", 
       color = "", 
       title = "Average College Tuition by State - Academic Year 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")


# Average Tuition & 5 Year Change - Bar
avg_tuit %>% 
  ggplot(aes(x = fct_reorder(state_abb, tuition), 
             y = tuition, 
             fill = tuition_5yr_pct_chg)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = round(tuition),
                angle = 90)) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_viridis() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) +
  labs(x = "State", 
       y = "Average Tuition",
       fill = "5 yr % change", 
       title = "Average College Tuition by State - Academic Year 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")
