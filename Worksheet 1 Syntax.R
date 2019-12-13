library(tidyverse)
library(NHANES)
library(ganimate)

colnames(NHANES)

nrow(NHANES)

head(NHANES)

NHANES %>% 
  select(ID) %>% 
  n_distinct()

NHANES_tidied <- NHANES %>% 
  distinct(ID, .keep_all = TRUE)

nrow(NHANES_tidied)

NHANES_tidied

NHANES_tidied %>%
  ggplot(aes(x = BMI)) + 
  geom_histogram(bins = 100, na.rm = TRUE)

filter(!is.na(Education))%>%
  group_by(Education) %>% 
  summarise(median = median(BMI, na.rm = TRUE))

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = fct_reorder(Education, BMI, median), y = BMI, colour = Education)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .5) +
  guides(colour = FALSE) + 
  labs(title = "Examining the effect of education level on median BMI", 
       x = "Education Level", 
       y = "BMI")

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram() +
  guides(fill = FALSE) + 
  labs(title = "Examining the effect of education level on BMI",
       x = "BMI", 
       y = "Number of cases") +
  facet_wrap(~ Education)
  scales = "free"

  NHANES_tidied %>% 
    filter(!is.na(Education) & !is.na(BMI)) %>%
    group_by(Education) %>% 
    ggplot(aes(x = BMI, fill = Education)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(aes(y = ..density..)) +
    guides(fill = FALSE) + 
    labs( title = "Examining the effect of education level on BMI", 
          x = "BMI", 
          y = "Density") + 
    facet_wrap(~ Education)
  
  NHANES_tidied %>% filter(!is.na(Work)) %>%
    group_by(Work) %>% 
    summarise(median = median(BMI, na.rm = TRUE))
  
  NHANES_tidied %>% 
    filter(!is.na(Education) & !is.na(Work)) %>%
    group_by(Education, Work) %>% 
    summarise(median = median(BMI, na.rm = TRUE))
  
  NHANES_tidied %>% 
    filter(!is.na(Education) & !is.na(Work)) %>%
    group_by(Education, Work) %>% 
    summarise(median = median(BMI, na.rm = TRUE)) %>%
    arrange(desc(median))
  
  NHANES_tidied %>% 
    filter(!is.na(Education) & !is.na(Work) & !is.na(BMI)) %>%
    ggplot(aes(x = fct_reorder(Education:Work, BMI, median), 
               y = BMI, 
               colour = Education:Work)) + 
    geom_boxplot() + 
    coord_flip() +
    guides(colour = FALSE) + 
    labs(title = "Examining the effect of education level and employment \nstatus on BMI",
         x = "Education X Working", 
         y = "BMI")
  
NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI)) %>%
  ggplot(aes(x = fct_reorder(Work, BMI, median), 
               y = BMI, 
               colour = Work)) + 
    geom_boxplot() + 
    coord_flip() +
    guides(colour = FALSE) + 
    labs(title = "Examining the effect of employment status on BMI",
         x = "Working", 
         y = "BMI")
  
NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI)) %>%
  ggplot(aes(x = fct_reorder(Education, BMI, median), 
             y = BMI, 
             colour = Education)) + 
  geom_boxplot() + 
  coord_flip() +
  guides(colour = FALSE) + 
  labs(title = "Examining the effect of education level on BMI",
       x = "Education",
       y = "BMI")

NHANES_tidied %>%
  select(Race1) %>%
  distinct()

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI) & Race1 != "Other") %>%
  ggplot(aes(x = fct_reorder(Race1, BMI, median), 
             y = BMI, 
             colour = Race1)) +
  geom_boxplot() + 
  guides(colour = FALSE) +
  labs(title = "Examining Race and BMI",
       x = "Race", 
       y = "BMI")

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(Work) & !is.na(BMI) & Gender != "Other") %>%
  ggplot(aes(x = fct_reorder(Gender, BMI, median), 
             y = BMI, 
             colour = Gender)) +
  geom_boxplot() + 
  guides(colour = FALSE) +
  labs(title = "Examining Gender and BMI",
       x = "Gender", 
       y = "BMI") %>% facet_wrap(~ Race1:Gender) %>%

install.packages("gifski")
install.packages("png")

library(gifski)
library(png)

NHANES_tidied %>%
  filter(!is.na(BMI) & !is.na(AgeDecade)) %>%
  ggplot(aes(x = BMI)) + 
  geom_histogram(bins = 100) +
  transition_states(AgeDecade, transition_length = 2, state_length = 2) +
  ease_aes("linear") + 
  labs(title = "Age decade: {closest_state}")

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = fct_reorder(Education, BMI, median), y = BMI, colour = Education)) +
  geom_violin() +
  geom_boxplot(alpha = .5) +
  guides(colour = FALSE) + 
  labs(title = "Examining the effect of education level on median BMI for Race = {closest_state}",
       x = "Education Level", 
       y = "BMI") +
  transition_states(Race1, transition_length = 2, state_length = 2) +
  ease_aes("linear") 

anim_save(filename = "my_plot.gif")
