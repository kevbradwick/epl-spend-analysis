library(tidyverse)

data <- read.csv(file = "92-21-income_expenditure_table_positions.csv")

# All the title winners
winners <- data %>% 
  group_by(season) %>% 
  arrange(position, .by_group = TRUE) %>% 
  filter(position == 1)

# all the title winners and a count of how many times they have won it
winners_summary <- winners %>% 
  group_by(club) %>% 
  tally(sort = T) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(count = n) %>% 
  select(club, count)

# plot bar graph
ggplot(winners_summary, aes(x = reorder(club, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), colour = "white", hjust = 2) +
  theme_light() +
  coord_flip() +
  labs(x = "Club", y = "Times won")

# what are the total spend of each club
total_spend <- winners %>% 
  group_by(club) %>% 
  summarise(
    total_wins = n(),
    total_spend = sum(expenditure_m),
    total_spend_fmt = formatC(total_spend, format = "f", big.mark = ",", digits = 1),
    average_expenditure = mean(expenditure_m),
    total_income = sum(income_m),
    average_income = mean(income_m),
    net_spend = total_income - total_spend,
    average_net_spend = average_income - average_expenditure,
    total_arrivals = sum(arrivals)
  ) %>% 
  arrange(desc(total_wins))

# scatter plot x = expenditure, y = position
ggplot(data, aes(expenditure_m, position)) +
  geom_point(colour = "steelblue") +
  theme_light() +
  labs(x = "Expenditure (M) EUR", y = "Position")

# Spending over time
spend_over_time <- data %>% 
  group_by(season) %>% 
  summarise(total_expenditure = sum(expenditure_m))

ggplot(spend_over_time, aes(x = season, y = total_expenditure)) +
  geom_line(colour = "steelblue") +
  geom_point(shape=21, color="steelblue", fill="white", size=2) +
  theme_light() +
  labs(x = "Season", y = "Expenditure (M) EUR")