library(tidyverse)
library(tidyjson)
library(dplyr)

transfer_data_raw <- read_json(path = "data/92-21_income_expense_raw.json")
epl_tables_raw <- read_json(path = "data/92-21_epl_tables_raw.json")

parse_number_value <- function(v) {
  is_k <- str_detect(v, "k$")
  v <- parse_number(str_replace_all(str_trim(v), "[^0-9\\.]+", ""))
  if (is_k) {
    return(v * 0.001)
  }
  return(v)
}

normalise_club_name <- function(v) {
  v <- str_trim(v)
  if (v %in% c("Arsenal FC")) {
    return("Arsenal")
  } else if (v %in% c("AFC Bournemouth")) {
    return("Bournemouth")
  } else if(v %in% c("Barnsley FC")) {
    return("Barnsley")
  } else if(v %in% c("Birmingham")) {
    return("Birmingham City")
  } else if(v %in% c("Blackburn")) {
    return("Blackburn Rovers")
  } else if(v %in% c("Blackpool FC")) {
    return("Blackpool")
  } else if(v %in% c("Bolton")) {
    return("Bolton Wanderers")
  } else if(v %in% c("Bradford")) {
    return("Bradford City")
  } else if(v %in% c("Brentford FC")) {
    return("Brentford")
  } else if(v %in% c("Brighton")) {
    return("Brighton & Hove Albion")
  } else if(v %in% c("Burnley FC")) {
    return("Burnley")
  } else if(v %in% c("Cardiff")) {
    return("Cardiff City")
  } else if(v %in% c("Charlton")) {
    return("Charlton Athletic")
  } else if(v %in% c("Chelsea FC")) {
    return("Chelsea")
  } else if(v %in% c("Coventry")) {
    return("Coventry City")
  } else if(v %in% c("Derby")) {
    return("Derby County")
  } else if(v %in% c("Everton FC")) {
    return("Everton")
  } else if(v %in% c("Fulham FC")) {
    return("Fulham")
  } else if(v %in% c("Huddersfield")) {
    return("Huddersfield Town")
  } else if(v %in% c("Ipswich")) {
    return("Ipswich Town")
  } else if(v %in% c("Leeds")) {
    return("Leeds United")
  } else if(v %in% c("Leicester")) {
    return("Leicester City")
  } else if(v %in% c("Liverpool FC")) {
    return("Liverpool")
  } else if(v %in% c("Norwich FC", "Norwich")) {
    return("Norwich City")
  } else if(v %in% c("Man City")) {
    return("Manchester City")
  } else if(v %in% c("Man Utd")) {
    return("Manchester United")
  } else if(v %in% c("Middlesbrough FC")) {
    return("Middlesbrough")
  } else if(v %in% c("Newcastle")) {
    return("Newcastle United")
  } else if(v %in% c("Nottm Forest")) {
    return("Nottingham Forest")
  } else if(v %in% c("Portsmouth FC")) {
    return("Portsmouth")
  } else if(v %in% c("QPR")) {
    return("Queens Park Rangers")
  } else if(v %in% c("Reading FC")) {
    return("Reading")
  } else if(v %in% c("Sheff Utd")) {
    return("Sheffield United")
  } else if(v %in% c("Sheff Wed")) {
    return("Sheffield Wednesday")
  } else if(v %in% c("Southampton FC")) {
    return("Southampton")
  } else if(v %in% c("Sunderland AFC")) {
    return("Sunderland")
  } else if(v %in% c("Spurs")) {
    return("Tottenham Hotspur")
  } else if(v %in% c("Swansea")) {
    return("Swansea City")
  } else if(v %in% c("Watford FC")) {
    return("Watford")
  } else if(v %in% c("West Brom")) {
    return("West Bromwich Albion")
  } else if(v %in% c("West Ham")) {
    return("West Ham United")
  } else if(v %in% c("Wigan")) {
    return("Wigan Athletic")
  } else if(v %in% c("Wimbledon FC")) {
    return("Wimbledon")
  } else if(v %in% c("Wolves")) {
    return("Wolverhampton Wanderers")
  }
  return(v)
}

# reshape the json array into a frame
transfer_data <- transfer_data_raw %>% 
  gather_array %>% 
  spread_values(
    season = jnumber("season"),
    club = jstring("club"),
    arrivals = jstring("arrival"),
    departures = jstring("departures"),
    income = jstring("income"),
    expenditure = jstring("expenditure"),
    balance = jstring("balance")
  ) %>% 
  # clean columns that are meant to be numbers by removing non numeric characters
  # and converting them to number type
  mutate(
    club = mapply(normalise_club_name, club),
    income_m = mapply(parse_number_value, income),
    expenditure_m = mapply(parse_number_value, expenditure),
    balance_m = mapply(parse_number_value, balance),
    arrivals = parse_number(arrivals),
    departures = parse_number(departures),
  ) %>% 
  select(season, club, income_m, expenditure_m, balance_m, arrivals, departures)

transfer_data$income_m[is.na(transfer_data$income_m)] <- 0
transfer_data$expenditure_m[is.na(transfer_data$expenditure_m)] <- 0

epl_tables <- epl_tables_raw %>% 
  gather_array %>% 
  spread_values(
    season = jnumber("season"),
    position = jstring("position"),
    club = jstring("club"),
    played = jstring("played"),
    won = jstring("won"),
    drawn = jstring("drawn"),
    lost = jstring("lost"),
    goals = jstring("goals"),
    goal_diff = jstring("goals_diff"),
    points = jstring("points")
  ) %>% 
  separate(goals, c("goals_for", "goals_against"), ":") %>% 
  mutate(
    position = parse_number(position),
    club = mapply(normalise_club_name, club),
    played = parse_number(played),
    won = parse_number(won),
    drawn = parse_number(drawn),
    lost = parse_number(lost),
    goals_for = parse_number(goals_for),
    goals_against = parse_number(goals_against),
    goal_diff = parse_number(goal_diff),
    points = parse_number(points)
  ) %>% 
  select(season, position, club, played, won, drawn, lost, goals_for, goals_against, goal_diff, points)

# this piece of code will recreate the table for each season
# sorted_tables <- epl_tables %>% 
#   group_by(season) %>% 
#   arrange(position, .by_group = TRUE)
# 
# sorted_tables %>% filter(season == 1992)

# list all winners
#sorted_tables %>% filter(position == 1)
#ds1_clubs <- epl_tables %>% distinct(club) %>% arrange(club)
#ds2_clubs <- transfer_data %>% distinct(club) %>% arrange(club)

out <- transfer_data %>% 
  left_join(epl_tables, by = c("season" = "season", "club" = "club")) %>% 
  select(
    season, position, club, played, won, drawn, lost, goals_for, goals_against, goal_diff, points,
    income_m, expenditure_m, balance_m, arrivals, departures)

write_csv(out, file = "92-21-income_expenditure_table_positions.csv")
