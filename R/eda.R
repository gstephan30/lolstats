library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(forcats)
library(tidytext)
library(tidyr)
theme_set(theme_light())

game_data <- read_rds("data/20220504_games.rds")

game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked")) %>% 
  mutate(datum = as_date(gameStartTimestamp),
         datum = floor_date(datum, "week")) %>% 
  count(summonerName, datum) %>% 
  ggplot(aes(datum, n)) +
  geom_col() +
  facet_wrap(~summonerName, ncol = 1) +
  labs(title = "Count of games of selected summoners",
       x = "Date", 
       y = "Games")

game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked")) %>% 
  count(summonerName, championName, sort = TRUE) %>% 
  filter(n > 2) %>% 
  ggplot(aes(n, reorder_within(championName, n, summonerName))) +
  geom_col() +
  facet_wrap(~summonerName, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Count of used Champions of selected summoners",
       subtitle = "Played at least three times",
       x = "Games",
       y = NULL)

game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked")) %>% 
  count(summonerName, win, championName) %>% 
  pivot_wider(names_from = win, 
              values_from = n, 
              values_fill = 0) %>% 
  janitor::clean_names() %>% 
  mutate(total = false+true, 
         win_perc = true/total) %>% 
  filter(total >= 10) %>% 
  arrange(desc(win_perc)) %>% 
  ggplot(aes(win_perc, fct_reorder(champion_name, win_perc), fill = summoner_name)) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_fill_manual(values = c("#003d5b", "#00798c", "#d1495b", "#edae49")) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = "Win percent per Summoner per Champion",
       x = "Wins in %",
       y = NULL, 
       caption = "played at least 10 times",
       fill = "Summoner")

game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked")) %>% 
  count(summonerName, win, lane) %>% 
  pivot_wider(names_from = win, 
              values_from = n, 
              values_fill = 0) %>% 
  janitor::clean_names() %>% 
  mutate(total = false+true, 
         win_perc = true/total) %>% 
  filter(total > 2) %>% 
  arrange(desc(win_perc)) %>% 
  ggplot(aes(win_perc, reorder_within(summoner_name, win_perc, lane), 
             fill = summoner_name,
             label = paste0(total, " games"))) +
  geom_col() +
  geom_text(hjust = -0.1) +
  facet_wrap(~lane, scales = "free_y") +
  scale_y_reordered() +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  scale_fill_manual(values = c("#003d5b", "#00798c", "#d1495b", "#edae49"), guide = "none") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Win percent per Summoner per Lane",
       x = "Wins in %",
       y = NULL, 
       caption = "played at least 10 times",
       fill = "Summoner") 



box_data <- game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked")) %>% 
  mutate(
    deaths_new = ifelse(deaths == 0, 1, deaths),
    kda = ceiling((kills+assists)/(deaths_new))) %>% 
  select(summonerName, kills, assists, deaths, deaths_new, kda) %>% 
  arrange(desc(kda)) %>% 
  group_by(summonerName) %>% 
  mutate(kda_mean = mean(kda), 
         n = n(),
         upper = quantile(kda, 0.95),
         lower = quantile(kda, 0.05),
         outlier = ifelse(upper < kda, 1, 0))

box_data %>% 
  ggplot(aes(kda, fct_reorder(summonerName, kda), fill = summonerName)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(
    data = box_data %>% 
      filter(outlier == 1), height = 0.1, color = "red", alpha = 0.4) +
  scale_fill_manual(values = c("#003d5b", "#00798c", "#d1495b", "#edae49"), guide = "none") +
  labs(title = "KDA Ratio by Summoner", 
       y = NULL, 
       x = "KDA") 
  

week_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
zeit_daten <- game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked")) %>% 
  select(summonerName, gameStartTimestamp) %>% 
  mutate(week_day = wday(gameStartTimestamp, abbr = FALSE, label = TRUE),
         week_day = factor(week_day, week_order, ordered = TRUE),
         stunde = hour(gameStartTimestamp))

zeit_daten %>% 
  count(week_day, summonerName) %>% 
  add_count(summonerName, wt = n, name = "nn") %>% 
  mutate(perc = n/nn) %>% 
  ggplot(aes(week_day, perc)) +
  geom_col() +
  coord_polar() +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~summonerName) +
  my_theme +
  labs(title = "What week day do they play most?", 
       y = NULL, 
       x = NULL)

zeit_daten %>% 
  count(stunde, summonerName) %>% 
  add_count(summonerName, wt = n, name = "nn") %>% 
  mutate(perc = n/nn) %>% 
  ggplot(aes(stunde, perc)) +
  geom_col() +
  coord_polar() +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~summonerName) +
  my_theme +
  labs(title = "What hour do they play most?", 
       y = NULL, 
       x = NULL)


read_tier <- function(url) {
  
  print(paste0("Reading: ", url))
  
  json_raw <- fromJSON(url, simplifyVector = FALSE)
  
  data <- tibble(
    key = names(json_raw),
    json = json_raw
  ) %>% 
    unnest_wider(json)
  
  return(data)
  Sys.sleep(0.2)
}

read_tier_division <- function(tier, division) {
  
  key <- "RGAPI-42a9ea27-87b8-49eb-84c1-c962fdfa89c2"
  tier <- "BRONZE"
  division <- "I"
  
  
  page_urls <- NULL
  for (i in 1:1000) {
    page_urls[i] <- paste0("https://euw1.api.riotgames.com/lol/league/v4/entries/RANKED_SOLO_5x5/", tier, "/", division, "?page=", i, "&api_key=", key)  
  }
  
  df <- map_df(page_urls, read_tier)
  
}

df %>% 
  filter(grepl("jocar", summonerName, ignore.case = TRUE))


df %>% 
  ggplot(aes(leaguePoints)) +
  geom_histogram()


df %>% 
  ggplot(aes(wins)) +
  geom_histogram()

df %>% 
  mutate(games = wins + losses) %>% 
  ggplot(aes(games)) +
  geom_histogram()

df %>% 
  select(summonerName, miniSeries) %>% 
  unnest_wider(miniSeries) %>% 
  filter(!is.na(progress))


paste0("https://euw1.api.riotgames.com/lol/match/v4/matchlists/by-account/9f4e881e-2127-48b0-b04c-8e8c620421ac?queue=420&endIndex=50&beginIndex=0&api_key=", key)



tibble(
  json = fromJSON("https://static.developer.riotgames.com/docs/lol/queues.json", simplifyVector = FALSE)
) %>% unnest_wider(json) %>% 
  print(n = 100)


game_data <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/EUW1_5766860584?api_key=", key) %>% 
  fromJSON(simplifyVector = FALSE)







all_game_data <- map_df(game_ids, function(x) get_game_data(x, key))
