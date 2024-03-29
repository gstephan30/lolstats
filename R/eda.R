library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(forcats)
library(tidytext)
library(tidyr)
theme_set(theme_light())

game_data <- read_rds("data/all_games.rds")

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

library(jsonlite)
library(dplyr)
library(tidyr)
queue_json <- "https://static.developer.riotgames.com/docs/lol/queues.json" %>% 
  jsonlite::fromJSON(simplifyVector = FALSE) %>% 
  tibble(key = names(.),
         value = .) %>% 
  unnest_wider(value)

game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")) %>% 
  select(summonerName, gameStartTimestamp, win, queueId, championName) %>% 
  left_join(queue_json) %>% 
  count(summonerName, description, sort = TRUE)
  
game_data %>% 
  count(summonerName, sort = TRUE)

library(ggplot2)
game_data %>% 
  select(gameId, summonerName) %>% 
  widyr::pairwise_count(summonerName, gameId, sort = TRUE, upper = FALSE) %>% 
  filter(n > 1) %>% 
  igraph::graph_from_data_frame() %>% 
  ggraph::ggraph(layout = "fr") +
  ggraph::geom_edge_link() +
  ggraph::geom_node_point() +
  ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE)


game_data %>% 
  filter(summonerName == "MissFortune")

cum_data <- game_data %>% 
  filter(summonerName == "Jocar") %>% 
  filter(queueId %in% c(420, 430)) %>% 
  arrange(desc(gameStartTimestamp)) %>%
  select(win, queueId) %>% 
  group_by(queueId) %>% 
  mutate(test = cumsum(win),
         id = 1:n(),
         perc = test/id) 

cum_data %>% 
  ggplot(aes(id, test)) +
  geom_col() +
  facet_wrap(~queueId, ncol = 1, scales = "free_x")

cum_data %>% 
  left_join(
    queue_json
  ) %>% 
  ggplot(aes(id, perc)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  facet_wrap(~description, ncol = 1, scales = "free_x") +
  theme_light()

game_data %>%
  select_if(is.list) %>% 
  sample_n(10) %>% 
  unnest_wider(challenges) %>% 
  glimpse()

game_data %>%
  select_if(is.list) %>% 
  select(perks) %>% 
  sample_n(10) %>% 
  mutate(game_id = 1:n()) %>% 
  unnest_wider(perks) %>% 
  unnest_wider(statPerks) %>% 
  unnest(styles) %>% 
  unnest_wider(styles) %>% 
  unnest(selections) %>% 
  unnest_wider(selections)


game_data %>%
  sample_n(10) %>% 
  select(championId, teams) %>% 
  unnest(teams) %>% 
  unnest_wider(teams) %>% 
  unnest(bans) %>% 
  unnest_wider(bans, names_sep = "_") %>% 
  unnest_wider(objectives) %>% 
  unnest_wider(baron, names_sep = "_") %>% 
  unnest_wider(champion, names_sep = "_")

game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")) %>%
  count(summonerName, role, sort = TRUE) %>% 
  ggplot(aes(n, role)) +
  geom_col() +
  facet_wrap(~summonerName, nrow = 1, scales = "free_x")

game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")) %>%
  filter(gameMode == "CLASSIC") %>% filter(role == "NONE") %>% select(championName, summonerName, lane, role)
  filter(summonerName == "Jocar") %>% 
  select(summonerName, teamPosition, role, individualPosition, lane, championName) %>% 
  count(teamPosition, role, individualPosition, lane, championName, sort = TRUE) %>% filter(championName == "Warwick")


g_days <- game_data %>% 
  select(summonerName, gameStartTimestamp, contains("level")) %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")) %>% 
  mutate(datum = lubridate::as_date(gameStartTimestamp)) %>% 
  select(summonerName, datum, summonerLevel) %>% 
  arrange(summonerName, datum, summonerLevel) %>% 
  distinct() %>% 
  
  group_by(summonerName, datum) %>% 
  filter(summonerLevel == min(summonerLevel)) %>% 
  group_by(summonerName) %>% 
  mutate(tag = 1:n()) %>% 
  ggplot(aes(tag, summonerLevel, group = summonerName, color = summonerName)) +
  geom_line() +
  theme_light() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003d5b", "#00798c", "#d1495b", "#edae49", "#f4a261")) +
  labs(title = "Level per Days played",
       x = "Days played",
       y = "Summoner Level", 
       color = "Summoner")
  

g_games <- game_data %>% 
  select(summonerName, gameStartTimestamp, gameId, summonerLevel) %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")) %>% 
  arrange(summonerName, gameStartTimestamp, summonerLevel) %>% 
  group_by(summonerName) %>% 
  mutate(game = 1:n()) %>% 
  ungroup() %>% 
  select(summonerName, game, summonerLevel) %>% 
  arrange(summonerName, game, summonerLevel) %>% 
  distinct() %>% 
  
  ggplot(aes(game, summonerLevel, group = summonerName, color = summonerName)) +
  geom_line() +
  theme_light() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003d5b", "#00798c", "#d1495b", "#edae49", "#f4a261")) +
  labs(title = "Level per Games played",
       x = "Games played",
       y = "Summoner Level", 
       color = "Summoner")

library(patchwork)
g_days + g_games + 
  plot_layout(guides = "collect") &
  theme(legend.position='bottom')



game_data %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")) %>% 
  select(gameId, summonerName, role, gameDuration, contains("total"), contains("turrent")) %>% 
  pivot_longer(cols = -c("gameId", "summonerName", "role", "gameDuration"), 
               names_to = "key", 
               values_to = "value") %>% 
  group_by(summonerName, key) %>% 
  summarise(total = sum(value), 
            games = n(),
            value_per_game = total/games) %>% 
  ggplot(aes(value_per_game, summonerName)) +
  geom_col() +
  facet_wrap(~key, scales = "free_x")

game_data %>% 
  select(gameMode, summonerName, pentaKills, quadraKills, championName, gameStartTimestamp) %>% 
  filter(summonerName %in% c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")) %>% 
  filter(pentaKills > 0 | quadraKills > 0)


read_rds("data/current_ranks.rds")
read_rds("data/all_games.rds") %>% select(gameStartTimestamp, gameVersion) %>% 
  filter(gameStartTimestamp == max(gameStartTimestamp)) %>% 
  distinct(gameVersion) %>% 
  pull()


"https://ddragon.leagueoflegends.com/cdn/12.14.1/data/en_US/profileicon.json" %>% 
  jsonlite::fromJSON(simplifyVector = FALSE) %>% 
  tibble(key = names(.),
         values = .) %>% 
  filter(key == "data") %>% 
  unnest(values) %>% 
  unnest_wider(values) %>% 
  unnest_wider(id)


"https://cdn.merakianalytics.com/riot/lol/resources/latest/en-US/championrates.json" %>% 
  fromJSON(simplifyVector = FALSE) %>% 
  tibble(key = names(.), 
         data = .) %>% 
  filter(key == "data") %>% 
  unnest(data) %>% 
  unnest_wider(data) %>% 
  unnest_wider(TOP, names_sep = "_") %>% 
  unnest_wider(JUNGLE, names_sep = "_") %>% 
  unnest_wider(MIDDLE, names_sep = "_") %>% 
  unnest_wider(BOTTOM, names_sep = "_") %>% 
  unnest_wider(UTILITY, names_sep = "_") 


"https://cdn.merakianalytics.com/riot/lol/resources/latest/en-US/championrates.json" %>% 
  fromJSON(simplifyVector = FALSE) %>% 
  tibble(key = names(.), 
         data = .) %>% 
  filter(key == "patch") %>% 
  unnest(data)


