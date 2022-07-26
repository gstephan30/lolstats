library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidytext)

champs_json <- "http://ddragon.leagueoflegends.com/cdn/12.8.1/data/en_US/champion.json" %>% 
  fromJSON(simplifyVector = FALSE)

champion_list <- tibble(
  key = names(champs_json),
  json = champs_json
) %>% 
  filter(key == "data") %>% 
  select(-key) %>% 
  unnest(json) %>% 
  unnest_wider(json) %>% 
  unnest_wider(info) %>% 
  unnest_wider(tags, names_sep = "_") %>% 
  unnest_wider(stats) %>% 
  unnest_wider(image) %>% 
  mutate(key = as.numeric(key))

id <- "vdJf0BY2BpP94nJqpLBPsdCflFB_Pv4nF7T2my6NqY5s"


summoners <- c("Jocar", "Paintrain100", "BigFish84", "locked", "Blua")
summoner_info <- map_df(summoners, get_summoner_data)

get_masteries <- function(sum_id) {
  mastery_url <- paste0("https://euw1.api.riotgames.com/lol/champion-mastery/v4/champion-masteries/by-summoner/", sum_id, "?api_key=", key)
  
  fromJSON(mastery_url) %>% 
    tibble() %>% 
    left_join(
      champion_list %>% 
        select(id, championId = key)
    ) 
  
}

mastery_levels <- map_df(summoner_info$id, get_masteries) %>% 
  left_join(
    summoner_info %>% 
      select(summonerId = id, summonerLevel, name)
  )

  

mastery_levels %>%
  select(name, summonerLevel, id, championLevel, championPoints) %>% 
  ggplot(aes(championPoints, id, fill = name)) +
  geom_col() +
  facet_wrap(~name, scales = "free_y")

mastery_levels %>% 
  count(name, wt = championPoints)

mastery_levels %>%
  select(name, summonerLevel, id, championLevel, championPoints) %>% 
  left_join(
    champion_list %>% 
      select(id, tags_1, tags_2)
    ) %>% 
  pivot_longer(
    names_to = "col",
    values_to = "tags",
    cols = c(tags_1, tags_2)
  ) %>% 
  select(-col) %>% 
  mutate(ind = 1) %>% 
  pivot_wider(
    names_from = tags,
    values_from = ind,
    values_fill = 0
  ) %>% 
  select(-summonerLevel, -id, -championLevel) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(fit = map(data, function(x) lm(championPoints ~ ., data = x)),
         tidy = map(fit, broom::tidy, conf.int = TRUE)) %>% 
  select(name, tidy) %>% 
  unnest(tidy) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(estimate, reorder_within(term, estimate, name), xmin = conf.low, xmax = conf.high)) +
  geom_errorbarh() +
  geom_point() +
  scale_y_reordered() +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed")


wins <- mastery_levels %>%
  select(name, summonerLevel, id, championLevel, championPoints) %>% 
  left_join(
    read_rds("data/all_games.rds") %>%
      filter(summonerName %in% summoners) %>% 
      
      select(name = summonerName, id = championName, win)
  )


wins %>% count(win, name)

wins %>% 
  mutate(game_id = 1:n()) %>% 
  left_join(
    champion_list %>% 
      select(id, tags_1, tags_2)
  ) %>% 
  pivot_longer(
    names_to = "col",
    values_to = "tags",
    cols = c(tags_1, tags_2)
  ) %>% 
  filter(!is.na(tags)) %>% 
  select(-col) %>% 
  mutate(ind = 1) %>% 
  pivot_wider(
    names_from = tags,
    values_from = ind,
    values_fill = 0
  ) %>% 
  select(name, win, Mage:Assassin) %>% 
  group_by(name) %>% 
  nest() %>% 
  filter(name != "Blua") %>% 
  mutate(fit = map(data, function(x) glm(win ~ ., data = x, family = binomial)),
         tidy = map(fit, broom::tidy, conf.int = TRUE, exponentiate = TRUE)) %>% 
  select(name, tidy) %>% 
  unnest(tidy) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(estimate, reorder_within(term, estimate, name), xmin = conf.low, xmax = conf.high)) +
  geom_errorbarh() +
  geom_point() +
  scale_y_reordered() +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 1, linetype = "dashed")

