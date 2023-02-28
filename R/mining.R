library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)
library(purrr)
library(readr)

key <- Sys.getenv("api_key")
source("R/update_ranks.R")
get_summoner_data <- function(name) {
  
  json_url <- paste0("https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", key)
  
  tibble(
    key = names(fromJSON(json_url, simplifyVector = FALSE)),
    json = fromJSON(json_url, simplifyVector = FALSE)
  ) %>% 
    mutate(json = map(json, as.character)) %>% 
    unnest(json) %>% 
    pivot_wider(
      names_from = key,
      values_from = json
    ) %>% 
    type_convert(col_types = cols()) %>% 
    mutate(revisionDate = as.POSIXct(revisionDate/1000, origin="1970-01-01"))
}
# get_summoner_data("Jocar")

get_game_ids <- function(name, games_check = 1000) {
  
  puuid <- get_summoner_data(name) %>% 
    pull(puuid)
  
  all_games <- NULL
  for (i in 1:10) {
    print((i-1)*100)
    all_games[[i]] <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/", puuid, "/ids?start=", (i-1)*100, "&count=100&api_key=", key) %>% 
      fromJSON() %>% 
      as_tibble()
  }  
  game_ids <- bind_rows(all_games) %>% pull(value)
  
  return(game_ids)
}

game_ids <- tibble(
  name = c("Jocar", "Paintrain100", "BigFish84", "locked", "blua")
) %>% 
  mutate(game_ids = map(name, get_game_ids))

game_file <- readr::read_rds("data/all_games.rds") %>% 
  mutate(game_id = paste0(platformId, "_", gameId))

new_games <- game_ids %>% 
  ungroup() %>% 
  unnest(game_ids) %>% 
  distinct(game_ids) %>% 
  select(game_id = 1) %>% 
  # anti_join(
  #   game_file %>% 
  #     distinct(game_id)
  # ) %>% 
  pull(game_id)

get_game_data <- function(game_id) {
  
  # game_id <- "EUW1_6133505836"
  
  print(paste0("Fetching data from: ", game_id))
  
  game_data <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/", game_id, "?api_key=", key) %>% 
    fromJSON(simplifyVector = FALSE)
  
  data_check <- tibble(
    key = names(game_data),
    json_raw = game_data
  ) %>% 
    filter(key == "info") %>% 
    unnest_wider(json_raw) %>% 
    unnest(participants) %>% 
    unnest_wider(participants) %>%
    mutate(gameStartTimestamp = as.POSIXct(gameStartTimestamp/1000, origin="1970-01-01")) 
  
  if(any(data_check$gameId) != 0) {
    data <- data_check |> 
      select(gameMode, gameDuration, gameStartTimestamp, queueId, championName, kills, deaths, assists, lane, summonerName, summonerLevel, win, everything())
  } else {
    data <- NULL
  }
  
  return(data)
  
}

get_games <- function(game_ids, sleep = 120) {
  
  game_ids <- unique(game_ids)
  
  steps <- 100
  print(paste0("Process will take: ", ((seq(1, length(game_ids), steps) %>% length() - 1) * 135 / 60), " minutes."))
  all_game_data <- NULL
  
  print(paste0("Starting in ", sleep, " sec ..."))
  Sys.sleep(sleep)
  
  stops <- seq(1, length(game_ids), steps)[-1]
  if (length(stops) == 0) {
    stops <- 0
  }
  
  for (i in seq_along(game_ids)) {
    
    if (!i %in% stops) {
      all_game_data[[i]] <- get_game_data(game_ids[i])
    } else {
      print("Schlafe 2min")
      Sys.sleep(135)
      all_game_data[[i]] <- get_game_data(game_ids[i])
    }
    
  }

  return(bind_rows(all_game_data))
}
all_game_data <- get_games(new_games)

file_name <- paste0("data/all_games.rds")
all_game_data %>% 
  bind_rows(game_file) %>% 
  distinct() %>% 
  write_rds(., file = file_name)
