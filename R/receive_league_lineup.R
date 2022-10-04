library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(httr)

queues <- c("RANKED_SOLO_5x5", "RANKED_FLEX_SR", "RANKED_FLEX_TT")
key <- Sys.getenv("api_key")

get_summoner_data <- function(name) {
  
  json_url <- paste0("https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", key)
  
  request <- http_status(GET(json_url))$category
  
  if (request == "Success") {
    data <- tibble(
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
      mutate(revisionDate = as.POSIXct(revisionDate/1000, origin="1970-01-01"))  %>% 
      mutate_all(as.character)
  } else {
    data <- tibble(
      id = NA, 
      accountId = NA, 
      puuid = NA,
      name = str_replace_all(name, "%20", " "),
      profileIconId = NA,
      revisionDate = NA,
      summonerLevel = NA
    ) %>% 
      mutate_all(as.character)
  }
  
  
}

receive_summoner_data <- function(lineup) {
  # lineup  <- lineup[1:40, ]
  
  stops <- seq(1, nrow(lineup), 20)
  summoner_data <- NULL
  
  for (i in 1:nrow(lineup)) {
    summoner <- pull(lineup[i, 2])
    summoner_clean <- pull(lineup[i, 11])
    
    print(paste0("Getting data from: ", summoner, " (", i,"/", nrow(lineup), ")"))
    
    if (i != stops || i == 1) {
      summoner_data[[i]] <- get_summoner_data(summoner_clean) %>% 
        mutate_all(as.character)
    }
    if (i %in% stops & i != 1) {
      print("Waiting 2min")
      Sys.sleep(120)
      summoner_data[[i]] <- get_summoner_data(summoner_clean) %>% 
        mutate_all(as.character)
    }
    
    # summoner_data <- bind_rows(summoner_data)
  }
  return(bind_rows(summoner_data))
}


update_league <- function(queue) {
  
  queue <- "RANKED_SOLO_5x5"
  
  print(paste0("Reading: ", queue))
  queue_json <- paste0("https://euw1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/", queue, "?api_key=", key)
  
  print("Reading JSON")
  data_raw <- queue_json %>% 
    fromJSON(simplifyVector = FALSE) %>% 
    tibble(key = names(.),
           values = .) 
  
  print("Getting League Info")
  league_info <- data_raw %>% 
    rowwise() %>% 
    mutate(value = ifelse(!is.list(values), paste(values), NA)) %>% 
    select(key,  value) %>% 
    filter(!is.na(value))
  
  print("Getting Lineup")
  lineup <- data_raw %>% 
    rowwise() %>% 
    mutate(value = ifelse(!is.list(values), paste(values), NA)) %>% 
    filter(is.na(value)) %>% 
    select(values) %>% 
    unnest(values) %>% 
    unnest_wider(values) %>% 
    mutate(cleaned_id = str_replace_all(summonerName, " ", "%20"))
  
  print("Preparing Summoner Data")
  Sys.sleep(180)
  summoner_data <- receive_summoner_data(lineup)
  
  file_name <- paste0("data/", pull(league_info, value)[1], "_", snakecase::to_snake_case(pull(league_info, value)[4]), "_summoner.rds")
  
  print(paste0("Saving data in: ", file_name))
  read_rds(file_name) %>% 
    mutate_all(as.character) %>% 
    bind_rows(
      summoner_data %>% 
        mutate_all(as.character) %>% 
        left_join(lineup, by = c("id" = "summonerId")) %>% 
        select(-name) 
      ) %>% 
    distinct() %>% 
    write_rds(file_name)
  
}

update_league(queues[1])

summoner_names <- list.files("data", "summoner", full.names = TRUE) %>% 
  read_rds() %>% mutate(games = wins + losses) %>% 
  select(cleaned_id, games) 

summoner_names [1:7] %>% 
  map(get_game_ids)

