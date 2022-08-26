update_ranks <- function() {
  library(dplyr)
  library(lubridate)
  library(jsonlite)
  library(tidyr)
  library(purrr)
  library(readr)
  
  key <- Sys.getenv("api_key")
  
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
      type_convert() %>% 
      mutate(revisionDate = as.POSIXct(revisionDate/1000, origin="1970-01-01"))
  }
  
  summoner <- tibble(
    name = c("Jocar", "Paintrain100", "BigFish84", "locked", "blua"),
  )
  
  
  
  get_current_rank <- function(info_id) {
    json_url <- paste0("https://euw1.api.riotgames.com/lol/league/v4/entries/by-summoner/", info_id, "?api_key=", key)
    
    tibble(
      key = names(fromJSON(json_url, simplifyVector = FALSE)),
      json = fromJSON(json_url, simplifyVector = FALSE)
    ) %>% 
      unnest_wider(json) 
  }
  
  info <- summoner %>% 
    mutate(time = now()) %>% 
    mutate(info = map(name, get_summoner_data)) %>% 
    unnest_wider(info, names_sep = "_") %>% 
    mutate(rank = map(info_id, get_current_rank)) %>% 
    unnest_wider(rank) %>% 
    select(time, everything(), -name, -info_name)
  
  print("Updating ...")
  read_rds("data/current_ranks.rds") %>% 
    bind_rows(info) %>% 
    distinct() %>% 
    saveRDS("data/current_ranks.rds")
}
update_ranks()
