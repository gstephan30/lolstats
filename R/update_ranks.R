update_ranks <- function() {
  library(dplyr)
  library(lubridate)
  library(jsonlite)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  
  key <- Sys.getenv("api_key")
  
  get_summoner_data <- function(name) {
    
    # name <- "Jocar#Jocar"
    split_summoner <- str_split(name, "#")[[1]]
    
    
    
    # json_url <- paste0("https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", key)
    json_url <- paste0("https://europe.api.riotgames.com/riot/account/v1/accounts/by-riot-id/", 
                       split_summoner[1], "/", 
                       split_summoner[2], "?api_key=", key)

    summoner_puuid <- tibble(
      key = names(fromJSON(json_url, simplifyVector = FALSE)),
      json = fromJSON(json_url, simplifyVector = FALSE)
    ) %>% 
      mutate(json = map(json, as.character)) %>% 
      unnest(json) %>% 
      pivot_wider(
        names_from = key,
        values_from = json
      ) 
    
    summoner_json <- paste0("https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-puuid/", 
                            summoner_puuid$puuid, "?api_key=", key)
    
    tibble(
      key = names(fromJSON(summoner_json, simplifyVector = FALSE)),
      json = fromJSON(summoner_json, simplifyVector = FALSE)
    ) |> mutate(json = map(json, as.character)) %>% 
      unnest(json) %>% 
      pivot_wider(
        names_from = key,
        values_from = json
      ) |> 
      type_convert() %>% 
      mutate(revisionDate = as.POSIXct(revisionDate/1000, origin="1970-01-01"))
  }
  
  summoner <- tibble(
    name = c("Jocar#Jocar", "Paintrain100#EUW", "BigFish84#EUW", 
             "locked#EUW", "blua#EUW", "Jesus#BuBaz"),
  )
  
  
  
  get_current_rank <- function(info_id) {
    
    #info_id <- "PUVIXbSAf37rmqeF90dnVLSy_nKUCCWVjv1i4a2h_DciV25Wjr5vgiRVLDd4Wi-rQ_w-7cSsQhje6A"
    #"/lol/league/v4/entries/by-puuid/{encryptedPUUID}"
    json_url <- paste0("https://euw1.api.riotgames.com/lol/league/v4/entries/by-puuid/", info_id, "?api_key=", key)
    # json_url <- paste0("https://euw1.api.riotgames.com/lol/league/v4/entries/by-summoner/", info_id, "?api_key=", key)
    
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
    mutate(info_id = info_puuid) %>%
    mutate(rank = map(info_id, get_current_rank)) %>% 
    unnest_wider(rank) %>% 
    select(time, everything(), summonerName = name) |> 
    separate(summonerName, c("summonerName", "tag"), "#") |> 
    select(-tag)
  
  print("Updating ...")
  read_rds("data/current_ranks.rds") %>% 
    bind_rows(info |>
                select(-puuid)) %>% 
    distinct() %>%
    saveRDS("data/current_ranks.rds")
}
update_ranks()
