# Flatten data/raw/matches/*.json -> data/all_games.rds (one row per participant).
#   Rscript R/build_all_games.R
suppressPackageStartupMessages({library(jsonlite); library(dplyr); library(purrr); library(tibble); library(readr)})

files <- list.files("data/raw/matches", pattern = "\\.json$", full.names = TRUE)
tracked <- read_rds("data/puuids.rds") %>% transmute(puuid, tracked_name = gameName)

scalar_fields <- function(x) {
  x <- x[vapply(x, function(v) is.atomic(v) && length(v) == 1, logical(1))]
  as_tibble(x)
}

flatten_match <- function(f) {
  j <- fromJSON(f, simplifyVector = FALSE)
  info <- j$info
  # Riot sometimes returns empty shells (0 participants, duration 0): skip them.
  if (length(info$participants) == 0 || isTRUE(info$gameDuration == 0)) return(NULL)
  game <- scalar_fields(info)
  game$matchId <- j$metadata$matchId
  map_dfr(info$participants, function(p) {
    row <- scalar_fields(p[setdiff(names(p), c("challenges", "perks", "missions"))])
    ch <- p$challenges
    if (length(ch)) {
      ch <- scalar_fields(ch)
      names(ch) <- paste0("challenges_", names(ch))
      row <- bind_cols(row, ch)
    }
    bind_cols(game, row)
  })
}

all_games <- map(files, flatten_match) %>%
  bind_rows() %>%
  left_join(tracked, by = "puuid") %>%
  mutate(
    # summonerName is blank in post-2023 matches: use the tracked name, else the Riot ID
    summonerName = coalesce(tracked_name, na_if(trimws(riotIdGameName), ""), na_if(trimws(summonerName), "")),
    gameStartTimestamp = as.POSIXct(gameStartTimestamp / 1000, origin = "1970-01-01", tz = "UTC"),
    gameCreation       = as.POSIXct(gameCreation / 1000, origin = "1970-01-01", tz = "UTC"),
    gameEndTimestamp   = as.POSIXct(gameEndTimestamp / 1000, origin = "1970-01-01", tz = "UTC")
  ) %>%
  select(gameMode, gameDuration, gameStartTimestamp, queueId, championName, kills, deaths,
         assists, lane, summonerName, summonerLevel, win, tracked_name, everything()) %>%
  distinct(matchId, puuid, .keep_all = TRUE)

stopifnot(nrow(all_games) > 0, !anyDuplicated(all_games[c("matchId", "puuid")]))
write_rds(all_games, "data/all_games.rds", compress = "gz")
message(sprintf("%d rows, %d matches, %d tracked rows, %s -> %s", nrow(all_games),
                n_distinct(all_games$matchId), sum(!is.na(all_games$tracked_name)),
                format(min(all_games$gameStartTimestamp)), format(max(all_games$gameStartTimestamp))))
