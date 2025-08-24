# ---- deps ----
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)

# Put your key in .Renviron: RIOT_API_KEY=xxxxxxxx
RIOT_KEY <- Sys.getenv("api_key")

stopifnot(nzchar(RIOT_KEY))  # fail early if missing

# Generic GET with header + simple exponential backoff on 429/5xx
riot_get_json <- function(url, max_retries = 5, quiet = FALSE) {
  pause <- 1
  for (attempt in seq_len(max_retries)) {
    resp <- GET(url, add_headers(`X-Riot-Token` = RIOT_KEY))
    sc <- status_code(resp)
    if (sc == 200) {
      return(fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE))
    }
    if (sc %in% c(429, 500, 502, 503, 504)) {
      if (!quiet) message(sprintf("[%d] %s -> retry in %ss", sc, url, pause))
      Sys.sleep(pause); pause <- min(pause * 2, 16)
      next
    }
    if (!quiet) message(sprintf("Non-retryable %d for %s", sc, url))
    return(NULL)
  }
  if (!quiet) message(sprintf("Max retries exceeded: %s", url))
  NULL
}

# ---- read your summoner dataset & get PUUIDs ----
get_puuids <- function(path = "data/current_ranks.rds") {
  read_rds(path) %>%
    distinct(info_puuid) %>%
    filter(!is.na(info_puuid), info_puuid != "") %>%
    pull(info_puuid)
}

# ---- match-v5: by-puuid match IDs (regional host) ----
# region_host "europe.api.riotgames.com" is correct for EUW/EUNE/TR/RU
get_match_ids <- function(puuid, start = 0, count = 100,
                          region_host = "europe.api.riotgames.com",
                          queue = NULL, type = NULL, startTime = NULL, endTime = NULL) {
  stopifnot(count <= 100)
  params <- list(start = start, count = count)
  if (!is.null(queue)) params$queue <- queue
  if (!is.null(type)) params$type <- type
  if (!is.null(startTime)) params$startTime <- as.integer(startTime)
  if (!is.null(endTime)) params$endTime <- as.integer(endTime)
  
  url <- modify_url(sprintf("https://%s/lol/match/v5/matches/by-puuid/%s/ids",
                            region_host, puuid), query = params)
  j <- riot_get_json(url)
  if (is.null(j)) character() else unlist(j, use.names = FALSE)
}

# Page through up to max_games per PUUID
get_match_ids_paged <- function(puuid, max_games = 1500, region_host = "europe.api.riotgames.com",
                                queue = NULL, type = NULL, startTime = NULL, endTime = NULL) {
  out <- character(); start <- 0
  repeat {
    left <- max_games - length(out); if (left <= 0) break
    batch <- get_match_ids(puuid, start = start, count = min(100, left),
                           region_host = region_host, queue = queue, type = type,
                           startTime = startTime, endTime = endTime)
    if (!length(batch)) break
    out <- unique(c(out, batch))
    start <- start + length(batch)
    Sys.sleep(1.05)  # polite pacing
  }
  out
}

# ---- match-v5: get single match payload & flatten like your df ----
get_match_data <- function(match_id, region_host = "europe.api.riotgames.com") {
  url <- sprintf("https://%s/lol/match/v5/matches/%s", region_host, match_id)
  j <- riot_get_json(url)
  if (is.null(j)) return(NULL)
  
  info <- j$info
  if (is.null(info) || is.null(info$participants)) return(NULL)
  
  tibble(key = names(j), json_raw = list(j)) %>%
    filter(key == "info") %>%
    unnest_wider(json_raw) %>%
    unnest_longer(participants) %>%
    unnest_wider(participants) %>%
    mutate(
      gameStartTimestamp = as.POSIXct(gameStartTimestamp/1000, origin = "1970-01-01", tz = "UTC")
    ) %>%
    select(
      gameMode, gameDuration, gameStartTimestamp, queueId,
      championName, kills, deaths, assists, lane, summonerName, summonerLevel, win,
      everything()
    )
}

# ---- top-level orchestration built around your current_ranks.rds ----
# - Reads PUUIDs from data/current_ranks.rds
# - Collects new match IDs vs existing data/all_games.rds
# - Fetches & appends
# --- controls you asked about ---
update_all_games <- function(current_ranks_path = "data/current_ranks.rds",
                             existing_games_path = "data/all_games.rds",
                             max_games_per_puuid = 1500,
                             region_host = "europe.api.riotgames.com",
                             queue_filter = NULL,
                             type_filter  = NULL,
                             startTime = NULL, endTime = NULL,
                             short_sleep_between_calls = 0.0,  # small per-call sleep
                             long_sleep_every = 60,            # batch size before long sleep
                             long_sleep_seconds = 210,         # your previous default (~3.5min)
                             verbose = TRUE,
                             debug_log_path = "logs/match_fetch.log",
                             dry_run = FALSE) {
  
  if (verbose) message("Reading PUUIDs from: ", current_ranks_path)
  puuids <- get_puuids(current_ranks_path)
  if (!length(puuids)) stop("No PUUIDs found in current_ranks.rds (info_puuid).")
  
  if (verbose) message(sprintf("Collecting match IDs for %d accounts…", length(puuids)))
  ids_list <- purrr::map(puuids, ~ get_match_ids_paged(.x,
                                                       max_games = max_games_per_puuid,
                                                       region_host = region_host,
                                                       queue = queue_filter, type = type_filter,
                                                       startTime = startTime, endTime = endTime))
  match_ids_all <- unique(unlist(ids_list, use.names = FALSE))
  
  existing <- if (file.exists(existing_games_path)) readr::read_rds(existing_games_path) else tibble::tibble()
  existing_ids <- existing %>%
    dplyr::mutate(game_id = dplyr::if_else("platformId" %in% names(.),
                                           paste0(.data$platformId, "_", .data$gameId),
                                           NA_character_)) %>%
    dplyr::transmute(game_id = dplyr::coalesce(.data$game_id,
                                               dplyr::if_else("metadata_matchId" %in% names(.),
                                                              .data$metadata_matchId, NA_character_))) %>%
    dplyr::pull() %>% unique() %>% stats::na.omit()
  
  new_ids <- setdiff(match_ids_all, existing_ids)
  if (verbose) message(sprintf("New matches to fetch: %d", length(new_ids)))
  if (!length(new_ids)) {
    if (verbose) message("Nothing to do.")
    return(invisible(existing))
  }
  
  # ensure log dir
  if (!dir.exists(dirname(debug_log_path))) dir.create(dirname(debug_log_path), recursive = TRUE)
  
  fetched <- vector("list", length(new_ids))
  failures <- tibble::tibble(match_id = character(), status = integer(), note = character())
  stops <- if (long_sleep_every > 0) seq(long_sleep_every, length(new_ids), by = long_sleep_every) else integer()
  
  for (i in seq_along(new_ids)) {
    mid <- new_ids[[i]]
    if (verbose) message(sprintf("[%d/%d] %s", i, length(new_ids), mid))
    
    # long sleep after each batch boundary (like your old code)
    if (i %in% stops) {
      if (verbose) message(sprintf("Long sleep: %ds (batch boundary)", long_sleep_seconds))
      Sys.sleep(long_sleep_seconds)
    }
    
    # fetch with tryCatch so a single bad id doesn't kill the run
    tmp <- tryCatch({
      get_match_data(mid, region_host = region_host)
    }, error = function(e) {
      failures <<- dplyr::bind_rows(failures, tibble::tibble(match_id = mid, status = NA_integer_, note = paste("R error:", e$message)))
      NULL
    })
    
    if (is.null(tmp) || !nrow(tmp)) {
      # quick probe: GET the endpoint once to capture status
      probe_url <- sprintf("https://%s/lol/match/v5/matches/%s", region_host, mid)
      resp <- try(httr::GET(probe_url, httr::add_headers(`X-Riot-Token` = RIOT_KEY)), silent = TRUE)
      st <- if (inherits(resp, "try-error")) NA_integer_ else httr::status_code(resp)
      failures <- dplyr::bind_rows(failures, tibble::tibble(match_id = mid, status = st, note = "empty/null payload"))
    } else {
      fetched[[i]] <- tmp
    }
    
    if (short_sleep_between_calls > 0) Sys.sleep(short_sleep_between_calls)
  }
  
  new_rows <- dplyr::bind_rows(fetched)
  if (nrow(new_rows) == 0) {
    if (verbose) message("Fetched 0 rows; not writing any files.")
    if (nrow(failures)) readr::write_csv(failures, debug_log_path)
    return(invisible(existing))
  }
  
  # Dedup and merge
  out <- dplyr::bind_rows(existing, new_rows) %>% dplyr::distinct()
  
  # --- safe write path you proposed ---
  new_file <- paste0("data/", basename(existing_games_path), "_new.rds")
  if (verbose) message("Writing to temp file: ", new_file)
  if (!dry_run) readr::write_rds(out, new_file)
  
  # --- sanity checks before swap ---
  ok <- TRUE
  if (file.exists(existing_games_path)) {
    # expect >= existing rows unless everything was duped
    if (nrow(out) < nrow(existing)) {
      warning("Sanity check failed: merged row count < existing.")
      ok <- FALSE
    }
  }
  # basic key uniqueness check if columns exist
  if (all(c("platformId","gameId") %in% names(out))) {
    keys <- out %>% dplyr::mutate(key = paste0(.data$platformId, "_", .data$gameId)) %>% dplyr::pull(key)
    if (anyDuplicated(keys)) {
      warning("Sanity check failed: duplicate platformId_gameId keys after merge.")
      ok <- FALSE
    }
  }
  
  if (nrow(failures)) {
    if (verbose) message("Writing failure log: ", debug_log_path)
    readr::write_csv(failures, debug_log_path)
  }
  
  if (dry_run) {
    if (verbose) message("[dry_run] Not swapping files.")
    return(invisible(out))
  }
  
  if (!ok) {
    warning("Not swapping in _new.rds due to failed sanity checks. Inspect files/logs.")
    return(invisible(out))
  }
  
  # atomic-ish swap
  backup <- paste0(existing_games_path, ".bak")
  if (file.exists(existing_games_path)) file.copy(existing_games_path, backup, overwrite = TRUE)
  file.rename(new_file, existing_games_path)
  if (verbose) message("Swap complete: ", existing_games_path)
  
  invisible(out)
}
out <- update_all_games(
  queue_filter = 450,                     # ARAM only
  short_sleep_between_calls = 0.10,       # smooth bursts
  long_sleep_every = 60,                  # like your old 'steps <- 60'
  long_sleep_seconds = 210,               # like your old 'sleep = 210'
  verbose = TRUE,
  debug_log_path = "logs/match_fetch.log"
)