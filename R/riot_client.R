# Riot API client: key loading, rate-limit-aware GET, resumable raw match cache.
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

PLATFORM_HOST <- "euw1.api.riotgames.com"
REGION_HOST   <- "europe.api.riotgames.com"

# Key: RIOT_API_KEY / api_key env var, else api_key= line from .env (never printed).
riot_key <- function(env_file = ".env") {
  key <- Sys.getenv("RIOT_API_KEY", Sys.getenv("api_key"))
  if (!nzchar(key) && file.exists(env_file)) {
    line <- grep("^\\s*(RIOT_API_KEY|api_key)\\s*=", readLines(env_file, warn = FALSE), value = TRUE)
    if (length(line)) key <- sub("^[^=]*=", "", line[1])
  }
  key <- gsub("^[\"'[:space:]]+|[\"'[:space:]]+$", "", key)
  if (!nzchar(key)) stop("No Riot API key found (set api_key in .env or RIOT_API_KEY).")
  key
}

# ---- rate limiting ---------------------------------------------------------
# The key is shared with everything else using it (account/rank lookups, other
# scripts, the dev portal), so we trust the server's own count, not just ours.
.rl <- new.env()
.rl$limits <- c(`1` = 20, `120` = 100)   # dev-key defaults, updated from headers
.rl$sent   <- numeric()                  # our request timestamps
.rl$margin <- 0.85                       # stay below 85% of every window

parse_pairs <- function(x) {
  # "20:1,100:120" -> c(`1` = 20, `120` = 100)
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  p <- strsplit(strsplit(x, ",")[[1]], ":")
  stats::setNames(as.numeric(vapply(p, `[`, "", 1)), vapply(p, `[`, "", 2))
}

rl_wait <- function() {
  repeat {
    now <- as.numeric(Sys.time())
    .rl$sent <- .rl$sent[.rl$sent > now - max(as.numeric(names(.rl$limits)))]
    waits <- vapply(names(.rl$limits), function(w) {
      allowed <- max(1, floor(.rl$limits[[w]] * .rl$margin))
      recent <- .rl$sent[.rl$sent > now - as.numeric(w)]
      if (length(recent) < allowed) 0 else min(recent) + as.numeric(w) - now
    }, numeric(1))
    if (max(waits) <= 0) break
    Sys.sleep(max(waits) + 0.05)
  }
  .rl$sent <- c(.rl$sent, as.numeric(Sys.time()))
}

rl_observe <- function(resp) {
  h <- headers(resp)
  lim <- parse_pairs(h[["x-app-rate-limit"]])
  if (!is.null(lim)) .rl$limits <- lim
  cnt <- parse_pairs(h[["x-app-rate-limit-count"]])
  if (is.null(cnt)) return(invisible())
  # If the server saw more requests than we sent (shared key), back off to the
  # window's remaining time instead of guessing.
  now <- as.numeric(Sys.time())
  for (w in intersect(names(cnt), names(.rl$limits))) {
    own <- sum(.rl$sent > now - as.numeric(w))
    foreign <- cnt[[w]] - own
    if (foreign > 0) .rl$sent <- c(.rl$sent, rep(now, foreign))
  }
}

# ---- HTTP ------------------------------------------------------------------
# Returns parsed JSON (list), or NULL for 404 / non-retryable client errors.
# Stops on 401/403 (expired/invalid key) so a long run fails loudly, not silently.
riot_get <- function(host, path, query = list(), max_retries = 8, quiet = FALSE,
                     raw_text = FALSE) {
  url <- modify_url(sprintf("https://%s%s", host, path), query = query)
  for (attempt in seq_len(max_retries)) {
    rl_wait()
    resp <- tryCatch(
      GET(url, add_headers(`X-Riot-Token` = riot_key()), timeout(30)),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      if (!quiet) message("  network error (", conditionMessage(resp), "), retry ", attempt)
      Sys.sleep(min(2^attempt, 60))
      next
    }
    rl_observe(resp)
    sc <- status_code(resp)
    if (sc == 200) {
      txt <- content(resp, as = "text", encoding = "UTF-8")
      return(if (raw_text) txt else fromJSON(txt, simplifyVector = FALSE))
    }
    if (sc == 429) {
      ra <- suppressWarnings(as.numeric(headers(resp)[["retry-after"]]))
      ra <- if (is.na(ra)) 10 else ra
      if (!quiet) message(sprintf("  429 rate limited, waiting %ss", ra + 1))
      Sys.sleep(ra + 1)
      next
    }
    if (sc %in% c(401, 403)) {
      stop(sprintf("HTTP %d from Riot: API key invalid/expired (dev keys last 24h). Regenerate it in .env.", sc),
           call. = FALSE)
    }
    if (sc >= 500) {
      if (!quiet) message(sprintf("  HTTP %d, retry %d", sc, attempt))
      Sys.sleep(min(2^attempt, 60))
      next
    }
    if (!quiet) message(sprintf("  HTTP %d for %s", sc, path))
    return(NULL)
  }
  if (!quiet) message("  giving up after retries: ", path)
  NULL
}

# ---- endpoints -------------------------------------------------------------
riot_account <- function(game_name, tag_line) {
  riot_get(REGION_HOST, sprintf("/riot/account/v1/accounts/by-riot-id/%s/%s",
                                URLencode(game_name, reserved = TRUE),
                                URLencode(tag_line, reserved = TRUE)))
}

riot_league_entries <- function(puuid) {
  riot_get(PLATFORM_HOST, sprintf("/lol/league/v4/entries/by-puuid/%s", puuid))
}

riot_match_ids_page <- function(puuid, start = 0, count = 100, queue = NULL, type = NULL,
                                startTime = NULL, endTime = NULL) {
  q <- list(start = start, count = count, queue = queue, type = type,
            startTime = startTime, endTime = endTime)
  j <- riot_get(REGION_HOST, sprintf("/lol/match/v5/matches/by-puuid/%s/ids", puuid),
                query = Filter(Negate(is.null), q))
  if (is.null(j)) NULL else as.character(unlist(j))
}

# All match ids Riot still serves for a puuid (pages until a short/empty page).
riot_match_ids_all <- function(puuid, page = 100, ...) {
  out <- character(); start <- 0
  repeat {
    b <- riot_match_ids_page(puuid, start = start, count = page, ...)
    if (is.null(b)) stop("match id listing failed at start=", start, call. = FALSE)
    out <- c(out, b)
    if (length(b) < page) break
    start <- start + page
  }
  unique(out)
}

# ---- raw match cache (one JSON file per match => crash/expired-key safe) -----
match_cache_path <- function(match_id, dir = "data/raw/matches") {
  file.path(dir, paste0(match_id, ".json"))
}

# Returns "cached", "fetched" or "missing" (404/unavailable). Atomic write.
riot_fetch_match <- function(match_id, dir = "data/raw/matches") {
  f <- match_cache_path(match_id, dir)
  if (file.exists(f) && file.size(f) > 0) return("cached")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  txt <- riot_get(REGION_HOST, sprintf("/lol/match/v5/matches/%s", match_id), raw_text = TRUE)
  if (is.null(txt)) return("missing")
  tmp <- paste0(f, ".part")
  writeLines(txt, tmp, useBytes = TRUE)
  file.rename(tmp, f)
  "fetched"
}
