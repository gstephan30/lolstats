# Step 1: resolve PUUIDs (account-v1) + snapshot ranks (league-v4)
# Step 2: list every match id Riot still serves and download each raw match once.
# Resumable: re-run any time (e.g. after regenerating the 24h dev key).
#   Rscript R/backfill_matches.R              # everything
#   Rscript R/backfill_matches.R 50           # fetch at most 50 new matches (smoke test)
source("R/riot_client.R")
suppressPackageStartupMessages({library(dplyr); library(readr); library(tibble)})

args <- commandArgs(trailingOnly = TRUE)
max_new <- if (length(args)) as.integer(args[1]) else Inf

accounts <- tribble(
  ~gameName,      ~tagLine,
  "Jocar",        "Jocar",
  "Paintrain100", "EUW",
  "BigFish84",    "EUW",
  "locked",       "EUW",
  "blua",         "EUW",
  "Jesus",        "BuBaz",
  "Berrett21",    "EUW"
)

dir.create("data", showWarnings = FALSE)
dir.create("logs", showWarnings = FALSE)

# PUUIDs are encrypted per API key/app, so resolve them with the current key
# instead of reusing info_puuid values from older data.
puuid_file <- "data/puuids.rds"
puuids <- if (file.exists(puuid_file)) read_rds(puuid_file) else tibble()
for (i in seq_len(nrow(accounts))) {
  a <- accounts[i, ]
  if (nrow(puuids) && any(tolower(puuids$gameName) == tolower(a$gameName) &
                          tolower(puuids$tagLine) == tolower(a$tagLine))) next
  message("Resolving ", a$gameName, "#", a$tagLine)
  j <- riot_account(a$gameName, a$tagLine)
  if (is.null(j)) { warning("Could not resolve ", a$gameName, "#", a$tagLine); next }
  puuids <- bind_rows(puuids, tibble(gameName = a$gameName, tagLine = a$tagLine, puuid = j$puuid))
}
write_rds(puuids, puuid_file)
stopifnot(nrow(puuids) > 0)

# Match id listing is cheap (1 call / 100 ids) and always refreshed.
ids_file <- "data/match_ids.rds"
all_ids <- tibble(gameName = character(), match_id = character())
for (i in seq_len(nrow(puuids))) {
  message("Listing matches for ", puuids$gameName[i])
  ids <- riot_match_ids_all(puuids$puuid[i])
  message("  ", length(ids), " ids")
  all_ids <- bind_rows(all_ids, tibble(gameName = puuids$gameName[i], match_id = ids))
}
write_rds(all_ids, ids_file)

todo <- unique(all_ids$match_id)
todo <- todo[!file.exists(match_cache_path(todo)) | file.size(match_cache_path(todo)) == 0]
message(sprintf("%d unique matches, %d still to download", length(unique(all_ids$match_id)), length(todo)))
todo <- head(todo, max_new)

status <- character(length(todo)); t0 <- Sys.time()
for (i in seq_along(todo)) {
  status[i] <- riot_fetch_match(todo[i])
  if (i %% 25 == 0 || i == length(todo)) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    message(sprintf("[%d/%d] %.1f/min, ETA %.0f min", i, length(todo), i / el * 60,
                    (length(todo) - i) / (i / el) / 60))
  }
}
if (any(status == "missing")) {
  write_csv(tibble(match_id = todo[status == "missing"]), "logs/missing_matches.csv")
}
message(sprintf("Done: %d fetched, %d missing, %d remaining",
                sum(status == "fetched"), sum(status == "missing"), max(0, length(todo) - length(status))))
