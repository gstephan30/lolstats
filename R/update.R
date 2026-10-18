# One-shot update: download new matches -> rebuild data/all_games.rds -> knit README.
#   Rscript R/update.R                # full update
#   Rscript R/update.R --no-readme    # skip knitting
#   Rscript R/update.R --max-new=50   # download at most 50 new matches (test runs)
args <- commandArgs(trailingOnly = TRUE)
max_new <- sub("^--max-new=", "", grep("^--max-new=", args, value = TRUE))
rscript <- file.path(R.home("bin"), "Rscript")

run_step <- function(label, script, extra = character()) {
  message("\n== ", label, " ==")
  status <- system2(rscript, c(script, extra))
  if (status != 0) stop(label, " failed (exit ", status, ")", call. = FALSE)
}

run_step("1/3 download new matches", "R/backfill_matches.R", max_new)
run_step("2/3 rebuild data/all_games.rds", "R/build_all_games.R")
if (!"--no-readme" %in% args) {
  message("\n== 3/3 knit README ==")
  rmarkdown::render("README.Rmd", quiet = TRUE)
  unlink("README.html")
}
message("\nUpdate complete.")
