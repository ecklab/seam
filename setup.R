# TODO: add package installation information
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_deps()

if (!require(GeomMLBStadiums)) {
  remotes::install_github("bdilday/GeomMLBStadiums")
}

# load packages necessary for setup
library("dplyr")
devtools::install_github(repo = "saberpowers/sabRmetrics")

# load R functions, some necessary for setup
devtools::load_all()

# create data-raw directory if it does not exist
if (!dir.exists("data-raw")) {
  dir.create("data-raw")
}

# create data directory if it does not exist
if (!dir.exists("data")) {
  dir.create("data")
}

# download full data from statcast if it does not exist locally
if (!file.exists("data-raw/statcast-all-pitches.csv")) {
  pitches = dl_statcast(start_year = 2021, end_year = 2024)    # TODO: this is where we edit year
  data.table::fwrite(pitches, "data-raw/statcast-all-pitches.csv")
}

# prepare player ids
if (!file.exists("data/player-ids.Rds")) {
  player_ids = get_player_ids()
  saveRDS(player_ids, "data/player-ids.Rds")
}

# process statcast data
# TODO: split this up more
if (!file.exists("data-raw/pitches-processed.csv")) {

  if (!exists("pitches") | !exists("player_ids")) {
    pitches = data.table::fread("data-raw/statcast-all-pitches.csv")
    player_ids = readRDS("data/player-ids.Rds")
  }

  # process
  pitches_processed = process_statcast(data = pitches, player_ids = player_ids)
  bip = get_bip(pitches_processed)
  b_lu = make_b_lu(bip)
  p_lu = make_p_lu(bip)
  batter_pool = get_batter_pool(bip = bip)
  pitcher_pool = get_pitcher_pool(bip = bip)

  # save
  data.table::fwrite(pitches_processed, "data-raw/pitches-processed.csv")
  saveRDS(bip, "data/bip.Rds")
  saveRDS(b_lu, "data/b-lu.Rds")
  saveRDS(p_lu, "data/p-lu.Rds")
  saveRDS(batter_pool, "data/batter-pool.Rds")
  saveRDS(pitcher_pool, "data/pitcher-pool.Rds")

}

# get team and stadium information
# TODO: really no reason to rely on outside data here
if (!file.exists("data/mlb-teams.Rds") | !file.exists("data/stadiums.Rds")) {
  mlb_teams = readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/mlb_logos.rds"))
  mlb_teams = mlb_teams %>%
    dplyr::select(stadium, full_team_name, team, team_abbr)
  stadiums = mlb_teams$team
  names(stadiums) = mlb_teams$stadium
  saveRDS(mlb_teams, "data/mlb-teams.Rds")
  saveRDS(stadiums, "data/stadiums.Rds")
}

# get stadium dimensions
if (!file.exists("data/stadium-paths.Rds")) {
  saveRDS(GeomMLBStadiums::MLBStadiumsPathData,
          "data/stadium-paths.Rds")
}

required_files = c(
  "data/b-lu.Rds",
  "data/batter-pool.Rds",
  "data/bip.Rds",
  "data/mlb-teams.Rds",
  "data/p-lu.Rds",
  "data/pitcher-pool.Rds",
  "data/player-ids.Rds",
  "data/stadium-paths.Rds",
  "data/stadiums.Rds"
)

if (all(sapply(required_files, file.exists))) {
  message("All required files exist.")
} else {
  warning("Some required files are missing. Consider deleting data-raw/pitches-processed.csv if it exists. Then re-run setup.R.")
}

# run tests
shiny::runTests()


# FIXME: says setup.R works... but app.R does not?!?!