library(tidyverse)

# load package functions
devtools::load_all()

# create data-raw directory if it does not exist
if (!dir.exists("data-raw")) {
  dir.create("data-raw")
}

# download and write data to disk
# this will take a non-trivial amount of time
pitches = dl_statcast(start_year = 2017, end_year = 2021)
data.table::fwrite(pitches, "data-raw/statcast-all-pitches.csv")

# pre data
player_ids = get_player_ids()
pitches = data.table::fread("data-raw/statcast-all-pitches.csv")
pitches_processed = process_statcast(data = pitches, player_ids = player_ids)
bip = get_bip(pitches_processed)
pitches_for_ratios = get_pitches_for_pitch_ratio(pitches_processed)
b_lu = make_b_lu(pitches_processed)
p_lu = make_p_lu(bip)
batter_pool = get_batter_pool(bip = bip)
pitcher_pool = get_pitcher_pool(bip = bip)

# write data to disk
data.table::fwrite(player_ids, "data-raw/player-ids.csv")
data.table::fwrite(pitches_processed, "data-raw/pitches-processed.csv")
data.table::fwrite(bip, "data-raw/bip.csv")
data.table::fwrite(pitches_for_ratios, "data-raw/pitches-for-ratios.csv")
data.table::fwrite(b_lu, "data-raw/b_lu.csv")
data.table::fwrite(p_lu, "data-raw/p_lu.csv")
data.table::fwrite(batter_pool, "data-raw/batter-pool.csv")
data.table::fwrite(pitcher_pool, "data-raw/pitcher-pool.csv")
