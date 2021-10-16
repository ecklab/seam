# download data ################################################################

# weeks = generate_weeks(start_year = 2015, end_year = 2021)
# dled_weeks = apply(weeks, 1, dl_week)
# dled_weeks = dled_weeks[sapply(dled_weeks, function(x) {nrow(x) != 0})]
# pitches = data.table::rbindlist(dled_weeks)
# data.table::fwrite(pitches, "data/statcast-all-pitches.csv")

# pre-shiny setup ##############################################################

# # load {seam} package
# # devtools::build_readme()
# # devtools::check()
# devtools::load_all()

# player_ids = get_player_ids()
# pitches = data.table::fread("data/statcast-all-pitches.csv")
# pitches_processed = process_statcast(data = pitches, player_ids = player_ids)
# bip = get_bip(pitches_processed)
# pitches_for_ratios = get_pitches_for_pitch_ratio(pitches_processed)
# b_lu = make_b_lu(bip)
# p_lu = make_p_lu(bip)
# batter_pool = get_batter_pool(bip = bip)
# pitcher_pool = get_pitcher_pool(bip = bip)

# data.table::fwrite(player_ids, "data/player-ids.csv")
# data.table::fwrite(pitches_processed, "data/pitches-processed.csv")
# data.table::fwrite(bip, "data/bip.csv")
# data.table::fwrite(pitches_for_ratios, "data/pitches-for-ratios.csv")
# data.table::fwrite(b_lu, "data/b_lu.csv")
# data.table::fwrite(p_lu, "data/p_lu.csv")
# data.table::fwrite(batter_pool, "data/batter-pool.csv")
# data.table::fwrite(pitcher_pool, "data/pitcher-pool.csv")

# shiny testing ################################################################

devtools::load_all()
library(dplyr)
library(ggplot2)

pitches_for_ratios = data.table::fread("data/pitches-for-ratios.csv")
bip = data.table::fread("data/bip.csv")
b_lu = data.table::fread("data/b_lu.csv")
p_lu = data.table::fread("data/p_lu.csv")
batter_pool = data.table::fread("data/batter-pool.csv")
pitcher_pool = data.table::fread("data/pitcher-pool.csv")

test_matchup = do_full_seam_matchup(
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander"),
  .pitches = pitches_for_ratios,
  .bip = bip,
  .batter_pool = batter_pool,
  .pitcher_pool = pitcher_pool,
  .ratio_batter = .85,
  .ratio_pitcher = .85
)

str(test_matchup)

p1 = plot_df(test_matchup$seam_df, stadium = "astros", batter = "Trout", pitcher = "Verlander", main = "Full SEAM")
p2 = plot_df(test_matchup$synth_pitcher_df, stadium = "astros", batter = "Trout", pitcher = "Verlander", main = "Synthetic Pitcher")
p3 = plot_df(test_matchup$synth_batter_df, stadium = "astros", batter = "Trout", pitcher = "Verlander", main = "Synthetic Batter")
p4 = plot_df(test_matchup$empirical_df, stadium = "astros", batter = "Trout", pitcher = "Verlander", main = "Empirical Matchup")
p5 = plot_df(test_matchup$empirical_pitcher_df, stadium = "astros", batter = "Trout", pitcher = "Verlander", main = "Empirical Pitcher")
p6 = plot_df(test_matchup$empirical_batter_df, stadium = "astros", batter = "Trout", pitcher = "Verlander", main = "Empirical Batter")
cowplot::plot_grid(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)
