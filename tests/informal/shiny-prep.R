# read in data created via statcast-utils
pitches_processed = data.table::fread("data/pitches-processed.csv")
bip = data.table::fread("data/bip.csv")
b_lu = data.table::fread("data/b_lu.csv")
p_lu = data.table::fread("data/p_lu.csv")
unique(pitches_processed$game_year) # verify correct years

# load {seam} package
# devtools::build_readme()
# devtools::check()
devtools::load_all()

# this should happen outside of the seam app
# TODO: write to disk, move to statcast-utils
batter_pool = get_batter_pool(bip = bip)
pitcher_pool = get_pitcher_pool(bip = bip)

# check loaded data and functions
nrow(bip)
lu_p(p_lu, "Justin Verlander")
lu_b(b_lu, "Mike Trout")
# can we pre-calculate this for all pitchers and lookup?
get_pitcher_pitches(.bip = bip,
                    .pitches = pitches_processed,
                    lu_p(p_lu, "Justin Verlander"))
get_matchup_hands(
  bip = bip,
  b_id = lu_b(b_lu, "Mike Trout"),
  p_id = lu_p(p_lu, "Justin Verlander")
)

# this happens inside of the seam app
ff_synth_trout_verlander_bip = make_bip_pool_synth_batter(
  .pitch_type = "FF",
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander"),
  .bip = bip,
  .batter_pool = batter_pool,
  .stand = "R",
  .p_throws = "R")
ff_synth_trout_verlander_estimated = kde(x = ff_synth_trout_verlander_bip$x,
                                         y = ff_synth_trout_verlander_bip$y,
                                         w = ff_synth_trout_verlander_bip$weight,
                                         lims = c(-150, 150, -10, 200)))
kde_to_df(ff_synth_trout_verlander_estimated)

################################################################################

get_pitcher_pitches(.bip = bip,
                    .pitches = pitches_processed,
                    lu_p(p_lu, "Justin Verlander"))

cu = make_bip_pool_synth_pitcher(
  .pitch_type = "CU",
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander"),
  .bip = bip,
  .pitcher_pool = pitcher_pool,
  .stand = "R",
  .p_throws = "R")
cu_dens = kde(x = cu$x,
                   y = cu$y,
                   w = cu$weight,
                   lims = c(-150, 150, -10, 200))
cu_df = kde_to_df(cu_dens)

ch = make_bip_pool_synth_pitcher(
  .pitch_type = "CH",
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander"),
  .bip = bip,
  .pitcher_pool = pitcher_pool,
  .stand = "R",
  .p_throws = "R")
ch_dens = kde(x = ch$x,
              y = ch$y,
              w = ch$weight,
              lims = c(-150, 150, -10, 200))
ch_df = kde_to_df(ch_dens)

ff = make_bip_pool_synth_pitcher(
  .pitch_type = "FF",
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander"),
  .bip = bip,
  .pitcher_pool = pitcher_pool,
  .stand = "R",
  .p_throws = "R")
ff_dens = kde(x = ff$x,
              y = ff$y,
              w = ff$weight,
              lims = c(-150, 150, -10, 200))
ff_df = kde_to_df(ff_dens)

sl = make_bip_pool_synth_pitcher(
  .pitch_type = "SL",
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander"),
  .bip = bip,
  .pitcher_pool = pitcher_pool,
  .stand = "R",
  .p_throws = "R")
sl_dens = kde(x = sl$x,
              y = sl$y,
              w = sl$weight,
              lims = c(-150, 150, -10, 200))
sl_df = kde_to_df(sl_dens)

################################################################################

get_pitcher_pitches(.bip = bip,
                    .pitches = pitches_processed,
                    lu_p(p_lu, "Justin Verlander"))

df = ch_df
df$z = ch_df$z * 0.0327 +
  cu_df$z * 0.156 +
  ff_df$z * 0.568 +
  sl_df$z * 0.244

c = 3.030303 * 2.121212

0.0327 * sum(ch$similarity ^ 2) +
  0.156 * sum(cu$similarity ^ 2) +
  0.568 * sum(ff$similarity ^ 2) +
  0.244 * sum(sl$similarity ^ 2)

library(tidyverse)
zzz = bip %>%
  filter(batter == lu_b(b_lu, "Mike Trout")) %>%
  filter(pitcher == lu_p(p_lu, "Justin Verlander"))

kde(x = zzz$x,
    y = zzz$y,
    lims = c(-150, 150, -10, 200))
