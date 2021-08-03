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

# test a matchup
do_full_seam_matchup(
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander"),
  .pitches = pitches_processed,
  .bip = bip,
  .batter_pool = batter_pool,
  .pitcher_pool = pitcher_pool
)
