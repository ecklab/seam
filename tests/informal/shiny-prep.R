# read in data created via statcast-utils
pitches_processed = data.table::fread("data/pitches-processed.csv")
bip = data.table::fread("data/bip.csv")
b_lu = data.table::fread("data/b_lu.csv")
p_lu = data.table::fread("data/p_lu.csv")

# this should happen outside of the seam app
batter_pool = get_batter_pool(bip = bip)
make_bip_pool_pitch(.pitch_type = "FF",
                    .batter = lu_b(b_lu, "Mike Trout"),
                    .pitcher = lu_p(p_lu, "Justin Verlander"),
                    .bip = bip,
                    .batter_pool = batter_pool,
                    .stand = "R",
                    .p_throws = "R")

# load {seam} package
devtools::build_readme()
devtools::check()
devtools::load_all()

# check loaded data and functions
nrow(bip)
lu_p(p_lu, "Justin Verlander")
lu_b(b_lu, "Mike Trout")
get_pitcher_pitches(.bip = bip,
                    .pitches = pitches_processed,
                    lu_p(p_lu, "Justin Verlander"))
get_matchup_hands(
  bip = bip,
  b_id = lu_b(b_lu, "Mike Trout"),
  p_id = lu_p(p_lu, "Justin Verlander")
)

