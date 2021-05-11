# read in data created via statcast-utils
pitches_procssed = data.table::fread("data/pitches-processed.csv")
bip = data.table::fread("data/bip.csv")
b_lu = data.table::fread("data/b_lu.csv")
p_lu = data.table::fread("data/p_lu.csv")

# load {seam} package
devtools::check()
devtools::load_all()

# check loaded data and functions
nrow(bip)
lu_p(p_lu, "Justin Verlander")
lu_b(b_lu, "Mike Trout")
get_pitcher_pitches(bip, lu_p(p_lu, "Justin Verlander"))
get_matchup_hands(
  bip = bip,
  b_id = lu_b(b_lu, "Mike Trout"),
  p_id = lu_p(p_lu, "Justin Verlander")
)
