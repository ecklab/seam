# this script details the pre-shiny processing that should be done outside of
# the app and written to disk to save time in the app
# because of this, these functions were NOT written with speed in mind
player_ids = get_player_ids()
# the following line assumes you have used statcast-utils to download data
pitches = data.table::fread("../statcast-utils/statcast-all-pitches.csv")
pitches_processed = process_statcast(data = pitches, player_ids = player_ids)
bip = get_bip(pitches_processed)
b_lu = make_b_lu(bip)
p_lu = make_p_lu(bip)

# below here tests some of the above

nrow(bip)
lu_p(p_lu, "Justin Verlander")
lu_b(b_lu, "Mike Trout")
get_pitcher_pitches(bip, lu_p(p_lu, "Justin Verlander"))
get_matchup_hands(
  .bip = bip,
  .batter = lu_b(b_lu, "Mike Trout"),
  .pitcher = lu_p(p_lu, "Justin Verlander")
)
