# code below here is meant to be run before the seam shiny app

player_ids = get_player_ids()
pitches = data.table::fread("../statcast-utils/statcast-all-pitches.csv")

# code below here is meant to be run within the seam shiny app

# code below here is scratch work


process_statcast = function(data) {

  # find duplicated columns
  dupes = c(
    which(names(data) == "pitcher")[2],
    which(names(data) == "fielder_2")[1]
  )

  # remove duplicated columns
  data = data[, -dupes, with = FALSE]

  # TODO: append names

  return(data)
}

get_bip = function(data) {

}
