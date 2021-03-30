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
