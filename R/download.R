generate_weeks = function(start_year = format(Sys.Date(), "%Y"),
                          end_year = format(Sys.Date(), "%Y")) {
  start = seq(as.Date(paste0(start_year, "-01-01")),
              as.Date(paste0(end_year, "-12-31")), by = "1 week")
  print(length(start))
  end = start + 6
  end[length(end)] = paste0(end_year, "-12-31")
  return(data.frame(start, end))
}

dl_week = function(week) {
  message("Processing the week starting on: ", week[1])
  url = paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true&game_date_gt=",
    week[1],
    "&game_date_lt=",
    week[2],
    "&type=details"
  )
  data.table::fread(url)
}

dl_statcast = function(start_year, end_year) {
  weeks = generate_weeks(start_year = start_year, end_year = end_year)
  dled_weeks = apply(weeks, 1, dl_week)
  dled_weeks = dled_weeks[sapply(dled_weeks, function(x) {nrow(x) != 0})]
  return(data.table::rbindlist(dled_weeks))
}
