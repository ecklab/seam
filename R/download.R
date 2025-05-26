dl_year = function(year) {
  message(paste("Processing stats for ", year, " season:"))

  cluster = parallel::makeCluster(parallel::detectCores())
  data_temp_year = sabRmetrics::download_baseballsavant(    # FIXME: need to try both again... try baseball savant and statcast...
    start_date = paste0(year, "-01-01"),
    end_date = paste0(year, "-12-31"),
    cl = cluster
  )
  parallel::stopCluster(cluster)

  return(data_temp_year)
}

dl_statcast = function(start_year, end_year) {
  years = seq.int(start_year, end_year)
  data = lapply(years, dl_year)
  return(data.table::rbindlist(data))
}

# TODO: need to edit this one... combine the first three tables using download_statsapi()
# SOOOO... look at test log, have column names of everthing just double check w process.R
