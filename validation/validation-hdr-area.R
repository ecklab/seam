# load packages and seam functions
library(tidyverse)
devtools::load_all()    # TODO: NOTHING! FULLY RUNS!

# load data
bip = readRDS("data/bip.Rds")
b_lu = as.data.frame(readRDS("data/b-lu.Rds")) # why does this break as a tibble....??
p_lu = as.data.frame(readRDS("data/p-lu.Rds")) # why does this break as a tibble....??

# modify pools for validation
batter_pool  = get_batter_pool(bip = bip, year_start = 2021, year_end = 2024)
pitcher_pool = get_pitcher_pool(bip = bip, year_start = 2021, year_end = 2024)

trn = bip %>%
  filter(game_year <= 2023)

trn_p = trn %>%
    group_by(pitcher) %>%
    summarize(n = n()) %>%
    pull(pitcher)
  
  trn_b = bip %>%
    group_by(batter) %>%
    summarize(n = n()) %>%
    pull(batter)

matchups = bip %>%    # directory of 2024 matchups
    filter(game_year == 2024) %>%
    group_by(batter, pitcher) %>%
    summarise(n = n()) %>%
    filter(n >= 8) %>%
    select(-n) %>%
    filter(batter %in% trn_b, pitcher %in% trn_p) %>%
    filter(batter != 672761) %>%
    filter(batter != 681624) %>%
    filter(batter != 690993) %>%
    filter(batter != 694192) %>%
    filter(batter != 807799)

get_area = function(alpha) {

  results = matrix(data = 0, nrow = nrow(matchups), ncol = 3)

  for (i in 1:nrow(matchups)) {

    est = do_full_seam_matchup(
      .batter = matchups[i, ]$batter,
      .pitcher = matchups[i, ]$pitcher,
      .bip = trn,
      .batter_pool = batter_pool,
      .pitcher_pool = pitcher_pool,
      .ratio_batter = .85,
      .ratio_pitcher = .85
    )

    results[i, ] = c(
      calc_hdr_size(alpha = alpha, synthetic = est$seam_df),
      calc_hdr_size(alpha = alpha, synthetic = est$empirical_pitcher_df),
      calc_hdr_size(alpha = alpha, synthetic = est$empirical_batter_df)
    )

  }

  return(results)

}

res_010 = get_area(alpha = 0.10)
res_025 = get_area(alpha = 0.25)
res_050 = get_area(alpha = 0.50)
res_075 = get_area(alpha = 0.75)
res_090 = get_area(alpha = 0.90)

saveRDS(res_010, file = "validation/conditional-hdr-area-010.Rds")
saveRDS(res_025, file = "validation/conditional-hdr-area-025.Rds")
saveRDS(res_050, file = "validation/conditional-hdr-area-050.Rds")
saveRDS(res_075, file = "validation/conditional-hdr-area-075.Rds")
saveRDS(res_090, file = "validation/conditional-hdr-area-090.Rds")
