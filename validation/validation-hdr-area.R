# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load data
bip = readRDS("data/bip.Rds")
b_lu = as.data.frame(readRDS("data/b-lu.Rds")) # why does this break as a tibble....??
p_lu = as.data.frame(readRDS("data/p-lu.Rds")) # why does this break as a tibble....??

# modify pools for validation
batter_pool = get_batter_pool(bip = bip, year_start = 2017, year_end = 2020)
pitcher_pool = get_pitcher_pool(bip = bip, year_start = 2017, year_end = 2020)

trn = bip %>%
  filter(game_year <= 2020)

matchups = bip %>%
  filter(game_year == 2021) %>%
  group_by(batter, pitcher) %>%
  summarise(n = n()) %>%
  filter(n >= 10) %>%
  select(-n) %>%
  filter(batter != 628451) %>% # only 2021 bip (cannot fit to trn)
  filter(batter != 677551) %>% # only 2021 bip (cannot fit to trn)
  filter(pitcher != 657093)    # only 2021 bip (cannot fit to trn)

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

res_025 = get_area(alpha = 0.25)
res_050 = get_area(alpha = 0.50)
res_075 = get_area(alpha = 0.75)

saveRDS(res_025, file = "validation/conditional-hdr-area-025.Rds")
saveRDS(res_050, file = "validation/conditional-hdr-area-050.Rds")
saveRDS(res_075, file = "validation/conditional-hdr-area-075.Rds")

colMeans(res_025)
apply(res_025, 2, sd)

colMeans(res_050)
apply(res_050, 2, sd)

colMeans(res_075)
apply(res_075, 2, sd)
