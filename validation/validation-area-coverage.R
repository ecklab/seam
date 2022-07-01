# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load data
bip = readRDS("data/bip.Rds")
b_lu = as.data.frame(readRDS("data/b-lu.Rds")) # why does this break as a tibble....??
p_lu = as.data.frame(readRDS("data/p-lu.Rds")) # why does this break as a tibble....??

# modify pools for validation
batter_pool  = get_batter_pool(bip = bip, year_start = 2017, year_end = 2020)
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

get_top_n_coverage = function(n) {

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

    tst = bip %>%
      filter(game_year > 2020) %>%
      filter(batter == matchups[i,]$batter) %>%
      filter(pitcher == matchups[i,]$pitcher)

    in_top = matrix(NA, nrow = nrow(tst), ncol = 3)

    for (j in 1:nrow(tst)) {

      in_top[j, ] = c(
        check_in_top_n(
          n = n,
          pitch = tst[j, c("x", "y")],
          synthetic = est$seam_df,
          plot = FALSE
        ),
        check_in_top_n(
          n = n,
          pitch = tst[j, c("x", "y")],
          synthetic = est$empirical_pitcher_df,
          plot = FALSE
        ),
        check_in_top_n(
          n = n,
          pitch = tst[j, c("x", "y")],
          synthetic = est$empirical_batter_df,
          plot = FALSE
        )
      )

      # print(in_top[j, ])

    }

    results[i, ] = colMeans(in_top)

  }

  colnames(results) = c("seam", "batter", "pitcher")
  return(results)

}

# TODO: this is currently very inefficient, lots of duplicated calculations

res_tnc_1000 = get_top_n_coverage(n = 1000) # 10%
res_tnc_2000 = get_top_n_coverage(n = 2000) # 20%
res_tnc_3000 = get_top_n_coverage(n = 3000) # 30%

saveRDS(res_tnc_1000, file = "validation/conditional-top-n-cov-1000.Rds")
saveRDS(res_tnc_2000, file = "validation/conditional-top-n-cov-2000.Rds")
saveRDS(res_tnc_3000, file = "validation/conditional-top-n-cov-3000.Rds")
