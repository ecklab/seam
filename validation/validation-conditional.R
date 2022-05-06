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

# function to perform conditional validation
validate_conditional = function() {

  alpha = c(0.10, 0.25, 0.50, 0.75, 0.90)

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

  matchup_results = vector(mode = "list", length = nrow(matchups))

  for (i in 1:nrow(matchups)) {

    batter = matchups[i,]$batter
    pitcher = matchups[i,]$pitcher

    print(c(batter, pitcher))

    tst = bip %>%
      filter(game_year > 2020) %>%
      filter(batter == matchups[i,]$batter) %>%
      filter(pitcher == matchups[i,]$pitcher)

    in_hdr = array(NA, c(nrow(tst), 3, length(alpha)))

    seam = do_full_seam_matchup(
      .batter = batter,
      .pitcher = pitcher,
      .bip = trn,
      .batter_pool = batter_pool,
      .pitcher_pool = pitcher_pool,
      .ratio_batter = .85,
      .ratio_pitcher = .85
    )

    for (j in 1:nrow(tst)) {

      # print(j)

      in_hdr[j, ,] = rbind(
        check_in_hdrs(
          alpha = alpha,
          pitch = tst[j, c("x", "y")],
          synthetic = seam$seam_df,
          plot = FALSE
        ),
        check_in_hdrs(
          alpha = alpha,
          pitch = tst[j, c("x", "y")],
          synthetic = seam$empirical_pitcher_df,
          plot = FALSE
        ),
        check_in_hdrs(
          alpha = alpha,
          pitch = tst[j, c("x", "y")],
          synthetic = seam$empirical_batter_df,
          plot = FALSE
        )
      )

    }

    result = rbind(
      colMeans(in_hdr[, , 1], na.rm = TRUE),
      colMeans(in_hdr[, , 2], na.rm = TRUE),
      colMeans(in_hdr[, , 3], na.rm = TRUE),
      colMeans(in_hdr[, , 4], na.rm = TRUE),
      colMeans(in_hdr[, , 5], na.rm = TRUE)
    )
    rownames(result) = c(0.10, 0.25, 0.50, 0.75, 0.90)
    colnames(result) = c("seam", "batter", "pitcher")

    matchup_results[[i]] = result

  }

  matchup_results

}

# run conditional validation
results = validate_conditional()

# store intermediate results
saveRDS(results, file = "validation/conditional-coverage.Rds")
