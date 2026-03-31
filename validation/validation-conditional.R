# load packages and seam functions
library(tidyverse)
devtools::load_all()    # TODO: NOTHING! FULLY RUNS!

# load data
bip = readRDS("data/bip.Rds")
b_lu = as.data.frame(readRDS("data/b-lu.Rds")) # why does this break as a tibble....??
p_lu = as.data.frame(readRDS("data/p-lu.Rds")) # why does this break as a tibble....??

# modify pools for validation
batter_pool = get_batter_pool(bip = bip, year_start = 2021, year_end = 2023)
pitcher_pool = get_pitcher_pool(bip = bip, year_start = 2021, year_end = 2023)

# test zone ------------------------------------
# trn = filter(bip, game_year <= 2023)
# train_ids = c(train$batter, train$hitter)

# print(train_ids)

# test = filter(bip, game_year == 2024)
# test_ids = c(test$batter, test$hitter)

# # basically

# train_ids = unique(train[, c("pitcher", "batter")])

# ----------------------------------------------

# function to perform conditional validation
validate_conditional = function() {

  alpha = c(0.10, 0.25, 0.50, 0.75, 0.90)

  trn = bip %>%
    filter(game_year <= 2023)

  trn_p = trn %>%
    group_by(pitcher) %>%
    summarize(n = n()) %>%
    pull(pitcher)
  
  trn_b = trn %>%
    group_by(batter) %>%
    summarize(n = n()) %>%
    pull(batter)
  
  # creating list of 2024 matchups with at least 8 bip
  matchups = bip %>%    # directory of 2024 matchups
    filter(game_year == 2024) %>%
    group_by(batter, pitcher) %>%
    summarise(n = n()) %>%
    filter(n >= 10) %>%
    # select(-n) %>%
    filter(batter %in% trn_b, pitcher %in% trn_p) #%>%
    # filter(batter != 672761) %>%
    # filter(batter != 681624) %>%
    # filter(batter != 690993) %>%
    # filter(batter != 694192) %>%
    # filter(batter != 807799)
  
  print("matchups:")
  print(matchups)    # NOTE: seems like 'matchups' is only 20 row by 2 col df... when line 52 says 'filter(n >= 10)'

  matchup_results = vector(mode = "list", length = nrow(matchups))

  for (i in 1:nrow(matchups)) {

    batter = matchups[i,]$batter
    pitcher = matchups[i,]$pitcher

    print(c(batter, pitcher))

    tst = bip %>%    # actual bip events for a 2024 matchup
      filter(game_year == 2024) %>%
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
    print("")
    print(head(seam$seam_df))
    print(head(seam$empirical_df))
    print(head(seam$synth_pitcher_df))
    print(head(seam$synth_batter_df))

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

  print(matchup_results)    # FIXME: look at console, but NaN values sometimes for seam predictions?

}

# run conditional validation
results = validate_conditional()

# store intermediate results
saveRDS(results, file = "validation/conditional-coverage.Rds")
