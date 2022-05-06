library(tidyverse)
devtools::load_all()

bip = readRDS("data/bip.Rds")
b_lu = as.data.frame(readRDS("data/b-lu.Rds")) # why does this break as a tibble....??
p_lu = as.data.frame(readRDS("data/p-lu.Rds")) # why does this break as a tibble....??

# modify pools for validation
batter_pool = get_batter_pool(bip = bip, year_start = 2017, year_end = 2020)
pitcher_pool = get_pitcher_pool(bip = bip, year_start = 2017, year_end = 2020)

trn = bip %>%
  filter(game_year <= 2020)

trn_b = trn %>%
  group_by(batter) %>%
  summarize(n = n()) %>%
  filter(n > 25) %>%
  pull(batter)

trn_p = trn %>%
  group_by(pitcher) %>%
  summarize(n = n()) %>%
  filter(n > 25) %>%
  pull(pitcher)

tst = bip %>%
  filter(game_year > 2020) %>%
  filter(batter %in% trn_b) %>%
  filter(pitcher %in% trn_p)

validate_all = function(alpha = c(0.10, 0.25, 0.50, 0.75, 0.90)) {

  in_hdr = array(NA, c(nrow(tst), 3, length(alpha)))

  for (i in 1:nrow(tst)) {

    batter = tst[i, ]$batter
    pitcher = tst[i, ]$pitcher

    # output matchups to find which causes warnings / error
    print(c(batter = batter, pitcher = pitcher))

    try({
      seam = do_full_seam_matchup(
        .batter = batter,
        .pitcher = pitcher,
        .bip = trn,
        .batter_pool = batter_pool,
        .pitcher_pool = pitcher_pool,
        .ratio_batter = .85,
        .ratio_pitcher = .85
      )

      in_hdr[i, , ] = rbind(
        check_in_hdrs(
          alpha = alpha,
          pitch = tst[i, c("x", "y")],
          synthetic = seam$seam_df,
          plot = FALSE
        ),
        check_in_hdrs(
          alpha = alpha,
          pitch = tst[i, c("x", "y")],
          synthetic = seam$empirical_pitcher_df,
          plot = FALSE
        ),
        check_in_hdrs(
          alpha = alpha,
          pitch = tst[i, c("x", "y")],
          synthetic = seam$empirical_batter_df,
          plot = FALSE
        )
      )
    })

    if (i %% 5 == 0) {
      print(i)
      mat = rbind(
        colMeans(in_hdr[, , 1], na.rm = TRUE),
        colMeans(in_hdr[, , 2], na.rm = TRUE),
        colMeans(in_hdr[, , 3], na.rm = TRUE),
        colMeans(in_hdr[, , 4], na.rm = TRUE),
        colMeans(in_hdr[, , 5], na.rm = TRUE)
      )
      rownames(mat) = alpha
      colnames(mat) = c("seam", "batter", "pitcher")
      print(mat)
    }

  }

  in_hdr

}

results = validate_all()
results

# testing parallel validation
library(foreach)
cl = parallel::makeForkCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)

alpha = alpha = c(0.10, 0.25, 0.50, 0.75, 0.90)
tst = tst[, c("batter", "pitcher", "x", "y")]

results = foreach(i = 1:nrow(tst)) %dopar% {

  batter = tst[i,]$batter
  pitcher = tst[i,]$pitcher

  in_hdr = matrix(NA, nrow = 3, ncol = length(alpha))

  try({
    seam = do_full_seam_matchup(
      .batter = batter,
      .pitcher = pitcher,
      .bip = trn,
      .batter_pool = batter_pool,
      .pitcher_pool = pitcher_pool,
      .ratio_batter = .85,
      .ratio_pitcher = .85
    )

    in_hdr = rbind(
      seam = check_in_hdrs(
        alpha = alpha,
        pitch = tst[i, c("x", "y")],
        synthetic = seam$seam_df,
        plot = FALSE
      ),
      e_pitch = check_in_hdrs(
        alpha = alpha,
        pitch = tst[i, c("x", "y")],
        synthetic = seam$empirical_pitcher_df,
        plot = FALSE
      ),
      e_batter = check_in_hdrs(
        alpha = alpha,
        pitch = tst[i, c("x", "y")],
        synthetic = seam$empirical_batter_df,
        plot = FALSE
      )
    )

  })

  in_hdr

}

# stop cluster
parallel::stopCluster(cl)

# store intermediate results
saveRDS(results, file = "validation/marginal-coverage.Rds")
