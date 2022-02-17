library(tidyverse)
devtools::load_all()

bip = readRDS("data/bip.Rds")
b_lu = as.data.frame(readRDS("data/b-lu.Rds")) # why does this break as a tibble....??
p_lu = as.data.frame(readRDS("data/p-lu.Rds")) # why does this break as a tibble....??

# modify pools for validation
batter_pool = get_batter_pool(bip = bip, year_start = 2017, year_end = 2020)
pitcher_pool = get_pitcher_pool(bip = bip, year_start = 2017, year_end = 2020)

validate_all = function(alpha = 0.25) {

  trn = bip %>%
    filter(game_year <= 2020)
  tst = bip %>%
    filter(game_year > 2020)

  in_hdr = matrix(NA, nrow = nrow(tst), ncol = 3)

  for (i in 1:nrow(tst)) {

    batter = tst[i, ]$batter
    pitcher = tst[i, ]$pitcher

    in_hdr[i, ] = try({

      seam = do_full_seam_matchup(
        .batter = batter,
        .pitcher = pitcher,
        .bip = trn,
        .batter_pool = batter_pool,
        .pitcher_pool = pitcher_pool,
        .ratio_batter = .85,
        .ratio_pitcher = .85
      )

      c(
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

    a1 = sum(in_hdr[1:i, 1] == "TRUE")
    b1 = sum(in_hdr[1:i, 1] == "FALSE")

    a2 = sum(in_hdr[1:i, 2] == "TRUE")
    b2 = sum(in_hdr[1:i, 2] == "FALSE")

    a3 = sum(in_hdr[1:i, 3] == "TRUE")
    b3 = sum(in_hdr[1:i, 3] == "FALSE")

    print(c(
      seam = a1 / (a1 + b1),
      batter = a2 / (a2 + b2),
      pitcher = a3 / (a3 + b3)
    ))
  }

  in_hdr

}

results = validate_all()
