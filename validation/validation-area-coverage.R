# load packages and seam functions
devtools::load_all()

# load data
bip = readRDS("data/bip.Rds")
b_lu = readRDS("data/b-lu.Rds")
p_lu = readRDS("data/p-lu.Rds")

# modify pools for validation
batter_pool  = get_batter_pool(bip = bip, year_start = 2017, year_end = 2020)
pitcher_pool = get_pitcher_pool(bip = bip, year_start = 2017, year_end = 2020)

trn = bip |>
  dplyr::filter(game_year <= 2020)

matchups = bip |>
  dplyr::filter(game_year == 2021) |>
  dplyr::group_by(batter, pitcher) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n >= 10) |>
  dplyr::select(-n) |>
  dplyr::filter(batter != 628451) |> # only 2021 bip (cannot fit to trn)
  dplyr::filter(batter != 677551) |> # only 2021 bip (cannot fit to trn)
  dplyr::filter(pitcher != 657093)   # only 2021 bip (cannot fit to trn)

method_list = c("seam", "batter", "pitcher", "seam_mod", "both")

get_top_n_coverage = function(n, d = 2) {

  print(n)

  results = matrix(data = 0, nrow = nrow(matchups), ncol = length(method_list))

  for (i in 1:nrow(matchups)) {

    # perform estimation for matchup
    est = do_full_seam_matchup(
      .batter = matchups[i, ]$batter,
      .pitcher = matchups[i, ]$pitcher,
      .bip = trn,
      .batter_pool = batter_pool,
      .pitcher_pool = pitcher_pool,
      .ratio_batter = .85,
      .ratio_pitcher = .85,
      .d = d
    )

    # create test set for matchup
    tst = bip
    tst = tst[tst$game_year == 2021, ]
    tst = tst[tst$batter == matchups[i,]$batter, ]
    tst = tst[tst$pitcher == matchups[i,]$pitcher, ]

    # create storage for in-out results per pitch for matchup
    in_top = matrix(NA, nrow = nrow(tst), ncol = length(method_list))

    for (j in 1:nrow(tst)) {
      in_top[j, ] = c(
        check_in_top_n(n = n, pitch = tst[j, c("x", "y")], synthetic = est$seam_df),
        check_in_top_n(n = n, pitch = tst[j, c("x", "y")], synthetic = est$empirical_pitcher_df),
        check_in_top_n(n = n, pitch = tst[j, c("x", "y")], synthetic = est$empirical_batter_df),
        check_in_top_n(n = n, pitch = tst[j, c("x", "y")], synthetic = est$seam_mod_df),
        check_in_top_n(n = n, pitch = tst[j, c("x", "y")], synthetic = est$empirical_both_df)
      )
    }
    # summarize and store conditional coverage for matchup
    results[i, ] = colMeans(in_top)
  }

  colnames(results) = method_list
  return(results)

}

# results for "all n"
graph_points = seq(from = 1500, to = 2500, by = 50)
results_many_n = parallel::mclapply(graph_points, get_top_n_coverage, mc.cores = 8)

# save results for many n as a list
saveRDS(results_many_n, file = "validation/conditional-top-n-cov-n.Rds")

# results for "all n" and varying "d"
results_many_n_d05 = parallel::mclapply(graph_points, get_top_n_coverage, mc.cores = 8, d = 0.5)
results_many_n_d10 = parallel::mclapply(graph_points, get_top_n_coverage, mc.cores = 8, d = 1.0)
results_many_n_d20 = parallel::mclapply(graph_points, get_top_n_coverage, mc.cores = 8, d = 2.0)
results_many_n_d30 = parallel::mclapply(graph_points, get_top_n_coverage, mc.cores = 8, d = 3.0)

# save results for many n as a list
saveRDS(results_many_n_d05, file = "validation/conditional-top-n-cov-n-d05.Rds")
saveRDS(results_many_n_d10, file = "validation/conditional-top-n-cov-n-d10.Rds")
saveRDS(results_many_n_d20, file = "validation/conditional-top-n-cov-n-d20.Rds")
saveRDS(results_many_n_d30, file = "validation/conditional-top-n-cov-n-d30.Rds")

# # results for some n
# res_tnc_1000 = get_top_n_coverage(n = 1000) # 10%
# res_tnc_2000 = get_top_n_coverage(n = 2000) # 20%
# res_tnc_3000 = get_top_n_coverage(n = 3000) # 30%
#
# saveRDS(res_tnc_1000, file = "validation/conditional-top-n-cov-1000.Rds")
# saveRDS(res_tnc_2000, file = "validation/conditional-top-n-cov-2000.Rds")
# saveRDS(res_tnc_3000, file = "validation/conditional-top-n-cov-3000.Rds")

# # misc calculations
# 2.33 * diff(asdf$seam_df$x[1:2]) * 2.33 * diff(unique(asdf$seam_df$y)[1:2]) * 3000
# 115000 * c(0.50, 0.80)
# 92000 / (2.33 * diff(asdf$seam_df$x[1:2]) * 2.33 * diff(unique(asdf$seam_df$y)[1:2]))
# 57500 / (2.33 * diff(asdf$seam_df$x[1:2]) * 2.33 * diff(unique(asdf$seam_df$y)[1:2]))
# 8 - 10 x points
# seq(from = 1500, to = 2500, length.out = 10)
