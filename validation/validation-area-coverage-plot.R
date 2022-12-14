# load packages and seam functions
library(ggplot2)
devtools::load_all()

# load previously calculated results
results_many_n = readRDS(file = "validation/conditional-top-n-cov-n.Rds")
results_many_n_d05 = readRDS(file = "validation/conditional-top-n-cov-n-d05.Rds")
results_many_n_d10 = readRDS(file = "validation/conditional-top-n-cov-n-d10.Rds")
results_many_n_d20 = readRDS(file = "validation/conditional-top-n-cov-n-d20.Rds")
results_many_n_d30 = readRDS(file = "validation/conditional-top-n-cov-n-d30.Rds")

process_results = function(res) {
  res = t(sapply(res, colMeans))
  graph_points = seq(from = 1500, to = 2500, by = 50)
  rownames(res) = graph_points
  res = as.data.frame(res)
  res$n = as.numeric(rownames(res))
  res = res |> tidyr::pivot_longer(cols = c("seam", "batter", "pitcher", "seam_mod", "both"))
  res
}

res_n_d05 = process_results(results_many_n_d05)
res_n_d10 = process_results(results_many_n_d10)
res_n_d20 = process_results(results_many_n_d20)
res_n_d30 = process_results(results_many_n_d30)

res_n_d05 = res_n_d05 |>
  dplyr::filter(name == "seam")
res_n_d05$name = "seam_05"

res_n_d10 = res_n_d30 |>
  dplyr::filter(name == "seam")
res_n_d10$name = "seam_10"

res_n_d30 = res_n_d30 |>
  dplyr::filter(name == "seam")
res_n_d30$name = "seam_30"

res = rbind.data.frame(res_n_d20, res_n_d05, res_n_d10, res_n_d30)

# res_tnc_n = t(sapply(results_many_n, colMeans))
# graph_points = seq(from = 1500, to = 2500, by = 50)
# rownames(res_tnc_n) = graph_points
# res_tnc_n = as.data.frame(res_tnc_n)
# res_tnc_n$n = as.numeric(rownames(res_tnc_n))
# res_tnc_n = res_tnc_n |> tidyr::pivot_longer(cols = c("seam", "batter", "pitcher", "seam_mod", "both"))

res |>
  ggplot() +
    aes(x = n, y = value, color = name) +
    geom_point() +
    geom_line() +
    xlab("Number of grids") +
    ylab("Average conditional coverage") +
    ggtitle("Conditional coverage vs region size") +
    theme_bw()


















# knitr::kable(round(res_tnc_n %>% spread(name, value), 3), format = "latex")
#
# ## standard error calculation
# res_tnc_n_se = t(sapply(
#   results_many_n,
#   FUN = function(xx)
#     apply(xx, 2, function(x)
#       sd(x) / sqrt(length(x)))
# ))
# res_tnc_n_se
#
#
# ## K-S calculations
# foo <- round(res_tnc_n %>% spread(name, value), 3)
# ks.test(unlist(foo[,1]), unlist(foo[,3]))
# ks.test(unlist(foo[,1]), unlist(foo[,2]))
#
#
# ## regression analysis
# bar <- do.call(rbind, results_many_n)
# colnames(bar)[1] <- "aseam"
# bar_graph_points = rep(seq(from = 1500, to = 2500, by = 50), each = 195)
# bar = as.data.frame(bar)
# bar$n = unlist(bar_graph_points)
# bar = bar |>
#   tidyr::pivot_longer(cols = c("aseam", "batter", "pitcher")) |>
#   mutate(dummy = ifelse(name == "pitcher",1,0))
#
#
# set.seed(13)
# m <- lm(value ~ n + name, data = bar)
# par(mfrow = c(2,2))
# plot(m)
# m2 <- lm(value ~ n + name + n*dummy -dummy, data = bar)
# test_stat <- anova(m, m2)$F[2]
#
# test <- replicate(1e4, {bar_boot <- bar[sample(1:nrow(bar), replace = T), ]
#   m <- lm(value ~ n + name, data = bar_boot)
#   m2 <- lm(value ~ n + name + n*dummy -dummy, data = bar_boot)
#   out <- anova(m, m2)
#   out$F[2]
# })
# mean(test > test_stat)
#
#
# set.seed(13)
# baz <- t(replicate(1e4, coef(lm(value ~ n + name,
#   data = bar[sample(1:nrow(bar), replace = T), ]))))
# mean(baz[, 3] > 0)
# mean(baz[, 4] > 0)
#
# qux <- as.data.frame(baz) |>
#   dplyr::select(n, namebatter, namepitcher) |>
#   rename("SEAM vs pitcher" = namepitcher, "SEAM vs batter" = namebatter) |>
#   tidyr::pivot_longer(cols = c("SEAM vs batter", "SEAM vs pitcher"))
# ggplot(qux, aes(x = value)) +
#   geom_density() +
#   geom_vline(xintercept = 0, color = "red", size = 1) +
#   theme_minimal() +
#   facet_wrap(~name)
