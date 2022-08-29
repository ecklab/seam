# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
results_many_n = readRDS(file = "validation/conditional-top-n-cov-n.Rds")

res_tnc_n = t(sapply(results_many_n, colMeans))
graph_points = seq(from = 1500, to = 2500, by = 100)
rownames(res_tnc_n) = graph_points
res_tnc_n = as.data.frame(res_tnc_n)
res_tnc_n$n = as.numeric(rownames(res_tnc_n))
res_tnc_n = res_tnc_n |> tidyr::pivot_longer(cols = c("seam", "batter", "pitcher"))

res_tnc_n |>
  ggplot() +
  aes(x = n, y = value, color = name) +
  geom_point() +
  geom_line() +
  ylab("Average conditional coverage") +
  ggtitle("Conditional coverage vs region size") +
  theme_bw()


knitr::kable(round(res_tnc_n %>% spread(name, value), 3), format = "latex")


foo <- round(res_tnc_n %>% spread(name, value), 3)
ks.test(unlist(foo[,1]), unlist(foo[,3]))


## standard error calculation
res_tnc_n_se = t(sapply(results_many_n, FUN = function(xx) apply(xx, 2, function(x) sd(x)/sqrt(length(x))) ))
res_tnc_n_se
