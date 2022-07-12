# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
results_many_n = readRDS(file = "validation/conditional-top-n-cov-n.Rds")

res_tnc_n = t(sapply(results_many_n, colMeans))
rownames(res_tnc_n) = graph_points
res_tnc_n = as.data.frame(res_tnc_n)
res_tnc_n$n = as.numeric(rownames(res_tnc_n))
res_tnc_n = res_tnc_n |> tidyr::pivot_longer(cols = c("seam", "batter", "pitcher"))

res_tnc_n |>
  ggplot() +
  aes(x = n, y = value, color = name) +
  geom_point() +
  geom_line() +
  theme_bw()
