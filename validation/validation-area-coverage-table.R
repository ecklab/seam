# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
res_tnc_1000 = readRDS(file = "validation/conditional-top-n-cov-1000.Rds")
res_tnc_2000 = readRDS(file = "validation/conditional-top-n-cov-2000.Rds")
res_tnc_3000 = readRDS(file = "validation/conditional-top-n-cov-3000.Rds")

results = rbind(
  colMeans(res_tnc_1000),
  colMeans(res_tnc_2000),
  colMeans(res_tnc_3000)
)

colnames(results) = c("seam", "pitcher", "batter")
rownames(results) = c("n = 1000", "n = 2000", "n = 3000")

# preview table
results

# output with latex formatting
knitr::kable(results, format = "latex")
