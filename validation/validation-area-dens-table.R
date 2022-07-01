# load packages and seam functions
library(tidyverse)
devtools::load_all()

# load previously calculated results
res_dns_1000 = readRDS(file = "validation/conditional-area-dens-1000.Rds")
res_dns_2000 = readRDS(file = "validation/conditional-area-dens-2000.Rds")
res_dns_3000 = readRDS(file = "validation/conditional-area-dens-3000.Rds")

# calculate constant for density calculations
x_diff = (150 + 150) / 99
y_diff = (200 + 30) / 99
c = x_diff * y_diff

res = rbind(
  c * colMeans(res_dns_1000),
  c * colMeans(res_dns_2000),
  c * colMeans(res_dns_3000)
)

colnames(res) = c("seam", "pitcher", "batter")
rownames(res) = c("n = 1000", "n = 2000", "n = 3000")

# preview table
res

# output with latex formatting
knitr::kable(res, format = "latex")
