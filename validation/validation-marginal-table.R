library(tidyverse)
devtools::load_all()

# reload intermediate marginal coverage results
results = readRDS(file = "validation/marginal-coverage.Rds")

# calculate marginal coverages
marg_covs = Reduce("+", results[sapply(results, Negate(anyNA))]) / length(results[sapply(results, Negate(anyNA))])

# output with latex formatting
knitr::kable(marg_covs, format = "latex")
