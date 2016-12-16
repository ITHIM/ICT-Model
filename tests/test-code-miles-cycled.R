## Test miles cycled bin values 
## Assumes that the 'milesCycled.pers' already exists in memory

# Specify region
local_region <- 0
# Specify scenario
local_sc <- "MS0.05_ebik0_eq0"

# Subset miles cycled for a region
mc_region <- subset(milesCycled.pers, HHoldGOR_B02ID == local_region)
# Subset columns for baseline and the selected scenario
mc_two_col <- subset(mc_region, select = c("baseline", local_sc))
# Create a table with bins for baseline
bltb <- as.data.frame(table (cut (mc_two_col$baseline, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(mc_two_col$baseline)))))
# Create a table with bins for scenario
sctb <- as.data.frame(table (cut (mc_two_col[[local_sc]], breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(mc_two_col[[local_sc]])))))
# Add a combined df
combined_bin_df <- cbind(bltb, sctb)
# Name the columns
names(combined_bin_df) <- c("baseline", "b-freq", local_sc, "sc-freq")
