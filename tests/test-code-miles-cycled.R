## Test miles cycled bin values 
## Assumes that the 'milesCycled.pers' already exists in memory
# Subset milescycled for England
mc_region <- subset(milesCycled.pers, HHoldGOR_B02ID == 0)
# Subset columns for baseline and the first scenario 'MS0.05_ebik0_eq0'
mc_two_col <- td1 <- subset(td, select = c(baseline, MS0.05_ebik0_eq0))
# Create a table with bins for baseline
bltb <- as.data.frame(table (cut (td1$baseline, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(td1$baseline)))))
# Create a table with bins for scenario
sctb <- as.data.frame(table (cut (td1$MS0.05_ebik0_eq0, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(td1$MS0.05_ebik0_eq0)))))
