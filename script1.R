library(NHANES)
d <- NHANES
names(d)
length(unique(d$ID))
library(dplyr)
ds <- d %>%
  select(Age, Gender, Diabetes, BMI, HHIncome, PhysActive)