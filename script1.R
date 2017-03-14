# load the NHANES package and dataset
library(NHANES)
d <- NHANES
names(d)

# we'll work with a subset of the data
# we'll select 6 variables
# we will look at the impact of 5 different 
# variables for predicting diabetes
library(dplyr)
ds <- d %>%
  select(Age, Gender, Diabetes, BMI, HHIncome, PhysActive)

# change ds back to a regular
# data.frame object since rattle
# doesn't handle the updated tbl_df format
# created using the dplyr functions
ds <- as.data.frame(ds)

# load the rattle package
library(rattle)

# to invoke the GUI, run the rattle() command
rattle()

