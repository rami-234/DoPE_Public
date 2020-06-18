#=================#
# Rami practising with parkrun data for learning purposes
#=================#

# Library
library(dplyr)

#====================#
# READ & CLEAN DATA  #
#====================#

# load aggregated finishers data
df_finishers = readRDS(file = "cleaned_data/runs_per_lsoa_2010to2020.Rds")
df_england = df_finishers[grep(pattern = "E",df_finishers$lsoa),] # restrict to England

#change the date format
df_england$date = as.Date(df_england$date) 

# filter only to 05 August 2017 
df_05Aug2017_temp = df_england %>% filter (date=="2017-08-05")
#delete column reporting the date (always the same)
df_05Aug2017= df_05Aug2017_temp[,2:3]
#change column names so they match column names of file runs_per_lsoa_010117_101218
colnames(df_05Aug2017) = c("code","run_count")

write.csv(df_05Aug2017,"./cleaned_data/runs_per_lsoa_05Aug2017.csv",row.names = F)


