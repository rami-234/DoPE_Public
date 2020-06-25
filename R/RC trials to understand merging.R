#trials to understand how merging works

#create two datasets to be merged
dat_runs <- data.frame ("Year"=c(2010,2010,2011,2011), "LSOA" = c(1,2,1,3), "run_count" = c(21,15,10,4))
dat_imd <- data.frame("LSOA" = c(1:4), "IMD" = c(5,6,7,8))

#try merging in a simple way by LSOA
#including 'all="TRUE"' to have all LSOAs
#use two different methods which give the same result
trial_merge<-merge(dat_runs,dat_imd,by="LSOA", all="TRUE")
trial_reduce= Reduce(function(x, y) merge(x, y,by="LSOA", all=TRUE), list(dat_runs,dat_imd))
#all LSOAs are in there but not all combinations LSOA-Year

#I create a dataframe with all combinations LSOA and Year
trial_aggr_all<-expand.grid(unique(dat_runs$Year),dat_imd$LSOA)
names(trial_aggr_all)<-c("Year","LSOA")

#I merge all possible combinations LSOA-Year with run_count
#I use two different methods which give the same result
trial2_merge<-merge(dat_runs,trial_aggr_all,by=c("LSOA","Year"), all="TRUE")
trial2_reduce= Reduce(function(x, y) merge(x, y,by=c("LSOA","Year"), all=TRUE), list(dat_runs,trial_aggr_all))

