# generate and merge data 


#===
# SET-UP
#===
rm(list=ls())
#install.packages("geosphere")
#install.packages("rgdal")
library(sp)
library(geosphere)
library(dplyr)
library(raster)
library(rgdal)

# LSOA parkrun participation
df_finishers = readRDS(file = "cleaned_data/runs_per_lsoa_2010to2020.Rds")
#df_england = df_finishers[grep(pattern = "E",df_finishers$lsoa),] # restrict to England
df_england = df_finishers[substr(x = df_finishers$lsoa,start = 1,stop = 1) == "E",] # restrict to England
# aggregate up by year 
df_england$date = as.Date(df_england$date)   
df_england$year = df_england$date %>% format("%Y")
df_aggregate = df_england %>% group_by(year,lsoa) %>% summarise(finishers = sum(finishers)) %>% ungroup()
colnames(df_aggregate) = c("year","code","run_count")

# LSOA centroids
#RC note: LSOA centroids from Paul's file rather than from code
lsoa_cntrds <- read.csv("lsoa_cntrds.csv")

# English parkrun event locations 2018
parkrun_events = read.csv("./raw_data/parkrun_data/event_info_scraped_10.12.18.csv", stringsAsFactors = F)[,-1]

# LSOA distance to nearest parkrun event
dist_M_full <- geosphere::distm(lsoa_cntrds[,2:3],parkrun_events[,2:3])
lsoa_distance <- apply(dist_M_full,1,FUN= function(x){round(min(x),0)} )
lsoa_distance <- data.frame(code = lsoa_cntrds$code,
                            mn_dstn = lsoa_distance / 1000) # in km
rm("dist_M_full","parkrun_events","lsoa_cntrds")

# LSOA total population
lsoa_pop = read.csv("./raw_data/IoD2019_Population_Denominators.csv",stringsAsFactors = F)
lsoa_pop[,-c(1:4)] = data.frame(apply(lsoa_pop[,-c(1:4)],2,function(x){as.numeric(as.character(gsub(",","",x)))}),stringsAsFactors = F)
lsoa_pop = lsoa_pop[,c(1,5,7)]
names(lsoa_pop) = c("code","total_pop","perc_non_working_age")
lsoa_pop$perc_non_working_age = 1-(lsoa_pop$perc_non_working_age / lsoa_pop$total_pop)

# density
lsoa_density = read.csv("./raw_data/Mid-2017 Population Density.csv",stringsAsFactors = F)[,c(1,4)]
lsoa_density = rename(lsoa_density, code = Code)
lsoa_density = merge(lsoa_density,lsoa_pop[,1:2],by="code")
lsoa_density$pop_density = round(lsoa_density$total_pop / lsoa_density$Area.Sq.Km,2)
lsoa_density = lsoa_density[,c(1,4)]

# LSOA IMD score
lsoa_imd = read.csv("./raw_data/IoD2019_Scores.csv", stringsAsFactors = F)
lsoa_imd = lsoa_imd[,-c(2:4,13:20)]
names(lsoa_imd) = c("code","imd","d_income","d_employment","d_education","d_health","d_crime","d_housing","d_enviroment")

# ethnicity
lsoa_ethnicity = read.csv("raw_data/LSOA_Ethnicity.csv",stringsAsFactors = F)
lsoa_ethnicity = lsoa_ethnicity[,3:5]
lsoa_ethnicity = data.frame(code = lsoa_ethnicity$geography.code,
                            perc_bme = 1-lsoa_ethnicity$Sex..All.persons..Age..All.categories..Age..Ethnic.Group..White..Total..measures..Value/lsoa_ethnicity$Sex..All.persons..Age..All.categories..Age..Ethnic.Group..All.categories..Ethnic.group..measures..Value)
lsoa_ethnicity = lsoa_ethnicity[!(grepl("W",lsoa_ethnicity$code)),]                            

# rural urban classification
lsoa_ruralurban <- read.csv("./raw_data/LSOA_Rural_Urban_Classification_2011.csv",stringsAsFactors = F) %>% 
  mutate(urban = RUC11CD %in% c("A1","B1", "C1","C2"))

#if 0 runs, combination LSOA-year missing from df_aggregate
#to addresss this, first create a dataframe with all combinations LSOA-Year
df_all_lsoas_years<-expand.grid(unique(df_aggregate$year),lsoa_imd$code) 
names(df_all_lsoas_years)<-c("year","code")
#if not aggregated, expand.grid gives massive dataset
#but here aggregated per year first then merged so ok,  sum will work
#(otherwise possible strategies would be: keep essential columns and if it crashes use less years or just sheffield)
#I check that n of LSOAs is the same for each year now:
n_lsoa=function(x,df=df_all_lsoas_years) {
  model=print(nrow(df_all_lsoas_years[df_all_lsoas_years$year==x,]))
  return(n_lsoa)}
n_lsoa_per_year <- lapply(X = 2010:2019,
                          FUN = n_lsoa)
#always the same n, 32844, good

#merge run_counts with all possible combinations of LSOA-Year
df_lsoa_year_runs<-merge(df_aggregate,df_all_lsoas_years,by=c("code","year"), all="TRUE")
#reduce function code below is exactly the same as merge code
#df_lsoa_year_runs = Reduce(function(x, y) merge(x, y,by=c("code","year"), all=TRUE), list(df_aggregate,df_all_lsoas_years))
# when I check this 
#it seems to have worked by looking at dataset BUT
#I check if n of LSOAs always the same for each year, should be 32844:
n_lsoa=function(x,df=df_lsoa_year_runs) {
  model=print(nrow(df_lsoa_year_runs[df_lsoa_year_runs$year==x,]))
  return(n_lsoa)}
n_lsoa_per_year <- lapply(X = 2010:2019,
                          FUN = n_lsoa)
#number of LSOAs goes from 33098 to 33643, why?? 
#something went wrong.. a problem with df_aggregate?

#merge lsoa, year and number of runs with all demographic/socio-economic info about LSOAs 
lsoa_df = Reduce(function(x, y) merge(x, y,by="code", all=TRUE), list(df_lsoa_year_runs, lsoa_distance, lsoa_imd, lsoa_pop,lsoa_density,lsoa_ethnicity,lsoa_ruralurban))
#most variables except run_count are NAs for initial codes such as 95EE04W1 until code E01000001
#Most LSOAs have all years but in messy order, e.g. 2017 before 2010, why?

#change NAs to 0
lsoa_df$run_count[is.na(lsoa_df$run_count)] = 0

#checks to see if dataset is ok:

#check: average ethnic density per year should always be the same
avrg_perc_bme=function(x,df=lsoa_df) {
  model=print(mean(lsoa_df$perc_bme[lsoa_df$year==x],  na.rm=T))
  return(avrg_perc_bme)}
avrg_perc_bme_per_year <- lapply(X = 2010:2019,
                                 FUN = avrg_perc_bme)
#always the same, great

#check: average imd score per year should always be the same
avrg_imd=function(x,df=lsoa_df) {
  model=print(mean(lsoa_df$imd[lsoa_df$year==x],  na.rm=T))
  return(avrg_imd)} 
avrg_imd_per_year <- lapply(X = 2010:2019,
                            FUN = avrg_imd)
#always the same, great

#I check that the number of lsoas is the same for each year
n_lsoa=function(x,df=lsoa_df) {
  model=print(nrow(lsoa_df[lsoa_df$year==x,]))
  return(n_lsoa)}
n_lsoa_per_year <- lapply(X = 2010:2019,
                          FUN = n_lsoa)
#not the same for each year, ranges from 35007 to 35552, why?

#export dataset to csv format
write.csv(lsoa_df,"./output/lsoa_df_08oct2010_28dec2019.csv",row.names = F)

#I make a random check on the csv file
df_merged <- read.csv("./output/lsoa_df_08oct2010_28dec2019.csv")
#I check a random LSOA
tr<-df_merged[df_merged$code=="E01027818",]
#seems fine
