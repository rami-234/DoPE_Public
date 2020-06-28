#=======#
# SETUP #
#=======#

#install.packages("corrplot")
#install.packages("stargazer")

library(stargazer)
library(reshape2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library("viridis")
library(corrplot)
library(ggplot2)
library(fitdistrplus)
library(stargazer)

#===========#
# LOAD DATA #
#===========#

df_merged <- read.csv("./output/lsoa_df_08oct2010_28dec2019.csv") 

#I take out 2010 as only from October
df_merged<-df_merged[df_merged$year!="2010",]

  df_merged<-df_merged%>%
  dplyr::select(year,run_count,imd, perc_bme, mn_dstn,  
                total_pop, pop_density,perc_non_working_age, urban)%>%
  mutate(run_rate = run_count/total_pop/52*1000) 
  #=============#
  #BASIC SUMMARY#
  #=============#
  summary(df_merged$run_count)
  summary(df_merged$run_rate)
  summary(df_merged$imd)
  summary(df_merged$perc_bme)
  summary(df_merged$mn_dstn)
  summary(df_merged$pop_density)
  summary(df_merged$total_pop)
  
  
#================================#
#DESCRIPTIVE STATS FOR EACH YEAR #
#================================#

#average number of finishers per year
avrg_run_count=function(x,df=df_merged) {
  model=mean(df_merged$run_count[df_merged$year==x],na.rm=T)
  return(model)}
avrg_run_count_per_year <- lapply(X = 2011:2019,
                                  FUN = avrg_run_count)

#checks on the above
#I had to add na.rm but why? I had transformed NAs for run_count to 0..
sum(is.na(df_merged$run_count)) #0, fine, no NAs
#but still unclear why I had to add na.rm

#for the plot, I need to transform list to numeric:
avrg_run_count_per_year_v= as.numeric(unlist(avrg_run_count_per_year))
#checks:
#class(avrg_run_count_per_year_v)
#class(avrg_run_count_per_year[[1]])

plot_avrg_runs<-ggplot() +
  geom_point(aes(x=2011:2019,y=avrg_run_count_per_year_v)) +
  geom_line(aes(x=2011:2019,y=avrg_run_count_per_year_v))+
  ggtitle("Average number of runs per LSOA per year")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year (2011 to 2019)")+
  ylab("Number of runs")+
  scale_x_continuous(breaks=c(round(2011:2019)))

ggsave(filename = "./output/avrg_runs_plot.png", device = "png")

#average run rate per year (run rate=weekly runs)
avrg_run_rate=function(x,df=df_merged) {
  model=print(mean(df_merged$run_rate[df_merged$year==x], na.rm=T))
  return(model)}
avrg_run_rate_per_year <- lapply(X = 2011:2019,
                                 FUN = avrg_run_rate)

#for the plot, I need to transform list to numeric:
avrg_run_rate_per_year_v= as.numeric(unlist(avrg_run_rate_per_year))
#checks:
class(avrg_run_rate_per_year_v)
class(avrg_run_rate_per_year[[1]])

plot_avrg_runs<-ggplot() +
  geom_point(aes(x=2011:2019,y=avrg_run_rate_per_year_v)) +
  geom_line(aes(x=2011:2019,y=avrg_run_rate_per_year_v))+
  ggtitle("Average number of weekly runs per 1000 per year")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year (2011 to 2019)")+
  ylab("Weekly runs")+
  scale_x_continuous(breaks=c(round(2011:2019)))

ggsave(filename = "./output/weekly_runs_plot.png", device = "png")

#number of local authorities with 0 runs per year
zero_runs_lsoa=function(x,df=df_merged) {
  model=print(length(df_merged$run_count[(df_merged$year==x) & (df_merged$run_count==0)]))
  return(model)}
zero_runs_per_year <- lapply(X = 2011:2019,
                             FUN = zero_runs_lsoa)
#decreases over time, as expected

#for the plot, I need to transform list to numeric:
zero_runs_per_year_v= as.numeric(unlist(zero_runs_per_year))
#checks:
class(zero_runs_per_year_v)
class(zero_runs_per_year_v[[1]])

plot_avrg_runs<-ggplot() +
  geom_point(aes(x=2011:2019,y=zero_runs_per_year_v)) +
  geom_line(aes(x=2011:2019,y=zero_runs_per_year_v))+
  ggtitle("LSOAs with 0 runs per year")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year (2011 to 2019)")+
  ylab("LSOAS with 0 runs")+
  scale_x_continuous(breaks=c(round(2011:2019)))

ggsave(filename = "./output/0_runs_plot.png", device = "png")

#average number of urban and rural finishers per year
avrg_run_count_u=function(x,df=df_merged) {
  model=mean(df_merged$run_count[(df_merged$year==x)&(df_merged$urban=="TRUE")],na.rm=T)
  return(model)}
avrg_run_count_u_per_year <- lapply(X = 2011:2019,
                                  FUN = avrg_run_count_u)
#transform to numeric for the plot
avrg_run_count_u_per_year_v= as.numeric(unlist(avrg_run_count_u_per_year))

avrg_run_count_r=function(x,df=df_merged) {
  model=mean(df_merged$run_count[(df_merged$year==x)&(df_merged$urban=="FALSE")],na.rm=T)
  return(model)}
avrg_run_count_r_per_year <- lapply(X = 2011:2019,
                                    FUN = avrg_run_count_r)

#for the plot, I need to transform list to numeric:
avrg_run_count_r_per_year_v= as.numeric(unlist(avrg_run_count_r_per_year))

ggplot() +
  geom_point(aes(x=2011:2019,y=avrg_run_count_u_per_year_v,col="urb")) +
  geom_line(aes(x=2011:2019,y=avrg_run_count_u_per_year_v,col="urb"))+
  geom_point(aes(x=2011:2019,y=avrg_run_count_r_per_year_v,col="rur")) +
  geom_line(aes(x=2011:2019,y=avrg_run_count_r_per_year_v,col="rur"))+
  ggtitle("Mean n of runs per LSOA by rural and urban")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Year (2011 to 2019)")+
  ylab("Number of runs")+
  scale_x_continuous(breaks=c(round(2011:2019)))

ggsave(filename = "./output/avrg_rural_urban_runs.png", device = "png")

###SCATTER PLOTS#######
#ggplot(df_merged, aes(x=imd,y=run_count))+
  #geom_point()
#ggsave(filename = "./output/corr_runs_imd.png", device = "png")

ggplot(df_merged[df_merged$year=="2011",], aes(x=imd,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2011 by LSOA IMD score")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("IMD score (higher=more deprived)")+
  ylab("Yearly runs")+
  ylim(0,1500)
#ggsave(filename = "./output/runs_imd_2011.png", device = "png")

ggplot(df_merged[df_merged$year=="2015",], aes(x=imd,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2015 by LSOA IMD score")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("IMD score (higher=more deprived)")+
  ylab("Yearly runs")+
  ylim(0,1500)
#ggsave(filename = "./output/runs_imd_2015.png", device = "png")

ggplot(df_merged[df_merged$year=="2019",], aes(x=imd,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2019 by LSOA IMD score")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("IMD score (higher=more deprived)")+
  ylab("Yearly runs")+
  ylim(0,1500)
#ggsave(filename = "./output/runs_imd_2019.png", device = "png")

ggplot(df_merged[df_merged$year=="2011",], aes(x=perc_bme,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2011 by LSOA ethnic minority density")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion BME")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2015",], aes(x=perc_bme,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2015 by LSOA ethnic minority density")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion BME")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2019",], aes(x=perc_bme,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2019 by LSOA ethnic minority density")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion BME")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2011",], aes(x=mn_dstn,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2011 by distance to LSOA centroid")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Distance")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2015",], aes(x=mn_dstn,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2015 by distance to LSOA centroid")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Distance")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2019",], aes(x=mn_dstn,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2019 by distance to LSOA centroid")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Distance")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2011",], aes(x=pop_density,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2011 by LSOA population density")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Population density")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2015",], aes(x=pop_density,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2015 by LSOA population density")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Population density")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2019",], aes(x=pop_density,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2019 by LSOA population density")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Population density")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2011",], aes(x=perc_non_working_age,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2011 by LSOA proportion NW age")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion non-working age")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2015",], aes(x=perc_non_working_age,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2015 by LSOA proportion NW age")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion non-working age")+
  ylab("Yearly runs")+
  ylim(0,1500)

ggplot(df_merged[df_merged$year=="2019",], aes(x=perc_non_working_age,y=run_count))+
  geom_point()+
  ggtitle("Runs in 2019 by LSOA proportion NW age")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion non-working age")+
  ylab("Yearly runs")+
  ylim(0,1500)

#===========================#
# COLOUR PLOT FOR EACH YEAR #
#===========================#
df <- read.csv("./output/lsoa_df_08oct2010_28dec2019.csv") %>% 
  mutate(urban = if_else(urban==TRUE, "Urban", "Rural"),
         urban = factor(urban, levels = c("Urban","Rural")),
         run_rate = if_else(year=="2010", run_count/total_pop/12*1000, run_count/total_pop/52*1000),
         imd_dec = cut(x = imd,
                       breaks = seq(0,100,10),        #  quantile(imd,seq(0,1,0.1)),
                       ordered_result = T,
                       labels = F)*10,
         bme_dec= cut(x = perc_bme,
                      breaks = seq(0,1,0.1),                  # quantile(perc_bme,seq(0,1,0.1)),
                      ordered_result = T,
                      labels = F)*10)%>%
  
  melt(id.vars = c("code","imd_dec","bme_dec","urban"),
       measure.vars ="run_rate", 
       value.name = "run_rate") %>%
  
  dplyr::select(imd_dec,bme_dec,run_rate,urban)

# aggregate data by deprivation and ethnic density
df <- aggregate(run_rate ~ bme_dec + imd_dec + urban, #+ #pop_density_bins, 
                data = df, 
                FUN= "mean")

# create and save colour plot for each year
for(i in 2011:2019){ 
  subset.df = subset(df, year = i)   
  year.plot = ggplot(subset.df) +      
    aes(as.factor(bme_dec), as.factor(imd_dec), fill= run_rate) + 
    geom_tile()+
    theme_classic()+
    scale_fill_viridis(discrete=FALSE,name = "Participation \n Rate") +
    xlab("Ethnic Density (%)")+
    ylab("Index of Multiple Deprivation (0-100)")  + 
    facet_wrap(~urban, nrow = 1) +
    labs(caption="Sources: Office for National Statistics \n and parkrunUK")+
    theme(legend.position = c(0.92,0.5))+
    theme(axis.text.x = element_text(hjust = -0),
          axis.text.y = element_text(vjust = -2),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())+
    annotate("text", x=8.5,y=9.5, label = "Most Deprived & \n Highest Ethnic Density", color = "black", size = 2, fontface = "bold")
  
  ggsave(filename = paste0("./output/This_file_is_from",i,".png",sep="") ,plot = year.plot,device="png") }




