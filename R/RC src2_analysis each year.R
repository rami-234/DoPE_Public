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

df_merged <- read.csv("./output/lsoa_df_08oct2010_28dec2019.csv") %>%
  dplyr::select(year,run_count,imd, perc_bme, mn_dstn,  
                total_pop, pop_density,perc_non_working_age)%>%
  mutate(run_rate = if_else(year=="2010", run_count/total_pop/12*1000, run_count/total_pop/52*1000)) 

#I take out 2010
df_merged<-df_merged[df_merged$year!="2010",]

#I change years to 1 to 9 as opposed to 2011 to 2019
df_merged$year[df_merged$year=="2011"]<-1
df_merged$year[df_merged$year=="2012"]<-2
df_merged$year[df_merged$year=="2013"]<-3
df_merged$year[df_merged$year=="2014"]<-4
df_merged$year[df_merged$year=="2015"]<-5
df_merged$year[df_merged$year=="2016"]<-6
df_merged$year[df_merged$year=="2017"]<-7
df_merged$year[df_merged$year=="2018"]<-8
df_merged$year[df_merged$year=="2019"]<-9


#================================#
#DESCRIPTIVE STATS FOR EACH YEAR #
#================================#

#average number of finishers per year
avrg_run_count=function(x,df=df_merged) {
  model=print(mean(df_merged$run_count[df_merged$year==x],na.rm=T))
return(avrg_run_count)}
avrg_run_count_per_year <- lapply(X = 2011:2019,
                  FUN = avrg_run_count)


#I had to add na.rm but why? I had transformed NAs for run_count to 0..
#I check
sum(is.na(df_merged$run_count)) #0, fine
#but still unclear why I had to add na.rm

#plot
avrg_run_count_per_year = unlist(avrg_run_count_per_year)
avrg_run_count_per_year = as.numeric(avrg_run_count_per_year)
ggplot() +
  geom_point(aes(x=2011:2019,y=avrg_run_count_per_year)) +
               geom_line(aes(x=2011:2019,y=avrg_run_count_per_year))

#average run rate per year
avrg_run_rate=function(x,df=df_merged) {
  model=print(mean(df_merged$run_rate[df_merged$year==x], na.rm=T))
  return(avrg_run_rate)}
avrg_run_rate_per_year <- lapply(X = 2011:2019,
                                  FUN = avrg_run_rate)


#number of local authorities with 0 runs per year
zero_runs_lsoa=function(x,df=df_merged) {
  model=print(length(df_merged$run_count[(df_merged$year==x) & (df_merged$run_count==0)]))
  return(zero_runs_lsoa)}
zero_runs_per_year <- lapply(X = 2011:2019,
                                 FUN = zero_runs_lsoa)
#decreases over time, as expected



    #===============================================================#
    #POISSON REGRESSION MODEL INCLUDING YEAR AS INDEPENDENT VARIABLE#
    #===============================================================#
     df=df_merged
     model1 =  glm(run_count ~ year+ imd + perc_bme +  pop_density + 
                     mn_dstn + perc_non_working_age + perc_bme*year + imd*year,
             data = df,
             family = poisson(link = "log"),
             offset = log(total_pop))
 
     stargazer::stargazer(model1,
                          type = "text",
                          out = "model1_table.txt")

     #======================================#
     #POISSON REGRESSION MODEL FOR EACH YEAR#
     #======================================#
     f_model = function(x, df=df_merged) {
      
       # Model 3: Poisson model with IMD and Ethnic density (and controls??) 
       #prioritise this for now and leave out model 1 and 2
       model =  glm(run_count ~ imd + perc_bme +  pop_density + mn_dstn + perc_non_working_age,
                      data = df,
                      family = poisson(link = "log"),
                      offset = log(total_pop),
                      subset=which(df$year == x)
                      )
       
    
       # return ratio
       return(model) 
     }
    
     model3_results <- lapply(X = 2010:2019,
                       FUN = f_model) 
     
     #I check results from 2018 
     model3_results[[9]]$coefficients #If I want I can add ["imd"] etc
     #coefficients quite different from publication.. why?
     #similar: constant -0.794, imd -0.035, #distance 0.092 
     #different: 
     #ethnic density -1.37 (-0.052 in publication)
     #pop density -0.000019 (-0.070 in publication)
     #age -0.781 (-0.01 in publication)
     
     
     #======================================#
     #TABLE WITH MODEL RESULTS FOR EACH YEAR#
     #======================================#
     #to be outputted as text
     stargazer(model3_results[[1]], 
               model3_results[[2]], 
               model3_results[[3]],
               model3_results[[4]],
               model3_results[[5]],
               model3_results[[6]],
               model3_results[[7]],
               model3_results[[8]],
               model3_results[[9]],
               model3_results[[10]],
               header = FALSE,
               column.labels	= c("Model 2010","Model 2011",
                                 "Model 2012", "Model 2013",
                                 "Model 2014", "Model 2015",
                                 "Model 2016", "Model 2017",
                                 "Model 2018", "Model 2019"),
               ci=FALSE, ci.level=0.95, #font.size= 9, 
               title="Poisson Log-link GLM Results",
               dep.var.labels = "Participation",
               covariate.labels = c("IMD Score",
                                    "Ethnic-Density",
                                    "Pop Density",
                                    "Distance(km)",
                                    "Non-working-age"),
               type="text", out="Model3table.txt")
    
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
     for(i in 2010:2019){ 
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
   
ggsave(filename = paste0("This_file_is_from",i,".png",sep="") ,plot = year.plot,device="png") }
     
      


