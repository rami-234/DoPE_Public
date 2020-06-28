#in order to avoid issues with interaction terms,
#change years to 1 to 9 as opposed to 2011 to 2019
for(i in 1:9){
  df_merged$year[df_merged$year==2010+i]<-i
}

    #===============================================================#
    #POISSON REGRESSION MODEL INCLUDING YEAR AS INDEPENDENT VARIABLE#
    #===============================================================#
     #df=df_merged
     model1 =  glm(run_count ~ year+ imd + perc_bme +  pop_density + 
                     mn_dstn + perc_non_working_age + perc_bme*year + imd*year,
             data = df_merged,
             family = poisson(link = "log"),
             offset = log(total_pop))
 
     stargazer::stargazer(model1,
                          type = "text",
                          out = "./output/model1_table.txt")
     
    exp(model1$coefficients)#rate ratios

     
     #======================================#
     #POISSON REGRESSION MODEL FOR EACH YEAR#
     #======================================#
     f_model = function(x, df=df_merged) {
      
       # Model 3, same variables as in published paper
       model =  glm(run_count ~ imd + perc_bme +  pop_density + mn_dstn + perc_non_working_age,
                      data = df,
                      family = poisson(link = "log"),
                      offset = log(total_pop),
                      subset=which(df$year == x)
                      )
       
    
       # return ratio
       return(model) 
     }
    
     model3_results <- lapply(X = 1:9,
                       FUN = f_model) 
     
     #I check results from 2018 
     model3_results[[9]]$coefficients #If I want I can add ["imd"] etc
     exp(model3_results[[9]]$coefficients) # rate ratios
     #compare with publication coefficients, some different
     
     
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
               # model3_results[[10]],
               apply.coef = exp, # translate coefs into RATE RATIOS, easier to interpret
               apply.se   = exp, # same with SEs
               header = FALSE,
               column.labels	= c(# "Model 2010",
                                 "Model 2011",
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
               type="text", out="Model3_run_rate_rate_ratios_table.txt")
    
     
     ######MODEL3 COEFFICIENT PLOTS########
     #I don't know how to tead the table or unlist model3_results
     #tb<-read.table(file="Model3table.txt", header=TRUE, sep="", quote="", dec=".")
     #so I import table as file and copy coefficients manually....
     
     IMD_coeff <- c(-0.052, -0.047, -0.039, -0.037, -0.037, -0.035, -0.034, -0.034, -0.035) 
     Ethnic_D_coeff <- c(-0.449,-0.607,-1.072,-1.405,-1.509,-1.532,-1.534, -1.467, -1.414)   
     Pop_Dens_coeff <- c(-0.00001,-0.00001, -0.00001, -0.00001, -0.00001, -0.00002, -0.00001, -0.00001, -0.00001) 
       Distance_coeff <- c(-0.141, -0.136, -0.126, -0.114, -0.110, -0.100, -0.094, -0.095,-0.081)
       NW_age_coeff<- c( -0.352,-0.404,-0.401, -0.385, -0.298, -0.191, -0.086, -0.116, -0.175)
     Constant <-  c(-3.709, -3.052, -2.359, -1.917, -1.608, -1.404, -1.262, -1.119,-0.949) 
     year_for_coeff<-2011:2019
     
     coeff.df<-data.frame(year_for_coeff,IMD_coeff,Ethnic_D_coeff,Pop_Dens_coeff,Distance_coeff, NW_age_coeff, Constant)
  
    ggplot() +
       geom_point(aes(x=2011:2019,y=IMD_coeff,col="IMD")) +
       geom_line(aes(x=2011:2019,y=IMD_coeff,col="IMD"))+
      geom_point(aes(x=2011:2019,y=Ethnic_D_coeff,col="Ethnic density"))+
      geom_line(aes(x=2011:2019,y=Ethnic_D_coeff,col="Ethnic density"))+
      geom_point(aes(x=2011:2019,y=Pop_Dens_coeff,col="Population Density"))+
      geom_line(aes(x=2011:2019,y=Pop_Dens_coeff,col="Population Density"))+
      geom_point(aes(x=2011:2019,y=Distance_coeff,col="Distance"))+
      geom_line(aes(x=2011:2019,y=Distance_coeff,col="Distance"))+
      geom_point(aes(x=2011:2019,y=NW_age_coeff,col="Non-working age"))+
      geom_line(aes(x=2011:2019,y=NW_age_coeff,col="Non-working age"))+ 
      geom_point(aes(x=2011:2019,y=Constant,col="Constant"))+
      geom_line(aes(x=2011:2019,y=Constant,col="Constant"))+ 
      ggtitle("Poisson regression coefficients for each year")+
       theme(plot.title = element_text(hjust = 0.5))+
       xlab("Year (2011 to 2019)")+
       ylab("coefficients")+
       scale_x_continuous(breaks=c(round(2011:2019)))      
     
     ggsave(filename = "./output/model3coeff.png", device = "png")
     
     #Now again with rate ratios:
     IMD_coeff <- c( 0.949, 0.954, 0.962, 0.964, 0.964, 0.965, 0.966, 0.966, 0.966) 
     Ethnic_D_coeff <- c(0.996, 0.994, 0.989, 0.986, 0.985, 0.985, 0.985, 0.985, 0.986)   
     Pop_Dens_coeff <- c(1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000) 
     Distance_coeff <- c(0.869, 0.873, 0.882, 0.892, 0.896, 0.905, 0.910, 0.909, 0.922)
     NW_age_coeff<- c(0.703, 0.668, 0.670, 0.681, 0.742, 0.826, 0.917, 0.891, 0.839)
     Constant <-  c(0.025, 0.047, 0.094, 0.147, 0.200, 0.246, 0.283, 0.327, 0.387) 
     year_for_coeff<-2011:2019
     
     coeff.df<-data.frame(year_for_coeff,IMD_coeff,Ethnic_D_coeff,Pop_Dens_coeff,Distance_coeff, NW_age_coeff, Constant)
     
     ggplot() +
       geom_point(aes(x=2011:2019,y=IMD_coeff,col="IMD")) +
       geom_line(aes(x=2011:2019,y=IMD_coeff,col="IMD"))+
       geom_point(aes(x=2011:2019,y=Ethnic_D_coeff,col="Ethnic density"))+
       geom_line(aes(x=2011:2019,y=Ethnic_D_coeff,col="Ethnic density"))+
       geom_point(aes(x=2011:2019,y=Pop_Dens_coeff,col="Population Density"))+
       geom_line(aes(x=2011:2019,y=Pop_Dens_coeff,col="Population Density"))+
      geom_point(aes(x=2011:2019,y=Distance_coeff,col="Distance"))+
      geom_line(aes(x=2011:2019,y=Distance_coeff,col="Distance"))+
       geom_point(aes(x=2011:2019,y=NW_age_coeff,col="Non-working age"))+
       geom_line(aes(x=2011:2019,y=NW_age_coeff,col="Non-working age"))+ 
       geom_point(aes(x=2011:2019,y=Constant,col="Constant"))+
       geom_line(aes(x=2011:2019,y=Constant,col="Constant"))+ 
       ggtitle("Rate ratios for each year from Poisson")+
       theme(plot.title = element_text(hjust = 0.5))+
       xlab("Year (2011 to 2019)")+
       ylab("Rate ratios")+
       scale_x_continuous(breaks=c(round(2011:2019)))      
     
     ggsave(filename = "./output/model3rateratios.png", device = "png")
     
     
     
     