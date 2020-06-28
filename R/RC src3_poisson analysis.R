#in order to avoid issues with interaction terms,
#change years to 1 to 9 as opposed to 2011 to 2019
df_merged$year[df_merged$year=="2011"]<-1
df_merged$year[df_merged$year=="2012"]<-2
df_merged$year[df_merged$year=="2013"]<-3
df_merged$year[df_merged$year=="2014"]<-4
df_merged$year[df_merged$year=="2015"]<-5
df_merged$year[df_merged$year=="2016"]<-6
df_merged$year[df_merged$year=="2017"]<-7
df_merged$year[df_merged$year=="2018"]<-8
df_merged$year[df_merged$year=="2019"]<-9

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
                          out = "./output/model1_table.txt")

     
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
               header = FALSE,
               column.labels	= c("Model 2011",
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
     
    
     