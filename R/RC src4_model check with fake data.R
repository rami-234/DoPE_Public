df.fake  = data.frame(run_count=c(20,30,40,50,60,70,80,90,100,
                                  25,35,45,55,65,75,85,95,105),
                                  year = c(2011:2019,2011:2019), 
                                 imd = rep(c(10,50,90),each=6), 
                                  pop_density = 4423.724,
                                  mn_dstn=c(3,3.5,4,4.5,5,6,6.5,7,7.5,
                                            8,8.5,9,9.5,10,10.5,11,11.5,12),
                                  perc_non_working_age=c(
                                    0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
                                    0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49),
                                  total_pop=1296,
                                   perc_bme=c(0.15,0.16,0.17,0.18,0.19,0.20,0.21,0.22,0.23,
                                              0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.30,0.31)
                                  )
                    

model1 =  glm(run_count ~ year+ imd + perc_bme +  pop_density + 
                mn_dstn + perc_non_working_age + perc_bme*year + imd*year,
              data = df_merged,
              family = poisson(link = "log"),
              offset = log(total_pop))                        

run_count_fake = predict(model1, newdata=df.fake,type='response')
                      
      # so use mean(df$.) for everything you want to hold constant, and input different imd values and then plot it with
        ggplot(df.fake) +
        geom_point(aes(x=year,y=run_count_fake,col=as.factor(imd))) +
        geom_line(aes(x=year,y=run_count_fake,col=as.factor(imd))) 
                                                 
                                                 