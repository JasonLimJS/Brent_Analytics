#Step 1: Extract all data sets from the master file
dat_extract("C:/Users/jason/Desktop/Seeking Alpha/BP")

#Step 2: Arrange PMI data
PMI_dat_set<- c("ChinaPMI","USPMI","GermanyPMI")  
country_set<- c("China","US","Germany")
combin_PMI<- PMI_combin(PMI_dat_set,country_set)
combin_PMI_lm<- combin_PMI[,c(1,5)]
sql_stat<- "SELECT * FROM combin_PMI_lm ORDER BY ID"
combin_PMI_lm<- sqldf(sql_stat)
combin_PMI_lm<- func_transform(combin_PMI_lm,"combin_PMI")
combin_PMI_lm<- year_func(combin_PMI_lm)
combin_PMI_lm<- id_filter(combin_PMI_lm,2010,4,2018,4)

#Step 3: Arrange Car Production Data

sql_stat<-"
SELECT a.ID, a.Production as Toyo_prod,
b.Production as Volk_prod FROM ToyotaCarProduction as a 
left join VolkswagenCarProduction as b ON a.ID=b.ID ORDER BY a.ID"
car_prod<- sqldf(sql_stat)
car_prod$prod<- car_prod$Toyo_prod + car_prod$Volk_prod
car_prod_dat<- car_prod[,c(-2,-3)]
car_prod_lm<- func_transform(car_prod_dat,"prod")
car_prod_lm<- year_func(car_prod_lm)
car_prod_lm<- id_filter(car_prod_lm,2010,4,2018,4)

#Step 4: Arrange US GDP Data
US_GDP_lm<- func_transform(USRealGDP,"US_Real_GDP")
US_GDP_lm<- year_func(US_GDP_lm)
US_GDP_lm<- id_filter(US_GDP_lm,2010,4,2018,4)

#Step 5: Arrange China GDP Data
China_GDP_lm<- func_transform(ChinaRealGDP,"China_Real_GDP")
China_GDP_lm<- year_func(China_GDP_lm)
China_GDP_lm<- id_filter(China_GDP_lm,2010,4,2018,4)

#Step 6: Arrange Saudi Production
saudi_production$saudi_production<- exp(saudi_production$saudi_production)
saudi_lm<- func_transform(saudi_production,"saudi_production")
saudi_lm<- year_func(saudi_lm)
saudi_lm<- id_filter(saudi_lm,2010,4,2018,4)

#Step 7: Arrange OPEC Spare Capacity
opec_spcap_lm<- func_transform(opec_spare_capacity,"spare_capacity")
opec_spcap_lm<- year_func(opec_spcap_lm)
opec_spcap_lm<- id_filter(opec_spcap_lm,2010,4,2018,4)

#Step 8: Arrange Rig Count
sql_stat<-"
SELECT ID,AVG(US_Oil_Rig_Count) as US_Oil_Rig_Count
FROM US_Oil_Rig_Count 
GROUP BY ID
ORDER BY ID"
rig_count<- sqldf(sql_stat)
rig_count_lm<- func_transform(rig_count,"US_Oil_Rig_Count")
rig_count_lm<- year_func(rig_count_lm)
rig_count_lm<- id_filter(rig_count_lm,2010,4,2018,4)

#Step 9: Arrange depdendent variables
sql_stat<- "SELECT ID, AVG(Brent_Crude_Oil_Price) as avg_brent
FROM BrentCrudeOil 
GROUP BY ID
ORDER BY ID"
brent_group<- sqldf(sql_stat)
brent_lm<- func_transform(brent_group,"avg_brent")
brent_lm<- year_func(brent_lm)
brent_lm<- id_filter(brent_lm,2010,4,2018,4)
plot(ts(brent_lm$ln_avg_brent_q1))
adf.test(ts(brent_lm$ln_avg_brent_q1))
#ln_avg_brent_q2 is stationary (Type 1,2,3)

#10. Building a set of models
#10.1. Create variable vectors
indvar_vector<- c("ln_y","ln_y_q1","ln_y_q1_d1")
indvar<- c("combin_PMI","prod","US_Real_GDP","China_Real_GDP","saudi_production","spare_capacity","US_Oil_Rig_Count")

for (i in 1:length(indvar)){
  assign(paste0("var_",i),gsub("y",indvar[i],indvar_vector))
}

var_combin<- c(var_1,var_2)
  for (i in 3:length(indvar)){
    var_combin<- c(var_combin,get(paste0("var_",i)))
  }

#10.2. Create data set vectors
dat_vector<-c("combin_PMI_lm","car_prod_lm","US_GDP_lm","China_GDP_lm","saudi_lm","opec_spcap_lm","rig_count_lm")

for (i in 1:length(dat_vector)){
  assign(paste0("dat_",i),rep(dat_vector[i],each=3))
}

dat_combin<- c(dat_1,dat_2)
for (i in 3:length(dat_vector)){
  dat_combin<- c(dat_combin,get(paste0("dat_",i)))
}

all_dat<- dep_ind_com("brent_lm",dat_combin[1],"ln_avg_brent",var_combin[1])


for (i in 2:length(dat_combin)){
  all_dat<-dep_ind_com2("all_dat",dat_combin[i],var_combin[i])
}

#10.3.1. Model Creation- with 3 variables
dep_array<- as.data.frame(t(combn(var_combin,m=3)))
names(dep_array)<- c("Var_1","Var_2","Var_3")
dep_array$ind<- rep("ln_avg_brent",nrow(dep_array))


for (i in 1: nrow(dep_array)){
  var_1<- as.character(dep_array[i,1])
  var_2<- as.character(dep_array[i,2])
  var_3<- as.character(dep_array[i,3])
  
  model<-lm(reformulate(c(var_1,var_2,var_3),"ln_avg_brent"),data=all_dat)
  model_HAC<- coeftest(model, type="HC0")
  intercept<- as.numeric(model$coefficients[1])
  beta_1<- as.numeric(model$coefficients[2])
  beta_2<- as.numeric(model$coefficients[3])
  beta_3<- as.numeric(model$coefficients[4])
  intercept_se<- as.numeric(model_HAC[1,2])
  beta_1_se<- as.numeric(model_HAC[2,2])
  beta_2_se<- as.numeric(model_HAC[3,2])
  beta_3_se<- as.numeric(model_HAC[4,2])
  intercept_pval<- as.numeric(model_HAC[1,4])
  beta_1_pval<- as.numeric(model_HAC[2,4])
  beta_2_pval<- as.numeric(model_HAC[3,4])
  beta_3_pval<- as.numeric(model_HAC[4,4])
  model_summ<-summary(model)
  adj_r_sqrt<- model_summ$adj.r.squared
  d1<- as.numeric(model_summ$fstatistic[1])
  d2<- as.numeric(model_summ$fstatistic[2])
  d3<- as.numeric(model_summ$fstatistic[3])
  f_stat<- pf(d1,d2,d3,lower.tail=FALSE)
  
  coef_vector<- c("intercept","beta_1","beta_2","beta_3")
  coef_se_vector<- paste0(coef_vector,"_se")
  coef_pval_vector<- paste0(coef_vector,"_pval")
  coef_vector<- c(coef_vector,coef_se_vector,coef_pval_vector,"adj_r_sqrt","f_stat")
  
  assign(paste0("coef_model_",i),as.numeric(get(coef_vector[1])))
  for (j in 2: length(coef_vector)){
    assign(paste0("coef_model_",i),c(get(paste0("coef_model_",i)),get(coef_vector[j])))
  }
}

coef_model_combin<- rbind(coef_model_1,coef_model_2)


for (i in 3: nrow(dep_array)){
  coef_model_combin<- rbind(coef_model_combin,get(paste0("coef_model_",i)))
}
  

coef_model_combin<- as.data.frame(coef_model_combin)
names(coef_model_combin)<- coef_vector
coef_model_combin<- cbind(dep_array,coef_model_combin)
write.csv(coef_model_combin,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\coef_model_combin.csv")

#10.3.2. Model Creation- with 2 variables
# dep_array_2<- as.data.frame(t(combn(var_combin,m=2)))
# names(dep_array_2)<- c("Var_1","Var_2")
# dep_array_2$ind<- rep("ln_avg_brent",nrow(dep_array_2))


# for (i in 1: nrow(dep_array_2)){
#   var_1<- as.character(dep_array_2[i,1])
#   var_2<- as.character(dep_array_2[i,2])
# 
#   model<-lm(reformulate(c(var_1,var_2),"ln_avg_brent"),data=all_dat)
#   model_HAC<- coeftest(model, type="HC0")
#   intercept<- as.numeric(model$coefficients[1])
#   beta_1<- as.numeric(model$coefficients[2])
#   beta_2<- as.numeric(model$coefficients[3])
#   intercept_se<- as.numeric(model_HAC[1,2])
#   beta_1_se<- as.numeric(model_HAC[2,2])
#   beta_2_se<- as.numeric(model_HAC[3,2])
#   intercept_pval<- as.numeric(model_HAC[1,4])
#   beta_1_pval<- as.numeric(model_HAC[2,4])
#   beta_2_pval<- as.numeric(model_HAC[3,4])
#   model_summ<-summary(model)
#   adj_r_sqrt<- model_summ$adj.r.squared
#   d1<- as.numeric(model_summ$fstatistic[1])
#   d2<- as.numeric(model_summ$fstatistic[2])
#   d3<- as.numeric(model_summ$fstatistic[3])
#   f_stat<- pf(d1,d2,d3,lower.tail=FALSE)
#   
#   coef_vector<- c("intercept","beta_1","beta_2")
#   coef_se_vector<- paste0(coef_vector,"_se")
#   coef_pval_vector<- paste0(coef_vector,"_pval")
#   coef_vector<- c(coef_vector,coef_se_vector,coef_pval_vector,"adj_r_sqrt","f_stat")
#   
#   assign(paste0("coef_model2_",i),as.numeric(get(coef_vector[1])))
#   for (j in 2: length(coef_vector)){
#     assign(paste0("coef_model2_",i),c(get(paste0("coef_model2_",i)),get(coef_vector[j])))
#   }
# }
# 
# coef_model2_combin<- rbind(coef_model2_1,coef_model2_2)


# for (i in 3: nrow(dep_array_2)){
#   coef_model2_combin<- rbind(coef_model2_combin,get(paste0("coef_model2_",i)))
# }
# 
# View(coef_model2_combin)
# 
# coef_model2_combin<- as.data.frame(coef_model2_combin)
# names(coef_model2_combin)<- coef_vector
# coef_model2_combin<- cbind(dep_array_2,coef_model2_combin)
# write.csv(coef_model2_combin,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\coef_model_combin_2var.csv")
# 
# plot(ts(all_dat$ln_US_Real_GDP_q1))
#10.4. Final Model

#10.4.1. Selected Model

final_mod_1<- lm(ln_avg_brent~ln_combin_PMI + ln_US_Real_GDP + ln_US_Oil_Rig_Count, data=all_dat)
summary(final_mod_1)
#10.4.2. Test for zero- mean residuals
resid_mean_mod_1<- mean((final_mod_1$residuals)) #Mean: 8.420054e-19

#10.4.3. Test for normally distributed residuals 
jb_test(final_mod_1$residuals) #Model 1 has normally distributed residuals

#10.4.4. Test for auto- correlated residuals
lm_test(all_dat,final_mod_1$residuals,final_mod_1$coefficients) #Model 1 has error terms that are not auto- correlated

#10.4.5. Test for hetroskedasticity
ols_test_breusch_pagan(final_mod_1,rhs=TRUE,multiple=TRUE) #Model 1 has homoscedastic error terms


#11. Fitting Auto.Arima (Time Series) Model on Predictive Variables
#11.1. Predict ln_US_Real_GDP
plot(ts(US_GDP_lm$ln_US_Real_GDP_q1_d1))
adf.test(ts(US_GDP_lm$ln_US_Real_GDP_q1_d1)) #Result: Stationary
ts_model<-auto.arima(ts(US_GDP_lm$ln_US_Real_GDP_q1_d1),stationary=TRUE,seasonal=TRUE) #ARIMA(1,0,1) with zero mean
ts_model_1<- as.numeric(predict(ts_model,2)$pred)[1] # -0.0032045651
ts_model_2<- as.numeric(predict(ts_model,2)$pred)[2] # 0.0002928209
ln_US_Real_GDP_q1_pred1<- ts_model_1 + US_GDP_lm$ln_US_Real_GDP_q1[nrow(US_GDP_lm)]
ln_US_Real_GDP_q1_pred2<- ts_model_2 + ln_US_Real_GDP_q1_pred1
ln_US_Real_GDP_pred1<- ln_US_Real_GDP_q1_pred1+ US_GDP_lm$ln_US_Real_GDP[nrow(US_GDP_lm)] #9.840171
ln_US_Real_GDP_pred2<- ln_US_Real_GDP_pred1 + ln_US_Real_GDP_q1_pred2 #9.845871

#11.2. Predict ln_spare_capacity
plot(ts(opec_spcap_lm$ln_spare_capacity_q2))
adf.test(ts(opec_spcap_lm$ln_spare_capacity_q2)) #Result: Stationary
ts_model<- auto.arima(ts(opec_spcap_lm$ln_spare_capacity_q2),stationary=TRUE,seasonal=TRUE) #ARIMA(2,0,1) with zero mean
ts_model_1<- as.numeric(predict(ts_model,2)$pred)[1] # -0.28680828
ts_model_2<- as.numeric(predict(ts_model,2)$pred)[2] # 0.05316633
ln_spare_capacity_pred1<- ts_model_1 + opec_spcap_lm$ln_spare_capacity[nrow(opec_spcap_lm)-1] #0.3175077
ln_spare_capacity_pred2<- ts_model_2 + opec_spcap_lm$ln_spare_capacity[nrow(opec_spcap_lm)] #0.3824701

#11.3. Predict ln_US_Oil_Rig_Count_q1
plot(ts(rig_count_lm$ln_US_Oil_Rig_Count_q2))
adf.test(ts(rig_count_lm$ln_US_Oil_Rig_Count_q2)) #Result: Stationary
ts_model<- auto.arima(ts(rig_count_lm$ln_US_Oil_Rig_Count_q2),stationary=TRUE,seasonal=TRUE) #ARIMA(3,0,0) with zero mean
ts_model_1<- as.numeric(predict(ts_model,2)$pred)[1] # -0.06752727
ts_model_2<- as.numeric(predict(ts_model,2)$pred)[2] # -0.05842026
ln_US_Oil_Rig_Count_pred1<- ts_model_1 + rig_count_lm$ln_US_Oil_Rig_Count[nrow(rig_count_lm)-1] #3.716662
ln_US_Oil_Rig_Count_pred2<- ts_model_2 + rig_count_lm$ln_US_Oil_Rig_Count[nrow(rig_count_lm)] #3.708524
ln_US_Oil_Rig_Count_q1_pred1<- ln_US_Oil_Rig_Count_pred1- rig_count_lm$ln_US_Oil_Rig_Count[nrow(rig_count_lm)] #0.05028151
ln_US_Oil_Rig_Count_q1_pred2<- ln_US_Oil_Rig_Count_pred2- ln_US_Oil_Rig_Count_pred1 #-0.008138749


final_mod1_coef<- as.numeric(final_mod_1$coefficients)
ind_pred1<- c(1,ln_US_Real_GDP_pred1,ln_spare_capacity_pred1,ln_US_Oil_Rig_Count_q1_pred1)
ln_avg_brent_pred1<- sum(final_mod1_coef*ind_pred1)
avg_brent_pred1<- exp(ln_avg_brent_pred1)

ind_pred2<- c(1,ln_US_Real_GDP_pred2,ln_spare_capacity_pred2,ln_US_Oil_Rig_Count_q1_pred2)
ln_avg_brent_pred2<- sum(final_mod1_coef*ind_pred2)
avg_brent_pred2<- exp(ln_avg_brent_pred2)

#12. Summary of predictive variables
summary(US_GDP_lm$ln_US_Real_GDP_q1)
summary(opec_spcap_lm$ln_spare_capacity)
summary(rig_count_lm$ln_US_Oil_Rig_Count_q1)
summary(combin_PMI_lm$ln_combin_PMI_q1)

#13. Scenario Analysis 
intercept<- as.numeric(final_mod_1$coefficients)[1]
pmi_coef<- as.numeric(final_mod_1$coefficients)[2]
gdp_coef<- as.numeric(final_mod_1$coefficients)[3]
rig_cnt_coef<- as.numeric(final_mod_1$coefficients)[4]
j<- 0
ln_avg_brent_pred_vector<- 0
pmi_2018q4<- -0.03406655
rig_2018q4<- 0.016092301



for (gdp_2018q4 in seq(0.003,0.0055,0.0005)){
  for (gdp_2019q1 in seq(0,0.0035,0.0005)){
    for (gdp_2019q2 in seq(0,0.0035,0.0005)){
      for (gdp_2019q3 in seq(0,0.0035,0.0005)){
        for (gdp_2019q4 in seq(0,0.0035,0.0005)){
          for (pmi_2019q1 in seq(-0.001,0.02,0.005)){
            for (pmi_2019q2 in seq(-0.001,0.02,0.005)){
              for (pmi_2019q3 in seq(-0.001,0.02,0.005)){
                for (pmi_2019q4 in seq(-0.001,0.02,0.005)){
                  for(rig_cnt_2019q1 in seq(0,0.08,0.04)){
                    for (rig_cnt_2019q2 in seq(0,0.08,0.04)){
                      for (rig_cnt_2019q3 in seq(0,0.08,0.04)){
                        for (rig_cnt_2019q4 in seq(0,0.08,0.04)){
                          
                          gdp<- US_GDP_lm$ln_US_Real_GDP[nrow(all_dat)]+ gdp_2018q4+ gdp_2019q1 + gdp_2019q2 + gdp_2019q3 + gdp_2019q4
                          assign(paste0("ln_avg_brent_pred"),
                                 intercept +
                                   pmi_coef*(combin_PMI_lm$ln_combin_PMI[nrow(all_dat)] + pmi_2018q4+ pmi_2019q1 + pmi_2019q2 + pmi_2019q3 + pmi_2019q4) +
                                   gdp_coef*gdp +
                                   rig_cnt_coef*(rig_count_lm$ln_US_Oil_Rig_Count[nrow(all_dat)] + rig_2018q4+ rig_cnt_2019q1 + rig_cnt_2019q2 + rig_cnt_2019q3 + rig_cnt_2019q4)
                          )
                          ln_avg_brent_pred_vector<- c(ln_avg_brent_pred_vector,ln_avg_brent_pred)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

#14. Stress Test

#14.0. Scenartio Analysis
intercept<- as.numeric(final_mod_1$coefficients)[1]
pmi_coef<- as.numeric(final_mod_1$coefficients)[2]
gdp_coef<- as.numeric(final_mod_1$coefficients)[3]
rig_cnt_coef<- as.numeric(final_mod_1$coefficients)[4]
j<- 0
ln_avg_brent_pred_vector<- 0


#14.1. Underperforming PMI

for (gdp_2018q4 in seq(0.003,0.0055,0.0005)){
  for (gdp_2019q1 in seq(0,0.0035,0.0005)){
    for (pmi in seq(-0.5,0.005,0.005)){
      for(rig_cnt in seq(0.002,0.085,0.005)){
        gdp<- US_GDP_lm$ln_US_Real_GDP[nrow(all_dat)]+ gdp_2018q4+ gdp_2019q1
        assign(paste0("ln_avg_brent_pred"),
               intercept +
                 pmi_coef*(combin_PMI_lm$ln_combin_PMI[nrow(all_dat)] + pmi_2018q4+ pmi) +
                 gdp_coef*gdp +
                 rig_cnt_coef*(rig_count_lm$ln_US_Oil_Rig_Count[nrow(all_dat)] + rig_2018q4+ rig_cnt)
        )
        ln_avg_brent_pred_vector<- c(ln_avg_brent_pred_vector,ln_avg_brent_pred)
      }
    }
  }
}

#14.2. Underperforming Rig Count
ln_avg_brent_pred_vector<- 0
for (gdp_2018q4 in seq(0.003,0.0055,0.0005)){
  for (gdp_2019q1 in seq(0,0.0035,0.0005)){
    for (pmi in seq(-0.02,0.0025,0.0005)){
      for(rig_cnt in seq(-0.02,0.005,0.005)){
        gdp<- US_GDP_lm$ln_US_Real_GDP[nrow(all_dat)]+ gdp_2018q4+ gdp_2019q1
        assign(paste0("ln_avg_brent_pred"),
               intercept +
                 pmi_coef*(combin_PMI_lm$ln_combin_PMI[nrow(all_dat)] + pmi_2018q4+ pmi) +
                 gdp_coef*gdp +
                 rig_cnt_coef*(rig_count_lm$ln_US_Oil_Rig_Count[nrow(all_dat)] + rig_2018q4+ rig_cnt)
        )
        ln_avg_brent_pred_vector<- c(ln_avg_brent_pred_vector,ln_avg_brent_pred)
      }
    }
  }
}

#14.3. Overexpansion of US GDP
ln_avg_brent_pred_vector<- 0
for (gdp_2018q4 in seq(0.003,0.0055,0.0005)){
  for (gdp_2019q1 in seq(0.003,0.0055,0.0005)){
    for (pmi in seq(-0.02,0.0025,0.0005)){
      for(rig_cnt in seq(0.002,0.085,0.005)){
        gdp<- US_GDP_lm$ln_US_Real_GDP[nrow(all_dat)]+ gdp_2018q4+ gdp_2019q1
        assign(paste0("ln_avg_brent_pred"),
               intercept +
                 pmi_coef*(combin_PMI_lm$ln_combin_PMI[nrow(all_dat)] + pmi_2018q4+ pmi) +
                 gdp_coef*gdp +
                 rig_cnt_coef*(rig_count_lm$ln_US_Oil_Rig_Count[nrow(all_dat)] + rig_2018q4+ rig_cnt)
        )
        ln_avg_brent_pred_vector<- c(ln_avg_brent_pred_vector,ln_avg_brent_pred)
      }
    }
  }
}

#14.4. Overexpansion of US  and underperforming PMI
ln_avg_brent_pred_vector<- 0
for (gdp_2018q4 in seq(0.003,0.0055,0.0005)){
  for (gdp_2019q1 in seq(0.003,0.0055,0.0005)){
    for (pmi in seq(-0.5,0.005,0.005)){
      for(rig_cnt in seq(0.002,0.085,0.005)){
        gdp<- US_GDP_lm$ln_US_Real_GDP[nrow(all_dat)]+ gdp_2018q4+ gdp_2019q1
        assign(paste0("ln_avg_brent_pred"),
               intercept +
                 pmi_coef*(combin_PMI_lm$ln_combin_PMI[nrow(all_dat)] + pmi_2018q4+ pmi) +
                 gdp_coef*gdp +
                 rig_cnt_coef*(rig_count_lm$ln_US_Oil_Rig_Count[nrow(all_dat)] + rig_2018q4+ rig_cnt)
        )
        ln_avg_brent_pred_vector<- c(ln_avg_brent_pred_vector,ln_avg_brent_pred)
      }
    }
  }
}

#14.5. Overexpansion of US  and underperforming rig count
ln_avg_brent_pred_vector<- 0
for (gdp_2018q4 in seq(0.003,0.0055,0.0005)){
  for (gdp_2019q1 in seq(0.003,0.0055,0.0005)){
    for (pmi in seq(-0.02,0.0025,0.0005)){
      for(rig_cnt in seq(-0.02,0.005,0.005)){
        gdp<- US_GDP_lm$ln_US_Real_GDP[nrow(all_dat)]+ gdp_2018q4+ gdp_2019q1
        assign(paste0("ln_avg_brent_pred"),
               intercept +
                 pmi_coef*(combin_PMI_lm$ln_combin_PMI[nrow(all_dat)] + pmi_2018q4+ pmi) +
                 gdp_coef*gdp +
                 rig_cnt_coef*(rig_count_lm$ln_US_Oil_Rig_Count[nrow(all_dat)] + rig_2018q4+ rig_cnt)
        )
        ln_avg_brent_pred_vector<- c(ln_avg_brent_pred_vector,ln_avg_brent_pred)
      }
    }
  }
}

#14.6. Underperforming PMI  and underperforming rig count
ln_avg_brent_pred_vector<- 0
for (gdp_2018q4 in seq(0.003,0.0055,0.0005)){
  for (gdp_2019q1 in seq(0,0.0035,0.0005)){
    for (gdp_2019q2 in seq(0,0.0035,0.0005)){
      for (gdp_2019q3 in seq(0,0.0035,0.0005)){
        for (gdp_2019q4 in seq(0,0.0035,0.0005)){
         for (pmi_2019q1 in seq(-0.5,0.005,0.005)){
          for (pmi_2019q2 in seq(-0.5,0.005,0.005)){
            for (pmi_2019q3 in seq(-0.5,0.005,0.005)){
              for (pmi_2019q4 in seq(-0.5,0.005,0.005)){
                for(rig_cnt_2019q1 in seq(-0.02,0.005,0.005)){
                  for (rig_cnt_2019q2 in seq(-0.02,0.005,0.005)){
                    for (rig_cnt_2019q3 in seq(-0.02,0.005,0.005)){
                      for (rig_cnt_2019q4 in seq(-0.02,0.005,0.005)){
                        
                      gdp<- US_GDP_lm$ln_US_Real_GDP[nrow(all_dat)]+ gdp_2018q4+ gdp_2019q1 + gdp_2019q2
                      assign(paste0("ln_avg_brent_pred"),
                         intercept +
                         pmi_coef*(combin_PMI_lm$ln_combin_PMI[nrow(all_dat)] + pmi_2018q4+ pmi_2019q1 + pmi_2019q2 + pmi_2019q3 + pmi_2019q4) +
                         gdp_coef*gdp +
                        rig_cnt_coef*(rig_count_lm$ln_US_Oil_Rig_Count[nrow(all_dat)] + rig_2018q4+ rig_cnt_2019q1 + rig_cnt_2019q2 + rig_cnt_2019q3 + rig_cnt_2019q4)
                        )
              ln_avg_brent_pred_vector<- c(ln_avg_brent_pred_vector,ln_avg_brent_pred)
              }
            }
    }
  }
  }
}
}
         }
        }
      }
    }
  }
}
###################### END #######################

write.csv(US_GDP_lm,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\US_GDP_lm.csv")
write.csv(opec_spcap_lm,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\opec_spcap_lm.csv")
write.csv(rig_count_lm,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\rig_count_lm.csv")
write.csv(rig_count_lm_2,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\rig_count_lm_2.csv")
write.csv(rig_count_lm_2,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\rig_count_lm_2.csv")
write.csv(combin_PMI_lm,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\combin_PMI_lm.csv")
write.csv(ln_avg_brent_pred_vector,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\ln_avg_brent_pred_vector.csv")
write.csv(ln_avg_brent_pred_vector,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\ln_avg_brent_stress1.csv")
write.csv(ln_avg_brent_pred_vector,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\ln_avg_brent_stress2.csv")
write.csv(ln_avg_brent_pred_vector,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\ln_avg_brent_stress3.csv")
write.csv(ln_avg_brent_pred_vector,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\ln_avg_brent_stress4.csv")
write.csv(ln_avg_brent_pred_vector,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\ln_avg_brent_stress5.csv")
write.csv(ln_avg_brent_pred_vector,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\ln_avg_brent_stress6.csv")
write.csv(brent_lm,"C:\\Users\\jason\\Desktop\\Seeking Alpha\\BP\\Processed Data\\brent_lm.csv")

sql_stat<-"
SELECT ID,AVG(US_Oil_Rig_Count) as US_Oil_Rig_Count
FROM US_Oil_Rig_Count 
GROUP BY ID
ORDER BY ID"
rig_count_2<- sqldf(sql_stat)
rig_count_lm_2<- func_transform(rig_count,"US_Oil_Rig_Count")
rig_count_lm_2<- year_func(rig_count_lm)
rig_count_lm_2<- id_filter(rig_count_lm,2010,4,2019,1)