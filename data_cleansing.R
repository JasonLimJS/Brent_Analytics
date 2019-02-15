library(readr)
library(sqldf)
library(stats)
library(sandwich)
library(lmtest)
library(plm)
library(Hmisc)
library(astsa)
library(plyr)
library(aTSA)
library(gtools)
library(moments)
library(forecast)
library(olsrr)

#1. Import CSV Data
dat_extract<- function(master_path){
  setwd(master_path)
  file_list<- list.files(path=master_path,pattern=".csv")
  file_list_1<- gsub(" ","",file_list)
  flie_list_2<- gsub(".csv","",file_list_1)
  
  for (i in 1:length(file_list)){
    in_file<- file_list[i]
    out_file<- flie_list_2[i]
    file<- paste0(master_path,"/",in_file)
    assign(out_file,read_csv(file),envir = globalenv())
  }
}

#2. Combine PMI of 3 largest manufacturing countries- US, China, Germany (2018)
PMI_combin<- function(PMI_dat_set,country_set){
  
  for (i in 1:length(PMI_dat_set)){ #PMI_dat_set and country_set are arrays of data
    sql_stat<- paste0("SELECT ID, AVG(",
                      country_set[i],
                      "_PMI_Actual) as mean_",
                      country_set[i],
                      "_PMI from ",
                      country_set[i],
                      "PMI group by ID order by ID")
    assign(paste0(country_set[i],"_PMI_qtr"),sqldf(sql_stat),envir= globalenv())
  }
  
  sql_stat_combin<- paste0(
    "SELECT a.*,b.* FROM ",
    paste0(country_set[1],"_PMI_qtr"),
    " as a LEFT JOIN (SELECT a.*,b.* FROM ",
    paste0(country_set[2],"_PMI_qtr"),
    " as a LEFT JOIN ",
    paste0(country_set[3],"_PMI_qtr"),
    " as b WHERE a.id= b.id ORDER BY a.id) as b WHERE a.id= b.id ORDER BY a.id DESC")
  
  combin_PMI<- sqldf(sql_stat_combin)
  combin_PMI<- combin_PMI[,c(-3,-5)]
  combin_PMI$combin_PMI<- rowSums(combin_PMI[,-1])
  return (combin_PMI)
}

#3. Create Quarterly Logical Vector
qtr_logic<- function(data){
  input_dat<- data
  input_dat$qtr<-as.numeric(substr(input_dat$ID,7,7))
  input_dat$q1<- as.numeric(input_dat$qtr==1)
  input_dat$q2<- as.numeric(input_dat$qtr==2)
  input_dat$q3<- as.numeric(input_dat$qtr==3)
  return (input_dat)
}

#3.Fitting MA (q) for q=1,2,..., 8

MA_model<- function(ts_dat){
  
  mod<- c("MA_1","pval_MA_1") 
  for (q in 2:8){
    mod_merge<- c(paste0("MA_",q),paste0("pval_MA_",q))
    mod<-cbind(mod,mod_merge)
  }
  
for (q in 1:8){
    assign(paste0("summ_MA_",q),sarima(ts(ts_dat),0,0,q))
    assign(paste0("logLik_MA_",q),as.numeric(get(paste0("summ_MA_",q))$fit[5]))
    assign(paste0("AIC_MA_",q),get(paste0("summ_MA_",q))$AIC)
    assign(paste0("mean_MA_",q),get(paste0("summ_MA_",q))$ttable["xmean",1])
    assign(paste0("pval_mean_MA_",q),get(paste0("summ_MA_",q))$ttable["xmean",4])
    
    for(k in 1:q){
      assign(paste0("MA_",k,"_MA_",q),get(paste0("summ_MA_",q))$ttable[paste0("ma",k),1])
      assign(paste0("pval_MA_",k,"_MA_",q),get(paste0("summ_MA_",q))$ttable[paste0("ma",k),4])
    }
    
  }
  
  for (q in 1:8){
    assign(paste0("Model_MA_",q),c(get(paste0("logLik_MA_",q)),
                                   get(paste0("AIC_MA_",q)),
                                   get(paste0("mean_MA_",q)),
                                   get(paste0("pval_mean_MA_",q))
                                   )
    )
    
    for (k in 1:q){
      assign(paste0("Model_MA_",q),
             c(get(paste0("Model_MA_",q)),get(paste0("MA_",k,"_MA_",q)))
      )
      
      assign(paste0("Model_MA_",q),
             c(get(paste0("Model_MA_",q)),get(paste0("pval_MA_",k,"_MA_",q)))
      )
    }
    
    assign(paste0("Model_MA_",q),
           c(get(paste0("Model_MA_",q)),seq(1,2*(8-q))*NA)
    )
    
    
  }
  
  model_ma_combin<-cbind(as.data.frame(Model_MA_1),as.data.frame(Model_MA_2))
  
  for (i in 3:7){
    model_ma_combin<- cbind(model_ma_combin,as.data.frame(get(paste0("Model_MA_",i))))
  }
  
  model_ma_combin<-cbind(model_ma_combin,as.data.frame(get(paste0("Model_MA_",8))[c(-19,-20)]))
  model_ma_combin<- t(model_ma_combin)
  
  as.character(mod)
  colnames(model_ma_combin)<-c("logLik","AIC","mean","p_val_mean",mod)
  rownames(model_ma_combin)<-paste0("Model_MA_",seq(1:8))
  return(model_ma_combin)

}

#4.Fitting AR (q) for q=1,2,..., 8

AR_model<- function(ts_dat){
  
  mod<- c("AR_1","pval_AR_1") 
  for (q in 2:8){
    mod_merge<- c(paste0("AR_",q),paste0("pval_AR_",q))
    mod<-cbind(mod,mod_merge)
  }
  
  for (q in 1:8){
    assign(paste0("summ_AR_",q),sarima(ts(ts_dat),q,0,0))
    assign(paste0("logLik_AR_",q),as.numeric(get(paste0("summ_AR_",q))$fit[5]))
    assign(paste0("AIC_AR_",q),get(paste0("summ_AR_",q))$AIC)
    assign(paste0("mean_AR_",q),get(paste0("summ_AR_",q))$ttable["xmean",1])
    assign(paste0("pval_mean_AR_",q),get(paste0("summ_AR_",q))$ttable["xmean",4])
    
    for(k in 1:q){
      assign(paste0("AR_",k,"_AR_",q),get(paste0("summ_AR_",q))$ttable[paste0("ar",k),1])
      assign(paste0("pval_AR_",k,"_AR_",q),get(paste0("summ_AR_",q))$ttable[paste0("ar",k),4])
    }
    
  }
  
  for (q in 1:8){
    assign(paste0("Model_AR_",q),c(get(paste0("logLik_AR_",q)),
                                   get(paste0("AIC_AR_",q)),
                                   get(paste0("mean_AR_",q)),
                                   get(paste0("pval_mean_AR_",q))
    )
    )
    
    for (k in 1:q){
      assign(paste0("Model_AR_",q),
             c(get(paste0("Model_AR_",q)),get(paste0("AR_",k,"_AR_",q)))
      )
      
      assign(paste0("Model_AR_",q),
             c(get(paste0("Model_AR_",q)),get(paste0("pval_AR_",k,"_AR_",q)))
      )
    }
    
    assign(paste0("Model_AR_",q),
           c(get(paste0("Model_AR_",q)),seq(1,2*(8-q))*NA)
    )
    
    
  }
  
  model_ar_combin<-cbind(as.data.frame(Model_AR_1),as.data.frame(Model_AR_2))
  
  for (i in 3:7){
    model_ar_combin<- cbind(model_ar_combin,as.data.frame(get(paste0("Model_AR_",i))))
  }
  
  model_ar_combin<-cbind(model_ar_combin,as.data.frame(get(paste0("Model_AR_",8))[c(-19,-20)]))
  model_ar_combin<- t(model_ar_combin)
  
  as.character(mod)
  colnames(model_ar_combin)<-c("logLik","AIC","mean","p_val_mean",mod)
  rownames(model_ar_combin)<-paste0("Model_AR_",seq(1:8))
  return(model_ar_combin)
  
}

#5. Log Transformation of Data Set

log_transform<-function(dat,y,yname){
  dat<- qtr_logic(dat)
  name0<- names(dat)
  t_var<- grep(y,names(dat))
  dat[,ncol(dat)+1]<- log(dat[,t_var])
  names(dat)<- c(name0,yname)
  return(dat)
}

#6.Functional Transformation of Data Set

func_transform<-function(dat,y){ 
  #dat only has 2 col, ID and variable
  #leveraged on qtr_logic()
  t_var_1<- grep("ID",names(dat))
  t_var_2<- grep(y,names(dat))
  dat<-dat[,c(t_var_1,t_var_2)]
  names(dat)<-c("ID","y")
  dat_func<- qtr_logic(dat)
  dat_func_lm<- qtr_logic(dat_func)
  dat_func_lm$ln_y<- log(dat_func_lm$y)
  dat_func_lm$ln_y_q4<-c(NA,NA,NA,NA,diff(dat_func_lm$ln_y,4))
  dat_func_lm$ln_y_q1<-c(NA,diff(dat_func_lm$ln_y,1))
  dat_func_lm$ln_y_q2<-c(NA,NA,diff(dat_func_lm$ln_y,2))
  dat_func_lm$ln_y_q3<-c(NA,NA,NA,diff(dat_func_lm$ln_y,3))
  dat_func_lm$ln_y_q1_d1<-c(NA,diff(dat_func_lm$ln_y_q1,1))
  dat_func_lm$ln_y_q2_d2<-c(NA,NA,diff(dat_func_lm$ln_y_q2,2))
  dat_func_lm$ln_y_q3_d3<-c(NA,NA,NA,diff(dat_func_lm$ln_y_q3,3))
  dat_func_lm$ln_y_q4_d4<-c(NA,NA,NA,NA,diff(dat_func_lm$ln_y_q4,4))
  names(dat_func_lm)<- gsub("y",y,names(dat_func_lm))
  return(dat_func_lm)
}

#7. year function

year_func <- function (dat) {
  dat<- dat
  dat$year <- as.numeric(gsub(" ","",substr(dat$ID,1,4)))
  return(dat)
}

#8. filter data by year

id_filter <- function (dat,yr1,qtr1,yr2,qtr2){
  dat<- dat
  dat$ID2<- dat$year*10 + dat$qtr
  filter1<- yr1*10 + qtr1
  filter2<- yr2*10 + qtr2
  sql_stat<- paste0("SELECT * from dat " ,
                    "WHERE ID2 > ",
                    filter1,
                    " and ID2 < ",
                    filter2," ORDER BY ID2")
  dat_filter <- sqldf(sql_stat)
  drop_id<- grep("ID2", names(dat_filter))
  dat_filter <- dat_filter[,-drop_id]
  return(dat_filter)
}

#9. Combine a series of data sets
dep_ind_com<- function(dat_a,dat_b,dep,ind){
  sql_stat<- paste0(
  "SELECT a.ID, a.",
  dep, ", b.",
  ind,
  " FROM ",
  dat_a,
  " AS a LEFT JOIN ",
  dat_b,
  " AS b ON a.ID=b.ID ORDER BY a.ID")
  dat_trans<- sqldf(sql_stat)
  return(dat_trans)
}

dep_ind_com2<- function(dat_a,dat_b,ind){
  sql_stat<- paste0(
    "SELECT a.* ,b.",
    ind,
    " FROM ",
    dat_a,
    " AS a LEFT JOIN ",
    dat_b,
    " AS b ON a.ID=b.ID ORDER BY a.ID")
  dat_trans<- sqldf(sql_stat)
  return(dat_trans)
}

#10. Jarque Berra Test- Normality of Residuals
jb_test<- function(resid){
  skew_resid<- skewness(resid)
  kurt_resid<- kurtosis(resid)
  n<- length(resid)
  jb_stat<- (n/6)*(skew_resid^2 + (kurt_resid-3)^2/4)
  p_val<- 1- pchisq(jb_stat,2)
  return(c(jb_stat,p_val))
}

#11. LM Test- Test for autocorrelation of residuals
lm_test<- function(dat,mod_resid,mod_coefficients){
  dat<-dat
  resid_lead<- Lag(mod_resid,1)
  resid<- mod_resid
  dat_test<- cbind(all_dat,resid, resid_lead)
  nvar<- length(mod_coefficients)
  for (i in 2:nvar){
    j<- i - 1
    assign(paste0("var_",j),names(mod_coefficients[i]))
  }
  resid_model<- lm(reformulate(c(var_1,var_2,var_3,"resid"),"resid_lead"),data=dat)
  summ_resid_model<- summary(resid_model)
  lm_test_stat<- summ_resid_model$r.squared*29
  lm_pval<- 1-pchisq(lm_test_stat,2)
  result<- c(lm_test_stat,lm_pval)
  names(result)<- c("lm_test_stat","lm_pval")
  return(result)
}
