pacman::p_load( lubridate, tidyverse, readxl, MatchIt, gbm, optmatch,sandwich,lmtest, randomForest, writexl)
rm(list = ls())
matching_data = "C:/Users/joh21006/OneDrive - University of Connecticut/Documents/Projects/ROI measurement/matchintg/data"
matching_output= "C:/Users/joh21006/OneDrive - University of Connecticut/Documents/Projects/ROI measurement/matching/output"

internal_funding_data=read_excel(paste(matching_data,"/internal funding data for matching.xlsx",sep=""))
names(internal_funding_data)<-str_replace_all(names(internal_funding_data), c(" " = "." , "," = "", "=" = "is","&." = "" ))

version_num=2
future_avg = "future_2yr_awarded_avg"
future_avg_name = "2-year avg"
set.seed(3)

# 2021-12-29
# version 1 matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2)
#version 2 matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_3yr_awarded_avg)
#version 3 matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_2yr_awarded_avg)


# 2021-12-30
#version 1 set.seed(3)  matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_3yr_awarded_avg)
#version 2 set.seed(3) matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_2yr_awarded_avg)
#version 3 set.seed(5) matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_2yr_awarded_avg)
#version 4 set.seed(5)  matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_3yr_awarded_avg)


# 2022-01-03
#version 1 set.seed(3)  matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_3yr_awarded_avg)
#version 2 set.seed(3) matchset <- internal_funding_data %>% drop_na(awarded_lag_1, awarded_lag_2,future_2yr_awarded_avg)


matchset <- internal_funding_data %>%drop_na(awarded_lag_1, awarded_lag_2,all_of(future_avg))
matchset = filter(matchset, grant.name_REP==1)

matchset$awarded_lags12 = matchset$awarded_lag_1 + matchset$awarded_lag_2
matchset$awarded_lags012 = matchset$total_awarded + matchset$awarded_lag_1 + matchset$awarded_lag_2
matchset$any_awarded_lags12 = ifelse(matchset$awarded_lags12>0 ,1,0)
matchset$any_awarded_lags012 = ifelse(matchset$awarded_lags012>0 ,1,0)

profs= "Asst.Professor + Assoc.Professor"
schools = "school.is.Agriculture.Health.Natural.Resources + school.is.Education  + school.is.Engineering + school.is.Fine.Arts + school.is.Nursing + school.is.Pharmacy"
funding = "any_awarded_lags12 + awarded_lags12"
other = "tenure_track.is.yes"
tmp=matchset[ , grepl( "dept" , names(matchset))]
x=names(tmp[, colSums(tmp) > 0])
depts = paste(x[grepl( "dept" , x )],collapse = "+")
indvars <- c(profs, schools, other, funding, depts)
matchobj_gbm_1 <-matchit(as.formula(paste("int.funding.awarded ~", paste(indvars, collapse="+"))) , 
                          data= matchset , method="optimal",distance =  "gbm")



# try with only CLAS dept dummies

profs= "Asst.Professor + Assoc.Professor"
schools = "school.is.Agriculture.Health.Natural.Resources + school.is.Education  + school.is.Engineering + school.is.Fine.Arts + school.is.Nursing + school.is.Pharmacy"
funding = "any_awarded_lags12 + awarded_lags12"
other = "tenure_track.is.yes"
tmp = filter(matchset,school.is.Liberal.Arts.and.Sciences==1)
tmp=tmp[ , grepl( "dept" , names(tmp))]
x=names(tmp[, colSums(tmp) > 0])
depts = paste(x,collapse = "+")
indvars <- c(profs, schools, other, funding,depts)

matchobj_gbm_2 <-matchit(as.formula(paste("int.funding.awarded ~", paste(indvars, collapse="+"))) , data= matchset , method="optimal",distance =  "gbm")


# try with only CLAS dept dummies & include current year in past avg

profs= "Asst.Professor + Assoc.Professor"
schools = "school.is.Agriculture.Health.Natural.Resources + school.is.Education  + school.is.Engineering + school.is.Fine.Arts + school.is.Nursing + school.is.Pharmacy"
funding = "any_awarded_lags012 + awarded_lags012"
other = "tenure_track.is.yes"
tmp = filter(matchset,school.is.Liberal.Arts.and.Sciences==1)
tmp=tmp[ , grepl( "dept" , names(tmp))]
x=names(tmp[, colSums(tmp) > 0])
depts = paste(x,collapse = "+")
indvars <- c(profs, schools, other, funding,depts)

matchobj_gbm_3 <-matchit(as.formula(paste("int.funding.awarded ~", paste(indvars, collapse="+"))) , data= matchset , method="optimal",distance =  "gbm")



# all dept dummies random forest
profs= "Asst.Professor + Assoc.Professor"
schools = "school.is.Agriculture.Health.Natural.Resources + school.is.Education  + school.is.Engineering + school.is.Fine.Arts + school.is.Nursing + school.is.Pharmacy"
funding = "any_awarded_lags12 + awarded_lags12"
other = "tenure_track.is.yes"
tmp=matchset[ , grepl( "dept" , names(matchset))]
x=names(tmp[, colSums(tmp) > 0])
depts = paste(x[grepl( "dept" , x )],collapse = "+")
indvars <- c(profs, schools, other, funding,depts)

matchobj_randomforest_1 <-matchit(as.formula(paste("int.funding.awarded ~", paste(indvars, collapse="+"))) , data= matchset , method="optimal",distance =  "randomforest")



# try with only CLAS dept dummies random forest
profs= "Asst.Professor + Assoc.Professor"
schools = "school.is.Agriculture.Health.Natural.Resources + school.is.Education  + school.is.Engineering + school.is.Fine.Arts + school.is.Nursing + school.is.Pharmacy"
funding = "any_awarded_lags12 + awarded_lags12"
other = "tenure_track.is.yes"
tmp = filter(matchset,school.is.Liberal.Arts.and.Sciences==1)
tmp=tmp[ , grepl( "dept" , names(tmp))]
x=names(tmp[, colSums(tmp) > 0])
depts = paste(x,collapse = "+")
indvars <- c(profs, schools, other, funding,depts)

matchobj_randomforest_2 <-matchit(as.formula(paste("int.funding.awarded ~", paste(indvars, collapse="+"))) , data= matchset , method="optimal",distance =  "randomforest")

# try with only CLAS dept dummies & include current year in past avg

profs= "Asst.Professor + Assoc.Professor"
schools = "school.is.Agriculture.Health.Natural.Resources + school.is.Education  + school.is.Engineering + school.is.Fine.Arts + school.is.Nursing + school.is.Pharmacy"
funding = "any_awarded_lags012 + awarded_lags012"
other = "tenure_track.is.yes"
tmp = filter(matchset,school.is.Liberal.Arts.and.Sciences==1)
tmp=tmp[ , grepl( "dept" , names(tmp))]
x=names(tmp[, colSums(tmp) > 0])
depts = paste(x,collapse = "+")
indvars <- c(profs, schools, other, funding,depts)

matchobj_randomforest_3 <-matchit(as.formula(paste("int.funding.awarded ~", paste(indvars, collapse="+"))) , data= matchset , method="optimal",distance =  "randomforest")




models = list(matchobj_gbm_1,matchobj_gbm_2,matchobj_gbm_3, matchobj_randomforest_1,
    matchobj_randomforest_2, matchobj_randomforest_3)

model_names = list("gbm_1","gbm_2","gbm_3","rf_1", "rf_2","rf_3")
ds =as.data.frame(summary(matchobj_gbm_1,un=TRUE)[1])

for (i in 1:length(models)) {
  ds=as.data.frame(summary(models[[i]],un=TRUE)[4])
  names(ds) = gsub("sum.matched.","",names(ds))
  ds$Abs.Std.Mean.Diff = abs(ds$Std..Mean.Diff.)
  ds=ds[c("Means.Treated","Means.Control","Abs.Std.Mean.Diff")]
  names(ds) = paste(names(ds), model_names[i],sep="_")
  ds$var_name= row.names(ds)
  if (i==1) {
    ds$var_order<- seq.int(nrow(ds))
    ds_match = ds
  }
  else {
    ds_match = merge(ds_match, ds, by = "var_name", all =TRUE)
  }
}

ds_match = ds_match[order(ds_match$var_order),]
ds_match = subset(ds_match, select = -c(var_order) )

write_xlsx(ds_match,paste(matching_output,"/balance_results_",Sys.Date(),"_v",version_num,".xlsx",sep=""))

depvars=c(future_avg, "awarded_lead_1", "awarded_lead_2","awarded_lead_3","awarded_lead_4")
depvar_names = list(future_avg_name, "Future year 1","Future year 2","Future year 3","Future year 4")

for (i in 1:length(models)) {
  ds  <- match.data(models[[i]])
  t=lapply(ds[,depvars], function(x) t.test(x ~  ds$int.funding.awarded , var.equal = FALSE))
  for (j in 1:length(depvars)) {
    x=as.data.frame(list(control_mean = t[[j]][[5]][[1]], treat_mean = t[[j]][[5]][[2]], mean_diff = t[[j]][[5]][[2]]-t[[j]][[5]][[1]], pval = t[[j]][[3]]))
    x$depvar=depvar_names[[j]]       
    x$model=model_names[[i]]       
    if (j==1) {
      x1= x
    }
    else {
      x1 = rbind(x1,x)
    }
  }                    
  if (i==1) {
    ttests= x1
  }
  else {
    ttests = rbind(ttests,x1)
  }
}                           

ttests_wide=reshape(data=ttests,timevar="model",direction = "wide",idvar ="depvar")

write_xlsx(ttests_wide,paste(matching_output,"/ttest_results_",Sys.Date(),"_v",version_num,".xlsx",sep=""))

                    

for (i in 1:length(models)) {
  for (r in 0:1) {
    ds  <- match.data(models[[i]]) %>% filter(int.funding.awarded== r)
    t=lapply(ds[,depvars], function(x) quantile(x, probs = seq(.5, 1, .05) ,  na.rm = TRUE))
    for (j in 1:length(depvars)) {
      x=as.data.frame(t[[j]][c("50%","75%","90%","100%")])
      names(x) = "value"
      x$quantiles = row.names(x)
      x$depvar=depvar_names[[j]]       
      x$model=model_names[[i]]
      x$rep_app = r
      if (j==1) {
        x1= x
      }
      else {
        x1 = rbind(x1,x)
      }
    }                    
    if (r==0) {
      x2= x1
    }
    else {
      x2 = rbind(x2,x1)
    }
  }
  if (i==1) {
    quantiles= x2
  }
  else {
    quantiles = rbind(quantiles, x2)
  }
}                           

quantiles_wide = reshape(data=quantiles,idvar = c("depvar", "quantiles","model"), 
                         timevar = "rep_app", direction = "wide")

names(quantiles_wide) = c("quantile","depvar","model","REP_0","REP_1")
quantiles_wide$diference = quantiles_wide$REP_1 - quantiles_wide$REP_0
quantiles_wide = reshape(data=quantiles_wide, idvar = c("depvar", "quantile"), 
            timevar = "model", direction = "wide")
write_xlsx(quantiles_wide,paste(matching_output,"/quantile_results_",Sys.Date(),"_v",version_num,".xlsx",sep=""))                           

                    
                    
                    
                    