library(gamlss)
library(readxl)
library(ggplot2)

cal_z_cores<-function(i1,data1=data1){
  #print(i)
  data3<-all_data[i1,];
  tem_data<-all_data[i1,];
  #data3=data3[data3$Site_ZZZ=='TT_GE_Premier',]
  data3[,'Sex']<-c('Male');
  data3[,'Site_ZZZ']<-c('TT_GE_Premier');
  res1<-predictAll(m2,newdata=data3,type='response',data=data1)
  
  #######################################
  #all population
  res<-res1
  if(tem_data$Sex=='Male'){
    
    if(i_rnd==0&j_rnd==0){
      res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])
      res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])} else{
        
        if((tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))|(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
            res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
              res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}}
        
        if(!(tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))&!(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
            res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
              res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}}} 
  }
  
  if(tem_data$Sex=='Female'){
    
    if(i_rnd==0&j_rnd==0){
      res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
      res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])} else{
        
        if((tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))|(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
            res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
              res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}}
        
        if(!(tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))&!(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
            res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
              res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}}} 
  }
  
  
  Z_score<-zzz_cent(obj=m2,type=c("z-scores"),mu=res$mu,sigma=res$sigma,nu=res$nu,
                    xname = 'Age',xvalues=tem_data$Age,yval=tem_data$tem_feature,
                    calibration=FALSE,lpar=3)
  # Z_score_sum<-c(Z_score_sum,Z_score)
  # 
  # Quant_score<-zzz_cent(obj=m2,type=c("z-scores"),mu=res$mu,sigma=res$sigma,nu=res$nu,
  #                       xname = 'Age',xvalues=tem_data$Age,yval=tem_data$tem_feature,
  #                       calibration=FALSE,lpar=3,cdf=TRUE)
  # Quant_score_sum<-c(Quant_score_sum,Quant_score)
  
}

cal_quant_cores<-function(i1,data1=data1){
  #print(i)
  data3<-all_data[i1,];
  tem_data<-all_data[i1,];
  #data3=data3[data3$Site_ZZZ=='TT_GE_Premier',]
  data3[,'Sex']<-c('Male');
  data3[,'Site_ZZZ']<-c('TT_GE_Premier');
  res1<-predictAll(m2,newdata=data3,type='response',data=data1)
  
  #######################################
  #all population
  res<-res1
  if(tem_data$Sex=='Male'){
    
    if(i_rnd==0&j_rnd==0){
      res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])
      res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])} else{
        
        if((tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))|(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
            res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
              res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}}
        
        if(!(tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))&!(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
            res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
              res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}}} 
  }
  
  if(tem_data$Sex=='Female'){
    
    if(i_rnd==0&j_rnd==0){
      res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
      res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])} else{
        
        if((tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))|(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
            res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))
              res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef[tem_data$Site_ZZZ])))}}
        
        if(!(tem_data$Site_ZZZ %in% names(m2$mu.coefSmo[[1]]$coef))&!(tem_data$Site_ZZZ %in% names(m2$sigma.coefSmo[[1]]$coef))){
          if(i_rnd==1&j_rnd==1){
            res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
            res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==1&j_rnd==0){
              res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
              res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
                res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
                res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}}} 
  }
  
  
  #Z_score<-zzz_cent(obj=m2,type=c("z-scores"),mu=res$mu,sigma=res$sigma,nu=res$nu,
  # xname = 'Age',xvalues=tem_data$Age,yval=tem_data$tem_feature,
  # calibration=FALSE,lpar=3)
  # Z_score_sum<-c(Z_score_sum,Z_score)
  # 
  Quant_score<-zzz_cent(obj=m2,type=c("z-scores"),mu=res$mu,sigma=res$sigma,nu=res$nu,
                        xname = 'Age',xvalues=tem_data$Age,yval=tem_data$tem_feature,
                        calibration=FALSE,lpar=3,cdf=TRUE)
  # Quant_score_sum<-c(Quant_score_sum,Quant_score)
}



fit_model=function(num){
  
  i_poly<-list_par[num,1]
  j_poly<-list_par[num,2]
  i_rnd<-list_par[num,3]
  j_rnd<-list_par[num,4]
  
  list_fit<-data.frame(matrix(0,1,9));
  colnames(list_fit)<-c('mu_poly','sigma_poly','mu_random','sigma_random','Global_Deviance','AIC','SBC','BIC','Selection');
  
  if(i_rnd==0&j_rnd==0)
  {m0<-gamlss(formula=feature~fp(Age,npoly=i_poly)+Sex,
              sigma.formula =feature~fp(Age,npoly=j_poly)+Sex,
              #nu.formula = feature~1,
              family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
              data=data1)
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } else if(i_rnd==0&j_rnd==1)
  {m0<-gamlss(formula=feature~fp(Age,npoly=i_poly)+Sex,
              sigma.formula =feature~fp(Age,npoly=j_poly)+Sex+random(Site_ZZZ),
              #nu.formula = feature~1,
              family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
              data=data1)
  
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } else if(i_rnd==1&j_rnd==0){m0<-gamlss(formula=feature~fp(Age,npoly=i_poly)+Sex+random(Site_ZZZ),
                                          sigma.formula =feature~fp(Age,npoly=j_poly)+Sex,
                                          #nu.formula = feature~1,
                                          family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
                                          data=data1)
  
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } else if(i_rnd==1&j_rnd==1){m0<-gamlss(formula=feature~fp(Age,npoly=i_poly)+Sex+random(Site_ZZZ),
                                          sigma.formula =feature~fp(Age,npoly=j_poly)+Sex+random(Site_ZZZ),
                                          #nu.formula = feature~1,
                                          family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
                                          data=data1)
  
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } 
  m1<-m0
  
  # list_fit[num,'mu_poly']<-i_poly;list_fit[num,'sigma_poly']<-j_poly;list_fit[num,'mu_random']<-i_rnd;list_fit[num,'sigma_random']<-j_rnd;
  # 
  # list_fit[num,'Global_Deviance']<-m1$G.deviance;list_fit[num,'AIC']<-m1$aic;list_fit[num,'SBC']<-m1$sbc;list_fit[num,'SBC']<-BIC(m1);
  # list_fit[num,'Selection']<-num;
  
  num=1;
  list_fit[num,'mu_poly']<-i_poly;list_fit[num,'sigma_poly']<-j_poly;list_fit[num,'mu_random']<-i_rnd;list_fit[num,'sigma_random']<-j_rnd;
  
  list_fit[num,'Global_Deviance']<-m1$G.deviance;list_fit[num,'AIC']<-m1$aic;list_fit[num,'SBC']<-m1$sbc;list_fit[num,'BIC']<-BIC(m1);
  list_fit[num,'Selection']<-num;
  
  list_fit1<-list_fit;
  return(list_fit1)
  
}



best_fit=function(sel_mu_poly,sel_sigma_poly,i_rnd,j_rnd){
  
  
  
  if(i_rnd==0&j_rnd==0)
  {m0<-gamlss(formula=feature~fp(Age,npoly=sel_mu_poly)+Sex,
              sigma.formula =feature~fp(Age,npoly=sel_sigma_poly)+Sex,
              #nu.formula = feature~1,
              family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
              data=data1)
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } else if(i_rnd==0&j_rnd==1)
  {m0<-gamlss(formula=feature~fp(Age,npoly=sel_mu_poly)+Sex,
              sigma.formula =feature~fp(Age,npoly=sel_sigma_poly)+Sex+random(Site_ZZZ),
              #nu.formula = feature~1,
              family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
              data=data1)
  
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } else if(i_rnd==1&j_rnd==0){m0<-gamlss(formula=feature~fp(Age,npoly=sel_mu_poly)+Sex+random(Site_ZZZ),
                                          sigma.formula =feature~fp(Age,npoly=sel_sigma_poly)+Sex,
                                          #nu.formula = feature~1,
                                          family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
                                          data=data1)
  
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } else if(i_rnd==1&j_rnd==1){m0<-gamlss(formula=feature~fp(Age,npoly=sel_mu_poly)+Sex+random(Site_ZZZ),
                                          sigma.formula =feature~fp(Age,npoly=sel_sigma_poly)+Sex+random(Site_ZZZ),
                                          #nu.formula = feature~1,
                                          family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),control=con,
                                          data=data1)
  
  # m1<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
  #            control=con,
  #            family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
  #            data=data1)
  
  } 
  
  return(m0)
  
}

zzz_cent<-function(obj=m2,type=c("centiles","z-scores"),mu=res$mu,sigma=res$sigma,nu=res$nu,
                   cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=data4$Age,yval=NULL,
                   calibration=FALSE,lpar=3,cdf=NULL)
{
  calc.cent <- function(xvar, cent,mu,sigma,nu,lpar) {
    o <- order(xvar)
    mat <- xvar[o]
    cent <- cent
    for (var in cent) {
      if (lpar == 1) {
        newcall <- call(qfun, var/100, mu = mu[o])
      }
      else if (lpar == 2) {
        newcall <- call(qfun, var/100, mu = mu[o], sigma = sigma[o])
      }
      else if (lpar == 3) {
        newcall <- call(qfun, var/100, mu = mu[o], sigma = sigma[o], 
                        nu = nu[o])
      }
      else {
        newcall <- call(qfun, var/100, mu = mu[o], sigma = sigma[o], 
                        nu = nu[o], tau = tau[o])
      }
      ll <- eval(newcall)
      mat <- cbind(mat, ll)
    }
    mat <- as.data.frame(mat)
    nnn <- paste("C", as.character(cent), sep = "")
    names(mat) <- c(xname, nnn)
    return(mat)
  }
  
  
  if (type == "centiles") {
    fname <- obj$family[1]
    qfun <- paste("q", fname, sep = "")
    xvar <- xvalues
    if (calibration) {
      z <- quantile(resid(obj), probs = cent/100)
      p <- pNO(z, mu = 0, sigma = 1)
      cent <- round(100 * p, digits = 2)
    }
    mat <- calc.cent(xvar = xvar, cent = cent,lpar=lpar,mu=mu,sigma=sigma,nu=nu)
    colnames(mat) <- c("x", as.character(cent))
    p2<-mat;
    return(p2)
  }
  
  if (type == "z-scores"&is.null(cdf)) {
    if (calibration)
      stop("calibration is not implemeted yet in z-scores")
    if (is.null(yval))
      stop("the y values should be set if type=z-scores is used")
    if (length(yval) != length(xvalues))
      stop("length of xvalues and yval is not the same")
    fname <- obj$family[1]
    qfun <- paste("p", fname, sep = "")
    if (lpar == 1) {
      newcall <- call(qfun, yval, mu = mu)
    }
    else if (lpar == 2) {
      newcall <- call(qfun, yval, mu = mu, sigma = sigma)
    }
    else if (lpar == 3) {
      newcall <- call(qfun, yval, mu = mu, sigma = sigma,
                      nu = nu)
    }
    else {
      newcall <- call(qfun, yval, mu = mu, sigma = sigma,
                      nu = nu, tau = tau)
    }
    cdf <- eval(newcall)
    rqres <- qnorm(cdf)
    return(rqres)
  } else if (type == "z-scores"&!is.null(cdf)) {
    if (calibration)
      stop("calibration is not implemeted yet in z-scores")
    if (is.null(yval))
      stop("the y values should be set if type=z-scores is used")
    if (length(yval) != length(xvalues))
      stop("length of xvalues and yval is not the same")
    fname <- obj$family[1]
    qfun <- paste("p", fname, sep = "")
    if (lpar == 1) {
      newcall <- call(qfun, yval, mu = mu)
    }
    else if (lpar == 2) {
      newcall <- call(qfun, yval, mu = mu, sigma = sigma)
    }
    else if (lpar == 3) {
      newcall <- call(qfun, yval, mu = mu, sigma = sigma,
                      nu = nu)
    }
    else {
      newcall <- call(qfun, yval, mu = mu, sigma = sigma,
                      nu = nu, tau = tau)
    }
    cdf <- eval(newcall)
    rqres <- cdf
    return(rqres)
  }
  
  
  
}





zzz_boot<-function(RDSfile,bootstrap_num,feature_path=feature_path){
  
  library(gamlss)
  library(ggplot2)
  library(reshape2)
  
  setwd(feature_path);
  results<-readRDS(RDSfile)
  
  model_ind<-which.min(results$list_fit$BIC);
  sel_mu_poly=results$list_fit$mu_poly[model_ind]
  sel_sigma_poly=results$list_fit$sigma_poly[model_ind]
  i_rnd=results$list_fit$mu_random[model_ind]
  j_rnd=results$list_fit$sigma_random[model_ind]
  m0<-results$m0
  m2<-results$m2
  
  data_original<-results$data1
  
  bsp_num<-bootstrap_num
  
  p2_median<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  p2_lower95CI<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  p2_upper95CI<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  p2_Gradient1<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  
  Male_p2_median<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  Male_p2_lower95CI<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  Male_p2_upper95CI<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  Male_p2_Gradient1<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  
  Female_p2_median<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  Female_p2_lower95CI<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  Female_p2_upper95CI<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  Female_p2_Gradient1<-data.frame(matrix(0,dim(results$p2)[1],bsp_num));
  
  con=gamlss.control(c.crit = 0.001, n.cyc = 100)
  c_age<-c(15,25,35,45,55,65,75,85,95)
  for(bsp in 1:bsp_num)
    
  { 
    print(bsp)  
    data1<-data_original
    boot_data<-data1;
    for(i_c_age in c_age)
    {
      tem_data<-data1[data1$Age>(i_c_age-10)&data1$Age<=i_c_age,];
      tem_ind<-sample(1:dim(tem_data)[1],dim(tem_data)[1],replace = TRUE);
      boot_data[data1$Age>(i_c_age-10)&data1$Age<=i_c_age,]<-tem_data[tem_ind,]
    }
    
    data1<-boot_data;
    
    
    if(i_rnd==1&j_rnd==1){
      m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
                 sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
                 control=con,
                 family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
                 data=data1)}else if(i_rnd==1&j_rnd==0){
                   m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
                              sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
                              control=con,
                              family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
                              data=data1)}else if(i_rnd==0&j_rnd==1){
                                m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
                                           sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
                                           control=con,
                                           family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
                                           data=data1)}else if(i_rnd==0&j_rnd==0){
                                             m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
                                                        sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
                                                        control=con,
                                                        family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
                                                        data=data1)}
    
    
    step_age<-0.1
    data3=data1[data1$Sex=='Male',];
    #data3=data3[data3$Site_ZZZ=='TT_GE_Premier',]
    tem_age<-seq(min(results$data1$Age),max(results$data1$Age),by=step_age)
    data3[1:length(tem_age),'Age']<-tem_age
    data3[1:length(tem_age),'Sex']<-c('Male');
    data3[1:length(tem_age),'Site_ZZZ']<-c('TT_GE_Premier');
    data4<-data3[1:length(tem_age),]
    
    res1<-predictAll(m2,newdata=data4,type='response')
    
    #######################################
    #all population
    res<-res1
    if(i_rnd==1&j_rnd==1){
      res$mu=res$mu*exp(-0.5*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
      res$sigma=res$sigma*exp(-0.5*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==1&j_rnd==0){
        res$mu=res$mu*exp(-0.5*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
        res$sigma=res$sigma*exp(-0.5*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
          res$mu=res$mu*exp(-0.5*m2$mu.coefficients['SexMale'])
          res$sigma=res$sigma*exp(-0.5*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==0&j_rnd==0){
            res$mu=res$mu*exp(-0.5*m2$mu.coefficients['SexMale'])
            res$sigma=res$sigma*exp(-0.5*m2$sigma.coefficients['SexMale'])}
    
    p2<-zzz_cent(obj=m2,type=c("centiles"),mu=res$mu,sigma=res$sigma,nu=res$nu,
                 cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=data4$Age,
                 calibration=FALSE,lpar=3)
    #female
    
    res<-res1
    if(i_rnd==1&j_rnd==1){
      res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
      res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==1&j_rnd==0){
        res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
        res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
          res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
          res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==0&j_rnd==0){
            res$mu=res$mu*exp(-1*m2$mu.coefficients['SexMale'])
            res$sigma=res$sigma*exp(-1*m2$sigma.coefficients['SexMale'])}
    
    
    female_p2<-zzz_cent(obj=m2,type=c("centiles"),mu=res$mu,sigma=res$sigma,nu=res$nu,
                        cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=data4$Age,
                        calibration=FALSE,lpar=3)
    
    #male
    
    res<-res1
    if(i_rnd==1&j_rnd==1){
      res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
      res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==1&j_rnd==0){
        res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])*exp(-(m2$mu.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$mu.coefSmo[[1]]$coef)))
        res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])}else if(i_rnd==0&j_rnd==1){
          res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])
          res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])*exp(-(m2$sigma.coefSmo[[1]]$coef['TT_GE_Premier']-mean(m2$sigma.coefSmo[[1]]$coef)))}else if(i_rnd==0&j_rnd==0){
            res$mu=res$mu*exp(-0*m2$mu.coefficients['SexMale'])
            res$sigma=res$sigma*exp(-0*m2$sigma.coefficients['SexMale'])}
    
    male_p2<-zzz_cent(obj=m2,type=c("centiles"),mu=res$mu,sigma=res$sigma,nu=res$nu,
                      cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=data4$Age,
                      calibration=FALSE,lpar=3)
    
    
    #一阶导数
    library(reshape2);
    colnames(p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI');
    mydata<-melt(p2,id='Age');colnames(mydata)<-c('Age','Percentile','Value')
    
    dim(p2)[1]-1
    Grad_p2<-(p2$median[2:dim(p2)[1]]-p2$median[1:(dim(p2)[1]-1)])/step_age
    Grad_p2<-data.frame(c(Grad_p2,Grad_p2[dim(p2)[1]-1]));
    p2<-cbind(p2,Grad_p2)
    colnames(p2)[dim(p2)[2]]<-c('Gradient1')
    
    
    #for all population with sex stratified
    #female data
    colnames(female_p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI');
    mydata<-melt(female_p2,id='Age');colnames(mydata)<-c('Age','Percentile','Value')
    Female_p2<-female_p2;
    
    #male data
    colnames(male_p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI');
    mydata<-melt(male_p2,id='Age');colnames(mydata)<-c('Age','Percentile','Value')
    Male_p2<-male_p2;
    
    
    dim(Female_p2)[1]-1
    Grad_Female_p2<-(Female_p2$median[2:dim(Female_p2)[1]]-Female_p2$median[1:(dim(Female_p2)[1]-1)])/step_age
    Grad_Female_p2<-data.frame(c(Grad_Female_p2,Grad_Female_p2[dim(Female_p2)[1]-1]));
    Female_p2<-cbind(Female_p2,Grad_Female_p2)
    colnames(Female_p2)[dim(Female_p2)[2]]<-c('Gradient1')
    
    dim(Male_p2)[1]-1
    Grad_Male_p2<-(Male_p2$median[2:dim(Male_p2)[1]]-Male_p2$median[1:(dim(Male_p2)[1]-1)])/step_age
    Grad_Male_p2<-data.frame(c(Grad_Male_p2,Grad_Male_p2[dim(Male_p2)[1]-1]));
    Male_p2<-cbind(Male_p2,Grad_Male_p2)
    colnames(Male_p2)[dim(Male_p2)[2]]<-c('Gradient1')
    
    
    p2_median[,bsp]<-p2$median;
    p2_lower95CI[,bsp]<-p2$lower95CI
    p2_upper95CI[,bsp]<-p2$upper95CI
    p2_Gradient1[,bsp]<-p2$Gradient1
    
    Male_p2_median[,bsp]<-Male_p2$median
    Male_p2_lower95CI[,bsp]<-Male_p2$lower95CI
    Male_p2_upper95CI[,bsp]<-Male_p2$upper95CI
    Male_p2_Gradient1[,bsp]<-Male_p2$Gradient1
    
    Female_p2_median[,bsp]<-Female_p2$median
    Female_p2_lower95CI[,bsp]<-Female_p2$lower95CI
    Female_p2_upper95CI[,bsp]<-Female_p2$upper95CI
    Female_p2_Gradient1[,bsp]<-Female_p2$Gradient1
    
  }
  
  results$p2_median<-p2_median
  results$p2_lower95CI<-p2_lower95CI
  results$p2_upper95CI<-p2_upper95CI
  results$p2_Gradient1<-p2_Gradient1
  
  results$Male_p2_median<-Male_p2_median
  results$Male_p2_lower95CI<-Male_p2_lower95CI
  results$Male_p2_upper95CI<-Male_p2_upper95CI
  results$Male_p2_Gradient1<-Male_p2_Gradient1
  
  results$Female_p2_median<-Female_p2_median
  results$Female_p2_lower95CI<-Female_p2_lower95CI
  results$Female_p2_upper95CI<-Female_p2_upper95CI
  results$Female_p2_Gradient1<-Female_p2_Gradient1
  
  return(results)
  
  saveRDS(results,paste0(results$str,'_',results$i,'_loop_bootstrap.rds'))
  
  
  
  
  #bootstrap plot
  #all population
  #Gradient
  results$p2[,'Gradient1_low']<-apply(p2_Gradient1, 1, function(x) quantile(x, probs = 0.025))
  results$p2[,'Gradient1_high']<-apply(p2_Gradient1, 1, function(x) quantile(x, probs = 0.975))
  
  results$Male_p2[,'Gradient1_low']<-apply(Male_p2_Gradient1, 1, function(x) quantile(x, probs = 0.025))
  results$Male_p2[,'Gradient1_high']<-apply(Male_p2_Gradient1, 1, function(x) quantile(x, probs = 0.975))
  
  results$Female_p2[,'Gradient1_low']<-apply(Female_p2_Gradient1, 1, function(x) quantile(x, probs = 0.025))
  results$Female_p2[,'Gradient1_high']<-apply(Female_p2_Gradient1, 1, function(x) quantile(x, probs = 0.975))
  
  #median
  results$p2[,'median_low']<-apply(p2_median, 1, function(x) quantile(x, probs = 0.025))
  results$p2[,'median_high']<-apply(p2_median, 1, function(x) quantile(x, probs = 0.975))
  
  results$Male_p2[,'median_low']<-apply(Male_p2_median, 1, function(x) quantile(x, probs = 0.025))
  results$Male_p2[,'median_high']<-apply(Male_p2_median, 1, function(x) quantile(x, probs = 0.975))
  
  results$Female_p2[,'median_low']<-apply(Female_p2_median, 1, function(x) quantile(x, probs = 0.025))
  results$Female_p2[,'median_high']<-apply(Female_p2_median, 1, function(x) quantile(x, probs = 0.975))
  
  
  #lower95CI
  results$p2[,'lower95CI_low']<-apply(p2_lower95CI, 1, function(x) quantile(x, probs = 0.025))
  results$p2[,'lower95CI_high']<-apply(p2_lower95CI, 1, function(x) quantile(x, probs = 0.975))
  
  results$Male_p2[,'lower95CI_low']<-apply(Male_p2_lower95CI, 1, function(x) quantile(x, probs = 0.025))
  results$Male_p2[,'lower95CI_high']<-apply(Male_p2_lower95CI, 1, function(x) quantile(x, probs = 0.975))
  
  results$Female_p2[,'lower95CI_low']<-apply(Female_p2_lower95CI, 1, function(x) quantile(x, probs = 0.025))
  results$Female_p2[,'lower95CI_high']<-apply(Female_p2_lower95CI, 1, function(x) quantile(x, probs = 0.975))
  
  
  #upper95CI
  results$p2[,'upper95CI_low']<-apply(p2_upper95CI, 1, function(x) quantile(x, probs = 0.025))
  results$p2[,'upper95CI_high']<-apply(p2_upper95CI, 1, function(x) quantile(x, probs = 0.975))
  
  results$Male_p2[,'upper95CI_low']<-apply(Male_p2_upper95CI, 1, function(x) quantile(x, probs = 0.025))
  results$Male_p2[,'upper95CI_high']<-apply(Male_p2_upper95CI, 1, function(x) quantile(x, probs = 0.975))
  
  results$Female_p2[,'upper95CI_low']<-apply(Female_p2_upper95CI, 1, function(x) quantile(x, probs = 0.025))
  results$Female_p2[,'upper95CI_high']<-apply(Female_p2_upper95CI, 1, function(x) quantile(x, probs = 0.975))
  
  
  
  
  png(filename = paste0(results$str,'_',results$i,'_all_without_sex_stratified_Gradient_bsp.png'), 
      width = 1480,           
      height = 1480,          
      units = "px",          
      bg = "white",          
      res = 300)     
  
  p3<-ggplot()+
    geom_line(data=results$p2,aes(x=Age,y=Gradient1),color=c('#666666'),linewidth=1,linetype=c('solid'))+
    geom_ribbon(data=results$p2,aes(x=Age,ymin=Gradient1_low,ymax=Gradient1_high),color=c('#666666'),alpha=0.2,linetype=c('dotted'))+
    # geom_line(data=results$p2,aes(x=Age,y=Gradient1_low),color=c('#666666'),linewidth=1,linetype=c('solid'),alpha=0.2)+
    # geom_line(data=results$p2,aes(x=Age,y=Gradient1_high),color=c('#666666'),linewidth=1,linetype=c('solid'),alpha=0.2)+
    labs(x='Age (years)',y=results$i)+
    theme_bw()+
    scale_x_log10()
  
  print(p3)  
  dev.off()
  
  
  
  png(filename = paste0(results$str,'_',results$i,'_all_without_sex_stratified_bsp.png'), 
      width = 1480,           
      height = 1480,          
      units = "px",          
      bg = "white",          
      res = 300)     
  
  p3<-ggplot()+
    #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
    geom_point(data=results$data1[results$data1$Sex=='Female',],aes(x=Age,y=tem_feature),
               colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
    geom_point(data=results$data1[results$data1$Sex=='Male',],aes(x=Age,y=tem_feature),
               colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
    geom_line(data=results$p2,aes(x=Age,y=median),color=c('#666666'),linewidth=1,linetype=c('solid'))+
    geom_ribbon(data=results$p2,aes(x=Age,ymin=median_low,ymax=median_high),color=c('#666666'),alpha=0.2,linetype=c('dotted'))+
    
    #geom_line(data=p2,aes(x=Age,y=lower99CI),color=c('#666666'),linewidth=1,linetype=c('dashed'))+
    geom_line(data=results$p2,aes(x=Age,y=lower95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
    geom_ribbon(data=results$p2,aes(x=Age,ymin=lower95CI_low,ymax=lower95CI_high),color=c('#666666'),alpha=0.2,linetype=c('dotted'))+
    
    geom_line(data=results$p2,aes(x=Age,y=upper95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
    geom_ribbon(data=results$p2,aes(x=Age,ymin=upper95CI_low,ymax=upper95CI_high),color=c('#666666'),alpha=0.2,linetype=c('dotted'))+
    #geom_line(data=p2,aes(x=Age,y=upper99CI),color=c('#666666'),linewidth=1,linetype=c('dashed'))+
    labs(x='Age (years)',y=results$i)+
    theme_bw()+
    scale_x_log10()
  
  print(p3)  
  dev.off()
  
  
  # #for all population with sex stratified
  # #female data
  # colnames(female_p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI');
  # mydata<-melt(female_p2,id='Age');colnames(mydata)<-c('Age','Percentile','Value')
  # Female_p2<-female_p2;
  # 
  # 
  # #male data
  # colnames(male_p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI');
  # mydata<-melt(male_p2,id='Age');colnames(mydata)<-c('Age','Percentile','Value')
  # Male_p2<-male_p2;
  # 
  # 
  # dim(Female_p2)[1]-1
  # Grad_Female_p2<-(Female_p2$median[2:dim(Female_p2)[1]]-Female_p2$median[1:(dim(Female_p2)[1]-1)])/step_age
  # Grad_Female_p2<-data.frame(c(Grad_Female_p2,Grad_Female_p2[dim(Female_p2)[1]-1]));
  # Female_p2<-cbind(Female_p2,Grad_Female_p2)
  # colnames(Female_p2)[dim(Female_p2)[2]]<-c('Gradient1')
  # 
  # dim(Male_p2)[1]-1
  # Grad_Male_p2<-(Male_p2$median[2:dim(Male_p2)[1]]-Male_p2$median[1:(dim(Male_p2)[1]-1)])/step_age
  # Grad_Male_p2<-data.frame(c(Grad_Male_p2,Grad_Male_p2[dim(Male_p2)[1]-1]));
  # Male_p2<-cbind(Male_p2,Grad_Male_p2)
  # colnames(Male_p2)[dim(Male_p2)[2]]<-c('Gradient1')
  
  
  png(filename = paste0(results$str,'_',results$i,'_all_with_sex_stratified_Gradient_bsp.png'), 
      width = 1480,           
      height = 1480,          
      units = "px",          
      bg = "white",          
      res = 300)     
  
  p3<-ggplot()+
    geom_line(data=results$Female_p2,aes(x=Age,y=Gradient1),color=c('#990000'),linewidth=1,linetype=c('solid'))+
    geom_ribbon(data=results$Female_p2,aes(x=Age,ymin=Gradient1_low,ymax=Gradient1_high),color=c('#990000'),alpha=0.2,linetype=c('dotted'))+
    geom_line(data=results$Male_p2,aes(x=Age,y=Gradient1),color=c('#00CCFF'),linewidth=1,linetype=c('solid'))+
    geom_ribbon(data=results$Male_p2,aes(x=Age,ymin=Gradient1_low,ymax=Gradient1_high),color=c('#00CCFF'),alpha=0.2,linetype=c('dotted'))+
    labs(x='Age (years)',y=results$i)+
    theme_bw()+
    scale_x_log10()
  
  print(p3)  
  dev.off()
  
  
  
  png(filename = paste0(results$str,'_',results$i,'_all_with_sex_stratified_bsp.png'), 
      width = 1480,           
      height = 1480,          
      units = "px",          
      bg = "white",          
      res = 300)     
  
  p3<-ggplot()+
    #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
    geom_point(data=results$data1[results$data1$Sex=='Female',],aes(x=Age,y=tem_feature),
               colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
    geom_point(data=results$data1[results$data1$Sex=='Male',],aes(x=Age,y=tem_feature),
               colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
    geom_line(data=results$Female_p2,aes(x=Age,y=median),color=c('#990000'),linewidth=1,linetype=c('solid'))+
    geom_ribbon(data=results$Female_p2,aes(x=Age,ymin=median_low,ymax=median_high),color=c('#990000'),alpha=0.2,linetype=c('dotted'))+
    #geom_line(data=Female_p2,aes(x=Age,y=lower99CI),color=c('#990000'),linewidth=2,linetype=c('dashed'))+
    #geom_line(data=Female_p2,aes(x=Age,y=lower95CI),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
    #geom_line(data=Female_p2,aes(x=Age,y=upper95CI),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
    #geom_line(data=Female_p2,aes(x=Age,y=upper99CI),color=c('#990000'),linewidth=2,linetype=c('dashed'))+
    
    geom_line(data=results$Male_p2,aes(x=Age,y=median),color=c('#00CCFF'),linewidth=1,linetype=c('solid'))+
    geom_ribbon(data=results$Male_p2,aes(x=Age,ymin=median_low,ymax=median_high),color=c('#00CCFF'),alpha=0.2,linetype=c('dotted'))+
    #geom_line(data=Male_p2,aes(x=Age,y=lower99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
    #geom_line(data=Male_p2,aes(x=Age,y=lower95CI),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
    #geom_line(data=Male_p2,aes(x=Age,y=upper95CI),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
    #geom_line(data=Male_p2,aes(x=Age,y=upper99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
    
    # geom_line(data=p2,aes(x=Age,y=median),color=c('#666666'),linewidth=1,linetype=c('solid'))+
    # #geom_line(data=Male_p2,aes(x=Age,y=lower99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
    # geom_line(data=p2,aes(x=Age,y=lower95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
    # geom_line(data=p2,aes(x=Age,y=upper95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
    # #geom_line(data=Male_p2,aes(x=Age,y=upper99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
    
  labs(x='Age (years)',y=results$i)+
    theme_bw()+
    scale_x_log10()
  
  print(p3)  
  
  dev.off()
  
}
