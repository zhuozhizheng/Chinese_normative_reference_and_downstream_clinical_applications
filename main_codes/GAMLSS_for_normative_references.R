#install.packages('gamlss')
rm(list=ls())

datapath='C:/ZZZ/work/manuscript/Lifespan-main'# change
setwd(datapath)
source("100.common-variables.r")
source("101.common-functions.r")

source("ZZZ_function.R")
source("300.variables.r")
source("301.functions.r")

data1<-read.csv('new_final_list1_update.csv',header=TRUE);
data1$Age<-data1$Age_y

# #loading for Global_feature
# MRI<-read.csv('Results_csv1.csv',header=TRUE);
# for(i in 1:dim(MRI)[1])
# {
#   MRI[i,'mean_thickness']<-
#     (MRI[i,'meanCT2_lhMeanThickness']*MRI[i,'meanCT2_lhVertex']+
#        MRI[i,'meanCT2_rhMeanThickness']*MRI[i,'meanCT2_rhVertex'])/(MRI[i,'meanCT2_lhVertex']+MRI[i,'meanCT2_rhVertex'])
# 
# 
#   MRI[i,'total_surface_arrea']<-MRI[i,'totalSA2_lh']+MRI[i,'totalSA2_rh'];
# }
# tem_feature<-colnames(MRI)[17:29];str='Global_feature';str_lab='Global_feature';
# tem_feature<-colnames(MRI)[17:29];str='Global_feature';str_lab='Global_feature';

#' #loading for regional for DK-atlas feture
# var<-c('aseg.vol.table','lh.aparc.volume.table','rh.aparc.volume.table',
#        'lh.aparc.thickness.table','rh.aparc.thickness.table',
#        'lh.aparc.area.table','rh.aparc.area.table','wmparc.vol.table')

var<-c('aseg.vol.table')
#'  var<-c('rh.aparc.area.table',
#'         'lh.aparc.thickness.table','rh.aparc.thickness.table',
#'         #'lh.aparc.foldind.table','rh.aparc.foldind.table',
#'         #'lh.aparc.curvind.table','rh.aparc.curvind.table',
#'         #'lh.aparc.gauscurv.table','rh.aparc.gauscurv.table',
#'         #'lh.aparc.meancurv.table','rh.aparc.meancurv.table',
#'          'wmparc.vol.table')
#'
# var<-c('aseg.vol.table')
str_lab='Local_feature';

#for(sheet in 1:1) #for global feature
for(sheet in var) #for local feature
{ 
  
  if(str_lab=='Local_feature')
  {
    setwd(datapath)
    MRI <- read_excel("Results1.xlsx",sheet=sheet)}
  
  if(sheet=="aseg.vol.table")
  {
    MRI[,'cerebellum_WM']<-MRI$`Left-Cerebellum-White-Matter`+MRI$`Right-Cerebellum-White-Matter`
    MRI[,'cerebellum_GM']<-MRI$`Left-Cerebellum-Cortex`+MRI$`Right-Cerebellum-Cortex`
    MRI[,'cerebellum_total']<-MRI[,'cerebellum_WM']+MRI[,'cerebellum_GM'];
    MRI[,'CC']<-MRI$CC_Anterior+MRI$CC_Central+MRI$CC_Mid_Anterior+
      MRI$CC_Mid_Posterior+MRI$CC_Posterior
  }
  
  #tem_feature<-colnames(MRI)[17:dim(MRI)[2]];
  tem_feature<-colnames(MRI)[43:51];
  #tem_feature<-colnames(MRI)[18:dim(MRI)[2]];
  #tem_feature<-c("Left-Thalamus","Right-Thalamus")
  #tem_feature<-c("Ventricles","mean_thickness","total_surface_arrea")
  tem_feature<-c("Right-Cerebellum-White-Matter","Right-Cerebellum-Cortex",
                 "Left-Cerebellum-White-Matter","Left-Cerebellum-Cortex")
  tem_feature<-c('CSF',"EstimatedTotalIntraCranialVol")
  #tem_feature<-c('GMV','sGMV','WMV',"Ventricles","mean_thickness","total_surface_arrea","cerebellum_total","Brain-Stem")
  #tem_feature<-c("cerebellum_total","Brain-Stem")
  str=sheet;
  
  
  if(str_lab=='Global_feature')
  {str='Global_feature';}
  
  
  
  
data_centiles<-NULL
data_centiles_qua<-NULL

savepath=paste0(datapath,'/test_zzz_update_20240116')

if (!(dir.exists(paste0(savepath,'/',str))))
{dir.create(paste0(savepath,'/',str))}

setwd(paste0(savepath,'/',str))


setwd(datapath)  
data1<-read.csv('new_final_list1_update.csv',header=TRUE);
data1$Age<-data1$Age_y
setwd(paste0(savepath,'/',str))
# data1=cbind(data1,MRI[,i])
# colnames(data1)[18]=c('tem_feature')
data1[,'Euler']<-data1$euler_number_l+data1$euler_number_r;
data1[is.na(data1$Euler),'Euler']<--2;

Euler_bh<-data1[,'Euler'];

Euler_bh<-Euler_bh[!is.na(Euler_bh)]
median_Euler<-median(Euler_bh);
low_Euler<-median_Euler-2*sd(Euler_bh[Euler_bh!=-2])

all_data<-data1[!is.na(data1$Age)&!is.na(data1$Sex)&
                  !is.na(data1$Site_ZZZ)&
                  !is.na(data1$baseline)&
                  is.na(data1$Image_Quality_lab)&
                  data1$Euler>=low_Euler,]

all_data<-na.omit(all_data);

Z_data<-list();
Quant_data<-list()

#tem_feature<-c("CC_Central","CC_Mid_Anterior","CC_Anterior",
#               "TotalGrayVol","BrainSegVol","BrainSegVol-to-eTIV",
#               "EstimatedTotalIntraCranialVol")

#for(i in tem_feature[1:length(tem_feature)])
for(i in tem_feature[1:length(tem_feature)])
{

if(file.exists(paste0(str,'_',i,'_loop_our_model.rds'))){print('file exist');next;print('file exist')};
   
if(i=='GA_name'|
   i=='WM-hypointensities'|i=='Left-WM-hypointensities'|i=='Right-WM-hypointensities'|
   i=='non-WM-hypointensities'|i=='Left-non-WM-hypointensities'|i=='Right-non-WM-hypointensities'|
   i=='Optic-Chiasm'|i=='MaskVol'|i=='MaskVol-to-eTIV'|i=='lhSurfaceHoles'|i=='rhSurfaceHoles'|
   i=='SurfaceHoles'|i=="3rd-Ventricle"|i=="4th-Ventricle" |i=="Left-vessel"|i=="Right-vessel"|
   i=="Left-choroid-plexus"|i=="Right-choroid-plexus"|i=="5th-Ventricle"|i=="WM-hypointensities"|
   i=="SupraTentorialVolNotVent"|i=="Left-Lateral-Ventricle"|i=="Left-Inf-Lat-Vent"|
   #i=="Left-Cerebellum-White-Matter"|i=="Left-Cerebellum-Cortex"|i=="Right-Lateral-Ventricle"|i=="Right-Inf-Lat-Vent"|
   #i=="Right-Cerebellum-White-Matter"|i=="Right-Cerebellum-Cortex"|i=="BrainSegVol"|i=="BrainSegVolNotVent"|
   i=="lhCortexVol"|i=="rhCortexVol"|i=="lhCerebralWhiteMatterVol" |i=="rhCerebralWhiteMatterVol"|
   i=="SupraTentorialVol"|i=="SupraTentorialVolNotVent"|i=="MaskVol"|i=="BrainSegVol-to-eTIV"|
   i=="MaskVol-to-eTIV"|i=="Left-Inf-Lat-Vent"|i=="Left-Inf-Lat-Vent"|i=="Left-Inf-Lat-Vent"|
   i=="CC_Mid_Anterior")  
{next}
  
 # if(sheet=='rh.DKTatlas.volume.table'&i=='rh_temporalpole_volume')
 #   
 # {next};
  
setwd(datapath)  
data1<-read.csv('new_final_list1_update.csv',header=TRUE);
data1$Age<-data1$Age_y
setwd(paste0(savepath,'/',str))

data1=cbind(data1,MRI[,i])

colnames(data1)[dim(data1)[2]]=c('tem_feature')

data1[,'Euler']<-data1$euler_number_l+data1$euler_number_r;
data1[is.na(data1$Euler),'Euler']<--2;

all_data<-data1[!is.na(data1$Age)&!is.na(data1$Sex)&
                  !is.na(data1$Site_ZZZ)&
                  !is.na(data1$baseline)&
                  is.na(data1$Image_Quality_lab)&
                  data1$Euler>=low_Euler,]

for(iz in 1:dim(all_data)[1]){all_data[iz,'individual_ID']<-paste0(all_data$Freesufer_Path1[iz],all_data$Freesufer_Path2[iz],all_data$Freesufer_Path3[iz],sep="_")}
library(dplyr)
all_data<- all_data %>% distinct(individual_ID, .keep_all=T)
rownames(all_data)<-all_data$individual_ID
all_data_original<-all_data

data1<-all_data

data1=data1[!is.na(data1$Age)&!is.na(data1$Sex)&!is.na(data1$Site_ZZZ)&
              !is.na(data1$tem_feature)&!is.na(data1$Data_baseline)&data1$Diagnosis=='HC',]

data1$Site_ZZZ<-as.factor(data1$Site_ZZZ)

data1$Sex<-as.factor(data1$Sex)


newdata1<-data1;
newdata1<-data1[order(data1$Age),]
data1<-data1[order(data1$Age),]

#data1[data1$tem_feature<=0,'tem_feature']<-1 

data1[,'feature']<-data1$tem_feature
all_data[,'feature']<-all_data$tem_feature

data2=data1;


#remove the extreme values
data1=data2;
data1<-data1[!is.na(data1$tem_feature),]
data1<-data1[data1$feature>(mean(data1$feature)-3*sd(data1$feature))&
               data1$feature<(mean(data1$feature)+3*sd(data1$feature)),]


#select the columns
data1<-data1[,c(1:14,16,dim(data1)[2]-3,dim(data1)[2]-2,dim(data1)[2]-1,dim(data1)[2])]

#data1[,'Age']<-log(data1$Age)
#data1<-data1[!is.null(data1$Sex),]
#step1 choose the best fit
# list_fit<-data.frame(matrix(0,3*3*2*2,9));
# colnames(list_fit)<-c('mu_poly','sigma_poly','mu_random','sigma_random','Global_Deviance','AIC','SBC','BIC','Selection');

#list_fit<-data.frame(matrix(0,3*3*2*2,9));
#list_fit<-data.frame(matrix(0,1,9));
#colnames(list_fit)<-c('mu_poly','sigma_poly','mu_random','sigma_random','Global_Deviance','AIC','SBC','BIC','Selection');

list_par<-data.frame(matrix(0,3*3*2*2,1));


con=gamlss.control()

# if(i=="rh_bankssts_thickness"|i=="rh_caudalanteriorcingulate_thickness"|i=="rh_caudalmiddlefrontal_thickness"|   
#    i=="rh_cuneus_thickness"|i=="rh_entorhinal_thickness"|i=="rh_fusiform_thickness"|              
#    i=="rh_inferiorparietal_thickness"|i=="rh_inferiortemporal_thickness"|
#    i=="rh_lateraloccipital_thickness"|i=="rh_lateralorbitofrontal_thickness"|
#    i=="rh_lingual_thickness"|i=="rh_isthmuscingulate_thickness"|i=="rh_isthmuscingulate_thickness"|
#    i=='rh_insula_area'|i=='rh_parstriangularis_area'|i=='lh_parstriangularis_area'|
#    i=='lh_parsopercularis_volume'|i=='lh_lateralorbitofrontal_thickness'|
#    i=='lh_medialorbitofrontal_thickness'|i=="rh_isthmuscingulate_thickness"|
#    i=="lh_posteriorcingulate_thickness"|
#    i=='rh_lateralorbitofrontal_thickness'|i=='rh_medialorbitofrontal_thickness'|
#    i=="rh_isthmuscingulate_thickness"|i=="rh_posteriorcingulate_thickness"|
#    
#    i=="lh_caudalanteriorcingulate_area"|i=="rh_caudalanteriorcingulate_area"|
#    i=="lh_transversetemporal_area"|i=="rh_transversetemporal_area"|
#    i=="Left-Thalamus")

if(i=='rh_insula_area'|i=='rh_parstriangularis_area'|i=='lh_parstriangularis_area'|
   i=='lh_parsopercularis_volume'|i=="lh_caudalanteriorcingulate_area"|i=="rh_caudalanteriorcingulate_area"|
   i=="lh_transversetemporal_area"|i=="rh_transversetemporal_area"|
   i=="Left-Thalamus"|
   i=="lh_lateralorbitofrontal_thickness"|
   i=="lh_pericalcarine_thickness"|
   i=="rh_caudalanteriorcingulate_thickness"|
   i=="rh_isthmuscingulate_thickness"|
   i=="rh_lateralorbitofrontal_thickness"|
   i=="rh_posteriorcingulate_thickness")

{con=gamlss.control(c.crit = 0.01, n.cyc = 3,autostep = FALSE)}

num=0;
#con=gamlss.control(c.crit = 0.01, n.cyc = 3,autostep = FALSE)


if(i!='lh_postcentral_thickness'&
   i!='rh_postcentral_thickness'&
   i!="rh_caudalanteriorcingulate_thickness"&
   i!="lh_pericalcarine_thickness"&
   i!="lh_rostralmiddlefrontal_volume"&
   i!="rh_rostralmiddlefrontal_volume"&
   i!="rh_parsopercularis_volume"&
   i!="rh_parstriangularis_volume"&
   i!="rh_rostralmiddlefrontal_area"&
   i!="rh_superiorfrontal_area"&
   i!="rh_insula_area")
{
for(i_poly in 1:3)
  {
  for(j_poly in 1:3)
  {
    for(i_rnd in 0:1)
    {
      for(j_rnd in 0:1)
      {
        num=num+1;
        list_par[num,1]<-i_poly
        list_par[num,2]<-j_poly
        list_par[num,3]<-i_rnd
        list_par[num,4]<-j_rnd
        
        #list_fit=fit_model(i_poly,j_poly,i_rnd,j_rnd,num)
        
        }}}}
library(doParallel)
library(foreach)


cl<-makeCluster(10)
registerDoParallel(cl)
my_data<-foreach(num=1:dim(list_par)[1],
                 .combine=rbind,
                 .packages = c('gamlss')) %dopar% fit_model(num)
stopCluster(cl)


# my_data<-data.frame(matrix(0,3*3*2*2,9));
# for(num in 1:dim(list_par)[1])
# {
# 
#         list_fit1=fit_model(num)
#         my_data[num,]<-list_fit1
# }

list_fit<-my_data
#list_fit<-list_par
print(list_fit)


#fit using the bestfit npoly and random with lowest BIC
model_ind<-which.min(list_fit$BIC);
sel_mu_poly=list_fit$mu_poly[model_ind]
sel_sigma_poly=list_fit$sigma_poly[model_ind]
i_rnd=list_fit$mu_random[model_ind]
j_rnd=list_fit$sigma_random[model_ind]
} else{sel_mu_poly=2
  sel_sigma_poly=2
  i_rnd=1
  j_rnd=1
  list_fit<-c('test')
  con=gamlss.control(c.crit = 0.01, n.cyc = 2,autostep = FALSE)
  }


# if(i=='rh_isthmuscingulate_thickness')
#   {sel_mu_poly=2
#    sel_sigma_poly=2
#    i_rnd=0
#    j_rnd=0
#    con=gamlss.control(c.crit = 0.1, n.cyc = 3,autostep = FALSE)
#    list_fit<-c('test')
#    }


data1$Sex<-factor(data1$Sex,levels=c('Female','Male'))

data1<-data1[!is.na(data1$Sex),]  
  
m0<-best_fit(sel_mu_poly,sel_sigma_poly,i_rnd,j_rnd)

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
tem_age<-seq(min(data1$Age),max(data1$Age),by=step_age)
data3[1:length(tem_age),'Age']<-seq(min(data1$Age),max(data1$Age),by=step_age)
data3[1:length(tem_age),'Sex']<-c('Male');
data3[1:length(tem_age),'Site_ZZZ']<-c('TT_GE_Premier');
data4<-data3[1:length(tem_age),]

#######################################
#all population#all population#all population#all population#all population#all population
#all population#all population#all population#all population#all population#all population
#for mu
data4$Sex<-as.factor(data4$Sex)
data4$Site_ZZZ<-as.factor(data4$Site_ZZZ)
tem_rnd<-matrix(0,dim(data4)[1],1);
Model.Frame<-model.frame(formula = m2$mu.formula,data=data4);
Model.Matrix<-model.matrix(m2$mu.formula,Model.Frame)
Fit.fix<-matrix(m2$mu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
Fit.fix['SexMale',1]<-0.5*Fit.fix['SexMale',1]
if(!is.null(m2$mu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(data4)[1]){
  if(data4$Site_ZZZ[iz] %in% names(m2$mu.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-mean(m2$mu.coefSmo[[1]]$coef)
  }else{tem_rnd[iz]<-mean(m2$mu.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};

mu<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
#for sigma
tem_rnd<-matrix(0,dim(data4)[1],1);
Model.Frame<-model.frame(formula = m2$sigma.formula,data=data4);
Model.Matrix<-model.matrix(m2$sigma.formula,Model.Frame)
Fit.fix<-matrix(m2$sigma.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
Fit.fix['SexMale',1]<-0.5*Fit.fix['SexMale',1]
if(!is.null(m2$sigma.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(data4)[1]){
  if(data4$Site_ZZZ[iz] %in% names(m2$sigma.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-mean(m2$sigma.coefSmo[[1]]$coef)
  }else{tem_rnd[iz]<-mean(m2$sigma.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};


sigma<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))

#sigma<-(as.vector(Model.Matrix %*% Fit.fix))
#for nu all_data

Model.Frame<-model.frame(formula = m2$nu.formula,data=data4);
Model.Matrix<-model.matrix(m2$nu.formula,Model.Frame)
Fit.fix<-matrix(m2$nu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
if(!is.null(m2$nu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
}else{print('no random effects for this term')};

nu<-as.vector(Model.Matrix[,colnames(Model.Matrix)[1]]*Fit.fix[1])

p2<-zzz_cent(obj=m2,type=c("centiles"),mu=mu,sigma=sigma,nu=nu,
             cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=data4$Age,
             calibration=FALSE,lpar=3)
p2[,'sigma']<-sigma

#female

#for mu
data4$Sex<-as.factor(data4$Sex)
data4$Site_ZZZ<-as.factor(data4$Site_ZZZ)
tem_rnd<-matrix(0,dim(data4)[1],1);
Model.Frame<-model.frame(formula = m2$mu.formula,data=data4);
Model.Matrix<-model.matrix(m2$mu.formula,Model.Frame)
Fit.fix<-matrix(m2$mu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
Fit.fix['SexMale',1]<-0
if(!is.null(m2$mu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(data4)[1]){
  if(data4$Site_ZZZ[iz] %in% names(m2$mu.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-m2$mu.coefSmo[[1]]$coef[as.character(data4$Site_ZZZ[iz])]
  }else{tem_rnd[iz]<-mean(m2$mu.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};

mu<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
#for sigma
tem_rnd<-matrix(0,dim(data4)[1],1);
Model.Frame<-model.frame(formula = m2$sigma.formula,data=data4);
Model.Matrix<-model.matrix(m2$sigma.formula,Model.Frame)
Fit.fix<-matrix(m2$sigma.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
Fit.fix['SexMale',1]<-0
if(!is.null(m2$sigma.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(data4)[1]){
  if(data4$Site_ZZZ[iz] %in% names(m2$sigma.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-m2$sigma.coefSmo[[1]]$coef[as.character(data4$Site_ZZZ[iz])]
  }else{tem_rnd[iz]<-mean(m2$sigma.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};


sigma<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))

#sigma<-(as.vector(Model.Matrix %*% Fit.fix))
#for nu all_data

Model.Frame<-model.frame(formula = m2$nu.formula,data=data4);
Model.Matrix<-model.matrix(m2$nu.formula,Model.Frame)
Fit.fix<-matrix(m2$nu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))

if(!is.null(m2$nu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
}else{print('no random effects for this term')};

nu<-as.vector(Model.Matrix[,colnames(Model.Matrix)[1]]*Fit.fix[1])

female_p2<-zzz_cent(obj=m2,type=c("centiles"),mu=mu,sigma=sigma,nu=nu,
             cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=data4$Age,
             calibration=FALSE,lpar=3)
female_p2[,'sigma']<-sigma

#male
#for mu
data4$Sex<-as.factor(data4$Sex)
data4$Site_ZZZ<-as.factor(data4$Site_ZZZ)
tem_rnd<-matrix(0,dim(data4)[1],1);
Model.Frame<-model.frame(formula = m2$mu.formula,data=data4);
Model.Matrix<-model.matrix(m2$mu.formula,Model.Frame)
Fit.fix<-matrix(m2$mu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))

if(!is.null(m2$mu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(data4)[1]){
  if(data4$Site_ZZZ[iz] %in% names(m2$mu.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-m2$mu.coefSmo[[1]]$coef[as.character(data4$Site_ZZZ[iz])]
  }else{tem_rnd[iz]<-mean(m2$mu.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};

mu<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
#for sigma
tem_rnd<-matrix(0,dim(data4)[1],1);
Model.Frame<-model.frame(formula = m2$sigma.formula,data=data4);
Model.Matrix<-model.matrix(m2$sigma.formula,Model.Frame)
Fit.fix<-matrix(m2$sigma.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))

if(!is.null(m2$sigma.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(data4)[1]){
  if(data4$Site_ZZZ[iz] %in% names(m2$sigma.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-m2$sigma.coefSmo[[1]]$coef[as.character(data4$Site_ZZZ[iz])]
  }else{tem_rnd[iz]<-mean(m2$sigma.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};


sigma<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))

#sigma<-(as.vector(Model.Matrix %*% Fit.fix))
#for nu all_data

Model.Frame<-model.frame(formula = m2$nu.formula,data=data4);
Model.Matrix<-model.matrix(m2$nu.formula,Model.Frame)
Fit.fix<-matrix(m2$nu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))

if(!is.null(m2$nu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
}else{print('no random effects for this term')};

nu<-as.vector(Model.Matrix[,colnames(Model.Matrix)[1]]*Fit.fix[1])

male_p2<-zzz_cent(obj=m2,type=c("centiles"),mu=mu,sigma=sigma,nu=nu,
                    cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=data4$Age,
                    calibration=FALSE,lpar=3)

male_p2[,'sigma']<-sigma


scale_fig=0.3

library(reshape2);
colnames(p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI','sigma');
mydata<-melt(p2,id='Age');colnames(mydata)<-c('Age','Percentile','Value')

dim(p2)[1]-1
Grad_p2<-(p2$median[2:dim(p2)[1]]-p2$median[1:(dim(p2)[1]-1)])/step_age
Grad_p2<-data.frame(c(Grad_p2,Grad_p2[dim(p2)[1]-1]));
p2<-cbind(p2,Grad_p2)
colnames(p2)[dim(p2)[2]]<-c('Gradient1')


png(filename = paste0(str,'_',i,'_all_without_sex_stratified_Gradient.png'), 
    width = 1480,           
    height = 1480,          
    units = "px",          
    bg = "white",          
    res = 300)     

p3<-ggplot()+
  #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
  # geom_point(data=data1[data1$Sex=='Female',],aes(x=Age,y=tem_feature),
  #            colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
  # geom_point(data=data1[data1$Sex=='Male',],aes(x=Age,y=tem_feature),
  #            colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
  geom_line(data=p2,aes(x=Age,y=Gradient1),color=c('#666666'),linewidth=1,linetype=c('solid'))+
  labs(x='Age (years)',y=c(i))+
  theme_bw()+
  scale_x_log10()

print(p3)  
dev.off()



png(filename = paste0(str,'_',i,'_all_without_sex_stratified.png'), 
    width = 1480,           
    height = 1480,          
    units = "px",          
    bg = "white",          
    res = 300)     

p3<-ggplot()+
  #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
  geom_point(data=data1[data1$Sex=='Female',],aes(x=Age,y=tem_feature),
             colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
  geom_point(data=data1[data1$Sex=='Male',],aes(x=Age,y=tem_feature),
             colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
  geom_line(data=p2,aes(x=Age,y=median),color=c('#666666'),linewidth=1,linetype=c('solid'))+
  geom_line(data=p2,aes(x=Age,y=lower99CI),color=c('#666666'),linewidth=1,linetype=c('dashed'))+
  geom_line(data=p2,aes(x=Age,y=lower95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
  geom_line(data=p2,aes(x=Age,y=upper95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
  geom_line(data=p2,aes(x=Age,y=upper99CI),color=c('#666666'),linewidth=1,linetype=c('dashed'))+
  labs(x='Age (years)',y=c(i))+
  theme_bw()+
  scale_x_log10()

print(p3)  
dev.off()


#for all population with sex stratified
#female data
colnames(female_p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI','sigma');
mydata<-melt(female_p2,id='Age');colnames(mydata)<-c('Age','Percentile','Value')
Female_p2<-female_p2;


#male data
colnames(male_p2)<-c('Age','lower99CI','lower95CI','median','upper95CI','upper99CI','sigma');
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


png(filename = paste0(str,'_',i,'_all_with_sex_stratified_Gradient.png'), 
    width = 1480,           
    height = 1480,          
    units = "px",          
    bg = "white",          
    res = 300)     

p3<-ggplot()+
  #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
  # geom_point(data=data1[data1$Sex=='Female',],aes(x=Age,y=tem_feature),
  #            colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
  # geom_point(data=data1[data1$Sex=='Male',],aes(x=Age,y=tem_feature),
  #            colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
  geom_line(data=Female_p2,aes(x=Age,y=Gradient1),color=c('#990000'),linewidth=1,linetype=c('solid'))+
  geom_line(data=Male_p2,aes(x=Age,y=Gradient1),color=c('#00CCFF'),linewidth=1,linetype=c('solid'))+
  labs(x='Age (years)',y=c(i))+
  theme_bw()+
  scale_x_log10()

print(p3)  
dev.off()


png(filename = paste0(str,'_',i,'_all_with_sex_stratified_sigma.png'), 
    width = 1480,           
    height = 1480,          
    units = "px",          
    bg = "white",          
    res = 300)     

p3<-ggplot()+
  #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
  # geom_point(data=data1[data1$Sex=='Female',],aes(x=Age,y=tem_feature),
  #            colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
  # geom_point(data=data1[data1$Sex=='Male',],aes(x=Age,y=tem_feature),
  #            colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
  geom_line(data=Female_p2,aes(x=Age,y=sigma),color=c('#990000'),linewidth=1,linetype=c('solid'))+
  geom_line(data=Male_p2,aes(x=Age,y=sigma),color=c('#00CCFF'),linewidth=1,linetype=c('solid'))+
  labs(x='Age (years)',y=c(i))+
  theme_bw()+
  scale_x_log10()

print(p3)  
dev.off()




png(filename = paste0(str,'_',i,'_all_with_sex_stratified.png'), 
    width = 1480,           
    height = 1480,          
    units = "px",          
    bg = "white",          
    res = 300)     

p3<-ggplot()+
  #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
  geom_point(data=data1[data1$Sex=='Female',],aes(x=Age,y=tem_feature),
             colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
  geom_point(data=data1[data1$Sex=='Male',],aes(x=Age,y=tem_feature),
             colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
  geom_line(data=Female_p2,aes(x=Age,y=median),color=c('#990000'),linewidth=1,linetype=c('solid'))+
  #geom_line(data=Female_p2,aes(x=Age,y=lower99CI),color=c('#990000'),linewidth=2,linetype=c('dashed'))+
  geom_line(data=Female_p2,aes(x=Age,y=lower95CI),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
  geom_line(data=Female_p2,aes(x=Age,y=upper95CI),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
  #geom_line(data=Female_p2,aes(x=Age,y=upper99CI),color=c('#990000'),linewidth=2,linetype=c('dashed'))+
  
  geom_line(data=Male_p2,aes(x=Age,y=median),color=c('#00CCFF'),linewidth=1,linetype=c('solid'))+
  #geom_line(data=Male_p2,aes(x=Age,y=lower99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
  geom_line(data=Male_p2,aes(x=Age,y=lower95CI),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
  geom_line(data=Male_p2,aes(x=Age,y=upper95CI),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
  #geom_line(data=Male_p2,aes(x=Age,y=upper99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
  
  # geom_line(data=p2,aes(x=Age,y=median),color=c('#666666'),linewidth=1,linetype=c('solid'))+
  # #geom_line(data=Male_p2,aes(x=Age,y=lower99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
  # geom_line(data=p2,aes(x=Age,y=lower95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
  # geom_line(data=p2,aes(x=Age,y=upper95CI),color=c('#666666'),linewidth=1,linetype=c('dotted'))+
  # #geom_line(data=Male_p2,aes(x=Age,y=upper99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
  
  labs(x='Age (years)',y=c(i))+
  theme_bw()+
scale_x_log10()

print(p3)  

dev.off()

#calculate the quantile 

Z_score_sum<-NULL;
Quant_score_sum<-NULL


#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%
#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%
#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%
#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%#all_data%%

all_data<-all_data[!is.na(all_data$feature)&!is.infinite(all_data$feature),]

#for mu
all_data$Sex<-as.factor(all_data$Sex)
all_data$Site_ZZZ<-as.factor(all_data$Site_ZZZ)
tem_rnd<-matrix(0,dim(all_data)[1],1);
Model.Frame<-model.frame(formula = m2$mu.formula,data=all_data);
Model.Matrix<-model.matrix(m2$mu.formula,Model.Frame)
Fit.fix<-matrix(m2$mu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
if(!is.null(m2$mu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(all_data)[1]){
  if(all_data$Site_ZZZ[iz] %in% names(m2$mu.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-m2$mu.coefSmo[[1]]$coef[as.character(all_data$Site_ZZZ[iz])]
  }else{tem_rnd[iz]<-mean(m2$mu.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};

Fit.fix['SexFemale',]<-0
mu<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
#for sigma
tem_rnd<-matrix(0,dim(all_data)[1],1);
Model.Frame<-model.frame(formula = m2$sigma.formula,data=all_data);
Model.Matrix<-model.matrix(m2$sigma.formula,Model.Frame)
Fit.fix<-matrix(m2$sigma.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
if(!is.null(m2$sigma.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
for(iz in 1:dim(all_data)[1]){
  if(all_data$Site_ZZZ[iz] %in% names(m2$sigma.coefSmo[[1]]$coef)){
    tem_rnd[iz]<-m2$sigma.coefSmo[[1]]$coef[as.character(all_data$Site_ZZZ[iz])]
  }else{tem_rnd[iz]<-mean(m2$sigma.coefSmo[[1]]$coef)}
}
}else{print('no random effects for this term')};

Fit.fix['SexFemale',]<-0
sigma<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))

#sigma<-(as.vector(Model.Matrix %*% Fit.fix))
#for nu

Model.Frame<-model.frame(formula = m2$nu.formula,data=all_data);
Model.Matrix<-model.matrix(m2$nu.formula,Model.Frame)
Fit.fix<-matrix(m2$nu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
if(!is.null(m2$nu.coefSmo[[1]]))
{Fit.fix[length(Fit.fix)]=0;
}else{print('no random effects for this term')};

nu<-as.vector(Model.Matrix[,colnames(Model.Matrix)[1]]*Fit.fix[1])



# p2<-zzz1_cent(obj=m2,type=c("centiles"),mu=mu,sigma=sigma,nu=nu,
#               cent = c(0.5, 2.5, 50, 97.5,99.5),xname = 'Age',xvalues=all_data$Age,
#               calibration=FALSE,lpar=3)
# 
# res1<-predictAll(m2,newdata=data4,type='response')


Z_score_sum<-zzz_cent(obj=m2,type=c("z-scores"),mu=mu,sigma=sigma,nu=nu,
                  xname = 'Age',xvalues=all_data$Age,yval=all_data$tem_feature,
                  calibration=FALSE,lpar=3)

Quant_score_sum<-zzz_cent(obj=m2,type=c("z-scores"),mu=mu,sigma=sigma,nu=nu,
                  xname = 'Age',xvalues=all_data$Age,yval=all_data$tem_feature,
                  calibration=FALSE,lpar=3,cdf=TRUE)


Z_score_sum<-data.frame(Z_score_sum);
colnames(Z_score_sum)<-c('Z_score');

Quant_score_sum<-data.frame(Quant_score_sum);
colnames(Quant_score_sum)<-c('Quant_score');

Z_data[[i]]<-Z_score_sum
Quant_data[[i]]<-Quant_score_sum

results<-list();
results$Female_p2<-Female_p2
results$Male_p2<-Male_p2
results$p2<-p2
results$m2<-m2
results$m0<-m0

results$list_fit<-list_fit

results$Zscore<-Z_data
results$Quant_data<-Quant_data
results$data1<-data1
results$all_data<-all_data
results$str<-str
results$i<-i
results$all_data_original<-all_data_original


saveRDS(results,paste0(str,'_',i,'_loop_our_model.rds'))


#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation
#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation
#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation
#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation

# set.seed(666)
# library('caret')
# folds<-createFolds(y=data1$individual_ID,k=5)
# Z_score_folds_HC1<-NULL
# Quant_score_folds_HC1<-NULL
# 
# for(i_fold in 1:5)
# {
#   train_data<-data1[-folds[[i_fold]],]
#   test_data<-data1[folds[[i_fold]],]
#   
# if(i_rnd==1&j_rnd==1){
#   m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
#              sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
#              control=con,
#              family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
#              data=train_data)}else if(i_rnd==1&j_rnd==0){
#                m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
#                           sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
#                           control=con,
#                           family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
#                           data=train_data)}else if(i_rnd==0&j_rnd==1){
#                             m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
#                                        sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex+random(Site_ZZZ),
#                                        control=con,
#                                        family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
#                                        data=train_data)}else if(i_rnd==0&j_rnd==0){
#                                          m2<-gamlss(formula=feature~bfpNA(Age,c(m0$mu.coefSmo[[1]]$power))+Sex,
#                                                     sigma.formula = feature~bfpNA(Age,c(m0$sigma.coefSmo[[1]]$power))+Sex,
#                                                     control=con,
#                                                     family = GG(mu.link='log',sigma.link = 'log',nu.link = 'identity'),
#                                                     data=train_data)}
#   
#   
#   
#   tem_rnd<-matrix(0,dim(test_data)[1],1);
#   Model.Frame<-model.frame(formula = m2$mu.formula,data=test_data);
#   Model.Matrix<-model.matrix(m2$mu.formula,Model.Frame)
#   Fit.fix<-matrix(m2$mu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
#   if(!is.null(m2$mu.coefSmo[[1]]))
#   {Fit.fix[length(Fit.fix)]=0;
#   for(iz in 1:dim(test_data)[1]){
#     if(test_data$Site_ZZZ[iz] %in% names(m2$mu.coefSmo[[1]]$coef)){
#       tem_rnd[iz]<-m2$mu.coefSmo[[1]]$coef[as.character(test_data$Site_ZZZ[iz])]
#     }else{tem_rnd[iz]<-mean(m2$mu.coefSmo[[1]]$coef)}
#   }
#   }else{print('no random effects for this term')};
#   
#   mu<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
#   #for sigma
#   tem_rnd<-matrix(0,dim(test_data)[1],1);
#   Model.Frame<-model.frame(formula = m2$sigma.formula,data=test_data);
#   Model.Matrix<-model.matrix(m2$sigma.formula,Model.Frame)
#   Fit.fix<-matrix(m2$sigma.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
#   if(!is.null(m2$sigma.coefSmo[[1]]))
#   {Fit.fix[length(Fit.fix)]=0;
#   for(iz in 1:dim(test_data)[1]){
#     if(test_data$Site_ZZZ[iz] %in% names(m2$sigma.coefSmo[[1]]$coef)){
#       tem_rnd[iz]<-m2$sigma.coefSmo[[1]]$coef[as.character(test_data$Site_ZZZ[iz])]
#     }else{tem_rnd[iz]<-mean(m2$sigma.coefSmo[[1]]$coef)}
#   }
#   }else{print('no random effects for this term')};
#   
#   
#   sigma<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
#   
#   #sigma<-(as.vector(Model.Matrix %*% Fit.fix))
#   #for nu
#   
#   Model.Frame<-model.frame(formula = m2$nu.formula,data=test_data);
#   Model.Matrix<-model.matrix(m2$nu.formula,Model.Frame)
#   Fit.fix<-matrix(m2$nu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
#   if(!is.null(m2$nu.coefSmo[[1]]))
#   {Fit.fix[length(Fit.fix)]=0;
#   }else{print('no random effects for this term')};
#   
#   nu<-as.vector(Model.Matrix[,colnames(Model.Matrix)[1]]*Fit.fix[1])
#   
#   
#   
#  
#   
#   
#   Z_score_folds_HC<-zzz_cent(obj=m2,type=c("z-scores"),mu=mu,sigma=sigma,nu=nu,
#                         xname = 'Age',xvalues=test_data$Age,yval=test_data$feature,
#                         calibration=FALSE,lpar=3)
#   
#   Z_score_folds_HC<-data.frame(Z_score_folds_HC);
#   
#   rownames(Z_score_folds_HC)<-data1$individual_ID[folds[[i_fold]]]
#   
#   Z_score_folds_HC1<-rbind(Z_score_folds_HC1,Z_score_folds_HC)
#   
#   
#   Quant_score_folds_HC<-zzz_cent(obj=m2,type=c("z-scores"),mu=mu,sigma=sigma,nu=nu,
#                             xname = 'Age',xvalues=test_data$Age,yval=test_data$feature,
#                             calibration=FALSE,lpar=3,cdf=TRUE)
#   Quant_score_folds_HC<-data.frame(Quant_score_folds_HC);
#   
#   rownames(Quant_score_folds_HC)<-data1$individual_ID[folds[[i_fold]]]
#   Quant_score_folds_HC1<-rbind(Quant_score_folds_HC1,Quant_score_folds_HC)
#   
# }
# 
# #five-fold cross-validation#five-fold cross-validation#five-fold cross-validation#five-fold cross-validation
# 
# results$Z_score_folds_HC<-Z_score_folds_HC1;
# results$Quant_score_folds_HC<-Quant_score_folds_HC1;
# saveRDS(results,paste0(str,'_',i,'_loop_our_model.rds'))

}
# 
# write.csv(Z_data,paste(str,'_data_z_score_Results_our_model.csv'))
# write.csv(Quant_data,paste(str,'_data_Quant_score_Results_our_model.csv'))
}



