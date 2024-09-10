#install.packages('gamlss')
rm(list=ls())

datapath='C:/ZZZ/work/manuscript/Lifespan-main'# change
setwd(datapath)
source("100.common-variables.r")
source("101.common-functions.r")

source("ZZZ_function.R")
source("300.variables.r")
source("301.functions.r")

datapath='C:/ZZZ/work/external_work/Suli'

savepath=paste0(datapath,'/MOGAD')
savepath0=paste0(datapath,'/MOGAD/normative_model')

setwd(datapath)#set the filepath
data1<-read.csv('MOGAD_data.csv',check.names=FALSE)
data2<-read.csv('HC_data.csv',check.names=FALSE)

str_lab='MOGAD';str='normative_model';

data1[,'G']<-c('MOGAD');
data2[,'G']<-c('HC');

sel_index<-colnames(data1)[c(1:41,55:130)];

data<-rbind(data1[,sel_index],data2[,sel_index])

data<-data[!is.na(data$Age)&!is.na(data$Sex)&
                  !is.na(data$Site_ZZZ),]


for(i in 1:dim(data)[1])
{
  data[i,'mean_thickness']<-
    (data[i,'meanCT2_lhMeanThickness']*data[i,'meanCT2_lhVertex']+
       data[i,'meanCT2_rhMeanThickness']*data[i,'meanCT2_rhVertex'])/(data[i,'meanCT2_lhVertex']+data[i,'meanCT2_rhVertex'])


  data[i,'total_surface_arrea']<-data[i,'totalSA2_lh']+data[i,'totalSA2_rh'];
}



data[,'cerebellum_WM']<-data$`Left-Cerebellum-White-Matter`+data$`Right-Cerebellum-White-Matter`
data[,'cerebellum_GM']<-data$`Left-Cerebellum-Cortex`+data$`Right-Cerebellum-Cortex`
data[,'cerebellum_total']<-data[,'cerebellum_WM']+data[,'cerebellum_GM'];
 
  
tem_feature<-c("chorod_plexus","GMV","sGMV","WMV","Ventricles",
               "CSF","mean_thickness","total_surface_arrea","EstimatedTotalIntraCranialVol")

  

  
data_centiles<-NULL
data_centiles_qua<-NULL
  

  
setwd(savepath0)
  
all_data<-data
  
  
 
# all_data<-na.omit(all_data);
  
Z_data<-list();
Quant_data<-list()
  
for(iz in 1:dim(all_data)[1]){all_data[iz,'individual_ID']<-paste0(all_data$Freesufer_Path1[iz],all_data$Freesufer_Path2[iz],all_data$Freesufer_Path3[iz],sep="_")}
library(dplyr)
all_data<- all_data %>% distinct(individual_ID, .keep_all=T)
rownames(all_data)<-all_data$individual_ID


all_data_original<-all_data
all_data_final<-all_data_original;

  #for(i in tem_feature[1:length(tem_feature)])
  for(i in tem_feature[1:length(tem_feature)])
  {
    
    #if(!file.exists(paste0(i,'_loop_our_model.rds'))){next;print('file not exist')};
    
   
    all_data<-all_data_original
    
    all_data[,'feature']<-all_data[,i]
   
  
    con=gamlss.control(c.crit = 0.01, n.cyc = 3,autostep = FALSE)
    
    setwd(savepath0)
    if(!file.exists(paste0(i,'_loop_our_model.rds'))){next}
    results<-readRDS(paste0(i,'_loop_our_model.rds'))
    m0<-results$m0
    m2<-results$m2
    if(!is.null(m2$mu.coefSmo[[1]])){i_rnd=1}else{i_rnd=0};
    if(!is.null(m2$sigma.coefSmo[[1]])){j_rnd=1}else{j_rnd=0};
    
    
    setwd(savepath)
    
    
    Z_score_sum<-NULL;
    Quant_score_sum<-NULL
    


    all_data<-all_data[!is.na(all_data$feature)&!is.infinite(all_data$feature),]
    #for mu
    all_data$Sex<-as.factor(all_data$Sex)
    all_data$Site_ZZZ<-as.factor(all_data$Site_ZZZ)
    tem_rnd<-matrix(0,dim(all_data)[1],1);
    tem_age<-matrix(0,dim(all_data)[1],1);
    
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
    
    
    mu<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
    
    mu<-exp(as.vector(Model.Matrix %*% Fit.fix))
            
    #for sigma
    tem_rnd<-matrix(0,dim(all_data)[1],1);
    tem_age<-matrix(0,dim(all_data)[1],1);
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
    
    
    sigma<-exp(as.vector(Model.Matrix %*% Fit.fix)+as.vector(tem_rnd))
    
    sigma<-exp(as.vector(Model.Matrix %*% Fit.fix))
    
    #for nu
    
    Model.Frame<-model.frame(formula = m2$nu.formula,data=all_data);
    Model.Matrix<-model.matrix(m2$nu.formula,Model.Frame)
    Fit.fix<-matrix(m2$nu.coefficients[colnames(Model.Matrix)],ncol=1,dimnames = list(colnames(Model.Matrix),'Beta'))
    if(!is.null(m2$nu.coefSmo[[1]]))
    {Fit.fix[length(Fit.fix)]=0;
    }else{print('no random effects for this term')};
    
    nu<-as.vector(Model.Matrix[,colnames(Model.Matrix)[1]]*Fit.fix[1])
    
    # 
    

    Z_score_sum<-zzz_cent(obj=m2,type=c("z-scores"),mu=mu,sigma=sigma,nu=nu,
                          xname = 'Age',xvalues=all_data$Age,yval=all_data$feature,
                          calibration=FALSE,lpar=3)
    
    Quant_score_sum<-zzz_cent(obj=m2,type=c("z-scores"),mu=mu,sigma=sigma,nu=nu,
                              xname = 'Age',xvalues=all_data$Age,yval=all_data$feature,
                              calibration=FALSE,lpar=3,cdf=TRUE)
    
    
    Z_score_sum<-data.frame(Z_score_sum);
    Quant_score_sum<-data.frame(Quant_score_sum);
    
    
    all_data_final[rownames(all_data),paste0('Zscore_',i)]<-Z_score_sum
    all_data_final[rownames(all_data),paste0('Centile_',i)]<-Quant_score_sum

    
  }


setwd(savepath)
write.csv(all_data_final,'stats_data.csv')
  



