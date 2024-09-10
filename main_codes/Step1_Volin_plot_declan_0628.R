#install.packages('gamlss')
rm(list=ls())

library(gamlss)
library(ggplot2)
library(reshape2)


datapath='C:/ZZZ/work/manuscript/Lifespan-main'# change
setwd(datapath)#set the filepath
source("100.common-variables.r")
source("101.common-functions.r")

source("ZZZ_function.R")
source("300.variables.r")
source("301.functions.r")

library(raincloudplots)
library(commonmark)
library(ggplot2)
#library(ggstatsplot)
library(lmerTest)
library(effectsize)
library(ggrain)
library(ggsci)
library(ggdist)
library(hrbrthemes)
library(coin)
library(bayestestR)
library(MatchIt)

#tem_zzz_feature<-c('Global_feature');str<-'Global'

tem_zzz_feature<-c('Global_feature','aseg.vol.table','lh.aparc.area.table','rh.aparc.area.table',
                    'lh.aparc.thickness.table','rh.aparc.thickness.table',
                    'lh.aparc.volume.table','rh.aparc.volume.table');


RDSfile<-"aseg.vol.table_Brain-Stem_loop_our_model.rds"
feature_path=paste0(datapath,'/test_zzz_update_20240116/Global_feature')# change
setwd(feature_path)
results<-readRDS(RDSfile)
data<-results$all_data_original
data<-data[,c(1:16)]
tem0<-results$all_data;
disease<-c('MCI','AD',"PD","SVD",
  "MS","AQP4Pos_NMOSD")
# tem0<-tem0[tem0$Diagnosis=='MCI'|
#              tem0$Diagnosis=='AD'|
#              tem0$Diagnosis=='PD'|
#              tem0$Diagnosis=='SVD'|
#              tem0$Diagnosis=='MS'|
#              tem0$Diagnosis=='AQP4Pos_NMOSD',];
# names<-rownames(tem0);


#tem_zzz_feature<-c('Global_feature');str<-'Global'

tem_zzz_feature<-c('Global_feature','aseg.vol.table','lh.aparc.area.table','rh.aparc.area.table',
                   'lh.aparc.thickness.table','rh.aparc.thickness.table',
                   'lh.aparc.volume.table','rh.aparc.volume.table');str<-'All'


for(i_zzz_feature in tem_zzz_feature)
{ 
  feature_path=paste0(datapath,'/test_zzz_update_20240116/',i_zzz_feature)# change
  setwd(feature_path)
  myfile <- list.files()
  RDSfile_all <- myfile[grep(myfile,pattern ="our_model.rds$")]   
  RDSfile_all
  
  
  Exd_var<-c("Global_feature_meanCT2_lhMeanThickness_loop_our_model.rds",
             "Global_feature_meanCT2_lhVertex_loop_our_model.rds" ,
             "Global_feature_meanCT2_rhMeanThickness_loop_our_model.rds",
             "Global_feature_meanCT2_rhVertex_loop_our_model.rds",
             "Global_feature_totalSA2_lh_loop_our_model.rds",
             "Global_feature_totalSA2_rh_loop_our_model.rds",
             'aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds',
             "aseg.vol.table_cerebellum_WM_loop_our_model.rds" ,
             #"aseg.vol.table_cerebellum_total_loop_our_model.rds",
             "aseg.vol.table_cerebellum_GM_loop_our_model.rds",
             # "aseg.vol.table_Brain-Stem_loop_our_model.rds",
             "lh.aparc.thickness.table_eTIV_loop_our_model.rds",
             "rh.aparc.thickness.table_eTIV_loop_our_model.rds",
             "lh.aparc.area.table_eTIV_loop_our_model.rds",
             "rh.aparc.area.table_eTIV_loop_our_model.rds",
             "lh.aparc.volume.table_eTIV_loop_our_model.rds",
             "rh.aparc.volume.table_eTIV_loop_our_model.rds")
  
  for(RDSfile_loop in RDSfile_all)
  {
    print(RDSfile_loop)
    if(RDSfile_loop %in% Exd_var){next}
    
    RDSfile<-RDSfile_loop
    
    setwd(feature_path);
    results<-readRDS(RDSfile)
    
    tem_data1<-results$Quant_data[results$i]
    
    tem_data1<-data.frame(tem_data1);
    rownames(tem_data1)<-rownames(results$all_data)
    
    tem_data1[is.infinite(tem_data1$Quant_score)|
                is.na(tem_data1$Quant_score),'Quant_score']<-0
    
    sel_index<-intersect(rownames(tem_data1),rownames(data))
    data<-cbind(data[sel_index,],tem_data1[sel_index,])
    colnames(data)[dim(data)[2]]<-results$i
    
  }
}



results_list<-list()

bootstrap_num<-5000

disease<-c('MCI','AD',"PD","SVD",
           "MS","AQP4Pos_NMOSD")

list<-data.frame(matrix(0,1,11))

colnames(list)<-c('feature','disease',
                  'cohens_d','cohens_d_CI_low','cohens_d_CI_high',
                  'median','per25','per75','mean','sd','boot_num')

num<-0

for(boot_num in 1018:bootstrap_num)
{

print(boot_num)
savepath<-paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results');

for(i in disease)
{ print(i)
  data0<-data 
  data1<-data0;
  tem_data_dis<-data1[data1$Diagnosis==i,];
  
  tem_data1<-rbind(tem_data_dis,data1[data1$Diagnosis=='HC',])
  
  tem_data1<-tem_data1[tem_data1$Age>=min(tem_data1[data1$Diagnosis==i,'Age'],na.rm = TRUE)&
                         tem_data1$Age<=max(tem_data1[data1$Diagnosis==i,'Age'],na.rm = TRUE),]
  
  tem_data1<-na.omit(tem_data1)
  
  tem_data1$Diagnosis<-factor(tem_data1$Diagnosis,levels=c('HC',i))
  match.it <- matchit(Diagnosis ~ Age + Sex +Site_ZZZ, data = tem_data1, method="nearest", ratio=1)
  
  tem_data1<-tem_data1[c(rownames(match.it$match.matrix),match.it$match.matrix),]
  
  tem_HC<-tem_data1[tem_data1$Diagnosis=='HC',];
  
  tem_data_dis<-na.omit(tem_data_dis)
  tem_data_dis<-tem_data_dis[sample(c(1:length(rownames(tem_data_dis))),length(rownames(tem_data_dis)),replace = TRUE),]

  library(foreach)
  library(doParallel)
  cl<-makeCluster(5);
  registerDoParallel(cl)
  
  
  feature_num<-length(colnames(data)[17:dim(data)[2]])
  
  results1<-foreach(feature=colnames(data)[17:dim(data)[2]]) %dopar% {
  
  library("effectsize")
  #num=num+1;
  tem_median<-quantile(tem_data_dis[,feature],0.5,na.rm=TRUE);
  tem_25per<-quantile(tem_data_dis[,feature],0.25,na.rm=TRUE);
  tem_75per<-quantile(tem_data_dis[,feature],0.75,na.rm=TRUE);
  tem_mean<-mean(tem_data_dis[,feature],na.rm=TRUE);
  tem_std<-sd(tem_data_dis[,feature],na.rm=TRUE);
  
  
  tem_data1<-rbind(tem_HC,tem_data_dis);
  
  tem_data1$Diagnosis<-factor(tem_data1$Diagnosis,levels=c('HC',i))
  tem_d<-cohens_d(tem_data1[,feature]~tem_data1[,'Diagnosis'],data=tem_data1,na.action="na.omit",method = "qr")
 
  return(data.frame(c(feature,i,tem_d$Cohens_d,tem_d$CI_low,tem_d$CI_high,
           tem_median,tem_25per,tem_75per,tem_mean,tem_std,boot_num)))
  
  # list[num,1]<-feature
  # list[num,2]<-i
  # list[num,3]<-tem_d$Cohens_d
  # list[num,4]<-tem_d$CI_low
  # list[num,5]<-tem_d$CI_high
  # list[num,6]<-tem_median
  # list[num,7]<-tem_25per
  # list[num,8]<-tem_75per
  # list[num,9]<-tem_mean
  # list[num,10]<-tem_std
  # list[num,11]<-boot_num
}       

# list[[i]][boot_num]<-results1
# list[i][boot_num]<-results1

for(num1 in 1:feature_num)
{
#print(num1)
tem_result<-data.frame(t(results1[[num1]]))
colnames(tem_result)<-colnames(list)

list<-rbind(list,tem_result);
}

# for(feature in colnames(data)[17:dim(data)[2]])
# {
#     
#     num=num+1;
#     tem_median<-quantile(tem_data_dis[,feature],0.5,na.rm=TRUE);
#     tem_25per<-quantile(tem_data_dis[,feature],0.25,na.rm=TRUE);
#     tem_75per<-quantile(tem_data_dis[,feature],0.75,na.rm=TRUE);
#     tem_mean<-mean(tem_data_dis[,feature],na.rm=TRUE);
#     tem_std<-sd(tem_data_dis[,feature],na.rm=TRUE);
#    
# 
#     tem_data1<-rbind(tem_HC,tem_data_dis);
# 
#     tem_data1$Diagnosis<-factor(tem_data1$Diagnosis,levels=c('HC',i))
#     #tem_stat<-lm(tem_data1[,feature]~tem_data1[,'Diagnosis'],data=tem_data1,na.action="na.omit",method = "qr")
# 
#     # 
#     # tem_stats<-summary(tem_stat)
#     # 
#     # cohens_d(tem_data1[,feature]~tem_data1[,'Diagnosis'],data=tem_data1,
#     #          distribution=approximate(nresample=1000))
#     tem_d<-cohens_d(tem_data1[,feature]~tem_data1[,'Diagnosis'],data=tem_data1,na.action="na.omit",method = "qr")
#     # tem_g<-hedges_g(tem_data1[,feature]~tem_data1[,'Diagnosis'],data=tem_data1,na.action="na.omit",method = "qr")
#     # tem_delta<-glass_delta(tem_data1[,feature]~tem_data1[,'Diagnosis'],data=tem_data1,na.action="na.omit",method = "qr")
#     # 
#     # model <- aov(tem_data1[,feature]~tem_data1[,'Diagnosis'],data=tem_data1,na.action="na.omit")
#     # 
#     # tem_eta2<-eta_squared(model)
#     # 
#     # G1<-tem_data1[tem_data1$Diagnosis=='HC',feature];
#     # G2<-tem_data1[tem_data1$Diagnosis==i,feature];
#     # 
#     # tem_stats_nonpara<-wilcox.test(G1,G2,na.action="na.omit",paired=FALSE)
#     # 
#     # tem_op<-p_overlap(G1,G2,parametric = FALSE)
#     # 
#     # tem_rb<-rank_biserial(G1,G2,paired = FALSE);
#     # tem_cld<-cliffs_delta(G1,G2,paired = FALSE)
#     # tem_u1<-cohens_u1(G1,G2,parametric = TRUE)
#     # 
#     # tem_data1[tem_data1$Diagnosis=='HC','label']<-0
#     # tem_data1[tem_data1$Diagnosis==i,'label']<-1
#     # 
#     # tem_logs<-glm(tem_data1[,'label']~tem_data1[,feature],family=binomial(link='logit'),
#     #               data=tem_data1,na.action="na.omit")#y和x变量搞反了，需要重新计算即可
#     # tem_logs_CI<-confint(tem_logs)
#     
#     
#     list[num,1]<-feature
#     list[num,2]<-i
#     # list[num,3]<-tem_stats$coefficients[2,3]
#     # list[num,4]<-tem_stats$coefficients[2,4]
#     # list[num,5]<-tem_stats$df[2]
#     # list[num,6]<-tem_stats$fstatistic[1]
#     # list[num,7]<-tem_stats$adj.r.squared
#     list[num,8]<-tem_d$Cohens_d
#     list[num,9]<-tem_d$CI_low
#     list[num,10]<-tem_d$CI_high
#     # list[num,11]<-tem_g$Hedges_g
#     # list[num,12]<-tem_g$CI_low
#     # list[num,13]<-tem_g$CI_high
#     # 
#     # list[num,14]<-tem_delta$Glass_delta
#     # list[num,15]<-tem_delta$CI_low
#     # list[num,16]<-tem_delta$CI_high
#     # 
#     # list[num,17]<-tem_eta2$Eta2
#     # list[num,18]<-tem_eta2$CI_low
#     # list[num,19]<-tem_eta2$CI_high
#     # 
#     # list[num,20]<-tem_logs$coefficients[2]
#     # list[num,21]<-tem_logs_CI[2,1]
#     # list[num,22]<-tem_logs_CI[2,2]
#     # 
#     # list[num,23]<-tem_op$Overlap;
#     # list[num,24]<-tem_op$CI_low
#     # list[num,25]<-tem_op$CI_high
#     # 
#     # list[num,26]<-tem_rb$r_rank_biserial;
#     # list[num,27]<-tem_rb$CI_low
#     # list[num,28]<-tem_rb$CI_high
#     # 
#     # list[num,29]<-tem_cld$r_rank_biserial;
#     # list[num,30]<-tem_cld$CI_low
#     # list[num,31]<-tem_cld$CI_high
#     # 
#     # list[num,32]<-tem_u1$Cohens_U1;
#     # list[num,33]<-tem_u1$CI_low
#     # list[num,34]<-tem_u1$CI_high
#     # 
#     # list[num,35]<-tem_stats_nonpara$statistic
#     # list[num,36]<-tem_stats_nonpara$p.value
#     # 
#     
#     list[num,37]<-tem_median
#     list[num,38]<-tem_25per
#     list[num,39]<-tem_75per
#     list[num,40]<-tem_mean
#     list[num,41]<-tem_std
#     list[num,42]<-boot_num
#     
#   }
#   
 }
 }

setwd(savepath)
saveRDS(list,paste0(str,'boot_MRI_measure_gourp_analysis_0628_part1018-1300.rds')) 
  


# 
# list_max<-list()
# var<-c('median','per25','per75','mean','sd')
# 
# for(para in var)
# {
#   num=0;
# for(i in 1:bootstrap_num)
# {
# 
# Greatest_list<-data.frame(matrix(NaN,length(disease),10))
# colnames(Greatest_list)<-colnames(list)
# 
# 
# for(dis_ind in 1:disease)
# {
#   tem_list<-list[list$boot_num==i&
#                    list$disease==dis_ind,];
#   
#   Greatest_list[num,]<-tem_list[which.max((tem_list[,para])),]
#  
# }
# 
# print(Greatest_list)
# 
# list_max[[para]]<-Greatest_list
# 
# 
# }
# }
# 
# saveRDS(list_max,paset0(str,'Greatest_feature_global.rds'));
# #saveRDS(list_max,paste0(str,'Greatest_feature_local.rds'));
