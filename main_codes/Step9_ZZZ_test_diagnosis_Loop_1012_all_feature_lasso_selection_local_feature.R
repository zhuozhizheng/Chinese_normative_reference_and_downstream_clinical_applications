#install.packages('gamlss')
rm(list=ls())
library(glmnet)
library(dplyr);
library(ggplot2)
#library(ggseg)
# install.packages("ggplot2")
library(ggplot2)
library(stringr)
library(robustbase)
# setwd('C:/ZZZ/work/manuscript/Lifespan-main/test_zzz/aseg.vol.table')
#install.packages('gamlss')
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

path<-list()
# str_lab<-'Global_feature'# change
# path[[1]]<-paste0(datapath,'/test_zzz/Global_feature');

# str_lab<-'local_feature'# change
# path[[1]]<-paste0(datapath,'/test_zzz/aseg.vol.table');
# path[[2]]<-paste0(datapath,'/test_zzz/lh.aparc.thickness.table');
# path[[3]]<-paste0(datapath,'/test_zzz/rh.aparc.thickness.table');
# path[[4]]<-paste0(datapath,'/test_zzz/lh.aparc.area.table');
# path[[5]]<-paste0(datapath,'/test_zzz/rh.aparc.area.table');
# path[[6]]<-paste0(datapath,'/test_zzz/lh.aparc.volume.table');
# path[[7]]<-paste0(datapath,'/test_zzz/rh.aparc.volume.table');

str_lab<-'global_and_local_feature'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/aseg.vol.table');
path[[2]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.thickness.table');
path[[3]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.thickness.table');
path[[4]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.area.table');
path[[5]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.area.table');
path[[6]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.volume.table');
path[[7]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.volume.table');
path[[8]]<-paste0(datapath,'/test_zzz_update_20240116/Global_feature');


savepath<-paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results/figuredps');

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
           #"aseg.vol.table_Brain-Stem_loop_our_model.rds",
           "lh.aparc.thickness.table_eTIV_loop_our_model.rds",
           "rh.aparc.thickness.table_eTIV_loop_our_model.rds",
           "lh.aparc.area.table_eTIV_loop_our_model.rds",
           "rh.aparc.area.table_eTIV_loop_our_model.rds",
           "lh.aparc.volume.table_eTIV_loop_our_model.rds",
           "rh.aparc.volume.table_eTIV_loop_our_model.rds",
           
           "Global_feature_meanCT2_lhMeanThickness_loop_our_model.rds",
           "Global_feature_meanCT2_lhVertex_loop_our_model.rds" ,
           "Global_feature_meanCT2_rhMeanThickness_loop_our_model.rds",
           "Global_feature_meanCT2_rhVertex_loop_our_model.rds",
           "Global_feature_totalSA2_lh_loop_our_model.rds",
           "Global_feature_totalSA2_rh_loop_our_model.rds",
           'aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds',
           "aseg.vol.table_cerebellum_WM_loop_our_model.rds" ,
           #"aseg.vol.table_cerebellum_total_loop_our_model.rds",
           "aseg.vol.table_cerebellum_GM_loop_our_model.rds",
           #"aseg.vol.table_Brain-Stem_loop_our_model.rds",
           "lh.aparc.thickness.table_eTIV_loop_our_model.rds",
           "rh.aparc.thickness.table_eTIV_loop_our_model.rds",
           "lh.aparc.area.table_eTIV_loop_our_model.rds",
           "rh.aparc.area.table_eTIV_loop_our_model.rds",
           "lh.aparc.volume.table_eTIV_loop_our_model.rds",
           "rh.aparc.volume.table_eTIV_loop_our_model.rds")


RDSfile_all<-NULL 
for(i in 1:length(path))
{
  feature_path=path[[i]];
  setwd(feature_path)
  myfile <- list.files()
  RDSfile_all0 <- myfile[grep(myfile,pattern ="our_model.rds$")]
  for(j in RDSfile_all0)
  {
    if(!(j %in% Exd_var))
    {
      #RDSfile_all0<-paste0(feature_path,'/',RDSfile_all0);
      RDSfile_all<-c(RDSfile_all,paste0(feature_path,'/',j));
    }
  }
}



results<-readRDS(RDSfile_all[1]);
#data<-results$all_data_original
data<-results$all_data[,1:17]
for(RDSfile_loop in RDSfile_all)
{
  
  if(!(RDSfile_loop %in% Exd_var)){
  print(RDSfile_loop)
  RDSfile<-RDSfile_loop
  results<-readRDS(RDSfile)
  tem_data1<-results$Quant_data[results$i]
  tem_data1<-data.frame(tem_data1);
  rownames(tem_data1)<-results$all_data$individual_ID;
  index<-intersect(rownames(tem_data1),rownames(data));
  data<-cbind(data[index,],tem_data1[index,])
  colnames(data)[dim(data)[2]]<-results$i
  }
}  

datazzz<-data;
data<-datazzz
data<-na.omit(data)
data$Sex<-as.character(data$Sex)
data[data$Sex=='Male','Sex']<-c(1);
data[data$Sex=='Female','Sex']<-c(0);
data$Sex<-as.numeric(data$Sex)


disease<-c('MCI','AD',"PD","SVD",
           "MS","AQP4Pos_NMOSD")

data1<-data[data$Diagnosis=='HC',];

for(i_dis in disease)
{
  data1<-rbind(data1,data[data$Diagnosis==i_dis,])
  
}

data<-data1;  
data<-na.omit(data)

start_col=18;
str<-str_lab;

#str='rh.DKTatlas.area.table';del_str='_area'
# setwd('C:/ZZZ/work/manuscript/Lifespan-main/test_zzz/Global_feature')
# 
# data<-read.csv('Global_feature _Results.csv',header = TRUE);start_col=21;str='Global_feature'




if (!(dir.exists(paste0(savepath,'/',str))))
{dir.create(paste0(savepath,'/',str))}

setwd(paste0(savepath,'/',str))


# data[data$Sex=='Female','Sex']<-1;
# data[data$Sex=='Male','Sex']<-0;
# data$Sex<-as.numeric(data$Sex);

tem_data1<-data;
list_eta<-NULL
num=0;
list_roc<-list()
list_roc_results<-list()
list_stats<-data.frame(matrix(NA,5,length(disease)));
colnames(list_stats)<-disease;
rownames(list_stats)<-c('median','Q25','Q75','Min','Max');
model<-list()
model_sum<-list()
feature_sum<-list()
Disease_risk<-list()
HC_risk<-list()
sum1<-list()
for(ii in disease[1:length(disease)])
{
  ii
  
  num=num+1;

tem_data<-data[data$Diagnosis=='HC'|data$Diagnosis==disease[num]&
                 data$Age>=min(data[data$Diagnosis==disease[num],'Age'],na.rm=TRUE)&
                 data$Age<=max(data[data$Diagnosis==disease[num],'Age'],na.rm=TRUE),];
  
  tem_data<-na.omit(tem_data)
  tem_data$Diagnosis<-factor(tem_data$Diagnosis,levels=c('HC',disease[num]))
  library(MatchIt)
  #tem_data$Sex<-as.factor(tem_data$Sex)
  match.it <- matchit(Diagnosis ~ Age + Sex+ + Site_ZZZ, data = tem_data, method="nearest", ratio=1)
  #a <- summary(match.it)
  tem_data<-tem_data[c(rownames(match.it$match.matrix),match.it$match.matrix),]
  tem_data<-na.omit(tem_data)
  

tem_data$Diagnosis<-factor(tem_data$Diagnosis,
                           levels = c('HC',disease[num]));

tem_data0<-tem_data[,c('Diagnosis','Age','Sex',colnames(tem_data)[start_col:dim(tem_data)[2]])];

#tem_data0<-tem_data0[!is.na(tem_data0$Age)&!is.na(tem_data0$Sex)&!is.na(tem_data0[,4]),]

tem_data0<-na.omit(tem_data0);

tem_data0[tem_data0$Diagnosis=='HC','label']<-0;

tem_data0[tem_data0$Diagnosis==disease[num],'label']<-1;

#tem_data0$label<-factor(tem_data0$label,levels=c('0','1'))

tem_data0<-tem_data0[is.finite(rowSums(tem_data0[,4:dim(tem_data0)[2]])),]

tem_data0<-tem_data0[,2:dim(tem_data0)[2]]


X=as.matrix(tem_data0[,1:(dim(tem_data0)[2]-1)]);
Y=as.matrix(tem_data0[,'label']);

mod_cv <- cv.glmnet(x=X,
                    y=Y, 
                    family="gaussian", nfolds = 10,
                    intercept = F, alpha=1)

best_lambda <- mod_cv$lambda.min

best_model <- glmnet(x=X, 
                     y=Y, 
                     alpha = 1, lambda = best_lambda)
tem_coef<-data.frame(matrix(coef(best_model)))

rownames(tem_coef)<-c('intercept',colnames(tem_data0)[1:(dim(tem_data0)[2]-1)]);
tem_coef
index_feature=which(tem_coef!=0);

index_feature<-index_feature[2:length(index_feature)]
sel_feature<-rownames(tem_coef)[index_feature]

feature_sum[[disease[num]]]<-sel_feature
sel_feature<-c('label',sel_feature);

tem_data0<-tem_data0[,sel_feature]

#tem_data0<-tem_data0[!is.infinite(rowSums(tem_data0[4:dim(tem_data0)[2]])),]
#tem_data0$Sex<-as.factor(tem_data0$Sex)

library(e1071)

svm_mdoel<-svm(as.factor(label)~.,data=tem_data0,kernel='radial',type='C',probability = TRUE)
tune_result<-tune(svm,as.factor(label)~.,data=tem_data0,kernel='radial',type='C',probability = TRUE,cross=10,
                  ranges=list(cost=c(0.1,1,10),gama=c(0.1,1,10)))
best_model<-tune_result$best.model


# pre<-glm(factor(label)~., 
#          family='binomial',data=tem_data0)
# summary(pre);

model[[disease[num]]]<-best_model

real<-tem_data0$label;

tem_predicted<-predict(best_model,tem_data0,probability=T)
predict.<-attr(tem_predicted, "probabilities")[,1]

#predict.<-predict.glm(pre,type = 'response',newdata = tem_data0)

library(pROC)
list_roc[[disease[num]]]<-pROC::roc(real, predict.,ci=TRUE)

tem_roc<-list_roc[[disease[num]]]
Sen<-tem_roc$sensitivities[which.max(tem_roc$sensitivities+tem_roc$specificities-1)]
Spe<-tem_roc$specificities[which.max(tem_roc$sensitivities+tem_roc$specificities-1)]
AUC<-tem_roc$auc
AUC_low_CI<-tem_roc$ci[1]
AUC_high_CI<-tem_roc$ci[3]
thr<-tem_roc$thresholds[which.max(tem_roc$sensitivities+tem_roc$specificities-1)]

predict_value=ifelse(tem_roc$original.predictor>thr,1,0);

ACC=sum(predict_value==tem_roc$original.response)/length(predict_value)


list_roc_results[[disease[num]]]['Sen']<-Sen
list_roc_results[[disease[num]]]['Spe']<-Spe
list_roc_results[[disease[num]]]['Acc']<-ACC
list_roc_results[[disease[num]]]['AUC']<-AUC
list_roc_results[[disease[num]]]['AUC_low_CI']<-AUC_low_CI
list_roc_results[[disease[num]]]['AUC_high_CI']<-AUC_high_CI

predict1=ifelse(predict.>0.5,1,0)


tem_data0[,'predict']=predict1;
library(ROCR)

pred<-prediction(predict.,real);

AUC=performance(pred,'auc')@y.values


tem_predict1<-predict(best_model,data,probability=T)

tem_predict<-data.frame(attr(tem_predict1, "probabilities")[,1])

tem_data1<-cbind(tem_data1,tem_predict);
colnames(tem_data1)[dim(data)[2]+num]<-paste0(disease[num]);


tem_data2<-tem_data1[tem_data1$Diagnosis=='HC',];

for(dis_z in c(disease))
{
  tem_data2<-rbind(tem_data2,tem_data1[tem_data1$Diagnosis==dis_z,])
}

HC_risk[[disease[num]]]<-tem_data2

tem_data2$Diagnosis<-factor(tem_data2$Diagnosis,levels=c('HC',disease))

rad_data<-aggregate(tem_data2[,disease[num]],by=list(type=tem_data2$Diagnosis),mean,na.rm=TRUE)

list_stats[1,disease[num]]<-quantile(tem_data2[tem_data2$Diagnosis==disease[num],disease[num]],0.5,na.rm=TRUE);
list_stats[2,disease[num]]<-quantile(tem_data2[tem_data2$Diagnosis==disease[num],disease[num]],0.25,na.rm=TRUE);
list_stats[3,disease[num]]<-quantile(tem_data2[tem_data2$Diagnosis==disease[num],disease[num]],0.75,na.rm=TRUE);
list_stats[4,disease[num]]<-quantile(tem_data2[tem_data2$Diagnosis==disease[num],disease[num]],0,na.rm=TRUE);
list_stats[5,disease[num]]<-quantile(tem_data2[tem_data2$Diagnosis==disease[num],disease[num]],1,na.rm=TRUE);


rad_data<-data.frame(rad_data)
rad_data1<-rad_data


boot_num=1000;
mean_value_boot<-data.frame(matrix(NA,1000,1))
for(dis in disease)
{

  tem_dis<-tem_data2[tem_data2$Diagnosis==dis,disease[num]];
  #mean_value[dis]<-mean(tem_dis,na.rm=TRUE);
  #sd_value[dis]<-sd(tem_dis,na.rm=TRUE);

  for(j in 1:boot_num)
  {
    tem_dis1<-tem_dis[sample(1:length(tem_dis), length(tem_dis), replace = TRUE)]
    mean_value_boot[j]<-median(tem_dis1,na.rm=TRUE);
    #sd_value_boot[j]<-sd(tem_dis1,na.rm=TRUE);
  }

  sum1[ii]<-print(paste0(ii,'-',dis,':mean=',median(tem_dis,na.rm=TRUE),
                         '  quantile 25%=',quantile(tem_dis,0.25,na.rm=TRUE),
                         '  quantile 75%=',quantile(tem_dis,0.75,na.rm=TRUE),
               '  lower CI=',quantile(mean_value_boot[1,],0.025),
               '  higher CI=',quantile(mean_value_boot[1,],0.975)))
  # print(mean(tem_dis,na.rm=TRUE))
  # print(quantile(mean_value_boot[1,],0.025))
  # print(quantile(mean_value_boot[1,],0.975))
}


library(fmsb)
rownames(rad_data1)<-rad_data1$type
rad_data1[,'Max']<-(1)
rad_data1[,'Min']<-(0)
rad_data1[,'Value']<-rad_data1$x

print(rad_data1)
rad_data0 <- data.frame(t(rad_data1[,c("Max", "Min", "Value")]));

Disease_risk[[disease[num]]]<-rad_data1

png(filename = paste0(str_lab,'_',disease[num],'_model_plot.png'),
    width = 1480,
    height = 1480,
    units = "px",
    bg = "white",
    res = 300)
colnames(rad_data0)[dim(rad_data0)[2]]<-c('NMO')
p<-radarchart(rad_data0,
              axistype = 1,
              # Customize the polygon
              pcol = "grey", 
              pfcol = scales::alpha("grey", 0.5), 
              plwd = 1, plty = 1,
              # Customize the grid
              cglcol = "grey", cglty = 2, 
              cglwd = 0.8,
              # Customize the axis
              axislabcol = "grey",
              # Variable labels
              vlcex = 1,palcex = 1, 
              vlabels = colnames(rad_data0),
              caxislabels = c(0,rad_data0['Max',1]/4, 
                              rad_data0['Max',1]/4*2, 
                              rad_data0['Max',1]/4*3, 
                              rad_data0['Max',1]/4*4))
print(p)
dev.off()


# 
# library(ggwordcloud)
# set.seed(1)
# for(dis in results$disease_comparison$disease)
# {
#   tem_list<-list[list$Disease==dis,]
#   tem_list[abs(tem_list$cohens_d)<0.2,'color_label']<-1;
#   tem_list[abs(tem_list$cohens_d)>=0.2,'color_label']<-2;
#   tem_list[abs(tem_list$cohens_d)>=0.5,'color_label']<-3;
#   tem_list[abs(tem_list$cohens_d)>=0.8,'color_label']<-4;
#   
#   mask_png <- png::readPNG(system.file("extdata/hearth.png",
#                                        package = "ggwordcloud", mustWork = TRUE))
#   
#   tem_list<-tem_list[tem_list$pFDR<0.05,]
#   png(filename = paste0(dis,'_',str_lab,'_all_features_importantance_plot.png'),
#       width = 1480,
#       height = 1480,
#       units = "px",
#       bg = "white",
#       res = 300)
#   
#   p<-ggplot(tem_list, aes(label = feature,
#                           size = cohens_d,color = color_label)) +
#     geom_text_wordcloud() +
#     scale_size_area(max_size = )+
#     theme_minimal()+
#     scale_color_gradient(limits=c(1,4),low = "gray", high = "darkred")
#   print(p)
#   dev.off()
#   
  
# p<-list()


}

png(filename = paste0(str,'_ROC_all_diseases_lasso_selection',".png"),
    width = 1480,
    height = 1000,
    units = "px",
    bg = "white",
    res = 300)

p<-ggroc(list_roc,linetype='solid',linewidth=1,alpha=0.5)+
  #geom_segment(aes(x=1,xend=0,y=1,yend=0),color='darkgrey',linetype='dashed')+
  theme_classic()
print(p)

dev.off()


res<-list();
res$list_roc_results_lasso_selection<-list_roc_results
res$list_roc_lasso_selection<-list_roc
res$model<-model
res$feature<-feature_sum
res$disease_risk_prediction<-Disease_risk
res$HC_risk_prediction<-HC_risk
res$list_stats<-list_stats
print(list_stats)
saveRDS(res,paste0(str,'_ROC_loop_lasso_selection.rds'));


for(dis in disease)
{
  tem_HC<-HC_risk[[dis]]
  tem_HC<-tem_HC[tem_HC$Diagnosis=='HC',];
  
  print(paste0(dis,dis,dis,dis))
  print(sum(tem_HC[,dis]>=0.95))
  print(sum(tem_HC[,dis]>=0.95)/dim(tem_HC)[1]);
  print(median(tem_HC[tem_HC[,dis]>=0.95,'Age'],na.rm=T))
  print(quantile(tem_HC[tem_HC[,dis]>=0.95,'Age'],0.25,na.rm=T))
  print(quantile(tem_HC[tem_HC[,dis]>=0.95,'Age'],0.75,na.rm=T))
  #print(sum(tem_HC[tem_HC[,dis]>=0.95,'Sex']))
  print((length(tem_HC[tem_HC[,dis]>=0.95,'Sex'])-sum(tem_HC[tem_HC[,dis]>=0.95,'Sex'])))
  print((length(tem_HC[tem_HC[,dis]>=0.95,'Sex'])-sum(tem_HC[tem_HC[,dis]>=0.95,'Sex']))/
          length(tem_HC[tem_HC[,dis]>=0.95,'Sex']))
  print(paste0("  "))
}

#write.csv(tem_data1,paste0(str,'_predicted_risk_of_diseases.csv'));



# num=1;
# for(ii in disease[1:length(disease)])
# {
#   ii
#   dev.off
#   png(filename = paste0(str,'_',ii,".png"), 
#       width = 1480,           
#       height = 1480,          
#       units = "px",          
#       bg = "white",          
#       res = 200)      
# 
# index=which(tem_data1$Diagnosis==ii);index=index[23]
# 
# if (ii=='PD')
# {index=which(tem_data1$Diagnosis=='PD');index=index[27]}
# 
# 
# 
# tem_label=tem_data1$Diagnosis[index]
# 
# 
# individual<-tem_data1[index,(dim(data)[2]+1):(dim(tem_data1)[2])];
# 
# individual<-data.frame(t(individual));
# 
# colnames(individual)[1]<-c('risk')
# individual[,'disease']<-rownames(individual)
# individual[,'sig']<-1
# individual[which.max(individual$risk),'sig']=2;
# 
# library(ggplot2)
# 
# barplot(individual$risk,
#         names.arg=rownames(individual),
#         xlim=c(0,1),
#         col=c(individual$sig),
#         xlab = 'Prdicted Risk',  # X轴名称
#         ylab = ' ',  # Y轴名称
#         main = paste0('The ture clinical dianosis is ',tem_label),horiz = TRUE)
# 
# dev.off
# 
# 
# library(ggradar)
# 
# # colnames(individual)[1]<-c('Var1')
# # colnames(individual)[2]<-c('Group')
# 
# png(filename = paste0(str,'_',ii,"_Radar.png"),
#     width = 1480,
#     height = 1480,
#     units = "px",
#     bg = "white",
#     res = 200)
# 
# individual1<-data.frame(matrix(0,1,length(disease)))
# 
# individual1[1,1:length(disease)]<-(individual[,1])
# individual1[1,1]<-paste0('G',1)
# 
# colnames(individual1)[1]<-c('Group');
# colnames(individual1)[1:length(disease)]<-c(disease[1:length(disease)]);
# 
# 
# p<-ggradar(individual1,
#            values.radar = c(0, 0.5, 1))
# 
# print(p)
# dev.off()

#}
print(list_stats)
