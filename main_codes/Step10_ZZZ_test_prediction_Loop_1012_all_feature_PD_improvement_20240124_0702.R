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
library(lsr)

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

savepath=paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results/figuredps');

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


feature_col<-NULL
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
    
    feature_col<-c(feature_col,results$i)
  }
}  

for(iz in 1:dim(data)[1]){data[iz,'individual_ID']<-
  paste0(data$Freesufer_Path1[iz],data$Freesufer_Path2[iz],data$Freesufer_Path3[iz],sep='_')}
data<-data %>% distinct(individual_ID, .keep_all = T);

rownames(data)<-data$individual_ID

data_backup<-data;

data<-data_backup

add_data<-read.csv('C:/ZZZ/work/manuscript/Lifespan项目/基本信息/Clincial_data_320231021/new_final_list1_update.csv',header=TRUE);
for(iz in 1:dim(add_data)[1]){add_data[iz,'individual_ID']<-
  paste0(add_data$Freesufer_Path1[iz],add_data$Freesufer_Path2[iz],add_data$Freesufer_Path3[iz],sep='_')}
add_data<-add_data %>% distinct(individual_ID, .keep_all = T);
rownames(add_data)<-add_data$individual_ID

row_ind<-intersect(rownames(add_data),rownames(data))

data<-cbind(data[row_ind,],add_data[row_ind,])

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

#start_col=20;
str<-str_lab;


if (!(dir.exists(paste0(savepath,'/',str))))
{dir.create(paste0(savepath,'/',str))}

setwd(paste0(savepath,'/',str))


tem_data1<-data;
list_eta<-NULL
num=0;
list_roc<-list()
list_roc_results<-list()
model<-list()

setwd(savepath);



# var_pre<-c('MMSE','MOCA','BVLT','CVLT','SDMT','PASAT',
#            'Pre_UPDRS_III_med_on','Pre_UPDRS_III_med_off','Preoperative_med_improvement_rate',
#            'SVD_score',
#            'EDSS')

data[,'Post_DBS_UPRDS_off']<-data$Pre_UPDRS_III_med_off+data$DBS_Improvement_difference_off;

# var_pre<-c('MMSE','MOCA','BVLT','CVLT','SDMT','PASAT',
#            'Pre_UPDRS_III_med_off','Pre_UPDRS_III_med_on','Post_DBS_UPRDS_off',
#            'SVD_score',
#            'EDSS')

# var_pre<-c('MMSE','MOCA','BVLT','CVLT','SDMT','PASAT',
#            'Pre_UPDRS_III_med_off','Pre_UPDRS_III_med_on','Post_DBS_UPRDS_off',
#            'EDSS','relapses')

# var_pre<-c('MMSE','MOCA','BVLT','CVLT','SDMT','PASAT',
#            'Pre_UPDRS_III_med_off','Pre_UPDRS_III_med_on','Post_DBS_UPRDS_off',
#            'EDSS')

var_pre<-c('Pre_UPDRS_III_med_on','Preoperative_med_improvement_rate',
           'Post_DBS_UPRDS_off','DBS_improvement_rate_off')
# 'Bergt_difference')


results=list();
#results=list();
list<-data.frame(matrix(0,length(var_pre)*length(c('HC',disease))*length(feature_col),25))

list_all<-data.frame(matrix(0,length(var_pre),15))

list_all_disease<-data.frame(matrix(0,length(var_pre)*length(c('HC',disease)),18))


colnames(list)<-c('clinical_feature','MRI_feature','disease','coefficient',
                  'pvalue_t','tvalue','degree1','degere2','Fvalue','R2','Adjust_R2',
                  'Eta2','Eta2_CI_low','Eta2_CI_high',
                  'Eta2_parital','Eta2_parital_CI_low','Eta2_parital_CI_high',
                  'rvalue','pvalue_r','MAE','MAPE','MSE','RMSE','MSLE','MedAE')

colnames(list_all)<-c('clinical_feature','pvalue_F',
                      'degree1','degree2','Fvalue','R2','Adjust_R2',
                      'rvalue','pvalue_r','MAE','MAPE','MSE','RMSE','MSLE','MedAE')

colnames(list_all_disease)<-c('clinical_feature','disease','pvalue_F',
                      'degree1','degree2','Fvalue','R2','Adjust_R2',
                      'rvalue','pvalue_r','rvalue_5%','rvalue_95%','MAE','MAPE','MSE','RMSE','MSLE','MedAE')

num=0;num1=0;num2=0;
for(i_var in 1:length(var_pre))
{
  num1=num1+1;
tem_data<-data[!is.na(data[,var_pre[i_var]]),];

tem_data[tem_data$Diagnosis=='HC','G']<-'HC'
tem_data[tem_data$Diagnosis!='HC','G']<-'Disease'

tem_data$G<-factor(tem_data$G,levels=c('HC','Disease'))


tem_data$Diagnosis<-factor(tem_data$Diagnosis,
                           levels = c('HC',disease));
library(ppcor)
#disease feature associations

list_r<-data.frame(matrix(NA,length(c('HC',disease)),length(feature_col)))
list_p<-data.frame(matrix(NA,length(c('HC',disease)),length(feature_col)))
colnames(list_r)<-feature_col;
colnames(list_p)<-feature_col;
rownames(list_r)<-c('HC',disease)
rownames(list_p)<-c('HC',disease)

for(i_dis in c('HC',disease))
{
  
  for(i_feature in feature_col)
    {
    num=num+1;
  # if(!(var_pre[i_var] %in% c('relapses','EDSS')))  
  # {data_ass<-data[data$Diagnosis==i_dis,c('Age','Sex','Education_years',var_pre[i_var],i_feature)];
  # data_ass<-na.omit(data_ass);if(dim(data_ass)[1]==0){next}
  # tem_st<-pcor.test(x=data_ass[,var_pre[i_var]],y=data_ass[,i_feature],
  #           z=data_ass[,c('Age','Sex','Education_years')],method='pearson')
  # 
  # data_ass_lm<-data_ass;colnames(data_ass_lm)[4:5]<-c('cli_feature','MR_feature');
  # tem_lm<-lm(cli_feature~MR_feature+Age+Sex+Education_years,data=data_ass_lm);
  # 
  # tem_eta2<-eta_squared(tem_lm,partial = FALSE);
  # tem_eta2_par<-eta_squared(tem_lm);
  # }
  # 
  #   if((var_pre[i_var] %in% c('relapses','EDSS')))  
  #   {data_ass<-data[data$Diagnosis==i_dis,c('Age','Sex',var_pre[i_var],i_feature)];
  #   data_ass<-na.omit(data_ass);if(dim(data_ass)[1]==0){next}
  #   tem_st<-pcor.test(x=data_ass[,var_pre[i_var]],y=data_ass[,i_feature],
  #                     z=data_ass[,c('Age','Sex')],method='pearson')
  #   
  #   data_ass_lm<-data_ass;colnames(data_ass_lm)[3:4]<-c('cli_feature','MR_feature');
  #   tem_lm<-lm(cli_feature~MR_feature+Age+Sex,data=data_ass_lm);
  #   
  #   tem_eta2<-eta_squared(tem_lm,partial = FALSE);
  #   tem_eta2_par<-eta_squared(tem_lm);
  #   
  #   }
  
    # data_ass<-data[data$Diagnosis==i_dis,c('Age','Sex',var_pre[i_var],i_feature)];
    # data_ass<-na.omit(data_ass);if(dim(data_ass)[1]==0){next}
    # tem_st<-pcor.test(x=data_ass[,var_pre[i_var]],y=data_ass[,i_feature],
    #                  z=data_ass[,c('Age','Sex')],method='pearson')
    # 
    # data_ass_lm<-data_ass;colnames(data_ass_lm)[3:4]<-c('cli_feature','MR_feature');
    # tem_lm<-lm(cli_feature~MR_feature+Age+Sex,data=data_ass_lm);
    # 
    # tem_eta2<-eta_squared(tem_lm,partial = FALSE);
    # tem_eta2_par<-eta_squared(tem_lm);
    
    
    data_ass<-data[data$Diagnosis==i_dis,c(var_pre[i_var],i_feature)];
    data_ass<-na.omit(data_ass);if(dim(data_ass)[1]==0){next}
    tem_st<-cor.test(x=data_ass[,var_pre[i_var]],y=data_ass[,i_feature],method='pearson')
    
    data_ass_lm<-data_ass;colnames(data_ass_lm)[1:2]<-c('cli_feature','MR_feature');
    tem_lm<-lm(cli_feature~MR_feature,data=data_ass_lm);
    
    tem_eta2<-eta_squared(tem_lm,partial = FALSE);
    tem_eta2_par<-eta_squared(tem_lm);
    
      
  tem_sum<-summary(tem_lm)
  list[num,1]<-var_pre[i_var];
  list[num,2]<-i_feature;
  list[num,3]<-i_dis
  list[num,4]<-tem_sum$coefficients[2,1]
  list[num,5]<-tem_sum$coefficients[2,4]
  list[num,6]<-tem_sum$coefficients[2,3]
  list[num,7]<-tem_sum$df[1]-1
  list[num,8]<-tem_sum$df[2]
  list[num,9]<-tem_sum$fstatistic[1]
  list[num,10]<-tem_sum$r.squared
  list[num,11]<-tem_sum$adj.r.squared
  
  list[num,12]<-tem_eta2$Eta2[1];
  list[num,13]<-tem_eta2$CI_low[1];
  list[num,14]<-tem_eta2$CI_high[1];
  
  list[num,15]<-tem_eta2_par$Eta2[1] ;
  list[num,16]<-tem_eta2_par$CI_low[1];
  list[num,17]<-tem_eta2_par$CI_high[1];
  
  list[num,18]<-tem_st$estimate;
  list[num,19]<-tem_st$p.value;
  
  list[num,20]<-mean(abs(tem_lm$residuals))
  list[num,21]<-mean(abs(tem_lm$residuals)/abs(data_ass_lm$cli_feature))
  list[num,22]<-mean(abs(tem_lm$residuals**2))
  list[num,23]<-sqrt(mean(abs(tem_lm$residuals**2)))
  list[num,24]<-mean((log(1+data_ass_lm$cli_feature)-log(1+tem_lm$fitted.values))**2)
  list[num,25]<-median(abs(tem_lm$residuals))
  # list_rlist[num,3]<-tem_sum$adj.r.squared[num,i_feature]<-tem_st$estimate 
  # list_p[num,i_feature]<-tem_st$p.value
  
  }
}

results$signal_feature_lm<-list;


# results_r[[var_pre[i_var]]]<-list_r
# results_p[[var_pre[i_var]]]<-list_p
# 
# saveRDS(results_r,'results_r.rds')
# saveRDS(results_p,'results_p.rds')




#for each disease
for(i_dis in c('HC',disease))
{
num2=num2+1;
if(i_dis!='SVD'&&var_pre[i_var]=="SVD_score"){next}

#tem_data0<-tem_data[tem_data$Diagnosis==i_dis,c(var_pre[i_var],'Age','Sex',feature_col)];

#tem_data0<-tem_data[tem_data$Diagnosis==i_dis,c(var_pre[i_var],'Age','Sex','Education_years',feature_col)];

tem_data0<-tem_data[tem_data$Diagnosis==i_dis,c(var_pre[i_var],feature_col)];

tem_data0<-na.omit(tem_data0);if(dim(tem_data0)[1]<10){next};
tem_data0_zz<-tem_data0;

###feature selection
tem_list<-list[list$clinical_feature==var_pre[i_var]&
                 list$disease==i_dis&list$pvalue_r<=0.05,]
tem_list<-tem_list[order(tem_list$pvalue_r),]

sel_feature_col<-tem_list[,'MRI_feature']

sel_feature_col<-sel_feature_col[!is.na(sel_feature_col)]



#tem_data0<-tem_data0[,c(var_pre[i_var],'Age','Sex',sel_feature_col)];
tem_data0<-tem_data0[,c(var_pre[i_var],sel_feature_col)];

tem_data0<-na.omit(tem_data0)



# X=as.matrix(tem_data0[,2:length(colnames(tem_data0))]);
# Y=as.matrix(tem_data0[,var_pre[i_var]]);
# 
# mod_cv <- cv.glmnet(x=X,
#                     y=Y, 
#                     family="gaussian", nfolds = 10,
#                     intercept = F, alpha=1)
# 
# best_lambda <- mod_cv$lambda.min
# 
# best_model <- glmnet(x=X, 
#                      y=Y, 
#                      alpha = 1, lambda = best_lambda)
# tem_coef<-data.frame(matrix(coef(best_model)))
# 
# rownames(tem_coef)<-c('intercept',colnames(tem_data0)[2:(dim(tem_data0)[2])]);
# tem_coef
# index_feature=which(tem_coef!=0);
# 
# if(length(index_feature)==1){next};
# index_feature<-index_feature[2:length(index_feature)]
# sel_feature<-rownames(tem_coef)[index_feature]
# 
# sel_feature<-c(var_pre[i_var],sel_feature);

#tem_data0<-tem_data0[,sel_feature]

#tem_data0<-tem_data0[!is.infinite(rowSums(tem_data0[4:dim(tem_data0)[2]])),]
#tem_data0$Sex<-as.factor(tem_data0$Sex)



tem_list_fig<-tem_list

rownames(tem_list_fig)<-tem_list_fig$MRI_feature

tem_list_fig<-tem_list_fig[sel_feature_col1,]
# sel_feature_col_fig<-str_remove(tem_list_fig$MRI_feature,'`')
# sel_feature_col_fig<-str_remove(sel_feature_col_fig,'`')


library(raincloudplots)
library(ggwordcloud)
setwd(savepath)
png(filename = paste0(i_dis,'_',var_pre[i_var],'_predictive_features_importantance_plot.png'),
    width = 1480,
    height = 1480,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(tem_list_fig, aes(label = MRI_feature,
                            size = -log(pvalue_r),color = rvalue)) +
  geom_text_wordcloud(eccentricity=1) +
  scale_size_area(max_size = 4)+
  theme_minimal()+
  scale_color_gradient(limits=c(min(tem_list_fig$rvalue),max(tem_list_fig$rvalue)),low = "#00CCFF",high = "#990000")
print(p)
dev.off()



colnames(tem_data0)[1]<-c('label')



library(autoReg)


#install.packages("export")
library(rrtable)
library(export)

model1<-glm(label~.,data=tem_data0,family = gaussian)

model<-step(model1,direction='both',scope=formula(label~.),trace=FALSE)
#model<-autoReg(model1,uni=TRUE,threshold=0.05, final= TRUE) 

saveRDS(model,paste0(i_dis,'_',var_pre[i_var],'.rds'));

table2doc(model,file=paste0(i_dis,'_',var_pre[i_var]), digits = 1, digitspvals = 3, add.rownames =TRUE)


#table2pptx(model)  #导出到ppt，可编辑数据
library(stringr)
sel_feature_col1<-names(model$coefficients)[2:length(model$coefficients)]

sel_feature_col1<-str_remove(sel_feature_col1,'`')
sel_feature_col1<-str_remove(sel_feature_col1,'`')
tem_data0<-tem_data0_zz[,c(var_pre[i_var],sel_feature_col1)]


tem_list_fig<-tem_list_fig[sel_feature_col1,]
# sel_feature_col_fig<-str_remove(tem_list_fig$MRI_feature,'`')
# sel_feature_col_fig<-str_remove(sel_feature_col_fig,'`')


library(raincloudplots)
library(ggwordcloud)
setwd(savepath)
png(filename = paste0(i_dis,'_',var_pre[i_var],'_predictive_features_importantance_plot.png'),
    width = 1480,
    height = 1480,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(tem_list_fig, aes(label = MRI_feature,
                            size = -log(pvalue_r),color = rvalue)) +
  geom_text_wordcloud(eccentricity=1) +
  scale_size_area(max_size = 4)+
  theme_minimal()+
  scale_color_gradient(limits=c(min(tem_list_fig$rvalue),max(tem_list_fig$rvalue)),low = "#00CCFF",high = "#990000")
print(p)
dev.off()



colnames(tem_data0)[1]<-c('label')



###cross-validation
library(caret)
set.seed(7);
folds<-createFolds(y=tem_data0$label,k=10);

data2<-tem_data0

for(i in 1:10)
{
  fold_test<-tem_data0[folds[[i]],]
  fold_train<-tem_data0[-folds[[i]],]
  
  print(paste0('***fold',i,'***'))
  
  fold_pre<-glm(label~.,family = 'gaussian',data=fold_train)
  fold_predict<-predict(fold_pre,type='response',newdata=fold_test);
  
  data2[folds[[i]],'Predict']<-fold_predict;
  
}


#更换为SVM交叉验证
library(e1071)
set.seed(7);
folds<-createFolds(y=tem_data0$label,k=10);

data2<-tem_data0

for(i in 1:10)
{
  fold_test<-tem_data0[folds[[i]],]
  fold_train<-tem_data0[-folds[[i]],]
  
  print(paste0('***fold',i,'***'))
  
  tune_result<-tune(svm,label~.,data=fold_train,kernel='radial',type='eps-regression',
                    ranges=list(cost=c(0.1,1,10),gama=c(0.1,1,10)))
  
  fold_pre<-tune_result$best.model
  
  
  #fold_pre<-svm(label~.,family = 'gaussian',data=fold_train)
  fold_predict<-predict(fold_pre,newdata=fold_test);
  
  data2[folds[[i]],'Predict']<-fold_predict;
  
}

real<-tem_data0$label;
tem_data0[,'predict']<-data2$Predict
predict.<-data2$Predict

real[real==0]=1;
# real<-real[!is.infinite(predict.)]
# predict.<-predict.[!is.infinite(predict.)]
# predict.<-predict.[!is.infinite(real)]
# real<-real[!is.infinite(real)]

tem_cor<-cor.test(real,predict.)

pre<-lm(label~.,family = 'gaussian', data=tem_data0)

tem_pre<-summary(pre);
# 
# real<-tem_data0$label;
# predict.<-predict.glm(pre,type = 'response',newdata = tem_data0)
# tem_cor<-cor.test(real,predict.)

tem_sum<-tem_pre
list_all_disease[num2,1]<-var_pre[i_var];
list_all_disease[num2,2]<-i_dis;
list_all_disease[num2,3]<-pf(tem_sum$fstatistic[1], tem_sum$df[1]-1, tem_sum$df[2], lower.tail = FALSE)
list_all_disease[num2,4]<-tem_sum$df[1]-1
list_all_disease[num2,5]<-tem_sum$df[2]
list_all_disease[num2,6]<-tem_sum$fstatistic[1]
list_all_disease[num2,7]<-tem_sum$r.squared
list_all_disease[num2,8]<-tem_sum$adj.r.squared

list_all_disease[num2,9]<-tem_cor$estimate;
list_all_disease[num2,10]<-tem_cor$p.value;
list_all_disease[num2,11]<-tem_cor$conf.int[1];
list_all_disease[num2,12]<-tem_cor$conf.int[2];

list_all_disease[num2,13]<-mean(abs(real-predict.),na.rm = TRUE)
list_all_disease[num2,14]<-mean(abs(real-predict.)/abs(real),na.rm = TRUE)
list_all_disease[num2,15]<-mean(abs((real-predict.)**2),na.rm = TRUE)
list_all_disease[num2,16]<-sqrt(mean(abs((real-predict.)**2),na.rm = TRUE))
list_all_disease[num2,17]<-mean((log(1+real)-log(1+predict.))**2,na.rm = TRUE)
list_all_disease[num2,18]<-median(abs((real-predict.)),na.rm = TRUE)

library(ggpubr)


png(filename = paste0(i_var,'_seperate_disease_clinical_association.png'),
    width = 1000,
    height = 1000,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(data2,aes(x=label,y=Predict))+
  #geom_point(size=2)+
  geom_point(size=1,color='grey',shape=16)+
  geom_smooth(method = 'lm', formula = y ~ x, se = T,
              linewidth=1,color='#990000')+
  stat_cor(data=data2, method = "pearson")+
  labs(x=paste0('Real ',var_pre[i_var]),y=paste0('Predicted ',var_pre[i_var]))+
  theme_classic()
print(p)
dev.off()


}
# tem_eta2<-eta_squared(pre,partial = FALSE);
# tem_eta2_par<-eta_squared(pre);





# #for all diseases
# tem_data0<-tem_data[,c(var_pre[i_var],'Age','Sex',feature_col,'Diagnosis')];
# tem_data0<-na.omit(tem_data0);
# tem_data0_zz<-tem_data0;
# tem_data0<-tem_data0[,c(var_pre[i_var],'Age','Sex',feature_col)];
# 
# # X=as.matrix(tem_data0[,2:length(colnames(tem_data0))]);
# # Y=as.matrix(tem_data0[,var_pre[i_var]]);
# # 
# # mod_cv <- cv.glmnet(x=X,
# #                     y=Y, 
# #                     family="gaussian", nfolds = 10,
# #                     intercept = F, alpha=1)
# # 
# # best_lambda <- mod_cv$lambda.min
# # 
# # best_model <- glmnet(x=X, 
# #                      y=Y, 
# #                      alpha = 1, lambda = best_lambda)
# # tem_coef<-data.frame(matrix(coef(best_model)))
# # 
# # rownames(tem_coef)<-c('intercept',colnames(tem_data0)[2:(dim(tem_data0)[2])]);
# # tem_coef
# # index_feature=which(tem_coef!=0);
# # 
# # index_feature<-index_feature[2:length(index_feature)]
# # sel_feature<-rownames(tem_coef)[index_feature]
# # 
# # sel_feature<-c(var_pre[i_var],sel_feature);
# # 
# # tem_data0<-tem_data0[,sel_feature]
# 
# #tem_data0<-tem_data0[!is.infinite(rowSums(tem_data0[4:dim(tem_data0)[2]])),]
# #tem_data0$Sex<-as.factor(tem_data0$Sex)
# colnames(tem_data0)[1]<-c('label')
# 
# pre<-lm(label~., 
#         family='gaussian',data=tem_data0)
# 
# tem_pre<-summary(pre);
# 
# real<-tem_data0$label;
# predict.<-predict.glm(pre,type = 'response',newdata = tem_data0)
# 
# 
# library(caret)
# 
# library(caret)
# set.seed(7);
# folds<-createFolds(y=tem_data0$label,k=10);
# 
# data2<-tem_data0
# 
# for(i in 1:10)
# {
#   fold_test<-tem_data0[folds[[i]],]
#   fold_train<-tem_data0[-folds[[i]],]
# 
#   print(paste0('***fold',i,'***'))
# 
#   fold_pre<-glm(label~.,family = 'gaussian',data=fold_train)
#   fold_predict<-predict(fold_pre,type='response',newdata=fold_test);
# 
#   data2[folds[[i]],'Predict']<-fold_predict;
#   #data2[folds[[i]],'Predict']<-fold_predict;
# }
# 
# real<-tem_data0$label;
# tem_data0[,'predict']<-data2$Predict
# predict.<-data2$Predict
# 
# # real<-tem_data0$label;
# # tem_data0[,'predict']<-predict.
# tem_cor<-cor.test(real,predict.)
# 
# 
# 
# 
# tem_sum<-tem_pre
# list_all[num1,1]<-var_pre[i_var];
# list_all[num1,2]<-pf(tem_sum$fstatistic[1], tem_sum$df[1]-1, tem_sum$df[2], lower.tail = FALSE)
# list_all[num1,3]<-tem_sum$df[1]-1
# list_all[num1,4]<-tem_sum$df[2]
# list_all[num1,5]<-tem_sum$fstatistic[1]
# list_all[num1,6]<-tem_sum$r.squared
# list_all[num1,7]<-tem_sum$adj.r.squared
# 
# list_all[num1,8]<-tem_cor$estimate;
# list_all[num1,9]<-tem_st$p.value;
# 
# list_all[num1,10]<-mean(abs(real-predict.))
# list_all[num1,11]<-mean(abs(real-predict.)/abs(real))
# list_all[num1,12]<-mean(abs((real-predict.)**2))
# list_all[num1,13]<-sqrt(mean(abs((real-predict.)**2)))
# list_all[num1,14]<-mean((log(1+real)-log(1+predict.))**2)
# list_all[num1,15]<-median(abs((real-predict.)))







}

setwd(savepath)
list[,'pFDR']<-p.adjust(list$pvalue_r,'fdr');
list_all_disease[,'pFDR']<-p.adjust(list_all_disease$pvalue_r,'fdr');
results$all_feature_lm_feature_disease_seperate<-list;
results$all_feature_lm_disease_seperate<-list_all_disease;

write.csv(list_all_disease,'clinical_association_PD_treatment.csv')
saveRDS(results,'clinical_association_PD_treatment.rds')

#plot list_all_disease
png(filename = paste0('heatmap_clinical_association_step_PD_treatment.png'),
    width = 2000,
    height = 1480,
    units = "px",
    bg = "white",
    res = 300)
list_all_disease<-list_all_disease[!is.na(list_all_disease$disease),]
list_all_disease$clinical_feature<-factor(list_all_disease$clinical_feature,levels=rev(c(var_pre)))
list_all_disease$disease<-factor(list_all_disease$disease,c('HC',disease))
heatmap=ggplot(data=list_all_disease,aes(x=disease,y=clinical_feature,fill=round(R2,2)))+
  geom_tile()+
  scale_fill_gradient2(limits=c(0,1),low='#00CCFF',midpoint =0.5,high='#990000',space='Lab')+
  geom_text(aes(disease,clinical_feature,label=round(R2,2)),color='black',size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle=45,vjust=1,size=10,hjust=1,color='black'),
        axis.text.y = element_text(angle=0,vjust=0,size=10,hjust=1,color='black'))+
  geom_vline(xintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),color='white',size=1)+
  #geom_hline(yintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),color='white',size=1)
  geom_hline(yintercept=c(1:12)-0.5,color='white',size=1)
print(heatmap)
dev.off()

png(filename = paste0('heatmap_clinical_association_adjust_R2_step_PD_treatment.png'),
    width = 3200,
    height = 1880,
    units = "px",
    bg = "white",
    res = 300)
list_all_disease<-list_all_disease[!is.na(list_all_disease$disease),]
list_all_disease$clinical_feature<-factor(list_all_disease$clinical_feature,levels=rev(c(var_pre)))
list_all_disease$disease<-factor(list_all_disease$disease,c('HC',disease))
heatmap=ggplot(data=list_all_disease,aes(x=disease,y=clinical_feature,fill=round(abs(Adjust_R2),2)))+
  geom_tile()+
  scale_fill_gradient2(limits=c(0,1),low='#00CCFF',midpoint =0.35,high='#990000',space='Lab')+
  geom_text(aes(disease,clinical_feature,label=round(Adjust_R2,2)),color='black',size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle=45,vjust=1,size=10,hjust=1,color='black'),
        axis.text.y = element_text(angle=0,vjust=0,size=10,hjust=1,color='black'))+
  geom_vline(xintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),color='white',size=1)+
  #geom_hline(yintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),color='white',size=1)
  geom_hline(yintercept=c(1:length(unique(list_all_disease$clinical_feature)))-0.5,color='white',size=1)
print(heatmap)
dev.off()


#plot list_all_disease
png(filename = paste0('heatmap_clinical_association_correlation_R_map_step_PD_treatment.png'),
    width = 3200,
    height = 1800,
    units = "px",
    bg = "white",
    res = 300)
list_all_disease<-list_all_disease[!is.na(list_all_disease$disease),]
list_all_disease$clinical_feature<-factor(list_all_disease$clinical_feature,levels=rev(c(var_pre)))
list_all_disease$disease<-factor(list_all_disease$disease,c('HC',disease))
heatmap=ggplot(data=list_all_disease,aes(x=disease,y=clinical_feature,fill=round(rvalue,2)))+
  geom_tile()+
  scale_fill_gradient2(limits=c(0,1),low='#00CCFF',midpoint =0.5,high='#990000',space='Lab')+
  geom_text(aes(disease,clinical_feature,label=round(rvalue,2)),color='black',size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text=element_text(size=16),
        axis.text.x = element_text(angle=0,vjust=0,size=12,hjust=0.5,color='black'),
        axis.text.y = element_text(angle=0,vjust=0,size=12,hjust=1,color='black'))+
  geom_vline(xintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),color='white',size=1)+
  geom_vline(xintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),color='white',size=1)+
  #geom_hline(yintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),color='white',size=1)
  geom_hline(yintercept=c(1:12)-0.5,color='white',size=1)+
  scale_x_discrete(labels = c("MCI" = "MCI","AD" = "AD", "PD" = "PD",
                              "SVD" = "CSVD","MS" = "MS",'AQP4Pos_NMOSD'="NMOSD"))+
  scale_y_discrete(labels = c("relapses" = "Relapse Number","MOCA" = "MoCA", "Post_DBS_UPRDS_off" = "UPRDS Receiving DBS",
                            "Pre_UPDRS_III_med_on" = "UPRDS Receiving Medicine","Pre_UPDRS_III_med_off" = "UPRDS without Therapy"))
print(heatmap)
dev.off()

png(filename = paste0('heatmap_clinical_association_correlation_mape_step_PD_treatment.png'),
    width = 3200,
    height = 1880,
    units = "px",
    bg = "white",
    res = 300)
list_all_disease<-list_all_disease[!is.na(list_all_disease$disease),]
list_all_disease$clinical_feature<-factor(list_all_disease$clinical_feature,levels=rev(c(var_pre)))
list_all_disease$disease<-factor(list_all_disease$disease,c('HC',disease))
heatmap=ggplot(data=list_all_disease,aes(x=disease,y=clinical_feature,fill=round(abs(MAPE),2)))+
  geom_tile()+
  scale_fill_gradient2(limits=c(0,2),low='#00CCFF',midpoint =1,high='#990000',space='Lab')+
  geom_text(aes(disease,clinical_feature,label=round(MAPE,2)),color='black',size=4)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text=element_text(size=16),
        axis.text.x = element_text(angle=0,vjust=0,size=12,hjust=0.5,color='black'),
        axis.text.y = element_text(angle=0,vjust=0,size=12,hjust=1,color='black'))+
  geom_vline(xintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),color='white',size=1)+
  #geom_hline(yintercept=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),color='white',size=1)
  geom_hline(yintercept=c(1:length(unique(list_all_disease$clinical_feature)))-0.5,color='white',size=1)+
  scale_x_discrete(labels = c("MCI" = "MCI","AD" = "AD", "PD" = "PD",
                              "SVD" = "CSVD","MS" = "MS",'AQP4Pos_NMOSD'="NMOSD"))+
  scale_y_discrete(labels = c("relapses" = "Relapse Number","MOCA" = "MoCA", "Post_DBS_UPRDS_off" = "UPRDS Receiving DBS",
                              "Pre_UPDRS_III_med_on" = "UPRDS Receiving Medicine","Pre_UPDRS_III_med_off" = "UPRDS without Therapy"))
print(heatmap)
dev.off()


#list_all_disease[,'degree1']<-p.adjust(list_all_disease$pvalue_F,'fdr')

