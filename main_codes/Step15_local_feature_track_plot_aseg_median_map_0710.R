# #results summary 1
# setwd('C:/ZZZ/work/manuscript/Lifespan-main/test_zzz/Global_feature');
#install.packages('gamlss')
rm(list=ls())
library(glmnet)
library(dplyr);
library(ggplot2)
#library(ggseg)
# install.packages("ggplot2")
library(stringr)
library(robustbase)
# setwd('C:/ZZZ/work/manuscript/Lifespan-main/test_zzz/aseg.vol.table')
#install.packages('gamlss')
library(gamlss)
library(reshape2)
library(pROC)

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

feature_path=paste0(datapath,'/test_zzz_update_20240116/aseg.vol.table');str_lab<-'Subcortical_feature'# change
setwd(feature_path)
savepath<-paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results/figures1');

myfile <- list.files()
RDSfile_all <- myfile[grep(myfile,pattern ="our_model.rds$")]   
RDSfile_all
RDSfile_all<-paste0(feature_path,'/',RDSfile_all)

setwd(savepath)
Exd_var<-c(paste0(feature_path,'/aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_cerebellum_WM_loop_our_model.rds') ,
           paste0(feature_path,'/aseg.vol.table_cerebellum_total_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_cerebellum_GM_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_Brain-Stem_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_cerebellum_GM_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_Left-Cerebellum-Cortex_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_Left-Cerebellum-White-Matter_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_Right-Cerebellum-Cortex_loop_our_model.rds'),
           paste0(feature_path,'/aseg.vol.table_Right-Cerebellum-White-Matter_loop_our_model.rds'))

#Brain health score BHC calculation

print(RDSfile_all[1])
RDSfile<-RDSfile_all[1]
results<-readRDS(RDSfile)
tem_data<-results$all_data[,c(1:16)];
tem_data_p<-data.frame(matrix(NA,6,1));
tem_data_p[,1]<-c('MCI','AD',"PD","SVD",
              "MS","AQP4Pos_NMOSD")
colnames(tem_data_p)<-c('disease')
rownames(tem_data_p)<-c('MCI','AD',"PD","SVD",
                        "MS","AQP4Pos_NMOSD")

for(RDSfile_loop in RDSfile_all)
{
  
  if(!(RDSfile_loop %in% Exd_var)){
    print(RDSfile_loop)
    RDSfile<-RDSfile_loop
    results<-readRDS(RDSfile)
    
    tem_data<-cbind(tem_data,
                    results$Quant_data[[length(results$Quant_data)]]);
    colnames(tem_data)[dim(tem_data)[2]]<-results$i
    
    tem_data_p<-cbind(tem_data_p,
                    results$disease_comparison$tvalue);
    colnames(tem_data_p)[dim(tem_data_p)[2]]<-results$i
  
    #data<-cbind(data,results$Quant_data[results$i])
    #colnames(data)[dim(data)[2]]<-results$i
  }
}  

disease<-c('MCI','AD',"PD","SVD",
           "MS","AQP4Pos_NMOSD")

setwd(savepath)
library(ggseg)
for (dis in disease)
{
data<-tem_data[tem_data$Diagnosis==dis,];

data<-na.omit(data)

data<-data[,17:dim(data)[2]]
median_value<-apply(data, 2, median)
median_value<-data.frame(median_value)

pvalue<-t(tem_data_p[dis,rownames(median_value)])
median_value[,'p']<-pvalue[rownames(median_value),dis]

median_value[,'pfdr']<-p.adjust(median_value[,'p'],'fdr')

median_value[median_value$pfdr>0.005|median_value$median_value>0.5,'median_value']<-NA

feature_seg<-rownames(median_value);
feature_seg[7]<-c("Left-Thalamus-Proper")
feature_seg[15]<-c("Right-Thalamus-Proper")

seg_data=data.frame(label=feature_seg,
                    deviation=median_value$median_value)

library(ggsci)
png(filename = paste0(str_lab,'_',dis,'_median_centerscore.png'),
    width = 880,
    height = 880,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(seg_data)+
  geom_brain(atlas=aseg,
             size = 0.1,
             side='coronal',
             mapping = aes(fill = deviation))+
  theme_void()+
  scale_fill_gradient2(limits=c(0,1),low='#00CCFF',midpoint =0.5,high='#990000')
  
print(p)
dev.off()

}
