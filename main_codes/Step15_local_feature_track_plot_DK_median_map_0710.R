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

feature_path_L=paste0(datapath,'/test_zzz_update_20240116/lh.aparc.thickness.table');
str_lab<-'lh.aparc.thickness';del_str='_thickness';sub_number<-7;hemis<-'left'

feature_path_R=paste0(datapath,'/test_zzz_update_20240116/rh.aparc.thickness.table');
str_lab<-'rh.aparc.thickness';del_str='_thickness';sub_number<-7;hemis<-'right'
# 
# feature_path_L=paste0(datapath,'/test_zzz_update_20240116/lh.aparc.area.table');
# str_lab<-'lh.aparc.area';del_str='_area';sub_number<-12.5;hemis<-'left'
# 
# feature_path_R=paste0(datapath,'/test_zzz_update_20240116/rh.aparc.area.table');
# str_lab<-'rh.aparc.area';del_str='_area';sub_number<-12.5;hemis<-'right'
# 
# feature_path_L=paste0(datapath,'/test_zzz_update_20240116/lh.aparc.volume.table');
# str_lab<-'lh.aparc.volume';del_str='_volume';sub_number<-8.2;hemis<-'left'
# #
# feature_path_R=paste0(datapath,'/test_zzz_update_20240116/rh.aparc.volume.table');
# str_lab<-'rh.aparc.volume';del_str='_volume';sub_number<-8.2;hemis<-'right'


setwd(feature_path_L)
myfile <- list.files()
RDSfile_all_L<- myfile[grep(myfile,pattern ="our_model.rds$")]   
RDSfile_all_L<-paste0(feature_path_L,'/',RDSfile_all_L)

setwd(feature_path_R)
myfile <- list.files()
RDSfile_all_R<- myfile[grep(myfile,pattern =".rds$")]   
RDSfile_all_R<-paste0(feature_path_R,'/',RDSfile_all_R)

RDSfile_all<-c(RDSfile_all_L,RDSfile_all_R)

savepath<-paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results/figures1')

setwd(savepath)
Exd_var<-c(paste0(feature_path_L,"/Global_feature_meanCT2_lhMeanThickness_loop_our_model.rds"),
           paste0(feature_path_L,"/Global_feature_meanCT2_lhVertex_loop_our_model.rds") ,
           paste0(feature_path_L,"/Global_feature_meanCT2_rhMeanThickness_loop_our_model.rds"),
           paste0(feature_path_L,"/Global_feature_meanCT2_rhVertex_loop_our_model.rds"),
           paste0(feature_path_L,"/Global_feature_totalSA2_lh_loop_our_model.rds"),
           paste0(feature_path_L,"/Global_feature_totalSA2_rh_loop_our_model.rds"),
           paste0(feature_path_L,"/aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds"),
           paste0(feature_path_L,"/aseg.vol.table_cerebellum_WM_loop_our_model.rds"),
           paste0(feature_path_L,"/aseg.vol.table_cerebellum_total_loop_our_model.rds"),
           paste0(feature_path_L,"/aseg.vol.table_cerebellum_GM_loop_our_model.rds"),
           paste0(feature_path_L,"/aseg.vol.table_Brain-Stem_loop_our_model.rds"),
           paste0(feature_path_L,"/lh.aparc.thickness.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_L,"/rh.aparc.thickness.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_L,"/lh.aparc.area.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_L,"/rh.aparc.area.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_L,"/lh.aparc.volume.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_L,"/rh.aparc.volume.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_R,"/Global_feature_meanCT2_lhMeanThickness_loop_our_model.rds"),
           paste0(feature_path_R,"/Global_feature_meanCT2_lhVertex_loop_our_model.rds") ,
           paste0(feature_path_R,"/Global_feature_meanCT2_rhMeanThickness_loop_our_model.rds"),
           paste0(feature_path_R,"/Global_feature_meanCT2_rhVertex_loop_our_model.rds"),
           paste0(feature_path_R,"/Global_feature_totalSA2_lh_loop_our_model.rds"),
           paste0(feature_path_R,"/Global_feature_totalSA2_rh_loop_our_model.rds"),
           paste0(feature_path_R,"/aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds"),
           paste0(feature_path_R,"/aseg.vol.table_cerebellum_WM_loop_our_model.rds"),
           paste0(feature_path_R,"/aseg.vol.table_cerebellum_total_loop_our_model.rds"),
           paste0(feature_path_R,"/aseg.vol.table_cerebellum_GM_loop_our_model.rds"),
           paste0(feature_path_R,"/aseg.vol.table_Brain-Stem_loop_our_model.rds"),
           paste0(feature_path_R,"/lh.aparc.thickness.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_R,"/rh.aparc.thickness.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_R,"/lh.aparc.area.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_R,"/rh.aparc.area.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_R,"/lh.aparc.volume.table_eTIV_loop_our_model.rds"),
           paste0(feature_path_R,"/rh.aparc.volume.table_eTIV_loop_our_model.rds"))


#Brain health score BHC calculation
results<-readRDS(RDSfile_all[1]);
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
    
    tem_data1<-results$Quant_data[[length(results$Quant_data)]];
    rownames(tem_data1)<-results$all_data$individual_ID
    
    index<-intersect(rownames(tem_data1),rownames(tem_data));
    
    tem_data<-cbind(tem_data[index,],
                    tem_data1[index,]);
    colnames(tem_data)[dim(tem_data)[2]]<-results$i
    
    tem_data_p<-cbind(tem_data_p,
                      results$disease_comparison$tvalue);
    colnames(tem_data_p)[dim(tem_data_p)[2]]<-results$i
    
  }
}  


library(ggseg)

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

feature_seg<-str_remove_all(string = feature_seg,pattern = del_str)

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
  geom_brain(atlas=dk,
             size = 0.1,
             position=position_brain(hemi ~ side),
             #hemi=hemis,
             mapping = aes(fill = deviation))+
  theme_void()+
  scale_fill_gradient2(limits=c(0,1),low='#00CCFF',midpoint =0.5,high='#990000')
  # scale_fill_gradient(limits=c(4.5,30),low='grey',high='#990000')
print(p)
dev.off()

}



