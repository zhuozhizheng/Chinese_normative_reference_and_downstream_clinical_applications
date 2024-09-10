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

feature_path=paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results/figures');str_lab<-'Subcortical_feature'# change
setwd(feature_path)
savepath<-paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results/figures1');

#tem_data<-readRDS('sd_inter_var_distribution.rds');str_lab<-'sd';str_lab1<-'SD';limvalue<-c(0.25,0.35);mid<-0.3

tem_data<-readRDS('mean_inter_var_distribution.rds');str_lab<-'mean';str_lab1<-'mean';limvalue<-c(0,1);mid<-0.5

#tem_data<-readRDS('cohens_d_inter_var_distribution.rds');str_lab<-'cohens_d';str_lab1<-"Cohens'd";limvalue<-c(0.2,1.2);mid<-0.5


disease<-c('MCI','AD',"PD","SVD",
           "MS","AQP4Pos_NMOSD")


for(del_str in c('_volume','_thickness','_area'))
{

setwd(savepath)
library(ggseg)
for (dis in disease)
{
  data<-tem_data[tem_data$diseasae==dis,];
  
  data<-na.omit(data)
  

feature_seg<-data$feature;

feature_seg<-str_remove_all(string = feature_seg,pattern = del_str)

data[,'ths']<-data$value
data[data$ths<0.2,'value']<-NA
data[data$ths<0.2,'CIdif']<-NA

seg_data=data.frame(label=feature_seg,
                    d=data$value,
                    CI=data$CIdif)



library(ggsci)
setwd(savepath)
png(filename = paste0(str_lab,'_',dis,'_',del_str,'_CIdif.png'),
    width = 1400,
    height = 850,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(seg_data)+
  geom_brain(atlas=dk,
             size = 0.1,
             position=position_brain(hemi ~ side),
             #hemi=hemis,
             mapping = aes(fill = CI))+
  theme_void()+
  scale_fill_gradient2(limits=c(0,0.41),low='#00CCFF',midpoint =0.2,high='#990000')
# scale_fill_gradient(limits=c(4.5,30),low='grey',high='#990000')
print(p)
dev.off()

library(ggsci)
setwd(savepath)
png(filename = paste0(str_lab,'_',dis,'_',del_str,'.png'),
    width = 1400,
    height = 850,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(seg_data)+
  geom_brain(atlas=dk,
             size = 0.1,
             position=position_brain(hemi ~ side),
             #hemi=hemis,
             mapping = aes(fill = d))+
  theme_void()+
  scale_fill_gradient2(limits=limvalue,low='#00CCFF',midpoint =mid,high='#990000')
  # scale_fill_gradient(limits=c(4.5,30),low='grey',high='#990000')
print(p)
dev.off()

}

}

