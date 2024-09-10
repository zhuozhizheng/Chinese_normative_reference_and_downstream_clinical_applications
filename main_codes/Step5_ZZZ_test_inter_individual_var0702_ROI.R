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

for(z in c(3:5))
{
path<-list()
if(z==1){
str_lab<-'global_feature'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/Global_feature');
}

if(z==2){
str_lab<-'aseg_feature'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/aseg.vol.table');
}

if(z==3){
str_lab<-'cortical_thickness'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.thickness.table');
path[[2]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.thickness.table');
}

if(z==4){
str_lab<-'cortical_volume'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.volume.table');
path[[2]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.volume.table');
}

if(z==5){
str_lab<-'cortical_area'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.area.table');
path[[2]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.area.table');
}

savepath=paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results')

Exd_var<-c("Global_feature_meanCT2_lhMeanThickness_loop_our_model.rds",
           "Global_feature_meanCT2_lhVertex_loop_our_model.rds" ,
           "Global_feature_meanCT2_rhMeanThickness_loop_our_model.rds",
           "Global_feature_meanCT2_rhVertex_loop_our_model.rds",
           "Global_feature_totalSA2_lh_loop_our_model.rds",
           "Global_feature_totalSA2_rh_loop_our_model.rds",
           'aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds',
           "aseg.vol.table_cerebellum_WM_loop_our_model.rds" ,
           "aseg.vol.table_cerebellum_total_loop_our_model.rds",
           "aseg.vol.table_cerebellum_GM_loop_our_model.rds",
           "aseg.vol.table_Brain-Stem_loop_our_model.rds",
           "lh.aparc.thickness.table_eTIV_loop_our_model.rds",
           "rh.aparc.thickness.table_eTIV_loop_our_model.rds",
           "lh.aparc.area.table_eTIV_loop_our_model.rds",
           "rh.aparc.area.table_eTIV_loop_our_model.rds",
           "lh.aparc.volume.table_eTIV_loop_our_model.rds",
           "rh.aparc.volume.table_eTIV_loop_our_model.rds",
           "aseg.vol.table_Left-Cerebellum-Cortex_loop_our_model.rds",
           "aseg.vol.table_Left-Cerebellum-White-Matter_loop_our_model.rds",
           "aseg.vol.table_Right-Cerebellum-Cortex_loop_our_model.rds",
           "aseg.vol.table_Right-Cerebellum-White-Matter_loop_our_model.rds")


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


setwd(feature_path)
results<-readRDS(RDSfile_all[1]);
data<-results$all_data
data<-data[,c(1:16)]
data$Age<-data$Age_y

for(RDSfile_loop in RDSfile_all)
{
  setwd(feature_path)
  
  if(!(RDSfile_loop %in% Exd_var))
  {
  print(RDSfile_loop)
  RDSfile<-RDSfile_loop
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

disease<-c('MCI','AD',"PD","SVD",
           "MS","AQP4Pos_NMOSD")

#data1<-data[data$Diagnosis=='HC',];
boot=1000
list<-data.frame(matrix(NA,length(disease)*boot,2))
colnames(list)[1]<-c('disease')
setwd(savepath)

num=0;
for(i_dis in disease)
{
  print(i_dis)
  
  data1<-data[data$Diagnosis==i_dis,17:dim(data)[2]]
  data1<-na.omit(data1)
  dist_matrix<-dist(data1,method='manhattan');
  data2<-as.matrix(dist_matrix)
  

  library(pheatmap)
  pheatmap(data2,cluster_rows = T,cluster_cols = T,legend = TRUE,
           legend_labels = NA, annotation_row = NULL, annotation_col = NULL,
           labels_row = F,
           labels_col = F,
           fontsize = 10,
           scale='none',
           #legend_breaks=c(0,5),
           #breaks=c(0,5),
           show_rownames = F, show_colnames = F,
           filename =paste0(str_lab,'_',i_dis,'.png'),
           width = 4,
           height= 4)

  
  mean_value<-sum(data2)/(dim(data2)[1]*dim(data2)[2]-dim(data2)[1])
  list[num,1]<-i_dis;
  list[num,2]<-mean_value
  
  
  data1<-data[data$Diagnosis==i_dis,17:dim(data)[2]]
  data1<-na.omit(data1)
  
  for(boot_num in 1:boot)
  {
  num=num+1;
  #print(boot_num)
  sel_index<-sample(x=c(1:dim(data1)[1]),dim(data1)[1],replace = T)
  dist_matrix<-dist(data1[sel_index,],method='manhattan');
  data2<-as.matrix(dist_matrix)
  mean_value<-c(mean_value,sum(data2)/(dim(data2)[1]*dim(data2)[2]-dim(data2)[1]))
  list[num,1]<-i_dis;
  list[num,2]<-sum(data2)/(dim(data2)[1]*dim(data2)[2]-dim(data2)[1])
  }

  # list[num,3]<-quantile(mean_value,0.5)
  # list[num,4]<-quantile(mean_value,0.25)
  # list[num,5]<-quantile(mean_value,0.75)
  # list[num,6]<-mean(mean_value)
  # list[num,7]<-sd(mean_value)
}

setwd(savepath)
saveRDS(list,paste0(str_lab,'_inter_var_distribution.rds'));

colnames(list)<-c('disease','value')

list$disease<-factor(list$disease,levels=c('MCI','AD',"PD","SVD",
                      "MS","AQP4Pos_NMOSD"))

# geom_flat_violin<-function(mapping=NULL,data=NULL,stat="ydensity",
#                            position='dodge',trim=TRUE,scale='area',
#                            show.legend=NA,inherit.aes=TRUE,...){
#   layer(
#     data=data,
#     mapping=mapping,
#     stat=stat,
#     geom=GeomFlatViolin,
#     position=position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       trim=trim,
#       scale=scale,
#       ...
#     )
#   )
# }

library(tidyverse)
library(gghalves)

png(filename = paste0(str_lab,'_inter_var_distribution_new.png'), 
    width = 1500,           
    height =750,          
    units = "px",          
    bg = "white",          
    res = 300)  


plot01<-ggplot(data=list,aes(disease, value)) + 
  # ggdist::stat_halfeye(aes(color=Diagnosis,fill=Diagnosis),alpha=0.8,adjust =1,position = 'identity',
  #                      width = 1, .width = c(0,1), justification = 0, point_colour = NA,density = "unbounded") + 
  
  ggdist::stat_halfeye(aes(color=disease,fill=disease),alpha=0.5,adjust =1,#position = 'identity',
                       width = 1, .width = 0, justification = 0, point_colour = NA,density = "unbounded") + 
  
  #geom_boxplot(aes(color=disease),width = .2) + 
  stat_summary(fun = "median", geom = "point",
               shape=16,size=4,alpha=1,
               aes(color = disease))+
  stat_summary(fun = "median", geom = "point",
               shape=16,size=2,alpha=1,
               color = 'white')+
  #geom_jitter(aes(color=Diagnosis),width = .05, alpha = .3) +
  #ggsci::scale_color_jama()+
  #ggsci::scale_fill_nejm() +
  #coord_flip()+
  #ggdist::stat_dots(aes(color=Diagnosis,fill=Diagnosis),side = "left", dotsize = .5, justification = 1.1, binwidth = .1)+
  labs(x='',y='Manhattan Distance',size=20) +
  #geom_hline(aes(yintercept=0.5),linetype=c('solid'),linewidth=1,alpha=0.3)+
  
  #geom_text(data=error2,aes(x=group,y=values+9,label=label),color="red",fontface="bold")+
  # theme(panel.background = element_blank(),
  #       axis.text=element_text(size=16),
  #       axis.line = element_line(colour = "#000000",size = 0.2),
  #       axis.text.x = element_text(angle = 30,vjust = 0.85,hjust = 0.75))+
  theme_bw()+
  theme(#panel.background = element_blank(),
        axis.text=element_text(size=12,colour = "#000000"),
        axis.line = element_line(colour = "#000000",size = 0.2),
        legend.position = 'none'
        )+
  coord_flip()+
  scale_x_discrete(labels = c("MCI" = "MCI","AD" = "AD", "PD" = "PD","SVD" = "CSVD","MS" = "MS",'AQP4Pos_NMOSD'="NMOSD"))

#hrbrthemes::theme_ipsum(base_family = "Roboto Condensed") +
# theme(
#   plot.title = element_markdown(hjust = 0.5,vjust = .5,color = "black",
#                                 size = 20, margin = margin(t = 1, b = 12)),
#   plot.subtitle = element_markdown(hjust = 0,vjust = .5,size=15),
#   plot.caption = element_markdown(face = 'bold',size = 12),
#   legend.position = "none")
print(plot01)
dev.off()

}
