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


datapath1=paste0(datapath,'/test_zzz_update_20240116/Declan_advices/results')# change
savepath1<-paste0(datapath1,'/figures')

setwd(datapath1)
data01<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part80.rds');
data02<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part81-345.rds');
data03<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part346-437.rds');
data04<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part438-667.rds');
data05<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part668-785.rds');
data06<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part786-926.rds');
data07<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part926-1017.rds');
data08<-readRDS('Allboot_MRI_measure_gourp_analysis_0628_part1018-1300.rds');

data<-rbind(data01,data02,data03,data04,
            data05,data06,data07,data08)

data<-data[data$feature!=0,]
#用雨云图显示global和local参数数据分布情况，包括mean,SD和cohen'd分布情况。
data$disease<-factor(data$disease,levels=(unique(data$disease)))
setwd(savepath1)
#mean值分布
library(tidyverse)
library(gghalves)

#str_lab<-'mean';str_lab1<-'mean'
str_lab<-'sd';str_lab1<-'SD';
#str_lab<-'cohens_d';str_lab1<-"Cohens'd";
list<-data.frame(matrix(NA,length(unique(data$feature))*length(unique(data$disease)),6))
colnames(list)<-c('diseasae','feature','value','lowCI','highCI','CIdif');

feature0<-unique(data$feature)
num=0;
for(feature0 in unique(data$feature))
{
  
  tem_data<-data[data$feature==feature0,]
  
  for(j in unique(data$disease))
  {
  num=num+1;
  list[num,1]<-j
  list[num,2]<-feature0
  list[num,3]<-median(as.numeric(tem_data[tem_data$disease==j,str_lab]),na.rm=T)
  list[num,4]<-quantile(as.numeric(tem_data[tem_data$disease==j,str_lab]),0.025,na.rm=T)
  list[num,5]<-quantile(as.numeric(tem_data[tem_data$disease==j,str_lab]),0.975,na.rm=T)
  list[num,6]<-list[num,5]-list[num,4]
  }
  
 
#   png(filename = paste0(str_lab,'_',feature0,'_inter_var_distribution.png'),
#     width = 1900,
#     height = 2400,
#     units = "px",
#     bg = "white",
#     res = 300)
# 
# 
#  plot01<-ggplot(data=tem_data,aes(tem_data[,'disease'], as.numeric(tem_data[,str_lab]))) +
#   # ggdist::stat_halfeye(aes(color=Diagnosis,fill=Diagnosis),alpha=0.8,adjust =1,position = 'identity',
#   #                      width = 1, .width = c(0,1), justification = 0, point_colour = NA,density = "unbounded") +
# 
#   ggdist::stat_halfeye(aes(color=disease,fill=disease),alpha=0.5,adjust =1,#position = 'identity',
#                        width = 1, .width = 0, justification = 0, point_colour = NA,density = "unbounded") +
# 
#   #geom_boxplot(aes(color=disease),width = .2) +
#   stat_summary(fun = "median", geom = "point",
#                shape=16,size=10,alpha=1,
#                aes(color = disease))+
#   stat_summary(fun = "median", geom = "point",
#                shape=16,size=5,alpha=1,
#                color = 'white')+
#   #geom_jitter(aes(color=Diagnosis),width = .05, alpha = .3) +
#   #ggsci::scale_color_jama()+
#   #ggsci::scale_fill_nejm() +
#   #coord_flip()+
#   #ggdist::stat_dots(aes(color=Diagnosis,fill=Diagnosis),side = "left", dotsize = .5, justification = 1.1, binwidth = .1)+
#   labs(title=paste0('Distribution of bootstrap ',str_lab1,' deviation'),x="",y="",colour = "#000000",size=20) +
#   #ggtitle('Distribution of bootstrap mean deviation')+
#   #geom_hline(aes(yintercept=0.5),linetype=c('solid'),linewidth=1,alpha=0.3)+
# 
#   #geom_text(data=error2,aes(x=group,y=values+9,label=label),color="red",fontface="bold")+
#   # theme(panel.background = element_blank(),
#   #       axis.text=element_text(size=16),
#   #       axis.line = element_line(colour = "#000000",size = 0.2),
#   #       axis.text.x = element_text(angle = 30,vjust = 0.85,hjust = 0.75))+
#   theme_bw()+
#   theme(#panel.background = element_blank(),
#     axis.text=element_text(size=12,colour = "#000000"),
#     axis.title = element_text(size=20,colour = "#000000",hjust=0.5),
#     axis.line = element_line(colour = "#000000",linewidth = 0.2),
#     legend.position = 'none')+
#   #scale_y_continuous(limits = c(0,1))+
# 
#   coord_flip()+
# 
#   scale_x_discrete(labels = c("MCI" = "MCI","AD" = "AD", "PD" = "PD","SVD" = "CSVD","MS" = "MS",'AQP4Pos_NMOSD'="NMOSD"))
# 
# #hrbrthemes::theme_ipsum(base_family = "Roboto Condensed") +
# # theme(
# #   plot.title = element_markdown(hjust = 0.5,vjust = .5,color = "black",
# #                                 size = 20, margin = margin(t = 1, b = 12)),
# #   plot.subtitle = element_markdown(hjust = 0,vjust = .5,size=15),
# #   plot.caption = element_markdown(face = 'bold',size = 12),
# #   legend.position = "none")
# print(plot01)
# dev.off()


}


#cohnes'd 0.8以上定义为large effect size
#d = 0.01,0.2,0.5,0.8,1.2,2.0分别被定义为very small,small,medium,large,very large和huge的效应量

tem_data<-list

tem_data[tem_data$feature=="Brain-Stem",'feature']<-'Brainstem';
tem_data[tem_data$feature=="cerebellum_total",'feature']<-'Cerebellum';
tem_data[tem_data$feature=="GMV",'feature']<-"GMV";
tem_data[tem_data$feature=="mean_thickness",'feature']<-'Cortical thickness';
tem_data[tem_data$feature=="sGMV",'feature']<-"sGMV";
tem_data[tem_data$feature=="total_surface_arrea" ,'feature']<-'Surface area';
tem_data[tem_data$feature=="Ventricles" ,'feature']<-'CSF';

tem_data$feature<-str_replace(tem_data$feature, "Left-", "lh_")
tem_data$feature<-str_replace(tem_data$feature, "Right-", "rh_")

tem_data[tem_data$diseasae=="SVD" ,'diseasae']<-'CSVD';

tem_data[tem_data$diseasae=="AQP4Pos_NMOSD" ,'diseasae']<-'NMOSD';

setwd(savepath1)


#global feature
  tem_list<-NULL

for(fea in unique(tem_data$feature)[c(1:8)])
     {
       tem_list<-rbind(tem_list,tem_data[tem_data$feature==fea,])
     }
    
    tem_list$feature<-factor(tem_list$feature,levels =c('GMV','sGMV','WMV','CSF','Cerebellum','Brainstem',
                                                        'Cortical thickness',
                                                        'Surface area')) 
    
    tem_list$diseasae<-factor(tem_list$diseasae,levels =c('MCI','AD','PD','CSVD','MS','NMOSD'))   
    tem_list[,'d']<-tem_list$value
    tem_list[,'CI']<-tem_list$CIdif
    # tem_list[abs(tem_list$value)<0.2,'d']<-NA
    # tem_list[abs(tem_list$value)<0.2,'CI']<-NA
    # 
    # tem_list[abs(tem_list$value)<0.2,'CIdif']<-NA
    # tem_list[abs(tem_list$value)<0.2,'value']<-NA
    
    
    
   p<-ggplot(tem_list, aes(y = feature, x = diseasae, fill = d) )+
      geom_tile(color = "white",
                lwd = 1,
                linetype = 1) +
      
      geom_text(aes(label = round(value,2)), color = "black", size = 3) +
      scale_fill_gradient2(limits=c(0.25,0.35),low = "#00CCFF",midpoint =0.3,
                           mid = "white",
                           high = "#990000") +
      coord_fixed()+
      
      theme(axis.text.x = element_text(hjust=1,color="black", 
                                       size=10, angle=45),
            axis.text.y = element_text( color="black", 
                                        size=10),
            legend.text = element_text( color="black", 
                                        size=10))+
      labs(x=NULL,y=NULL)
    print(p);
    ggsave('Global_heatmap_SD.png',
      width = 15,
      height = 15,
      units = c("cm"),
      dpi = 300)
    
    # p<-ggplot(tem_list, aes(y = feature, x = diseasae, fill = CI) )+
    #   geom_tile(color = "white",
    #             lwd = 1,
    #             linetype = 1) +
    #   
    #   geom_text(aes(label = round(CIdif,2)), color = "black", size = 3) +
    #   scale_fill_gradient2(limits=c(0,0.41),low = "#00CCFF",midpoint =0.2,
    #                        #mid = "white",
    #                        high = "#990000") +
    #   coord_fixed()+
    #   
    #   theme(axis.text.x = element_text(hjust=1,color="black", 
    #                                    size=10, angle=45),
    #         axis.text.y = element_text( color="black", 
    #                                     size=10),
    #         legend.text = element_text( color="black", 
    #                                     size=10))+
    #   labs(x=NULL,y=NULL)
    # print(p);
    # ggsave('Global_heatmap_CI.png',
    #        width = 15,
    #        height = 15,
    #        units = c("cm"),
    #        dpi = 300)
    # 
    # 

  
  
  # #subcortical volume
  # tem_list<-NULL
  # for(fea in unique(tem_data$feature)[c(9:11,14:21,24:28)])
  # {
  #   tem_list<-rbind(tem_list,tem_data[tem_data$feature==fea,])
  # }
  # 
  # tem_list$feature<-factor(tem_list$feature,levels =unique(tem_data$feature)[c(9:11,14:21,24:28)]) 
  # 
  # tem_list$diseasae<-factor(tem_list$diseasae,levels =c('MCI','AD','PD','CSVD','MS','NMOSD'))   
  # tem_list[,'d']<-tem_list$value
  # tem_list[,'CI']<-tem_list$CIdif
  # tem_list[abs(tem_list$value)<0.5,'d']<-NA
  # tem_list[abs(tem_list$value)<0.5,'CI']<-NA
  # 
  # 
  # 
  # p<-ggplot(tem_list, aes(y = feature, x = diseasae, fill = d) )+
  #   geom_tile(color = "white",
  #             lwd = 1,
  #             linetype = 1) +
  #   
  #   geom_text(aes(label = round(value,2)), color = "white", size = 3) +
  #   scale_fill_gradient2(limits=c(-1,1),low = "#00CCFF",
  #                        mid = "white",
  #                        high = "#990000") +
  #   coord_fixed()+
  #   
  #   theme(axis.text.x = element_text(hjust=1,color="black", 
  #                                    size=10, angle=45),
  #         axis.text.y = element_text( color="black", 
  #                                     size=10),
  #         legend.text = element_text( color="black", 
  #                                     size=10))+
  #   labs(x=NULL,y=NULL)
  # print(p);
  # 
  # 
  # ggsave('Subcortical_heatmap_d.png',
  #        width = 15,
  #        height = 15,
  #        units = c("cm"),
  #        dpi = 300)
  # 
  # p<-ggplot(tem_list, aes(y = feature, x = diseasae, fill = CI) )+
  #   geom_tile(color = "white",
  #             lwd = 1,
  #             linetype = 1) +
  #   
  #   geom_text(aes(label = round(CIdif,2)), color = "white", size = 3) +
  #   scale_fill_gradient2(limits=c(-1,1),low = "#00CCFF",
  #                        mid = "white",
  #                        high = "#990000") +
  #   coord_fixed()+
  #   
  #   theme(axis.text.x = element_text(hjust=1,color="black", 
  #                                    size=10, angle=45),
  #         axis.text.y = element_text( color="black", 
  #                                     size=10),
  #         legend.text = element_text( color="black", 
  #                                     size=10))+
  #   labs(x=NULL,y=NULL)
  # print(p);
  # ggsave('Subcortical_heatmap_CI.png',
  #        width = 15,
  #        height = 15,
  #        units = c("cm"),
  #        dpi = 300)
  # 
  
  
  #Cortical volume
 #  tem_list<-NULL
 #  for(fea in unique(tem_data$feature)[c(165:231)])
 #  {
 #    tem_list<-rbind(tem_list,tem_data[tem_data$feature==fea,])
 #  }
 #  
 # tem_list$feature<-str_remove_all(string = tem_list$feature,pattern = '_volume')
 #  #tem_list$feature<-factor(tem_list$feature,levels =unique(tem_data$feature)[c(9:11,14:21,24:28)]) 
 #  
 #  tem_list$diseasae<-factor(tem_list$diseasae,levels =c('MCI','AD','PD','CSVD','MS','NMOSD'))   
 #  tem_list[,'d']<-tem_list$value
 #  tem_list[,'CI']<-tem_list$CIdif
 #  tem_list[abs(tem_list$value)<0.5,'d']<-NA
 #  tem_list[abs(tem_list$value)<0.5,'CI']<-NA
  
  
  
  # p<-ggplot(tem_list, aes(y = feature, x = diseasae, fill = d) )+
  #   geom_tile(color = "white",
  #             lwd = 1,
  #             linetype = 1) +
  #   
  #   geom_text(aes(label = round(value,2)), color = "white", size = 3) +
  #   scale_fill_gradient2(limits=c(-1,1),low = "#00CCFF",
  #                        mid = "white",
  #                        high = "#990000") +
  #   coord_fixed()+
  #   
  #   theme(axis.text.x = element_text(hjust=1,color="black", 
  #                                    size=10, angle=45),
  #         axis.text.y = element_text( color="black", 
  #                                     size=10),
  #         legend.text = element_text( color="black", 
  #                                     size=10))+
  #   labs(x=NULL,y=NULL)
  # print(p);
  # 
  # 
  # ggsave('Cortical_heatmap_d.png',
  #        width = 30,
  #        height = 30,
  #        units = c("cm"),
  #        dpi = 300)
  
  # p<-ggplot(tem_list, aes(y = feature, x = diseasae, fill = CI) )+
  #   geom_tile(color = "white",
  #             lwd = 1,
  #             linetype = 1) +
  #   
  #   geom_text(aes(label = round(CIdif,2)), color = "white", size = 3) +
  #   scale_fill_gradient2(limits=c(-1,1),low = "#00CCFF",
  #                        mid = "white",
  #                        high = "#990000") +
  #   coord_fixed()+
  #   
  #   theme(axis.text.x = element_text(hjust=1,color="black", 
  #                                    size=10, angle=45),
  #         axis.text.y = element_text( color="black", 
  #                                     size=10),
  #         legend.text = element_text( color="black", 
  #                                     size=10))+
  #   labs(x=NULL,y=NULL)
  # print(p);
  # ggsave('Cortical_heatmap_CI.png',
  #        width = 15,
  #        height = 15,
  #        units = c("cm"),
  #        dpi = 300) 
  # 
  
  
    
# setwd(savepath1)
# saveRDS(list,paste0(str_lab,'_inter_var_distribution.rds'))

