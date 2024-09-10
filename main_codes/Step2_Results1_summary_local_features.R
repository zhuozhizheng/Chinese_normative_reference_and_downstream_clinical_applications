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
library(MatchIt)

path<-list()
str_lab<-'local_feature'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/aseg.vol.table');
path[[2]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.thickness.table');
path[[3]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.thickness.table');
path[[4]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.area.table');
path[[5]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.area.table');
path[[6]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.volume.table');
path[[7]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.volume.table');

savepath=paste0(datapath,'/test_zzz_update_20240116/figure3')

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
           "lh.aparc.area.table_rh_WhiteSurfArea_area_loop_our_model.rds",
           "rh.aparc.area.table_rh_WhiteSurfArea_area_loop_our_model.rds",
           "lh.aparc.thickness.table_rh_MeanThickness_thickness_loop_our_model.rds",
           "rh.aparc.thickness.table_rh_MeanThickness_thickness_loop_our_model.rds")


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


para<-c('cohens_d')


list_all<-list()
results<-readRDS(RDSfile_all[1])

list=data.frame(matrix(NaN,length(results$disease_comparison$disease)*length(RDSfile_all)),9)

num=0;
for(RDSfile_loop in RDSfile_all)
{
  
  if(!(RDSfile_loop %in% Exd_var)){
    print(RDSfile_loop)
    RDSfile<-RDSfile_loop
    results<-readRDS(RDSfile)
    
    list_all[[RDSfile_loop]]<-results;
    
    results$disease_comparison$disease
    
    for(dis_ind in 1:length(results$disease_comparison$disease))
    {num=num+1;
    
    list[num,1]<-'HC'
    list[num,2]<-results$disease_comparison$disease[dis_ind]
    tem<-results$disease_comparison[para]
    list[num,3]<-round(tem[dis_ind,1],2)
    tem<-results$disease_comparison[paste0(para,'_CI_low')]
    list[num,4]<-round(tem[dis_ind,1],2)
    tem<-results$disease_comparison[paste0(para,'_CI_high')]
    list[num,5]<-round(tem[dis_ind,1],2)
    tem<-results$disease_comparison$tvalue
    list[num,6]<-tem[dis_ind]
    tem<-results$disease_comparison$pvalue
    list[num,7]<-tem[dis_ind]
    tem<-results$disease_comparison$degree
    list[num,8]<-tem[dis_ind]
    list[num,9]<-results$i
    }
    
    colnames(list)<-c('HC','Disease',
                      para,paste0(para,'_CI_low'),paste0(para,'_CI_high'),
                      'pvalue','tvalue','degree','feature')
  }
}  


list<-list[!is.na(list[,para]),]
list[,'pFDR']<-p.adjust(list$pvalue,method='fdr')

library(ggwordcloud)
set.seed(666)
#setwd('C:/ZZZ/work/manuscript/Lifespan-main/test_zzz/local_feature_figs')
setwd(savepath)
for(dis in results$disease_comparison$disease)
{
tem_list<-list[list$Disease==dis,]
tem_list[abs(tem_list$cohens_d)<0.2,'color_label']<-1;
tem_list[abs(tem_list$cohens_d)>=0.2,'color_label']<-2;
tem_list[abs(tem_list$cohens_d)>=0.5,'color_label']<-3;
tem_list[abs(tem_list$cohens_d)>=0.8,'color_label']<-4;

mask_png <- png::readPNG(system.file("extdata/hearth.png",
                                       package = "ggwordcloud", mustWork = TRUE))

# if(dis=="MCI"){tem_list<-tem_list[tem_list$pFDR<10^-6,]}
# if(dis=="AD"){tem_list<-tem_list[tem_list$pFDR<10^-25,]}
# if(dis=="PD"){tem_list<-tem_list[tem_list$pFDR<10^-6,]}
# if(dis=="SVD"){tem_list<-tem_list[tem_list$pFDR<10^-15,]}
# if(dis=="MS"){tem_list<-tem_list[tem_list$pFDR<10^-15,]}
# if(dis=="AQP4Pos_NMOSD"){tem_list<-tem_list[tem_list$pFDR<10^-10,]}

tem_list<-tem_list[order(tem_list$cohens_d,decreasing = T),]

tem_list<-tem_list[1:50,]

setwd(savepath)
png(filename = paste0(dis,'_',str_lab,'_all_features_importantance_plot1.png'),
    width = 1480,
    height = 1480,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(tem_list, aes(label = feature,
                 size = cohens_d,color = color_label)) +
  geom_text_wordcloud(eccentricity=1) +
  scale_size_area(max_size = 4)+
  theme_minimal()+
  scale_color_gradient(limits=c(0,4),low = "grey", high = "darkred")
print(p)
dev.off()
}


######other var######other var######other var######other var######other var
para_other<-c('Overlap')
setwd(feature_path)
list_all_other<-list()
if(str_lab=='Global_feature'){results<-readRDS(RDSfile_all[1])}

list_other=data.frame(matrix(NaN,length(results$disease_comparison$disease)*length(RDSfile_all)),9)

num=0;
for(RDSfile_loop in RDSfile_all)
{
  
  if(!(RDSfile_loop %in% Exd_var)){
    print(RDSfile_loop)
    RDSfile<-RDSfile_loop
    results<-readRDS(RDSfile)
    
    list_all_other[[RDSfile_loop]]<-results;
    
    results$disease_comparison$disease
    
    for(dis_ind in 1:length(results$disease_comparison$disease))
    {num=num+1;
    
    list_other[num,1]<-'HC'
    list_other[num,2]<-results$disease_comparison$disease[dis_ind]
    tem<-results$disease_comparison[para_other]
    list_other[num,3]<-round(tem[dis_ind,1],2)
    tem<-results$disease_comparison[paste0(para_other,'_CI_low')]
    list_other[num,4]<-round(tem[dis_ind,1],2)
    tem<-results$disease_comparison[paste0(para_other,'_CI_high')]
    list_other[num,5]<-round(tem[dis_ind,1],2)
    tem<-results$disease_comparison$tvalue
    list_other[num,6]<-tem[dis_ind]
    tem<-results$disease_comparison$pvalue
    list_other[num,7]<-tem[dis_ind]
    tem<-results$disease_comparison$degree
    list_other[num,8]<-tem[dis_ind]
    list_other[num,9]<-results$i
    }
    
    colnames(list_other)<-c('HC','Disease',
                      para_other,paste0(para_other,'_CI_low'),paste0(para_other,'_CI_high'),
                      'pvalue','tvalue','degree','feature')
  }
}  


list_other<-list_other[!is.na(list_other[,para_other]),]
list_other[,'pFDR']<-p.adjust(list_other$pvalue,method='fdr')

# library(ggwordcloud)
# set.seed(1)
# for(dis in results$disease_comparison$disease)
# {
#   tem_list_other<-list_other[list_other$Disease==dis,]
#   tem_list_other[abs(tem_list_other[,para])<0.2,'color_label']<-1;
#   tem_list_other[abs(tem_list_other[,para])>=0.2,'color_label']<-2;
#   tem_list_other[abs(tem_list_other[,para])>=0.5,'color_label']<-3;
#   tem_list_other[abs(tem_list_other[,para])>=0.8,'color_label']<-4;
#   
#   mask_png <- png::readPNG(system.file("extdata/hearth.png",
#                                        package = "ggwordcloud", mustWork = TRUE))
#   
#   png(filename = paste0(dis,'_',str_lab,'_all_features_importantance_plot_overlap.png'),
#       width = 1480,
#       height = 1480,
#       units = "px",
#       bg = "white",
#       res = 300)
#   
#   ggplot(tem_list, aes(label = feature,
#                        size = cohens_d,color = color_label)) +
#     geom_text_wordcloud() +
#     scale_size_area(max_size = 20)+
#     theme_minimal()+
#     scale_color_gradient(low = "gray", high = "darkred")
#   dev.off()
# }
######other var######other var######other var######other var######other var######other var


Greatest_list<-data.frame(matrix(NaN,length(results$disease_comparison$disease),10))
colnames(Greatest_list)<-colnames(list)

 Greatest_list_other<-data.frame(matrix(NaN,length(results$disease_comparison$disease),10))
 colnames(Greatest_list_other)<-colnames(list_other)

for(dis_ind in 1:length(results$disease_comparison$disease))
{
  tem_list<-list[list$Disease==results$disease_comparison$disease[dis_ind],];
  tem_list_other<-list_other[list_other$Disease==results$disease_comparison$disease[dis_ind],];
  
  Greatest_list[dis_ind,]<-tem_list[which.max((tem_list[,para])),]
  Greatest_list_other[dis_ind,]<-tem_list_other[which.max((tem_list[,para])),]
}

print(Greatest_list)
print(Greatest_list_other)

setwd(savepath);
Greatest_feature<-list();
Greatest_feature$Greatest_list_local<-Greatest_list;
Greatest_feature$Greatest_list_other_local<-Greatest_list_other;

saveRDS(Greatest_feature,'Greatest_feature_local.rds');


#forest plot
# 
# Greatest_list[,'ZZ_feature']<-c('thichness','thickness','thicness','thickness',
#                                 'sGMV','surface_area','braintem','brainstem',
#                                 'cerebellum','thickness','thickness')
# 
# Greatest_list[,' ']<-paste(rep(" ",dim(Greatest_list)[1]),collapse = " ");
# Greatest_list[,'Cohens d (95% CI)']<-sprintf("%.2f (%.2f to %.2f)",
#                                              Greatest_list$cohens_d,
#                                              Greatest_list$cohens_d_CI_low,
#                                              Greatest_list$cohens_d_CI_high)
# library(forestploter)
# plot<-forest(Greatest_list[,c('Disease'," ",'Cohens d (95% CI)')],
#                                 est=Greatest_list$cohens_d,
#                                 lower=Greatest_list$cohens_d_CI_low,
#                                 upper=Greatest_list$cohens_d_CI_high,
#                                 ci_column=2)





#overlap denisty plot#overlap denisty plot#overlap denisty plot#overlap denisty plot
#overlap denisty plot#overlap denisty plot#overlap denisty plot#overlap denisty plot
#overlap denisty plot#overlap denisty plot#overlap denisty plot#overlap denisty plot
setwd(feature_path)
results<-readRDS(RDSfile_all[1]);
data<-results$all_data[,1:17]

for(RDSfile_loop in RDSfile_all)
{
  
  if(!(RDSfile_loop %in% Exd_var)){
    print(RDSfile_loop)
    RDSfile<-RDSfile_loop
    results<-readRDS(RDSfile)
    tem_data1<-results$Quant_data[results$i]
    tem_data1<-data.frame(tem_data1)
    rownames(tem_data1)<-results$all_data$individual_ID;
    index<-intersect(rownames(data),rownames(tem_data1));
    data<-cbind(data[index,],tem_data1[index,])
    colnames(data)[dim(data)[2]]<-results$i
  }
}  

data0<-data;




p<-list()
library(ggpubr)
 for(feature in colnames(data)[18:dim(data0)[2]])
 {
num=0;
data=data0;
colnames(data)[colnames(data)==feature]<-'tem_feature';

for(dis in results$disease_comparison$disease)
{
num=num+1  
tem_data<-data[data$Diagnosis=='HC'|data$Diagnosis==dis,]
tem_data<-data[data$Diagnosis=='HC'|data$Diagnosis==dis&
                 data$Age>=min(data[data$Diagnosis==dis,'Age'],na.rm=TRUE)&
                 data$Age<=max(data[data$Diagnosis==dis,'Age'],na.rm=TRUE),];

tem_data<-na.omit(tem_data)
tem_data$Diagnosis<-factor(tem_data$Diagnosis,levels=c('HC',dis));

tem_data$Diagnosis<-factor(tem_data$Diagnosis,levels=c('HC',dis))
match.it <- matchit(Diagnosis ~ Age + Sex + Site_ZZZ, data = tem_data, method="nearest", ratio=1)
#a <- summary(match.it)
tem_data<-tem_data[c(rownames(match.it$match.matrix),match.it$match.matrix),]

tem_data<-na.omit(tem_data)
setwd(savepath)

if((dis %in% Greatest_list$Disease)&(feature %in% Greatest_list$feature))###修改此处内容#修改此处内容#修改此处内容#修改此处内容#修改此处内容
{png(filename = paste0(feature,'_',dis,'_vs_HC_density_plot.png'),
    width = 1480,
    height = 1480,
    units = "px",
    bg = "white",
    res = 300)

p1<-ggplot(tem_data,aes(x = tem_feature, fill = Diagnosis))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), #主网格线
  panel.grid.minor = element_blank(), #次网格线
  panel.border = element_blank(), #边框
  axis.title = element_blank(),  #轴标题
  axis.text = element_blank(), # 文本
  axis.ticks = element_blank()) +
  labs(x=" ", y="density",title=paste0(feature,'_',dis,':',
                                        "Overlap=",round(results$disease_comparison$Overlap[num],2),
                                       ',95% CI=[',round(results$disease_comparison$Overlap_CI_low[num],2),
                                       ',',round(results$disease_comparison$Overlap_CI_high[num],2),']'))
#print(p[[num]])
print(p1)
dev.off()
}
}}




