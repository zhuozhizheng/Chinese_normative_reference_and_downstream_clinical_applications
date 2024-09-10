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
library(MatchIt)
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

feature_path=paste0(datapath,'/test_zzz_update_20240116/Global_feature');str_lab<-'Global_feature'# change
savepath=paste0(datapath,'/test_zzz_update_20240116/figure3')
setwd(feature_path)
myfile <- list.files()
RDSfile_all <- myfile[grep(myfile,pattern ="our_model.rds$")]   
RDSfile_all


Exd_var<-c("Global_feature_meanCT2_lhMeanThickness_loop_our_model.rds",
           "Global_feature_meanCT2_lhVertex_loop_our_model.rds" ,
           "Global_feature_meanCT2_rhMeanThickness_loop_our_model.rds",
           "Global_feature_meanCT2_rhVertex_loop_our_model.rds",
           "aseg.vol.table_cerebellum_WM_loop_our_model.rds" ,
           #"aseg.vol.table_cerebellum_total_loop_our_model.rds",
           "aseg.vol.table_cerebellum_GM_loop_our_model.rds",
           "aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds",
           "Global_feature_totalSA2_lh_loop_our_model.rds",
           "Global_feature_totalSA2_rh_loop_our_model.rds")

para<-c('cohens_d')



list_all<-list()
setwd(feature_path)
if(str_lab=='Global_feature'){results<-readRDS(RDSfile_all[1])}

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
set.seed(1)
for(dis in results$disease_comparison$disease)
{
  tem_list<-list[list$Disease==dis,]
  tem_list[abs(tem_list$cohens_d)<0.2,'color_label']<-1;
  tem_list[abs(tem_list$cohens_d)>=0.2,'color_label']<-2;
  tem_list[abs(tem_list$cohens_d)>=0.5,'color_label']<-3;
  tem_list[abs(tem_list$cohens_d)>=0.8,'color_label']<-4;
  
  mask_png <- png::readPNG(system.file("extdata/hearth.png",
                                       package = "ggwordcloud", mustWork = TRUE))
  
  tem_list<-tem_list[tem_list$pFDR<0.05,]
  
  setwd(savepath)
  png(filename = paste0(dis,'_',str_lab,'_all_features_importantance_plot.png'),
      width = 1480,
      height = 1480,
      units = "px",
      bg = "white",
      res = 300)
  
  p<-ggplot(tem_list, aes(label = feature,
                          size = cohens_d,color = color_label)) +
    geom_text_wordcloud() +
    scale_size_area(max_size = )+
    theme_minimal()+
    scale_color_gradient(limits=c(1,4),low = "gray", high = "darkred")
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
# tem_list<-tem_list[tem_list$pFDR<0.05,]
# png(filename = paste0(dis,'_',str_lab,'_all_features_importantance_plot.png'),
#     width = 1480,
#     height = 1480,
#     units = "px",
#     bg = "white",
#     res = 300)
# 
# p<-ggplot(tem_list, aes(label = feature,
#                         size = cohens_d,color = color_label)) +
#   geom_text_wordcloud() +
#   scale_size_area(max_size = )+
#   theme_minimal()+
#   scale_color_gradient(limits=c(1,4),low = "gray", high = "darkred")
# print(p)
# dev.off()
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
Greatest_feature_global<-list();
Greatest_feature_global$Greatest_list<-Greatest_list;
Greatest_feature_global$Greatest_list_other<-Greatest_list_other;

saveRDS(Greatest_feature_global,'Greatest_feature_global.rds');

#forest plot
setwd(savepath);
Greatest_list[,'ZZ_feature']<-Greatest_list$feature

Greatest_list[,' ']<-paste(rep(" ",dim(Greatest_list)[1]),collapse = " ");
Greatest_list[,'Cohens d (95% CI)']<-sprintf("%.2f (%.2f to %.2f)",
                                             Greatest_list$cohens_d,
                                             Greatest_list$cohens_d_CI_low,
                                             Greatest_list$cohens_d_CI_high)
library(forestploter)
plot<-forest(Greatest_list[,c('Disease'," ",'Cohens d (95% CI)')],
             est=Greatest_list$cohens_d,
             lower=Greatest_list$cohens_d_CI_low,
             upper=Greatest_list$cohens_d_CI_high,
             ci_column=2)

# geom_point(aes(x=mean, y=label))+
#   geom_errorbarh(aes(y=label,xmin=lower, xmax=upper),
#                  height=0.5, # 控制左右端点两条小竖线的长短
#                  size=1)+
#   theme_bw()
# 
# ggplot(Greatest_list, aes(y = Disease, x = cohens_d) )+
#   geom_pointrange(aes(xmin = cohens_d_CI_low, xmax = cohens_d_CI_high),
#                   width=0.2, size=1, color="gray", fill="black", shape=1)+
#   theme_classic()




#overlap denisty plot
setwd(feature_path)
if(str_lab=='Global_feature'){results<-readRDS(RDSfile_all[1]);
data<-results$all_data[,1:17]}

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




setwd(savepath)
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
    tem_data$Diagnosis<-factor(tem_data$Diagnosis,levels=c('HC',dis))
    match.it <- matchit(Diagnosis ~ Age + Sex + Site_ZZZ, data = tem_data, method="nearest", ratio=1)
    #a <- summary(match.it)
    tem_data<-tem_data[c(rownames(match.it$match.matrix),match.it$match.matrix),]
    
    tem_data<-na.omit(tem_data)
    
    
    png(filename = paste0(feature,'_',dis,'_vs_HC_density_plot.png'),
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
  }}




