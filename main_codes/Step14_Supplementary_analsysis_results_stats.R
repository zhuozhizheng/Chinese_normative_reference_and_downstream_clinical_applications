rm(list = ls())

datapath = 'C:/ZZZ/work/manuscript/Lifespan-main'# change
savepath0 = paste0(datapath, '/test_zzz_update_20240116/Supplementary_figures_and_tables')

setwd(datapath)
source("100.common-variables.r")
source("101.common-functions.r")

source("ZZZ_function.R")
source("300.variables.r")
source("301.functions.r")
library(ggpubr)

library(scales)
#eResult 1
#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex
#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex
#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex
#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex#lifespan trajectory stratified by sex
rdspath=paste0(datapath,'/test_zzz_update_20240116/Global_feature')
setwd(rdspath);
rdsfile<-c('Global_feature_GMV_loop_our_model.rds',
           'Global_feature_sGMV_loop_our_model.rds',
           'Global_feature_WMV_loop_our_model.rds',
           "Global_feature_Ventricles_loop_our_model.rds",
           "aseg.vol.table_cerebellum_total_loop_our_model.rds",
           "aseg.vol.table_Brain-Stem_loop_our_model.rds",
           "Global_feature_total_surface_arrea_loop_our_model.rds",
           "Global_feature_mean_thickness_loop_our_model.rds")

#results$disease_comparison_paired$
  
para<-c('disease1','disease2','cohens_d','hedges_g','glass_delta','Eta2','rank_biserial',
          'cliffs_delta','cohens_u1','Overlap')
para1<-c('cohens_d','hedges_g','glass_delta','Eta2','rank_biserial',
         'cliffs_delta','cohens_u1','Overlap')


for(i in rdsfile)
{
  setwd(rdspath)
  results<-readRDS(i);
  
  data1<-results$data1
  Female_p2<-results$Female_p2
  Male_p2<-results$Male_p2
  
 
  setwd(savepath0)
  png(filename = paste0(results$str,'_',results$i,'_all_with_sex_stratified.png'),
      width = 1480,
      height = 1480,
      units = "px",
      bg = "white",
      res = 300)

  if(i!="Global_feature_mean_thickness_loop_our_model.rds")
  {
  p3<-ggplot()+
    #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
    geom_point(data=data1[data1$Sex=='Female',],aes(x=Age,y=tem_feature/10000),
               colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
    geom_point(data=data1[data1$Sex=='Male',],aes(x=Age,y=tem_feature/10000),
               colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
    geom_line(data=Female_p2,aes(x=Age,y=median/10000),color=c('#990000'),linewidth=1,linetype=c('solid'))+
    geom_line(data=Female_p2,aes(x=Age,y=lower95CI/10000),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
    geom_line(data=Female_p2,aes(x=Age,y=upper95CI/10000),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
  

    geom_line(data=Male_p2,aes(x=Age,y=median/10000),color=c('#00CCFF'),linewidth=1,linetype=c('solid'))+
    #geom_line(data=Male_p2,aes(x=Age,y=lower99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
    geom_line(data=Male_p2,aes(x=Age,y=lower95CI/10000),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
    geom_line(data=Male_p2,aes(x=Age,y=upper95CI/10000),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
    #scale_x_continuous(breaks=c(6,18,60,100))+
    labs(x='Age (years)',y=c(results$i))+
    scale_x_continuous(
      trans = log2_trans(),
      breaks = c(6,18,60,100))+
    
    theme_classic()+
    theme(axis.text=element_text(size=20))

  print(p3)

  dev.off()
  }else
  {
    p3<-ggplot()+
    #geom_line(data=mydata,aes(x=Age,y=Value,group=Percentile,color=Percentile))+
    geom_point(data=data1[data1$Sex=='Female',],aes(x=Age,y=tem_feature),
               colour=c('#990000'),shape=16,size=3,alpha = 0.1)+
    geom_point(data=data1[data1$Sex=='Male',],aes(x=Age,y=tem_feature),
               colour=c('#00CCFF'),shape=17,size=3,alpha = 0.1)+
    geom_line(data=Female_p2,aes(x=Age,y=median),color=c('#990000'),linewidth=1,linetype=c('solid'))+
    geom_line(data=Female_p2,aes(x=Age,y=lower95CI),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
    geom_line(data=Female_p2,aes(x=Age,y=upper95CI),color=c('#990000'),linewidth=1,linetype=c('dotted'))+
    
    
    geom_line(data=Male_p2,aes(x=Age,y=median),color=c('#00CCFF'),linewidth=1,linetype=c('solid'))+
    #geom_line(data=Male_p2,aes(x=Age,y=lower99CI),color=c('#00CCFF'),linewidth=2,linetype=c('dashed'))+
    geom_line(data=Male_p2,aes(x=Age,y=lower95CI),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
    geom_line(data=Male_p2,aes(x=Age,y=upper95CI),color=c('#00CCFF'),linewidth=1,linetype=c('dotted'))+
    labs(x='Age (years)',y=c(results$i))+
    scale_x_continuous(
        trans = log2_trans(),
        breaks = c(6,18,60,100))+
      #theme(axis.text=element_text(size=30))+
      theme_classic()+
      theme(axis.text=element_text(size=20))
    
    #scale_x_log10()
    
  print(p3)
  
  dev.off()
  }
  
}

#eResult 1 subcortical seg map
#cohens D mapping and overlap mapping#cohens D mapping and overlap mapping
#cohens D mapping and overlap mapping#cohens D mapping and overlap mapping
#cohens D mapping and overlap mapping#cohens D mapping and overlap mapping

feature_path=paste0(datapath,'/test_zzz_update_20240116/aseg.vol.table');str_lab<-'Subcortical_feature'# change
setwd(feature_path)
myfile <- list.files()
RDSfile_all <- myfile[grep(myfile,pattern ="our_model.rds$")]   
RDSfile_all
RDSfile_all<-paste0(feature_path,'/',RDSfile_all)

setwd(savepath0)

Exd_var<-c('aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model.rds',
           "aseg.vol.table_cerebellum_WM_loop_our_model.rds" ,
           "aseg.vol.table_cerebellum_total_loop_our_model.rds",
           "aseg.vol.table_cerebellum_GM_loop_our_model.rds",
           "aseg.vol.table_Brain-Stem_loop_our_model.rds")

#Brain health score BHC calculation
if(str_lab=='Subcortical_feature'){results<-readRDS(RDSfile_all[1]);
data<-data.frame(results$disease_comparison);
data$feature<-results$i}



for(RDSfile_loop in RDSfile_all)
{
  if(!(RDSfile_loop %in% Exd_var)){
    print(RDSfile_loop)
    RDSfile<-RDSfile_loop
    results<-readRDS(RDSfile)
    
    tem_data<-data.frame(results$disease_comparison);
    tem_data$feature<-results$i
    
    data<-rbind(data,tem_data)
  }
}  


                                     
library(ggseg)

feature_seg<-data;
feature_seg[feature_seg$feature=="Left-Thalamus",'feature']<-c("Left-Thalamus-Proper")
feature_seg[feature_seg$feature=="Right-Thalamus",'feature']<-c("Right-Thalamus-Proper")

para1<-c('cohens_d','hedges_g','glass_delta','Eta2','rank_biserial',
         'cliffs_delta','cohens_u1','Overlap')

para<-c('cohens_d')

dis_var1<-c('MCI','AD','PD','SVD','MS','AQP4Pos_NMOSD');

for(i_dis in dis_var1)
{
tem_feature_seg<-feature_seg[feature_seg$disease==i_dis,]
seg_data=data.frame(label=tem_feature_seg$feature,
                    feature=tem_feature_seg$cohens_d)

library(ggsci)
setwd(savepath0)
png(filename = paste0(i_dis,'_',para,"_cohens_d.png"),
    width = 2480,
    height = 880,
    units = "px",
    bg = "white",
    res = 300)

p<-ggplot(seg_data)+
  geom_brain(atlas=aseg,
             size = 0.1,
             side='coronal',
             mapping = aes(fill = feature))+
  theme_void()+
  scale_fill_gradient2(limits=c(-1,1),low='#00CCFF',midpoint =0,high='#990000')
  #scale_fill_gradient(low='grey',high='#990000')

print(p)
dev.off()
}


para<-c('Overlap')

dis_var1<-c('MCI','AD','PD','SVD','MS','AQP4Pos_NMOSD');

for(i_dis in dis_var1)
{
  tem_feature_seg<-feature_seg[feature_seg$disease==i_dis,]
  seg_data=data.frame(label=tem_feature_seg$feature,
                      feature=tem_feature_seg$Overlap)
  
  library(ggsci)
  setwd(savepath0)
  png(filename = paste0(i_dis,'_',para,"_Overlap.png"),
      width = 2480,
      height = 880,
      units = "px",
      bg = "white",
      res = 300)
  
  p<-ggplot(seg_data)+
    geom_brain(atlas=aseg,
               size = 0.1,
               side='coronal',
               mapping = aes(fill = feature))+
    theme_void()+
    scale_fill_gradient2(limits=c(0,1),low='#00CCFF',midpoint =0.5,high='#990000')
  #scale_fill_gradient(low='grey',high='#990000')
  
  print(p)
  dev.off()
}

# library(scales)
# png(filename = paste0(str_lab,"_trajectory.png"),
#     width = 3480,
#     height = 1880,
#     units = "px",
#     bg = "white",
#     res = 300)
# 
# p<-ggplot(data=list,mapping=aes(x=Age,y=Value,colour=Feature,
#                                 linetype='dotted'))+
#   geom_line(size=1)+
#   geom_line(linetype='solid',size=2,alpha=0.2)+
#   geom_point(data=max_data,aes(x=Age,y=Value+0.03,colour=Feature),shape = 6, size = 2, stroke = 1)+
#   #geom_text(data=max_data,aes(x=Age,y=Value+0.08,colour=Feature),size=3,label=max_data$Age)+
#   scale_x_continuous(
#     trans = log2_trans(),
#     breaks = c(6,18,60,100)
#   )+
#   
#   # scale_x_continuous(breaks = c(6,18,60,100))+
#   # scale_x_log10(breaks=)+
#   theme_classic()+
#   
#   labs(x='Age (years)',y='Normalized Structural Measure')
# #scale_color_jama()
# 
# # labs(x='Age (years)',y='Normalized Structural Measure')+
# # scale_color_jama()
# 
# p
# dev.off()



#eResult 1 cortical DK map
#cohens D mapping and overlap mapping#cohens D mapping and overlap mapping
#cohens D mapping and overlap mapping#cohens D mapping and overlap mapping
#cohens D mapping and overlap mapping#cohens D mapping and overlap mapping


# feature_path_L=paste0(datapath,'/test_zzz_update_20240116/lh.aparc.thickness.table');
# str_lab<-'lh.aparc.thickness';del_str='_thickness';hemis<-'left'
# 
# feature_path_R=paste0(datapath,'/test_zzz_update_20240116/rh.aparc.thickness.table');
# str_lab<-'rh.aparc.thickness';del_str='_thickness';hemis<-'right'
# 
# feature_path_L=paste0(datapath,'/test_zzz_update_20240116/lh.aparc.area.table');
# str_lab<-'lh.aparc.area';del_str='_area';hemis<-'left'
# 
# feature_path_R=paste0(datapath,'/test_zzz_update_20240116/rh.aparc.area.table');
# str_lab<-'rh.aparc.area';del_str='_area';hemis<-'right'
# 
feature_path_L=paste0(datapath,'/test_zzz_update_20240116/lh.aparc.volume.table');
str_lab<-'lh.aparc.volume';del_str='_volume';hemis<-'left'
#
feature_path_R=paste0(datapath,'/test_zzz_update_20240116/rh.aparc.volume.table');
str_lab<-'rh.aparc.volume';del_str='_volume';hemis<-'right'


setwd(feature_path_L)
myfile <- list.files()
RDSfile_all_L<- myfile[grep(myfile,pattern =".rds$")]   
RDSfile_all_L<-paste0(feature_path_L,'/',RDSfile_all_L)

setwd(feature_path_R)
myfile <- list.files()
RDSfile_all_R<- myfile[grep(myfile,pattern =".rds$")]   
RDSfile_all_R<-paste0(feature_path_R,'/',RDSfile_all_R)

RDSfile_all<-c(RDSfile_all_L,RDSfile_all_R)


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
           "rh.aparc.volume.table_eTIV_loop_our_model.rds")


results<-readRDS(RDSfile_all[4]);
tem_data<-results$disease_comparison;

for(RDSfile_loop in RDSfile_all)
{
  
  if(!(RDSfile_loop %in% Exd_var)){
    print(RDSfile_loop)
    RDSfile<-RDSfile_loop
    results<-readRDS(RDSfile)
    
    if(is.null(results$disease_comparison)){next}
    
    tem_data1<-results$disease_comparison;
    tem_data1$feature<-results$i
    
    tem_data<-rbind(tem_data,tem_data1);
    
  }
}  


library(ggseg)


setwd(savepath0)

para<-c('cohens_d')

dis_var1<-c('MCI','AD','PD','SVD','MS','AQP4Pos_NMOSD');

library(ggseg)
library(stringr)
for (i_dis in dis_var1)
{
  data<-tem_data[tem_data$disease==i_dis,];
  
  feature_seg<-data;
  
  feature_seg$feature<-str_remove_all(string = feature_seg$feature,pattern = del_str)
  
  seg_data=data.frame(label=feature_seg$feature,
                      feature=feature_seg$cohens_d)
  
  
  library(ggsci)
  png(filename = paste0(i_dis,'_',para,'_',del_str,'.png'),
      width = 2480,
      height = 880,
      units = "px",
      bg = "white",
      res = 300)
  
  p<-ggplot(seg_data)+
    geom_brain(atlas=dk,
               size = 0.1,
               position=position_brain(hemi ~ side),
               #hemi=hemis,
               mapping = aes(fill = feature))+
    theme_void()+
    scale_fill_gradient2(limits=c(-1,1),low='#00CCFF',midpoint =0,high='#990000')
  print(p)
  dev.off()
  
}




para<-c('Overlap')

dis_var1<-c('MCI','AD','PD','SVD','MS','AQP4Pos_NMOSD');

library(ggseg)
library(stringr)
for (i_dis in dis_var1)
{
  data<-tem_data[tem_data$disease==i_dis,];
  
  feature_seg<-data;
  
  feature_seg$feature<-str_remove_all(string = feature_seg$feature,pattern = del_str)
  
  seg_data=data.frame(label=feature_seg$feature,
                      feature=feature_seg$Overlap)
  
  
  library(ggsci)
  png(filename = paste0(i_dis,'_',para,'_',del_str,'.png'),
      width = 2480,
      height = 880,
      units = "px",
      bg = "white",
      res = 300)
  
  p<-ggplot(seg_data)+
    geom_brain(atlas=dk,
               size = 0.1,
               position=position_brain(hemi ~ side),
               #hemi=hemis,
               mapping = aes(fill = feature))+
    theme_void()+
    scale_fill_gradient2(limits=c(0,1),low='#00CCFF',midpoint =0.5,high='#990000')
  print(p)
  dev.off()
  
}





#eResult 1
#patient recruitment#patient recruitment#patient recruitment#patient recruitment
#patient recruitment#patient recruitment#patient recruitment#patient recruitment
#patient recruitment#patient recruitment#patient recruitment#patient recruitment

#install.packages('gamlss')

data1 <- read.csv('new_final_list1_update.csv', header = TRUE)

data1$Age <- data1$Age_y

# #loading for Global_feature
MRI <- read.csv('Results_csv1.csv', header = TRUE)

for (i in 1:dim(MRI)[1])
{
  MRI[i, 'mean_thickness'] <-
    (MRI[i, 'meanCT2_lhMeanThickness'] * MRI[i, 'meanCT2_lhVertex'] +
       MRI[i, 'meanCT2_rhMeanThickness'] * MRI[i, 'meanCT2_rhVertex']) / (MRI[i, 'meanCT2_lhVertex'] +
                                                                            MRI[i, 'meanCT2_rhVertex'])
  
  
  MRI[i, 'total_surface_arrea'] <-
    MRI[i, 'totalSA2_lh'] + MRI[i, 'totalSA2_rh']
  
}

tem_feature <-
  colnames(MRI)[17:29]
str = 'Global_feature'
str_lab = 'Global_feature'


data_centiles <- NULL
data_centiles_qua <- NULL

setwd(datapath)
data1 <- read.csv('new_final_list1_update.csv', header = TRUE)

data1$Age <- data1$Age_y

data1[, 'Euler'] <- data1$euler_number_l + data1$euler_number_r

data1[is.na(data1$Euler), 'Euler'] <- -2


Euler_bh <- data1[, 'Euler']


Euler_bh <- Euler_bh[!is.na(Euler_bh)]
median_Euler <- median(Euler_bh)

low_Euler <- median_Euler - 2 * sd(Euler_bh[Euler_bh != -2])

all_data <- data1[!is.na(data1$Age) & !is.na(data1$Sex) &
                    !is.na(data1$Site_ZZZ) &
                    !is.na(data1$baseline)&
                    is.na(data1$Image_Quality_lab) &
                    #data1$Euler >= low_Euler&
                    data1$Diagnosis=='HC'|
                    data1$Diagnosis=='MCI'|
                    data1$Diagnosis=='AD'|
                    data1$Diagnosis=='PD'|
                    data1$Diagnosis=='SVD'|
                    data1$Diagnosis=='MS'|
                    data1$Diagnosis=='AQP4Pos_NMOSD', ]

dis_var<-c('HC','MCI','AD','PD','SVD','MS','AQP4Pos_NMOSD')
 
data<-all_data[order(all_data$Age),]

data<-all_data[order(all_data$Freesufer_Path2),]

data<-data[!is.na(data$Freesufer_Path2)&data$Freesufer_Path2!="ZZZ",]
site_var<-unique(data$Freesufer_Path2);

for(i in 1:length(site_var))
{
  if(i<10)
    {data[data$Freesufer_Path2==site_var[i],'new_site']<-paste0('Site0',i)}
    else{data[data$Freesufer_Path2==site_var[i],'new_site']<-paste0('Site',i)}
}


for(i_dis in dis_var)
{

  if(i_dis=='HC'){str="Normal Controls"}
  if(i_dis=='MCI'){str="MCI"}
  if(i_dis=='AD'){str="AD"}
  if(i_dis=='PD'){str="PD"}
  if(i_dis=='SVD'){str="CSVD"}
  if(i_dis=='MS'){str="MS"}
  if(i_dis=='AQP4Pos_NMOSD'){str="NMOSD"}
  
tem<-data[data$Diagnosis==i_dis,]
print(paste0('number of ',str," is ", dim(tem)[1]))
print(paste0('Mean age of ',str," is ", mean((tem$Age),na.rm=T)))
print(paste0('SD age of ',str," is ", sd((tem$Age),na.rm=T)))
print(paste0('Female number of ',str," is ", sum((tem$Sex=='Female'))))
print(paste0('Male number of ',str," is ", sum((tem$Sex=='Male'))))
print("*******")
print("*******")

for(i in unique(tem$new_site))
{
  if(sum(tem$new_site==i)<5){tem<-tem[-(which(tem$new_site==i)),]}
}



setwd(savepath0)
png(filename = paste0(i_dis,'_site_age_number.png'),
    width = 4480,
    height = 1000,
    units = "px",
    bg = "white",
    res = 300)

p1<-ggplot(tem,aes(x=new_site,y=Age))+
  geom_boxplot(size=0.1,fill='white')+
  geom_jitter(alpha=0.1)+
  ggtitle(paste0(str,' Age Distribution'))+
  labs(x='',y='Age(years)')+
  theme(panel.background = element_blank(),
        axis.text.x=element_text(angle = 45,vjust = 0.5,hjust = 0.5))

p2<-ggplot(tem,aes(x=new_site))+
  geom_bar()+
  ggtitle(paste0(str,' Counts'))+
  labs(x='',y='Case number')+
  theme(panel.background = element_blank(),
        axis.text.x=element_text(angle = 45,vjust = 0.5,hjust = 0.5))

p3<-ggarrange(p1,p2,ncol=2,nrow=1)
print(p3)
dev.off()

}



#eResult 3
#between groups effect size, overlap coefficient#between groups effect size, overlap coefficient
#between groups effect size, overlap coefficient#between groups effect size, overlap coefficient
#between groups effect size, overlap coefficient#between groups effect size, overlap coefficient


dis_var<-c('HC','MCI','AD','PD','SVD','MS','AQP4Pos_NMOSD')
rdspath=paste0(datapath,'/test_zzz_update_20240116/Global_feature')
setwd(rdspath);
rdsfile<-c('Global_feature_GMV_loop_our_model.rds',
           'Global_feature_sGMV_loop_our_model.rds',
           'Global_feature_WMV_loop_our_model.rds',
           "Global_feature_Ventricles_loop_our_model.rds",
           "aseg.vol.table_cerebellum_total_loop_our_model.rds",
           "aseg.vol.table_Brain-Stem_loop_our_model.rds",
           "Global_feature_mean_thickness_loop_our_model.rds",
           "Global_feature_total_surface_arrea_loop_our_model.rds")



para<-c('disease1','disease2','cohens_d','hedges_g','glass_delta','Eta2','rank_biserial',
        'cliffs_delta','cohens_u1','Overlap')
para1<-c('cohens_d','hedges_g','glass_delta','Eta2','rank_biserial',
         'cliffs_delta','cohens_u1','Overlap')


for(i in rdsfile)
{
  setwd(rdspath)
  results<-readRDS(i);
  data_HC<-results$disease_comparison;
  data_HC[,'disease1']<-'HC';
  data_HC[,'disease2']<-data_HC$disease;
  data_HC<-data_HC[,para];
  data_HC_inv<-data_HC;
  data_HC_inv[,para[c(1,2)]]<-data_HC[,para[c(2,1)]]
  data_HC_inv[,para1]<-(-data_HC[,para1]);
  
  data_HC_all<-rbind(data_HC,data_HC_inv);
  
  data<-rbind(data_HC_all,results$disease_comparison_paired[,para])
  
  data$disease1<-factor(data$disease1,levels=c(dis_var))
  data$disease2<-factor(data$disease2,levels=c(dis_var))
  tem_data<-NULL
  for(i1 in 1:(length(dis_var)-1))
  {for(i2 in (i1+1):length(dis_var))
  {
    tem_data=rbind(tem_data,data[data$disease1==dis_var[i1]&data$disease2==dis_var[i2],])
    
  }}
  
  tem_data$disease1<-as.character(tem_data$disease1)
  tem_data$disease2<-as.character(tem_data$disease2)
  unique(tem_data$disease1)  
  tem_data[tem_data$disease1=='SVD','disease1']<-c('CSVD');
  tem_data[tem_data$disease1=='AQP4Pos_NMOSD','disease1']<-c('NMOSD');
  
  tem_data[tem_data$disease2=='SVD','disease2']<-c('CSVD');
  tem_data[tem_data$disease2=='AQP4Pos_NMOSD','disease2']<-c('NMOSD');
  
  tem_data$disease1<-factor(tem_data$disease1,levels=c('HC','MCI','AD','PD','CSVD','MS','NMOSD'))
  tem_data$disease2<-factor(tem_data$disease2,levels=c('HC','MCI','AD','PD','CSVD','MS','NMOSD'))
  
  setwd(savepath0)
  png(filename = paste0(i,'_disease_paired_effect_size.png'),
      width = 4000,
      height = 2000,
      units = "px",
      bg = "white",
      res = 300)
  
  p1<-ggplot(tem_data,aes(disease1,disease2,fill=cohens_d))+
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1)+
    geom_text(aes(label = round(cohens_d,2)), 
              color = "white", size = 4) +
    coord_fixed()+
    theme(panel.background = element_blank(),
          axis.text.x=element_text(angle = 45,vjust = 1,hjust = 1))+
    labs(x='',y='')
  
  p2<-ggplot(tem_data,aes(disease1,disease2,fill=abs(Overlap)))+
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1)+
    geom_text(aes(label = round(abs(Overlap),2)), 
              color = "white", size = 4) +
    coord_fixed()+
    theme(panel.background = element_blank(),
          axis.text.x=element_text(angle = 45,vjust = 1,hjust = 1))+
    labs(x='',y='')
  
  p3<-ggarrange(p1,p2,ncol=2,nrow=1);
  print(p3)
  dev.off()
}


           
para1<-c('cohens_d','hedges_g','glass_delta','Eta2','rank_biserial',
         'cliffs_delta','cohens_u1','Overlap')


# #eResult 3 
# #BHI distribution#BHI distribution#BHI distribution#BHI distribution#BHI distribution
# #BHI distribution#BHI distribution#BHI distribution#BHI distribution#BHI distribution
# #BHI distribution#BHI distribution#BHI distribution#BHI distribution#BHI distribution
# 
# path<-list()
# str_lab<-'global_local_feature'# change
# path[[1]]<-paste0(datapath,'/test_zzz_quant_update/aseg.vol.table');
# path[[2]]<-paste0(datapath,'/test_zzz_quant_update/lh.aparc.thickness.table');
# path[[3]]<-paste0(datapath,'/test_zzz_quant_update/rh.aparc.thickness.table');
# path[[4]]<-paste0(datapath,'/test_zzz_quant_update/lh.aparc.area.table');
# path[[5]]<-paste0(datapath,'/test_zzz_quant_update/rh.aparc.area.table');
# path[[6]]<-paste0(datapath,'/test_zzz_quant_update/lh.aparc.volume.table');
# path[[7]]<-paste0(datapath,'/test_zzz_quant_update/rh.aparc.volume.table');
# path[[8]]<-paste0(datapath,'/test_zzz_quant_update/Global_feature');
# 
# 
# 
# 
# setwd(savepath0)
# 
# Exd_var<-c("Global_feature_meanCT2_lhMeanThickness_loop_our_model_quant_update.rds",
#            "Global_feature_meanCT2_lhVertex_loop_our_model_quant_update.rds" ,
#            "Global_feature_meanCT2_rhMeanThickness_loop_our_model_quant_update.rds",
#            "Global_feature_meanCT2_rhVertex_loop_our_model_quant_update.rds",
#            "Global_feature_totalSA2_lh_loop_our_model_quant_update.rds",
#            "Global_feature_totalSA2_rh_loop_our_model_quant_update.rds",
#            'aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model_quant_update.rds',
#            "aseg.vol.table_cerebellum_WM_loop_our_model_quant_update.rds" ,
#            #"aseg.vol.table_cerebellum_total_loop_our_model_quant_update.rds",
#            "aseg.vol.table_cerebellum_GM_loop_our_model_quant_update.rds",
#            #"aseg.vol.table_Brain-Stem_loop_our_model_quant_update.rds",
#            "lh.aparc.thickness.table_eTIV_loop_our_model_quant_update.rds",
#            "rh.aparc.thickness.table_eTIV_loop_our_model_quant_update.rds",
#            "lh.aparc.area.table_eTIV_loop_our_model_quant_update.rds",
#            "rh.aparc.area.table_eTIV_loop_our_model_quant_update.rds",
#            "lh.aparc.volume.table_eTIV_loop_our_model_quant_update.rds",
#            "rh.aparc.volume.table_eTIV_loop_our_model_quant_update.rds",
#            
#            "Global_feature_meanCT2_lhMeanThickness_loop_our_model_quant_update.rds",
#            "Global_feature_meanCT2_lhVertex_loop_our_model_quant_update.rds" ,
#            "Global_feature_meanCT2_rhMeanThickness_loop_our_model_quant_update.rds",
#            "Global_feature_meanCT2_rhVertex_loop_our_model_quant_update.rds",
#            "Global_feature_totalSA2_lh_loop_our_model_quant_update.rds",
#            "Global_feature_totalSA2_rh_loop_our_model_quant_update.rds",
#            'aseg.vol.table_EstimatedTotalIntraCranialVol_loop_our_model_quant_update.rds',
#            "aseg.vol.table_cerebellum_WM_loop_our_model_quant_update.rds" ,
#            #"aseg.vol.table_cerebellum_total_loop_our_model_quant_update.rds",
#            "aseg.vol.table_cerebellum_GM_loop_our_model_quant_update.rds",
#            #"aseg.vol.table_Brain-Stem_loop_our_model_quant_update.rds",
#            "lh.aparc.thickness.table_eTIV_loop_our_model_quant_update.rds",
#            "rh.aparc.thickness.table_eTIV_loop_our_model_quant_update.rds",
#            "lh.aparc.area.table_eTIV_loop_our_model_quant_update.rds",
#            "rh.aparc.area.table_eTIV_loop_our_model_quant_update.rds",
#            "lh.aparc.volume.table_eTIV_loop_our_model_quant_update.rds",
#            "rh.aparc.volume.table_eTIV_loop_our_model_quant_update.rds")
# 
# 
# RDSfile_all<-NULL 
# for(i in 1:length(path))
# {
#   feature_path=path[[i]];
#   setwd(feature_path)
#   myfile <- list.files()
#   RDSfile_all0 <- myfile[grep(myfile,pattern ="our_model_quant_update.rds$")]
#   for(j in RDSfile_all0)
#   {
#     if(!(j %in% Exd_var))
#     {
#       #RDSfile_all0<-paste0(feature_path,'/',RDSfile_all0);
#       RDSfile_all<-c(RDSfile_all,paste0(feature_path,'/',j));
#     }
#   }
# }
# 
# 
# #Brain health score BHC calculation
# results<-readRDS(RDSfile_all[1]);
# data<-results$all_data[,1:17]
# 
# for(RDSfile_loop in RDSfile_all)
# {
#   
#   if(!(RDSfile_loop %in% Exd_var)){
#     print(RDSfile_loop)
#     RDSfile<-RDSfile_loop
#     results<-readRDS(RDSfile)
#     tem_data1<-results$Quant_data[results$i]
#     tem_data1<-data.frame(tem_data1);
#     rownames(tem_data1)<-results$all_data$individual_ID;
#     index<-intersect(rownames(tem_data1),rownames(data));
#     data<-cbind(data[index,],tem_data1[index,])
#     colnames(data)[dim(data)[2]]<-results$i
#   }
# }  
# 
# feature_var<-c(colnames(data)[18:dim(data)[2]])
# 
# data0<-data[,c('Diagnosis',feature_var)];
# 
# disease_var<-results$disease_comparison$disease
# 
# data1<-data0[data0$Diagnosis=='HC',];
# 
# for(dis in disease_var)
# {
#   data1<-rbind(data1,data0[data0$Diagnosis==dis,]);
# }
# 
# sum(data1$Diagnosis=='HC',na.rm=T)
# 
# data1<-na.omit(data1);
# 
# data2<-data1;
# data2$Diagnosis<-factor(data2$Diagnosis,levels=c('HC',disease_var))
# 
# data1[data1$Diagnosis=='HC','label']<-0;
# data1[data1$Diagnosis!='HC','label']<-1;
# 
# data1<-data1[,c('label',feature_var)];
# 
# library(caret)
# 
# set.seed(7);
# folds<-createFolds(y=data1$label,k=10);
# 
# max=0;num=0;
# 
# for(i in 1:10)
# {
#   
#   fold_test<-data1[folds[[i]],]
#   fold_train<-data1[-folds[[i]],]
#   
#   print(paste0('***fold',i,'***'))
#   
#   fold_pre<-glm(label~.,family = binomial(link='logit'),data=fold_train)
#   fold_predict<-predict(fold_pre,type='response',newdata=fold_test);
#   
#   data2[folds[[i]],'Predict']<-fold_predict-fold_pre$coefficients['(Intercept)'];
#  
# }
# 
# rad_data<-aggregate(data2$Predict,by=list(type=data2$Diagnosis),mean)
# 
# W0<-(-rad_data[rad_data$type=='HC','x'])
# 
# data2[,'Predict']<-(-(data2[,'Predict']+W0))
# 
# 
# rad_data<-aggregate(data2$Predict,by=list(type=data2$Diagnosis),mean,na.rm=TRUE)
# 
# rad_data<-data.frame(rad_data)
# rad_data1<-rad_data
# 
# data2$Diagnosis<-factor(data2$Diagnosis,levels=c('HC',disease_var))
#   data2$Predict
# library(ggridges)
# 
#   setwd(savepath0)
#   png(filename = paste0('BHI_distribution_plot.png'),
#       width = 2000,
#       height = 2000,
#       units = "px",
#       bg = "white",
#       res = 300)
#   
# p<-ggplot(data2, aes(x = Predict, y = Diagnosis, fill = Diagnosis)) +
#   geom_density_ridges(aes(point_color=Diagnosis,point_fill=Diagnosis),
#     jittered_points = TRUE,
#                       position = position_points_jitter(width = 0.05, height = 0),
#                       point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.5,)+
#   stat_density_ridges(quantile_lines = TRUE, quantiles = 2,alpha = 0.5)+
#   #geom_vline(xintercept = 0,size=1)+
#   theme_minimal()
# print(p)
# dev.off()


#eResult 3 
#disease risk prediction#disease risk prediction#disease risk prediction#disease risk prediction
#disease risk prediction#disease risk prediction#disease risk prediction#disease risk prediction
#disease risk prediction#disease risk prediction#disease risk prediction#disease risk prediction

library(ggridges)
path<-list()
str_lab<-'global_local_feature'# change
path[[1]]<-paste0(datapath,'/test_zzz_update_20240116/aseg.vol.table');
path[[2]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.thickness.table');
path[[3]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.thickness.table');
path[[4]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.area.table');
path[[5]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.area.table');
path[[6]]<-paste0(datapath,'/test_zzz_update_20240116/lh.aparc.volume.table');
path[[7]]<-paste0(datapath,'/test_zzz_update_20240116/rh.aparc.volume.table');
path[[8]]<-paste0(datapath,'/test_zzz_update_20240116/Global_feature');

setwd(savepath0)

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


#Brain health score BHC calculation
results<-readRDS(RDSfile_all[1]);
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

disease_var<-results$disease_comparison$disease
data0<-data
data_backup<-data0





data<-data0[data0$Diagnosis=='HC'&!is.na(data0$Diagnosis),];

for(i_dis in disease_var)
{
  data<-rbind(data,data0[data0$Diagnosis==i_dis&!is.na(data0$Diagnosis),]);
}

#feature_var<-c(colnames(data)[18:dim(data)[2]])

#data0<-data[,c('Diagnosis',feature_var)];

path<-paste0(datapath,'/test_zzz_update_20240116/figure3/global_and_local_feature')
setwd(path);
results<-readRDS('global_and_local_feature_ROC_loop_lasso_selection.rds')

#table classification
setwd(savepath0)
data1<-rbind(round(results$list_roc_results_lasso_selection$MCI,2),
             round(results$list_roc_results_lasso_selection$AD,2),
                   round(results$list_roc_results_lasso_selection$PD,2),
                         round(results$list_roc_results_lasso_selection$SVD,2),
                               round(results$list_roc_results_lasso_selection$MS,2),
                                     round(results$list_roc_results_lasso_selection$AQP4Pos_NMOSD,2))

data1[,'AUC_low_CI']<-c(paste0(disease_var,' Model'))
data1<-data.frame(data1[,c('AUC_low_CI',colnames(data1)[1:4])])
write.csv(data1,'classification_details.csv')

data$Sex<-as.character(data$Sex)
data[data$Sex=='Male','Sex']<-c(1);
data[data$Sex=='Female','Sex']<-c(0);
data$Sex<-as.numeric(data$Sex)

for(i_dis in disease_var)
{
  model<-results$model[[i_dis]]
  
 # model<-results$model$MCI
  
  
  
  data$Diagnosis<-factor(data$Diagnosis,levels = c('HC',disease_var))
  data[,'Predict']<-predict(model,type='response',newdata=data);

setwd(savepath0)
png(filename = paste0(i_dis,'_disease_risk_prediction_plot.png'),
    width = 2000,
    height = 2000,
    units = "px",
    bg = "white",
    res = 300)

datafig<-data;
# datafig[datafig$Diagnosis=='SVD','Diagnosis']<-c('CSVD');
# datafig[datafig$Diagnosis=='AQP4Pos_NMOSD','Diagnosis']<-c('NMOSD');

p<-ggplot(data, aes(x = Predict, y = Diagnosis, fill = Diagnosis)) +
  geom_density_ridges(aes(point_color=Diagnosis,point_fill=Diagnosis),
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.5,)+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,alpha = 0.5)+
  scale_y_discrete(labels = c("MCI" = "MCI","AD" = "AD", "PD" = "PD",
                              "SVD" = "CSVD","MS" = "MS",'AQP4Pos_NMOSD'="NMOSD"))+
  #geom_vline(xintercept = 0,size=1)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        axis.text=element_text(size=16))
print(p)
dev.off()
}



#eResult 3 
#normal model comparison#normal model comparison#normal model comparison
#normal model comparison#normal model comparison#normal model comparison
#normal model comparison#normal model comparison#normal model comparison
path1<-paste0(datapath,'/test_zzz_update_20240116/Global_feature')
path2<-paste0(datapath,'/test_zzz_brainchart/Global_feature')
path3<-paste0(datapath,'/test_zzz_brainchart_calibrate/Global_feature')

feature_var<-c('GMV','sGMV','WMV','Ventricles',
               'mean_thickness','total_surface_arrea')

num=0;num1=0;
data_cor=data.frame(matrix(NA,length(feature_var)*3,4))
colnames(data_cor)<-c('feature','group','value','pvalue');
data_MAPE=data_cor
colnames(data_MAPE)<-c('feature','group','value','pvalue');

for(i in feature_var)
{
  print(i)
  setwd(path1)
  data1<-readRDS(paste0('Global_feature_',i,'_loop_our_model.rds'));
  setwd(path2)
  data2<-readRDS(paste0('Global_feature_',i,'_loop_our_model_brainchart.rds'));
  setwd(path3)
  data3<-readRDS(paste0('Global_feature_',i,'_loop_our_model_brainchart_calibrate.rds'));
  
  p1<-data1$p2
  p2<-data2$p2
  p3<-data3$p2
  
  num1=num1+1
  num=(num1-1)*3+1;
  data_cor[num,1]<-i
  data_cor[num,2]<-c('p1p2')
  data_cor[num,3]<-round(cor.test(p1$median/10000,p2$median)$estimate,3)
  data_cor[num,4]<-round(cor.test(p1$median/10000,p2$median)$p.value,3)
  
  data_cor[num+1,1]<-i
  data_cor[num+1,2]<-c('p1p3')
  data_cor[num+1,3]<-round(cor.test(p1$median/10000,p3$median)$estimate,3)
  data_cor[num+1,4]<-round(cor.test(p1$median/10000,p3$median)$p.value,3)
  
  data_cor[num+2,1]<-i
  data_cor[num+2,2]<-c('p2p3')
  data_cor[num+2,3]<-round(cor.test(p2$median,p3$median)$estimate,3)
  data_cor[num+2,4]<-round(cor.test(p2$median,p3$median)$p.value,3)
  
  
  data_MAPE[num,1]<-i
  data_MAPE[num,2]<-c('p1p2')
  data_MAPE[num,3]<-round(abs(mean((p1$median/10000-p2$median)/p2$median)),3)
  #data_MAPE[num,4]<-round(cor.test(p1$median,p2$median)$p.value,2)
  
  data_MAPE[num+1,1]<-i
  data_MAPE[num+1,2]<-c('p1p3')
  data_MAPE[num+1,3]<-round(abs(mean((p1$median/10000-p3$median)/p3$median)),3)
  #data_MAPE[num+1,4]<-round(cor.test(p1$median/10000,p3$median)$p.value,2)
  
  data_MAPE[num+2,1]<-i
  data_MAPE[num+2,2]<-c('p2p3')
  data_MAPE[num+2,3]<-round(abs(mean((p2$median-p3$median)/p3$median)),3)
  #data_MAPE[num+2,4]<-round(cor.test(p2$median,p3$median)$p.value,2)
}

library(ggpubr)

data_cor$feature<-factor(data_cor$feature,levels=c(feature_var));
data_MAPE$feature<-factor(data_MAPE$feature,levels=c(feature_var));

setwd(savepath0)
png(filename = paste0('normal_model_comparison_plot.png'),
    width = 5000,
    height = 1500,
    units = "px",
    bg = "white",
    res = 300)

f1<-ggplot(data_cor,aes(x=feature,y=value,shape=group,color=feature))+
  geom_point(size=4,alpha=0.4)+
  theme(#panel.background = element_blank(),
        axis.text.x=element_text(angle = 45,vjust = 1,hjust = 1))+
  labs(x='',y='')+ggtitle('Pearson Correlation')

f2<-ggplot(data_MAPE,aes(x=feature,y=value,shape=group,color=feature))+
  geom_point(size=4,alpha=0.4)+
  theme(axis.text.x=element_text(angle = 45,vjust = 1,hjust = 1))+
  labs(x='',y='')+ggtitle('MASE')

f3<-ggarrange(f1,f2,ncol=2,nrow=1);

print(f3)

dev.off()
feature_var<-'mean_thickness'




setwd('C:/ZZZ/work/manuscript/Lifespan-main/test_zzz_quant_update/figure3/global_and_local_feature')
results<-readRDS('global_and_local_feature_ROC_loop_lasso_selection.rds')



#eResult 4
#local feature cluster plot#local feature cluster plot#local feature cluster plot#local feature cluster plot
#local feature cluster plot#local feature cluster plot#local feature cluster plot#local feature cluster plot
#local feature cluster plot#local feature cluster plot#local feature cluster plot#local feature cluster plot
#local feature cluster plot#local feature cluster plot#local feature cluster plot#local feature cluster plot

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
  
  tem_list<-tem_list[1:30,]
  
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


