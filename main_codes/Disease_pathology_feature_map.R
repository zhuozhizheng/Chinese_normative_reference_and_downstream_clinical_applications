setwd('C:/ZZZ/work/manuscript/Lifespan-main/test_zzz_update_20240116/Declan_advices/results')

tem<-readRDS('Globalboot_MRI_measure_gourp_analysis.rds');


tem<-tem[tem$disease!=0,]
feature<-unique(tem$feature)
global<-feature[1:8];

sel_index<-NULL
for(i in 1:dim(tem)[1])
{
  
  if(tem$feature[i] %in% global)
  {
    sel_index<-c(sel_index,i);
  }
  
}

tem0<-tem[sel_index,]
#volume<-unique(tem$feature[1:48]);
sel_feature<-NULL
num=0;
list<-data.frame(matrix(NA,length(unique(tem0$boot_num))*length(unique(tem0$disease)),3))
colnames(list)<-c('boot_num','disease','feature')
for(i in unique(tem0$boot_num))
{
  print(i)
  for(dis in unique(tem0$disease))
  {
  num=num+1;
  tem1<-tem0[tem0$disease==dis&tem0$boot_num==i,]
  list[num,1]<-i;
  list[num,2]<-dis;
  list[num,3]<-tem1$feature[which.min(tem1$mean)]
  }
}



df<-list %>%
  filter(feature %in% global) %>%
  group_by(disease,feature) %>%
  summarise(counts=n())

df$counts<-df$counts/1000

df<-df %>%
  arrange(disease,desc(feature)) %>%
  mutate(lab_ypos=cumsum(counts)-0.5*counts)



df$disease<-factor(df$disease,levels=disease)

df[,'percentage']<-df$counts
df[df$percentage<0.5,'percentage']<-NA

for(i in 1:dim(df)[1])
{
if(!is.na(df[i,'percentage']))
{
df[i,'percentage']<-round(df$percentage[i],2)
}
}

df[,'feature1']<-df$feature

df[is.na(df$percentage),'feature1']<-NA
df1<-df[!is.na(df$percentage),]


ggplot(df,aes(x=disease,y=counts))+
  geom_bar(aes(color=feature,fill=feature),stat='identity',alpha=0.5)+
    geom_text(
      aes(y=lab_ypos,label=percentage,group=feature),
      color='white'
    )+
  theme_classic ()+
  labs(x=NULL,y=NULL)+
  theme(#panel.background = element_blank(),
    axis.text=element_text(size=16,colour = "#000000"),
    axis.line = element_line(colour = "#000000",size = 0.2),
    
  )+
  coord_flip()+
  scale_x_discrete(labels = c("MCI" = "MCI","AD" = "AD", "PD" = "PD","SVD" = "CSVD","MS" = "MS",'AQP4Pos_NMOSD'="NMOSD"))

