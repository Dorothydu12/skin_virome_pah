#OTU表格
df <- read.table("otu.txt",sep="\t",header = T,row.names = 1,check.names = F)
head(df)
#环境因子数据
env <- read.table("Dust_chemical_metadata.txt",sep="\t",header = T,row.names = 1,check.names = F)
head(env)
print(decorana(t(df)))
#根据DCA1的Axis Lengths值进行选择，如果>4.0选CCA；如果在3.0-4.0之间，选RDA和CCA都可以；如果<3.0, 选择RDA分析即可
RDA <- rda(t(df),env,scale = T)
 #查看结果，把P值<0.05的环境因子筛选出来#
envfit(RDA,env,permutations=999)
RDA=rda(t(df)~Citalopram + Amitriptyline + Benzyl_paraben + Propyl_paraben + Codeine + Cetirizine + Ethyl_paraben + Methyl_paraben + Gabapentin + Roxithromycin + Caffeine + Desvenlafaxin + Fluconazole + Trimethoprim,data = env)   
vif.cca(RDA) #查看，若出现>20（不相关）的环境因子，需要剔除；若超过3个，需逐步从大到小依次剔除#
anova(dbRDA, by="term") #ANOVA 显著性分析
#提取数据
df_rda <- data.frame(RDA$CCA$u[,1:2],rownames(env))
colnames(df_rda)=c("RDA1","RDA2","samples")
# 提取物种得分
df_rda_score <- data.frame(RDA$CCA$v[,1:2])
#计算轴标签数据（=轴特征值/sum(所有轴的特征值)）
RDA1 =round(RDA$CCA$eig[1]/sum(RDA$CCA$eig)*100,2)
RDA2 =round(RDA$CCA$eig[2]/sum(RDA$CCA$eig)*100,2)

#读入分组文件
group <- read.table("sample_group.txt", sep='\t', header=T)
#修改列名
colnames(group) <- c("samples","group")
#将绘图数据和分组合并
df_rda <- merge(df_rda,group,by="samples")
color=c("#1597A5","#FFC24B","#FEB3AE")#颜色变量



p1<-ggplot(data=df_rda,aes(x=dbRDA1,y=dbRDA2,
                           color=cluster))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_point(size=3,shape=16)+#绘制点图并设定大小
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+#图中虚线
  geom_text(aes(label=samples, y=RDA2+0.03,x=RDA1+0.03,  vjust=0),size=3)+#添加数据点的标签
  # guides(color=guide_legend(title=NULL))+#去除图例标题
  labs(x=paste0("RDA1 (",RDA1,"%)"),
       y=paste0("RDA2 (",RDA2,"%)"))+#将x、y轴标题改为贡献度
  stat_ellipse(data=df_rda,
               level=0.95,
               linetype = 1,linewidth=0.5,
               show.legend = T)+
  scale_color_manual(values = color) +#点的颜色设置
  scale_fill_manual(values = c("#1597A5","#FFC24B","#FEB3AE"))+
  theme(axis.title.x=element_text(size=12),#修改X轴标题文本
        axis.title.y=element_text(size=12,angle=90),#修改y轴标题文本
        axis.text.y=element_text(size=10),#修改x轴刻度标签文本
        axis.text.x=element_text(size=10),#修改y轴刻度标签文本
        panel.grid=element_blank())#隐藏网格线

#提取环境因子得分
df_rda_env <- RDA$CCA$biplot[,1:2]
df_rda_env <- as.data.frame(df_rda_env)
head(df_rda_env)
# 添加环境因子数据
p1+
  geom_segment(data=df_rda_env,aes(x=0,y=0,xend=df_rda_env[,1],yend=df_rda_env[,2]),
               color="black",size=0.8,
               arrow=arrow(angle = 35,length=unit(0.3,"cm")))+
  geom_text(data=df_rda_env,aes(x=df_rda_env[,1],y=df_rda_env[,2],
                                label=rownames(df_rda_env)),size=3.5,
            color="blue", 
            hjust="inward",
            vjust=0.5*(1-sign(df_rda_env[,2])))+
  theme(legend.position = "top")