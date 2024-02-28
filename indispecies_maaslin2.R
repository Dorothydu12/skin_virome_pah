library(indicspecies)
data=read.csv("vOTU_normed_coverage.csv",header = T,row.names = 1)
G=read.csv("group.csv",header = T,row.names = 1)
indval = multipatt(data, G$group,control = how(nperm=999)) 


library(Maaslin2)
fit_data = Maaslin2(
  input_data = data, 
  input_metadata = G, 
  output = "maaslin2_virus_output", 
  fixed_effects = c("Cutotype"))


pheatmap(data,
         scale = "none",
         color = c(colorRampPalette(colors = c("#3671AD", "white"))(length(bk)/2),colorRampPalette(colors = c("white", "#B82A2A"))(length(bk)/2)),
         legend_breaks=seq(-4,4,2),
         breaks=bk)