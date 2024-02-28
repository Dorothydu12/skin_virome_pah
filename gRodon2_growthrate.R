library(gRodon)
genes <- readDNAStringSet("FMD_003_bin.3.fa.ffn")
CDS_IDs <- readLines("CDS_names.txt")
#sed -n '/##FASTA/q;p' *.gff | awk '$3=="CDS"' | awk '{print $9'} | awk 'gsub(";.*","")' | awk 'gsub("ID=","")' > CDS_names.txt
gene_IDs <- gsub(" .*","",names(genes))
genes <- genes[gene_IDs %in% CDS_IDs]
highly_expressed<- grepl("ribosomal protein",names(genes),ignore.case = T)
predictGrowth(genes, highly_expressed, mode = "metagenome_v2")

##plot density 
mu <- ddply(data, "Source_cluster", summarise, grp.mean=mean(d))
p<-ggplot(data, aes(x=d, fill=Source_cluster)) +
  +     geom_density(alpha=0.4)+geom_vline(data=mu, aes(xintercept=grp.mean, color=Source_cluster),
                                           +                                        linetype="dashed")