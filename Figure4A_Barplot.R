library(clusterProfiler)
library(org.Hs.eg.db)
library(DOSE)
library(data.table)
library(ggplot2)
library(ggsci)
library(tidyverse)
library(ReactomePA)
protein <- fread("protein_IP.txt", header=T, sep="\t")
colnames(protein)[2] <- "Group"
genes.entrez <- bitr(protein$Gene, fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
data <- enrichPathway(gene=genes.entrez$ENTREZID, pvalueCutoff = 0.05, readable=TRUE)
data2 <- gsePathway(genes.entrez$ENTREZID, pvalueCutoff = 0.05, pAdjustMethod = "BH", verbose = FALSE)

cnetplot(data, showCategory=12, node_label="gene", cex_label_gene = 0.3, colorEdge = T)
cnetplot(data, showCategory=12, node_label="category", cex_label_category =0.3) 

gene <- fread("Gene.txt", header=T, sep="\t")
gene <- transform(gene, Group=factor(Group, levels=c("TF", "Coactivator/Mediator", "Coregulator", "Corepressor/NuRD", "Spliceosome",
                                                     "SWI/SNF", "Cohesin/Condensin/Histone", "Other")))


g <- ggplot(gene, aes(x=reorder(Gene, -LogPEP), y=LogPEP, fill=Group))
g <- g + geom_bar(stat="identity")
g <- g + scale_fill_manual(values=c("#efc000", "#0073c2", "#7aa6dc", "#ee7800","#9079b6", 
                                    "#e4007f", "#8f7700", "#86868664"))
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + xlab("Gene") + ylab("log10(sum PEP score)")
g <- g + theme_classic()
g
