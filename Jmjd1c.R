setwd("D:/TEST3/")
diff_gene <- read.table("GC_PC123.gene_summary.txt", sep="\t", header=TRUE, row.names=1)
diff_gene=as.data.frame(diff_gene)
gene_list=diff_gene[,c("neg.p.value","neg.lfc")]

colnames(gene_list)=c("pval","logFC")
gene_list$threshold = as.factor(abs(gene_list$logFC) > 0.585 & gene_list$pval < 0.01)
colored_point<-gene_list[gene_list$threshold == "TRUE",]
Spgenes<-colored_point[rownames(colored_point) == "Jmjd1c" ,]
gene_list$threshold<-as.character(gene_list$threshold)
gene_list$threshold[which(rownames(gene_list) == "Jmjd1c" )]<-"Jmjd1c"
gene_list$threshold[which(gene_list$logFC >0.585 & gene_list$threshold ==TRUE)] <- "UP"
colnames(gene_list)[3]<-"Significant"
gene_list$Significant[which(gene_list$Significant == "TRUE")]<-"Down"
gene_list$Significant[which(gene_list$Significant == "FALSE")]<-"Not Sig"
Mycolors<-c("Black","Black","Gray","Black")
library("ggplot2")
pdf("vocano.pdf")

g = ggplot(data=gene_list, aes(x=logFC, y=-log10(pval),color=Significant)) + geom_point(alpha=0.4, size=1.75)  + xlim(c(-6, 6)) + ylim(c(0, 6)) +xlab("log2 fold change") + ylab("-log10 p-value") + theme_set(theme_bw()) + theme(panel.grid.major=element_line(colour=NA)) + scale_color_manual(values = Mycolors)
print(g)
dev.off()

