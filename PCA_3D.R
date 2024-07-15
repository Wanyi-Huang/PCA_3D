rm(list=ls())
options(stringsAsFactors = F)

#read data from vcf
eigvec <- read.table("snp.ldak.weight.vect", header = F, stringsAsFactors = F)
colnames(eigvec) <- c("FID", "Sample", paste0("PC", 1:nrow(eigvec)))
write.table(eigvec[2:ncol(eigvec)], file = "ldak.eigenvector.xls", sep = "\t", row.names = F, col.names = T, quote = F)
eigval <- read.table("snp.ldak.weight.values", header = F)
pcs <- paste0("PC", 1:nrow(eigval))
eigval[nrow(eigval),1] <- 0
percentage <- eigval$V1/sum(eigval$V1)*100
eigval_df <- as.data.frame(cbind(pcs, eigval[,1], percentage), stringsAsFactors = F)
names(eigval_df) <- c("PCs", "variance", "proportion")
eigval_df$variance <- as.numeric(eigval_df$variance)
eigval_df$proportion <- as.numeric(eigval_df$proportion)
write.table(eigval_df, file = "ldak.eigenvalue.xls", sep = "\t", quote = F, row.names = F, col.names = T)
#read ev value
inp<-read.table("ldak.eigenvector.xls",sep="\t",header=T,row.names = 1)
#plot
library(scatterplot3d)
poptable <- read.table("pca.pop.txt", header = T, comment.char = "")
pop_order <- poptable[match(rownames(inp), poptable[,1]),]
inp<-data.frame(inp,pop_order)
s3d<-scatterplot3d(inp[1:3],color = "black",pch = 21,bg = inp$Color,cex.symbols = 2.5,
                   cex.axis = 1.3, cex.lab = 1.3,type = "h",angle = 225,
                   xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5), zlim = c(-0.5, 0.5))
text(s3d$xyz.convert(inp[,c("PC1","PC2","PC3")]+0.03),
     labels = rownames(inp),
     cex=1,col="black")
legend("right",col = "black", legend = levels(factor(inp$Subtype)),pt.bg =unique(inp$Color), 
       pch = 21, horiz = F,inset=-0.15,cex = 1.3,xpd = T)
dev.off()
