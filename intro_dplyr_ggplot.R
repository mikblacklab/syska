# first SYSKA
# Introduces dplyr and integration with ggplot2
#

library(dplyr)
library(ggplot2)


#data is stored on the Xsan in staff_groups/blacklab/Documents/syska_data/
#data is an reduced sample of iHS scores by position, annotated with rsIds and gene names
load("ann.Rdata")

#arrange dataset based on |iHS| values (ascending order)
ann1 = ann %>% arrange(abs(iHS))

#arrange dataset in descending order based on
ann1 = ann %>% arrange( desc(abs(iHS)))

#create rankings based on sorted position. Largest |iHS| = 1
ann1$rank = as.numeric(rownames(ann1))

#remove rows containing na's (have no gene annotation)
ann1=na.omit(ann1)

genes=c("ABCG2", "SLC2A9", "PPARGC1A")
ann1 %>% filter(gene %in% genes) %>% group_by(gene) %>% summarise(min(rank), quantile(rank, 0.25),median(rank), mean(rank),quantile(rank, 0.75), max(rank))
a<- ann1 %>%  group_by(gene) %>% summarise(min_rank=min(rank), firstQu=quantile(rank, 0.25),med=median(rank), me=mean(rank), thirdQu=quantile(rank, 0.75), max_rank=max(rank), len=length(rank), v=var(rank)) %>% arrange(me, v)
head(a)

#create boxplot for rank distribution of selected genes
ann1 %>% filter(gene %in% genes) %>% group_by(gene) %>% ggplot(. , aes(x=gene, y=rank)) + geom_boxplot() +theme_bw()

