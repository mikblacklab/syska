# first SYSKA
# Introduces dplyr and integration with ggplot2
#

library(dplyr)
library(ggplot2)



#data is an reduced sample of iHS scores by position, annotated with rsIds and gene names
#data is stored on the Xsan in staff_groups/blacklab/Documents/syska_data/
load("ann.Rdata")

#arrange dataset based on |iHS| values (ascending order)
# %>% should be read as 'then do', it is also know as a pipe
ann1 = ann %>% arrange(abs(iHS))

#arrange dataset in descending order based on
ann1 = ann %>% arrange( desc(abs(iHS)))

#create rankings based on sorted position. Largest |iHS| = 1
ann1$rank = as.numeric(rownames(ann1))

#remove rows containing na's (have no gene annotation)
ann1=na.omit(ann1)

genes=c("ABCG2", "SLC2A9", "PPARGC1A")

#for the genes we want, group the ranks by gene and then calculate various statistics about the rank
ann1 %>% filter(gene %in% genes) %>%
  group_by(gene) %>%
  summarise(min(rank),
            quantile(rank, 0.25),median(rank),
            mean(rank),
            quantile(rank, 0.75),
            max(rank)
            )

#take data and for each gene report on rank using a custom version of summary
a<- ann1 %>%
  group_by(gene) %>%
  summarise(min_rank=min(rank),
            firstQu=quantile(rank, 0.25),
            med=median(rank), me=mean(rank),
            thirdQu=quantile(rank, 0.75),
            max_rank=max(rank),
            len=length(rank), #how many snps in the gene
            v=var(rank)) %>%
  arrange(me, v)

#look at top genes with lowest mean for rank
head(a)

#filter for genes we want then create boxplot to look at distribution of rank in those genes
# the '.' in ggplot(. ,aes()) means take the incoming data from the pipe
ann1 %>%
  filter(gene %in% genes) %>%
  group_by(gene) %>%
  ggplot(. , aes(x=gene, y=rank)) + geom_boxplot() +theme_bw()

