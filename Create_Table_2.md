Create Table 2
================
Leslie (ZW)
2024-05-14

# Read data

``` r
chr_all <- read.csv("~/OneDrive - Beth Israel Lahey Health/2023_parquet_db_methylation/Data/infinium-methylationepic-v-1-0-b5-manifest-file.csv",skip=7)
names(chr_all)[2]<-'CpG'
S54 <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240701_Enet_AHI.csv')
diab <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240701_Enet_Diab.csv')
S91 <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240701_Enet_MinO2.csv')
hyper <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240701_Enet_hyper.csv')
S54$SLPA54<-'AHI'
diab$Diabetes<-'Diabetes'
hyper$Hypertension<-'Hypertension'
S91$MinSpO2<-'MinimumSpO2'
```

# Create table 2

``` r
# Merge the results to create count table
c1<-merge(S54[,c(1,5)], S91[c(1,5)], by='CpG',all=T)
c2<-merge(c1, diab[c(1,5)], by='CpG',all=T)
c3<-merge(c2, hyper[c(1,5)], by='CpG',all=T)
c3[is.na(c3)] <- ''

# Get overlapped traits and create table 2
c3$Overlap <- paste0(c3$SLPA54, ' ', c3$MinSpO2, ' ', c3$Diabetes, ' ', c3$Hypertension)
c3$count <- sapply(c3$Overlap, FUN=function(x) stri_stats_latex(x)[[4]])
tab<- subset(c3[,c(1,6,7)],count>1)
tab<- merge(tab,chr_all[,c(2,12,16)],by='CpG')
tab$Gene <- sub(";.*", "", tab$UCSC_RefGene_Name)
tab.2 <- tab[,c(1,4,6,2)]
tab.2$Overlap <- gsub('(\\w{4,}) ', '\\1, ', tab.2$Overlap)
tab.2$Overlap <- trimws(tab.2$Overlap, whitespace = ", ")
names(tab.2)[2:4] <- c('Chromosome', 'Gene Name', 'Phenotypes')
tab.2 <- subset(tab.2, !Phenotypes == '  Diabetes, Hypertension')

kable(tab.2, row.names = FALSE)
```

| CpG        | Chromosome | Gene Name | Phenotypes                    |
|:-----------|:-----------|:----------|:------------------------------|
| cg00572560 | 10         |           | AHI Diabetes                  |
| cg00574958 | 11         | CPT1A     | AHI MinimumSpO2, Diabetes     |
| cg03246954 | 19         | MKNK2     | AHI MinimumSpO2               |
| cg06690548 | 4          | SLC7A11   | AHI Hypertension              |
| cg14476101 | 1          | PHGDH     | AHI MinimumSpO2, Hypertension |
| cg14656297 | 9          | FXN       | AHI MinimumSpO2               |
| cg19693031 | 1          | TXNIP     | AHI Diabetes, Hypertension    |
| cg23281327 | 10         |           | AHI MinimumSpO2               |
| cg23440058 | 3          | KALRN     | AHI MinimumSpO2               |

``` r
#write.csv(tab.2, '~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table2.csv', row.names = FALSE)
```
