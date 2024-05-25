Supplementary Info Preparation
================
Leslie (ZW)
2024-05-13

# Create Supplementary Table S1

``` r
pheno_MESA <-read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Data/MESA_data/20240229_MESA_crp.csv')
pheno_MESA <- pheno_MESA[complete.cases(pheno_MESA$crp1),]

pheno_MESA$CRP_group <- as.factor(ifelse(pheno_MESA$crp1<1, 'Low (<1)', 
                                        ifelse(pheno_MESA$crp1>3, 'High (>3)', 'Borderline (1-3)')))
pheno_MESA$CRP_group <- relevel(pheno_MESA$CRP_group, ref = 'Low (<1)' )
pheno_MESA$overall<-'Overall'

# Set labels to show in table without changing column names
pheno_MESA$crp1 <- pheno_MESA$crp1 %>%
  ff_label("CRP (mg/L)") 
pheno_MESA$gender1 <- pheno_MESA$gender1 %>%
  ff_label("Gender (%)") 
pheno_MESA$age1c <- pheno_MESA$age1c %>%
  ff_label("Age") 
pheno_MESA$bmi1c <- pheno_MESA$bmi1c %>%
  ff_label("BMI") 
pheno_MESA$race1c <- factor(pheno_MESA$race1c,levels = c("1","2","3","4"),
                          labels = c("White", "Chinese", "Black", "Hispanic/Latino"))

Demo_var <- c("age1c", "bmi1c", "crp1", 'gender1', 'race1c')
Tab.S1 <- pheno_MESA %>% 
  summary_factorlist("CRP_group", Demo_var, na_include=FALSE,cont = "mean") %>%
  ff_percent_only() 
```

    ## Warning: Unknown columns: `p`

``` r
# Keep only 1 level for variables with 2 levels
keep <- c(1:4,6:9) 
Tab.S1 <- Tab.S1[keep,]
Tab.S1.overall <- pheno_MESA %>% summary_factorlist('overall', Demo_var, na_include=FALSE, cont = "mean") %>%
  ff_percent_only()
```

    ## Warning: Unknown columns: `p`

``` r
Tab.S1.overall <- Tab.S1.overall[keep,]
Tab.S1 <- merge(Tab.S1, Tab.S1.overall, by=c('label','levels'))
# Rearrange the table
Tab.S1 <- Tab.S1[c(4:8,1:3), c(1,2,6,3:5)]
Tab.S1$levels[4] <- 'Female'
Tab.S1$label[5] <- ''
Tab.S1$Variable <- NA
Tab.S1$Variable[c(1:3)] <- paste0(Tab.S1$label[c(1:3)], ' (', Tab.S1$levels[c(1:3)], ')')
Tab.S1$Variable[4] <- paste0(Tab.S1$label[4], ' - ', Tab.S1$levels[4])
Tab.S1$Variable[5:8] <- Tab.S1$levels[5:8]
Tab.S1 <- Tab.S1[,c(7,3:6)]
group_name <- data.frame(Variable='Race (%)', Overall=rep(NA,1), Low=rep(NA,1), 
                                    Borderline=rep(NA,1), High=rep(NA,1) )
names(group_name)[3:5] <- names(Tab.S1)[3:5]
Tab.S1 <- rbind(Tab.S1, group_name)
Tab.S1 <- Tab.S1[c(1:4,9,5:8),]
Tab.S1[is.na(Tab.S1)] <- ''
kable(Tab.S1, row.names = FALSE)
```

| Variable               | Overall     | Low (\<1)   | Borderline (1-3) | High (\>3) |
|:-----------------------|:------------|:------------|:-----------------|:-----------|
| Age (Mean (SD))        | 62.2 (10.2) | 61.5 (10.7) | 62.9 (10.2)      | 62.3 (9.8) |
| BMI (Mean (SD))        | 28.3 (5.5)  | 25.6 (4.2)  | 28.0 (4.6)       | 30.8 (6.0) |
| CRP (mg/L) (Mean (SD)) | 3.8 (5.9)   | 0.6 (0.2)   | 1.8 (0.6)        | 8.3 (8.0)  |
| Gender (%) - Female    | 52.4        | 42.0        | 48.0             | 65.1       |
| Race (%)               |             |             |                  |            |
| White                  | 39.3        | 41.2        | 39.9             | 37.1       |
| Black                  | 26.0        | 20.2        | 24.6             | 32.3       |
| Chinese                | 12.1        | 22.2        | 11.5             | 4.2        |
| Hispanic/Latino        | 22.6        | 16.3        | 24.0             | 26.4       |

``` r
#write.csv(Tab.S1, '~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table_S1.csv', row.names = FALSE)
```

# Create Figure S1

``` r
# Read data
rmrs <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240513_crp_surveyPC.csv')
rprs <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240523_PRS_CRP.csv')
pl <- rbind(rmrs,rprs)

pl <- pl[order(pl$pheno),]
row.names(pl)<-1:nrow(pl)
pl$se <- (pl$upper - pl$lower) / (2 * qnorm(0.975))
names(pl) <- c('name','beta','pvalue','lower','upper','CRP_measures','se')

data <- pl[26:29,]
data$labels <- paste0(data$pheno,' (',data$group,')')
data$CRP_measures <- c("MRS-CRP",  "PRS-EUR",  "PRS-wsum-All", 'PRS-wsum-Hispanic')

fc <- data.frame(labels = data$CRP_measures, Beta = round(data$beta,2),
                 lower=round(data$lower,2),upper=round(data$upper,2),
                 p.value=format(data$pvalue, scientific=TRUE, digits=2))

fc$labels<-as.factor(fc$labels)
fc<-fc[order(fc$labels),]
fc$colour<-c('white','gray95','white','gray95')

fctab <- as.data.frame(apply(fc,2,as.character))
fctab$ci<-paste0(fctab$Beta,'  (',fctab$lower, ' - ',fctab$upper,')')
fctab$colour<-c('white','gray95','white','gray95')
fctab <-rbind(fctab,data.frame(labels = "Association", Beta= "", lower = "", upper = "", 
                               p.value = "p-value",ci="Coefficient (95% CI)",colour='white'))
fctab$labels = fct_rev(relevel(as.factor(fctab$labels), "Association"))

mid<-ggplot(fc,aes(y = fct_rev(labels),x=Beta,xmin=lower,xmax=upper)) + theme_classic()  +
  geom_hline(aes(yintercept = labels, colour = colour), size = 7) +scale_colour_identity() +
   geom_point(aes(x=Beta), shape=15, size=3) + 
  geom_linerange(aes(xmin=lower, xmax=upper)) + geom_vline(xintercept = 0, linetype="dashed") +
  annotate("text", x = .1, y = 12, label = "Blood-CRP",fontface='bold') +
  coord_cartesian(ylim=c(1,5)) +
  labs(x="Coefficient Estimate",y='') + theme(legend.position="none") +
  theme(axis.line.y = element_blank(), axis.ticks.y= element_blank(),
        axis.text.y= element_blank(), axis.title.y= element_blank())
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
tab<-ggplot(data = fctab, aes(y = labels)) +
  geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_text(aes(x = 0, label = labels), hjust = 0, fontface = "bold") +
  geom_text(aes(x = 4, label = ci), fontface = ifelse(fctab$ci == "Coefficient (95% CI)", "bold", "plain")) +
  geom_text(aes(x = 7, label = p.value), hjust = 1,fontface = ifelse(fctab$p.value == "p-value", "bold", "plain")) +
  scale_colour_identity() +
  theme_void() +
  theme(plot.margin = margin(5, 0, 35, 0))

grid.arrange(tab,mid,ncol=2)
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Create Figure S2

## Data Preparation

``` r
rmrs1 <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240513_crp_surveyPC.csv')
m1.num <- subset(rmrs1, pheno%in%c('SLPA54','SLPA91','SLPA92') & group%in%'MRS_CRP')
m1.cat <- subset(rmrs1, pheno%in%c('DIABETES2_INDICATOR','HYPERTENSION','LongSlp') 
           & group%in%'MRS_CRP')
m1.crp <- subset(rmrs1, pheno%in%c('LongSlp') & group%in%'Blood_CRP')
m1.num$mod<-'1'
m1.cat$mod<-'1'
m1.crp$mod<-'1'

rmrs2 <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240514_MRSCRP_mod2.csv')
m2.num <- subset(rmrs2, pheno%in%c('SLPA54','SLPA91','SLPA92') & group%in%'MRS_CRP')
m2.cat <- subset(rmrs2, pheno%in%c('DIABETES2_INDICATOR','HYPERTENSION','LongSlp') 
           & group%in%'MRS_CRP')
m2.crp <- subset(rmrs2, pheno%in%c('LongSlp')  & group%in%'Blood_CRP')
m2.num$mod <- '2'
m2.cat$mod <- '2'
m2.crp$mod <- '2'

FigS2A <- rbind(m1.num, m2.num)
FigS2B <- rbind(m1.cat, m2.cat)
FigS2C <- rbind(m1.crp, m2.crp)

FigS2B$coef<-exp(FigS2B$coef)
FigS2B$lower<-exp(FigS2B$lower)
FigS2B$upper<-exp(FigS2B$upper)
FigS2C$coef<-exp(FigS2C$coef)
FigS2C$lower<-exp(FigS2C$lower)
FigS2C$upper<-exp(FigS2C$upper)
```

## Fig S2A

``` r
row.names(FigS2A) <- 1:nrow(FigS2A)
sub <- split(FigS2A, FigS2A$mod) 
m1 <- sub[[1]] %>% dplyr::select(-c(group,mod))
names(m1)<-paste0('m1_', names(m1))
m2 <- sub[[2]]%>% dplyr::select(-c(group,mod)) 
names(m2) <- paste0('m2_',names(m2))

FigS2A <- cbind(m1, m2) %>% dplyr::select(-c(m2_pheno))
names(FigS2A)[1] <- 'Phenotype'

FigS2A <- rbind(FigS2A,data.frame(Phenotype=c('MRS-CRP: Model 1 vs Model 2'),
                           m1_coef=rep(NA,1), m1_p.val=rep(NA,1), m1_lower=rep(NA,1), m1_upper=rep(NA,1),
                           m2_coef=rep(NA,1), m2_p.val=rep(NA,1), m2_lower=rep(NA,1), m2_upper=rep(NA,1)))
FigS2A <- FigS2A[c(4,1:3),]
row.names(FigS2A) <- 1:nrow(FigS2A)
FigS2A$Phenotype[c(2:4)] <- c('AHI','Minimum SpO2','Mean SpO2')
FigS2A$Phenotype <- ifelse(is.na(FigS2A$m1_coef), 
                      FigS2A$Phenotype,
                      paste0("   ", FigS2A$Phenotype))
# NA to blank or NA will be transformed to carachter.
FigS2A$m1_coef<-round(FigS2A$m1_coef,3)
FigS2A$m2_coef<-round(FigS2A$m2_coef,3)
FigS2A$m1_p.val<-format(FigS2A$m1_p.val, scientific=TRUE, digits=2)
FigS2A$m2_p.val<-format(FigS2A$m2_p.val, scientific=TRUE, digits=2)

# Add two blank columns for CI
FigS2A$`95% CI` <- paste(rep(" ", 50), collapse = " ")
# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
FigS2A$m1_lower <- round(FigS2A$m1_lower,3)
FigS2A$m2_lower <- round(FigS2A$m2_lower,3)
FigS2A$m1_upper <- round(FigS2A$m1_upper,4)
FigS2A$m2_upper <- round(FigS2A$m2_upper,3)

FigS2A$CI <- paste(sprintf("(%.2f, %.2f)", FigS2A$m1_lower, FigS2A$m1_upper), 
                sprintf("(%.2f, %.2f)", FigS2A$m2_lower, FigS2A$m2_upper), 
                sep = "\n")
FigS2A$Beta <- paste(sprintf("%.2f", FigS2A$m1_coef),
                sprintf("%.2f", FigS2A$m2_coef),
                sep = "\n")
FigS2A$p.val <- paste(FigS2A$m1_p.val, FigS2A$m2_p.val,
                sep = "\n")
FigS2A$CI[grepl("NA", FigS2A$CI)] <- "" # Any NA to blank
FigS2A$Beta[grepl("NA", FigS2A$Beta)] <- "" # Any NA to blank
FigS2A$p.val[grepl("NA", FigS2A$p.val)] <- "" # Any NA to blank

#################### plot
# Set-up theme
tm <- forest_theme(base_size = 8,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c( "#AF0040","#377eb8"),
                   footnote_gp = gpar(col = "blue"),
                   legend_name = "Model",
                   legend_value = c("Model 1", "Model 2" ),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"),
                   # Table cell padding, width 4 and heights 3
                   )
#> refline_lty will be deprecated, use refline_gp instead.
names(FigS2A)[11]<-'95%CI'
plot <- forest(FigS2A[,c(1,12,13,11,10)], 
            est = list(FigS2A$m1_coef,
                       FigS2A$m2_coef),
            lower = list(FigS2A$m1_lower,
                         FigS2A$m2_lower),
            upper = list(FigS2A$m1_upper,
                         FigS2A$m2_upper),
            ci_column = 5,
            ref_line = 0, 
            theme = tm)
plot
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Fig S2B

``` r
row.names(FigS2B) <- 1:nrow(FigS2B)
sub <- split(FigS2B, FigS2B$mod) 
m1 <- sub[[1]] %>% dplyr::select(-c(group,mod))
names(m1) <- paste0('m1_', names(m1))
m2 <- sub[[2]]%>% dplyr::select(-c(group,mod))
names(m2) <- paste0('m2_', names(m2))
FigS2B <- cbind(m1,m2) %>% dplyr::select(-c(m2_pheno))
names(FigS2B)[1] <- 'Phenotype'

FigS2B <- rbind(FigS2B, data.frame(Phenotype=c('MRS-CRP: Model 1 vs Model 2'),
                           m1_coef=rep(NA,1),m1_p.val=rep(NA,1), m1_lower=rep(NA,1),m1_upper=rep(NA,1),
                           m2_coef=rep(NA,1), m2_p.val=rep(NA,1),m2_lower=rep(NA,1),m2_upper=rep(NA,1) ))
FigS2B <- FigS2B[c(4,1:3),]
row.names(FigS2B) <- 1:nrow(FigS2B)
FigS2B$Phenotype[c(2,3,4)] <- c('Diabetes', 'Hypertension', 'Long Sleep')
FigS2B$Phenotype <- ifelse(is.na(FigS2B$m1_coef), 
                      FigS2B$Phenotype,
                      paste0("   ", FigS2B$Phenotype))
# NA to blank or NA will be transformed to carachter.
FigS2B$m1_coef<-round(FigS2B$m1_coef,3)
FigS2B$m2_coef<-round(FigS2B$m2_coef,3)
FigS2B$m1_p.val<-format(FigS2B$m1_p.val, scientific=TRUE, digits=2)
FigS2B$m2_p.val<-format(FigS2B$m1_p.val, scientific=TRUE, digits=2)

# Add two m1ank columns for CI
FigS2B$`95% CI` <- paste(rep(" ", 50), collapse = " ")
# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
FigS2B$m1_lower <- round(FigS2B$m1_lower,3)
FigS2B$m2_lower <- round(FigS2B$m2_lower,3)
FigS2B$m1_upper <- round(FigS2B$m1_upper,3)
FigS2B$m2_upper <- round(FigS2B$m2_upper,3)

FigS2B$CI <- paste(sprintf("(%.2f, %.2f)", FigS2B$m1_lower, FigS2B$m1_upper),
                sprintf("(%.2f, %.2f)", FigS2B$m2_lower, FigS2B$m2_upper),
                sep = "\n")
FigS2B$Beta <- paste(sprintf("%.2f", FigS2B$m1_coef),
                sprintf("%.2f", FigS2B$m2_coef),
                sep = "\n")
FigS2B$p.val <- paste( FigS2B$m1_p.val, FigS2B$m2_p.val,
                sep = "\n")

FigS2B$CI[grepl("NA", FigS2B$CI)] <- "" # Any NA to blank
FigS2B$Beta[grepl("NA", FigS2B$Beta)] <- "" # Any NA to blank
FigS2B$p.val[grepl("NA", FigS2B$p.val)]<- ""

#################### plot
names(FigS2B)[11] <- '95%CI'
names(FigS2B)[12] <- 'Odds Ratio'
plot <- forest(FigS2B[,c(1,12,13,11,10)], 
            est = list(FigS2B$m1_coef,
                       FigS2B$m2_coef),
            lower = list(FigS2B$m1_lower,
                         FigS2B$m2_lower),
            upper = list(FigS2B$m1_upper,
                         FigS2B$m2_upper),
            ci_column = 5,
            ref_line = 1, 
            theme = tm)
plot
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Fig S2C

``` r
row.names(FigS2C) <- 1:nrow(FigS2C)
sub <- split(FigS2C, FigS2C$mod) 
m1 <- sub[[1]] %>% dplyr::select(-c(group,mod))
names(m1) <- paste0('m1_', names(m1))
m2 <- sub[[2]]%>% dplyr::select(-c(group,mod))
names(m2) <- paste0('m2_', names(m2))
FigS2C <- cbind(m1,m2) %>% dplyr::select(-c(m2_pheno))
names(FigS2C)[1] <- 'Phenotype'

FigS2C <- rbind(FigS2C, data.frame(Phenotype=c('Blood-CRP: Model 1 vs Model 2'),
                           m1_coef=rep(NA,1),m1_p.val=rep(NA,1), m1_lower=rep(NA,1),m1_upper=rep(NA,1),
                           m2_coef=rep(NA,1), m2_p.val=rep(NA,1),m2_lower=rep(NA,1),m2_upper=rep(NA,1) ))
FigS2C <- FigS2C[c(2,1),]
row.names(FigS2C) <- 1:nrow(FigS2C)
FigS2C$Phenotype[c(2)] <- c('Long Sleep')
FigS2C$Phenotype <- ifelse(is.na(FigS2C$m1_coef), 
                      FigS2C$Phenotype,
                      paste0("   ", FigS2C$Phenotype))
# NA to blank or NA will be transformed to carachter.
FigS2C$m1_coef<-round(FigS2C$m1_coef,3)
FigS2C$m2_coef<-round(FigS2C$m2_coef,3)
FigS2C$m1_p.val<-format(FigS2C$m1_p.val, scientific=TRUE, digits=2)
FigS2C$m2_p.val<-format(FigS2C$m1_p.val, scientific=TRUE, digits=2)

# Add two m1ank columns for CI
FigS2C$`95% CI` <- paste(rep(" ", 50), collapse = " ")
# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
FigS2C$m1_lower <- round(FigS2C$m1_lower,3)
FigS2C$m2_lower <- round(FigS2C$m2_lower,3)
FigS2C$m1_upper <- round(FigS2C$m1_upper,3)
FigS2C$m2_upper <- round(FigS2C$m2_upper,3)

FigS2C$CI <- paste(sprintf("(%.2f, %.2f)", FigS2C$m1_lower, FigS2C$m1_upper),
                sprintf("(%.2f, %.2f)", FigS2C$m2_lower, FigS2C$m2_upper),
                sep = "\n")
FigS2C$Beta <- paste(sprintf("%.2f", FigS2C$m1_coef),
                sprintf("%.2f", FigS2C$m2_coef),
                sep = "\n")
FigS2C$p.val <- paste(FigS2C$m1_p.val, FigS2C$m2_p.val,
                sep = "\n")

FigS2C$CI[grepl("NA", FigS2C$CI)] <- "" # Any NA to blank
FigS2C$Beta[grepl("NA", FigS2C$Beta)] <- "" # Any NA to blank
FigS2C$p.val[grepl("NA", FigS2C$p.val)]<- ""

#################### plot
names(FigS2C)[11] <- '95%CI'
names(FigS2C)[12] <- 'Odds Ratio'
plot <- forest(FigS2C[,c(1,12,13,11,10)], 
            est = list(FigS2C$m1_coef,
                       FigS2C$m2_coef),
            lower = list(FigS2C$m1_lower,
                         FigS2C$m2_lower),
            upper = list(FigS2C$m1_upper,
                         FigS2C$m2_upper),
            ci_column = 5,
            ref_line = 1, 
            theme = tm)
plot
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Create Figure S3

## Fig S3A

``` r
womrs <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240517_slp_dis_wo.csv')

# Figure S3A
pnum <- womrs
pnum <- pnum[order(pnum$pheno),]
row.names(pnum)<-1:nrow(pnum)

sub <- split(pnum,pnum$condition) 
bas <- sub[[1]] %>% dplyr::select(-c(condition)) # baseline
names(bas) <- paste0('bas_', names(bas))
cha <- sub[[2]] %>% dplyr::select(-c(condition)) # change
names(cha) <- paste0('cha_', names(cha))
dia<-sub[[3]] %>% dplyr::select(-c(condition)) # diab
names(dia) <- paste0('dia_', names(dia))
hyp<-sub[[4]] %>% dplyr::select(-c(condition)) # hyp
names(hyp) <- paste0('hyp_', names(hyp))
pnum<-cbind(bas, cha, dia, hyp) %>% dplyr::select(-c(cha_pheno, dia_pheno, hyp_pheno))
names(pnum)[1] <- 'Phenotype'

pnum <- rbind(pnum,data.frame(Phenotype=c('Without adjusting for MRS_CRP'),
                           bas_coef=rep(NA,1), bas_p.val=rep(NA,1), bas_lower=rep(NA,1), 
                           bas_upper=rep(NA,1), cha_coef=rep(NA,1), cha_p.val=rep(NA,1),
                           cha_lower=rep(NA,1), cha_upper=rep(NA,1), dia_coef=rep(NA,1), 
                           dia_p.val=rep(NA,1), dia_lower=rep(NA,1), dia_upper=rep(NA,1),
                           hyp_coef=rep(NA,1), hyp_p.val=rep(NA,1), hyp_lower=rep(NA,1),
                           hyp_upper=rep(NA,1)))
pnum <- pnum[c(4,1:3),]
row.names(pnum) <- 1:nrow(pnum)
pnum$Phenotype[c(2:4)] <- c('AHI','Minimum SpO2','Mean SpO2')

pnum$Phenotype <- ifelse(is.na(pnum$bas_coef), 
                      pnum$Phenotype,
                      paste0("   ", pnum$Phenotype))
# NA to blank or NA will be transformed to carachter.
pnum$bas_coef <- round(pnum$bas_coef,3)
pnum$cha_coef <- round(pnum$cha_coef,3)
pnum$hyp_coef <- round(pnum$hyp_coef,3)
pnum$dia_coef <- round(pnum$dia_coef,3)
pnum$bas_p.val <- format(pnum$bas_p.val, digits=2, scientific=TRUE)
pnum$cha_p.val <- format(pnum$cha_p.val, digits=2, scientific=TRUE)
pnum$hyp_p.val <- format(pnum$hyp_p.val, digits=2, scientific=TRUE)
pnum$dia_p.val <- format(pnum$dia_p.val, digits=2, scientific=TRUE)

# Add two basank columns for CI
pnum$`95% CI` <- paste(rep(" ", 50), collapse = " ")
# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
pnum$bas_lower <- round(pnum$bas_lower,3)
pnum$cha_lower <- round(pnum$cha_lower,3)
pnum$hyp_lower <- round(pnum$hyp_lower,3)
pnum$dia_lower <- round(pnum$dia_lower,3)
pnum$bas_upper <- round(pnum$bas_upper,4)
pnum$cha_upper <- round(pnum$cha_upper,3)
pnum$hyp_upper <- round(pnum$hyp_upper,3)
pnum$dia_upper <- round(pnum$dia_upper,3)

pnum$CI <- paste(sprintf("(%.2f, %.2f)", pnum$bas_lower, pnum$bas_upper), 
                sprintf("(%.2f, %.2f)", pnum$cha_lower, pnum$cha_upper),
                sprintf("(%.2f, %.2f)", pnum$dia_lower, pnum$dia_upper),
                sprintf("(%.2f, %.2f)", pnum$hyp_lower, pnum$hyp_upper),
                sep = "\n")
pnum$Beta <- paste(sprintf("%.2f", pnum$bas_coef),
                sprintf("%.2f", pnum$cha_coef),
                sprintf("%.2f", pnum$dia_coef),
                sprintf("%.2f", pnum$hyp_coef),
                sep = "\n")
pnum$p.val <- paste( pnum$bas_p.val, pnum$cha_p.val, pnum$dia_p.val, pnum$hyp_p.val,
                sep = "\n")
pnum$CI[grepl("NA", pnum$CI)] <- "" # Any NA to blank
pnum$Beta[grepl("NA", pnum$Beta)] <- "" # Any NA to blank
pnum$p.val[grepl("NA", pnum$p.val)] <- "" # Any NA to blank

#################### plot
tm <- forest_theme(base_size = 8,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#762a83", "#AF0040","#377eb8","#4daf4a"),
                   footnote_gp = gpar(col = "blue"),
                   legend_name = "CRP_Type",
                   legend_value = c("Cog_baseline", "Cog_change", "Diabetes",  "Hypertension" ),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"),
                   # Table cell padding, width 4 and heights 3
                   )

plot <- forest(pnum[,c(1,20,21,19,18)],
            est = list(pnum$bas_coef,
                       pnum$cha_coef,
                       pnum$dia_coef,
                       pnum$hyp_coef),
            lower = list(pnum$bas_lower,
                         pnum$cha_lower,
                         pnum$dia_lower,
                         pnum$hyp_lower), 
            upper = list(pnum$bas_upper,
                         pnum$cha_upper,
                         pnum$dia_upper,
                         pnum$hyp_upper),
            ci_column = 5,
            ref_line = 0, # 1
            theme = tm)

plot
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Fig S3B

``` r
wmrs <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240517_slp_dis_w.csv')

# Figure S3B
pnum <- wmrs
pnum <- pnum[order(pnum$pheno),]
row.names(pnum)<-1:nrow(pnum)

sub <- split(pnum,pnum$condition) 
bas <- sub[[1]] %>% dplyr::select(-c(condition)) # baseline
names(bas) <- paste0('bas_', names(bas))
cha <- sub[[2]] %>% dplyr::select(-c(condition)) # change
names(cha) <- paste0('cha_', names(cha))
dia<-sub[[3]] %>% dplyr::select(-c(condition)) # diab
names(dia) <- paste0('dia_', names(dia))
hyp<-sub[[4]] %>% dplyr::select(-c(condition)) # hyp
names(hyp) <- paste0('hyp_', names(hyp))
pnum<-cbind(bas, cha, dia, hyp) %>% dplyr::select(-c(cha_pheno, dia_pheno, hyp_pheno))
names(pnum)[1] <- 'Phenotype'

pnum <- rbind(pnum,data.frame(Phenotype=c('Adjusting for MRS_CRP'),
                           bas_coef=rep(NA,1), bas_p.val=rep(NA,1), bas_lower=rep(NA,1), 
                           bas_upper=rep(NA,1), cha_coef=rep(NA,1), cha_p.val=rep(NA,1),
                           cha_lower=rep(NA,1), cha_upper=rep(NA,1), dia_coef=rep(NA,1), 
                           dia_p.val=rep(NA,1), dia_lower=rep(NA,1), dia_upper=rep(NA,1),
                           hyp_coef=rep(NA,1), hyp_p.val=rep(NA,1), hyp_lower=rep(NA,1),
                           hyp_upper=rep(NA,1)))
pnum <- pnum[c(4,1:3),]
row.names(pnum) <- 1:nrow(pnum)
pnum$Phenotype[c(2:4)] <- c('AHI','Minimum SpO2','Mean SpO2')

pnum$Phenotype <- ifelse(is.na(pnum$bas_coef), 
                      pnum$Phenotype,
                      paste0("   ", pnum$Phenotype))
# NA to blank or NA will be transformed to carachter.
pnum$bas_coef <- round(pnum$bas_coef,3)
pnum$cha_coef <- round(pnum$cha_coef,3)
pnum$hyp_coef <- round(pnum$hyp_coef,3)
pnum$dia_coef <- round(pnum$dia_coef,3)
pnum$bas_p.val <- format(pnum$bas_p.val, digits=2, scientific=TRUE)
pnum$cha_p.val <- format(pnum$cha_p.val, digits=2, scientific=TRUE)
pnum$hyp_p.val <- format(pnum$hyp_p.val, digits=2, scientific=TRUE)
pnum$dia_p.val <- format(pnum$dia_p.val, digits=2, scientific=TRUE)

# Add two basank columns for CI
pnum$`95% CI` <- paste(rep(" ", 50), collapse = " ")
# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
pnum$bas_lower <- round(pnum$bas_lower,3)
pnum$cha_lower <- round(pnum$cha_lower,3)
pnum$hyp_lower <- round(pnum$hyp_lower,3)
pnum$dia_lower <- round(pnum$dia_lower,3)
pnum$bas_upper <- round(pnum$bas_upper,4)
pnum$cha_upper <- round(pnum$cha_upper,3)
pnum$hyp_upper <- round(pnum$hyp_upper,3)
pnum$dia_upper <- round(pnum$dia_upper,3)

pnum$CI <- paste(sprintf("(%.2f, %.2f)", pnum$bas_lower, pnum$bas_upper), 
                sprintf("(%.2f, %.2f)", pnum$cha_lower, pnum$cha_upper),
                sprintf("(%.2f, %.2f)", pnum$dia_lower, pnum$dia_upper),
                sprintf("(%.2f, %.2f)", pnum$hyp_lower, pnum$hyp_upper),
                sep = "\n")
pnum$Beta <- paste(sprintf("%.2f", pnum$bas_coef),
                sprintf("%.2f", pnum$cha_coef),
                sprintf("%.2f", pnum$dia_coef),
                sprintf("%.2f", pnum$hyp_coef),
                sep = "\n")
pnum$p.val <- paste(pnum$bas_p.val, pnum$cha_p.val, pnum$dia_p.val, pnum$hyp_p.val,
                sep = "\n")
pnum$CI[grepl("NA", pnum$CI)] <- "" # Any NA to blank
pnum$Beta[grepl("NA", pnum$Beta)] <- "" # Any NA to blank
pnum$p.val[grepl("NA", pnum$p.val)] <- "" # Any NA to blank

#################### plot
tm <- forest_theme(base_size = 8,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#762a83", "#AF0040","#377eb8","#4daf4a"),
                   footnote_gp = gpar(col = "blue"),
                   legend_name = "CRP_Type",
                   legend_value = c("Cog_baseline", "Cog_change", "Diabetes",  "Hypertension" ),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"),
                   # Table cell padding, width 4 and heights 3
                   )

plot <- forest(pnum[,c(1,20,21,19,18)],
            est = list(pnum$bas_coef,
                       pnum$cha_coef,
                       pnum$dia_coef,
                       pnum$hyp_coef),
            lower = list(pnum$bas_lower,
                         pnum$cha_lower,
                         pnum$dia_lower,
                         pnum$hyp_lower), 
            upper = list(pnum$bas_upper,
                         pnum$cha_upper,
                         pnum$dia_upper,
                         pnum$hyp_upper),
            ci_column = 5,
            ref_line = 0, # 1
            theme = tm)

plot
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Create Figure S4

## Data Preparation

``` r
woprs <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240513_crp_surveyPC.csv')
wo.num <- subset(woprs, pheno%in%c('lgCrp','SLPA54','SLPA91','SLPA92') & group %in% 'MRS_CRP')
wo.cat <- subset(woprs, pheno%in%c('DIABETES2_INDICATOR','HYPERTENSION','LongSlp') 
           & group%in%'MRS_CRP')
wo.num$mod<-'1' # without adjusting for PRS
wo.cat$mod<-'1'

wprs <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240524_wprs.csv')
w.num <- subset(wprs, pheno%in%c('lgCrp','SLPA54','SLPA91','SLPA92'))
w.cat <- subset(wprs, pheno%in%c('DIABETES2_INDICATOR','HYPERTENSION','LongSlp'))
w.num$mod <- '2' # adjusting for PRS
w.cat$mod <- '2'

FigS4A <- rbind(wo.num, w.num)
FigS4B <- rbind(wo.cat, w.cat)

FigS4B$coef <- exp(FigS4B$coef)
FigS4B$lower <- exp(FigS4B$lower)
FigS4B$upper <- exp(FigS4B$upper)
```

## Fig S4A

``` r
row.names(FigS4A) <- 1:nrow(FigS4A)
sub <- split(FigS4A, FigS4A$mod) 
m1 <- sub[[1]] %>% dplyr::select(-c(group,mod))
names(m1)<-paste0('m1_', names(m1))
m2 <- sub[[2]]%>% dplyr::select(-c(group,mod)) 
names(m2) <- paste0('m2_',names(m2))

FigS4A <- cbind(m1, m2) %>% dplyr::select(-c(m2_pheno))
names(FigS4A)[1] <- 'Phenotype'

FigS4A <- rbind(FigS4A, data.frame(Phenotype= c('Association with MRS-CRP'),
                           m1_coef=rep(NA,1), m1_p.val=rep(NA,1), m1_lower=rep(NA,1), m1_upper=rep(NA,1),
                           m2_coef=rep(NA,1), m2_p.val=rep(NA,1), m2_lower=rep(NA,1), m2_upper=rep(NA,1)))
FigS4A <- FigS4A[c(5,1:4),]
row.names(FigS4A) <- 1:nrow(FigS4A)
FigS4A$Phenotype[c(2:5)] <- c('log(CRP)', 'AHI','Minimum SpO2','Mean SpO2')
FigS4A$Phenotype <- ifelse(is.na(FigS4A$m1_coef), 
                      FigS4A$Phenotype,
                      paste0("   ", FigS4A$Phenotype))
# NA to blank or NA will be transformed to carachter.
FigS4A$m1_coef <- round(FigS4A$m1_coef,3)
FigS4A$m2_coef <- round(FigS4A$m2_coef,3)
FigS4A$m1_p.val <- format(FigS4A$m1_p.val, digits=2, scientific=TRUE)
FigS4A$m2_p.val <- format(FigS4A$m2_p.val, digits=2, scientific=TRUE)

# Add two blank columns for CI
FigS4A$`95% CI` <- paste(rep(" ", 50), collapse = " ")
# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
FigS4A$m1_lower <- round(FigS4A$m1_lower,3)
FigS4A$m2_lower <- round(FigS4A$m2_lower,3)
FigS4A$m1_upper <- round(FigS4A$m1_upper,4)
FigS4A$m2_upper <- round(FigS4A$m2_upper,3)

FigS4A$CI <- paste(sprintf("(%.2f, %.2f)", FigS4A$m1_lower, FigS4A$m1_upper), 
                sprintf("(%.2f, %.2f)", FigS4A$m2_lower, FigS4A$m2_upper), 
                sep = "\n")
FigS4A$Beta <- paste(sprintf("%.2f", FigS4A$m1_coef),
                sprintf("%.2f", FigS4A$m2_coef),
                sep = "\n")
FigS4A$p.val <- paste(FigS4A$m1_p.val, FigS4A$m2_p.val,
                sep = "\n")
FigS4A$CI[grepl("NA", FigS4A$CI)] <- "" # Any NA to blank
FigS4A$Beta[grepl("NA", FigS4A$Beta)] <- "" # Any NA to blank
FigS4A$p.val[grepl("NA", FigS4A$p.val)] <- "" # Any NA to blank

#################### plot
# Set-up theme
tm <- forest_theme(base_size = 8,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c( "#AF0040","#377eb8"),
                   footnote_gp = gpar(col = "blue"),
                   legend_name = "Model",
                   legend_value = c("Without adjusting for PRS-CRP", "Adjusting for PRS-CRP" ),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"),
                   # Table cell padding, width 4 and heights 3
                   )
#> refline_lty will be deprecated, use refline_gp instead.
names(FigS4A)[11]<-'95%CI'
plot <- forest(FigS4A[,c(1,12,13,11,10)], 
            est = list(FigS4A$m1_coef,
                       FigS4A$m2_coef),
            lower = list(FigS4A$m1_lower,
                         FigS4A$m2_lower),
            upper = list(FigS4A$m1_upper,
                         FigS4A$m2_upper),
            ci_column = 5,
            ref_line = 0, 
            theme = tm)
plot
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Fig S4B

``` r
row.names(FigS4B) <- 1:nrow(FigS4B)
sub <- split(FigS4B, FigS4B$mod) 
m1 <- sub[[1]] %>% dplyr::select(-c(group,mod))
names(m1) <- paste0('m1_', names(m1))
m2 <- sub[[2]]%>% dplyr::select(-c(group,mod))
names(m2) <- paste0('m2_', names(m2))
FigS4B <- cbind(m1,m2) %>% dplyr::select(-c(m2_pheno))
names(FigS4B)[1] <- 'Phenotype'

FigS4B <- rbind(FigS4B, data.frame(Phenotype=c('Association with MRS-CRP'),
                           m1_coef=rep(NA,1),m1_p.val=rep(NA,1), m1_lower=rep(NA,1),m1_upper=rep(NA,1),
                           m2_coef=rep(NA,1), m2_p.val=rep(NA,1),m2_lower=rep(NA,1),m2_upper=rep(NA,1) ))
FigS4B <- FigS4B[c(4,1:3),]
row.names(FigS4B) <- 1:nrow(FigS4B)
FigS4B$Phenotype[c(2,3,4)] <- c('Diabetes', 'Hypertension', 'Long Sleep')
FigS4B$Phenotype <- ifelse(is.na(FigS4B$m1_coef), 
                      FigS4B$Phenotype,
                      paste0("   ", FigS4B$Phenotype))
# NA to blank or NA will be transformed to carachter.
FigS4B$m1_coef<-round(FigS4B$m1_coef,3)
FigS4B$m2_coef<-round(FigS4B$m2_coef,3)
FigS4B$m1_p.val<-format(FigS4B$m1_p.val, scientific=TRUE, digits=2)
FigS4B$m2_p.val<-format(FigS4B$m2_p.val, digits=2, scientific=TRUE)

# Add two m1ank columns for CI
FigS4B$`95% CI` <- paste(rep(" ", 50), collapse = " ")
# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
FigS4B$m1_lower <- round(FigS4B$m1_lower,3)
FigS4B$m2_lower <- round(FigS4B$m2_lower,3)
FigS4B$m1_upper <- round(FigS4B$m1_upper,3)
FigS4B$m2_upper <- round(FigS4B$m2_upper,3)

FigS4B$CI <- paste(sprintf("(%.2f, %.2f)", FigS4B$m1_lower, FigS4B$m1_upper),
                sprintf("(%.2f, %.2f)", FigS4B$m2_lower, FigS4B$m2_upper),
                sep = "\n")
FigS4B$Beta <- paste(sprintf("%.2f", FigS4B$m1_coef),
                sprintf("%.2f", FigS4B$m2_coef),
                sep = "\n")
FigS4B$p.val <- paste(FigS4B$m1_p.val, FigS4B$m2_p.val,
                sep = "\n")

FigS4B$CI[grepl("NA", FigS4B$CI)] <- "" # Any NA to blank
FigS4B$Beta[grepl("NA", FigS4B$Beta)] <- "" # Any NA to blank
FigS4B$p.val[grepl("NA", FigS4B$p.val)]<- ""

#################### plot
names(FigS4B)[11] <- '95%CI'
names(FigS4B)[12] <- 'Odds Ratio'
plot <- forest(FigS4B[,c(1,12,13,11,10)], 
            est = list(FigS4B$m1_coef,
                       FigS4B$m2_coef),
            lower = list(FigS4B$m1_lower,
                         FigS4B$m2_lower),
            upper = list(FigS4B$m1_upper,
                         FigS4B$m2_upper),
            ci_column = 5,
            ref_line = 1, 
            theme = tm)
plot
```

![](Supplementary_Preparation_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# Create Supplementary Table S6

Creation of table S3-S4 please refer to PRS_selection.Rmd

``` r
Tab.S6 <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240524_MNR_OE.csv')
Tab.S6$CI <- paste0('(', round(exp(Tab.S6$lower),2), ', ', round(exp(Tab.S6$upper),2), ')')
Tab.S6 <- Tab.S6[,c(1,2,3,5,8)]
names(Tab.S6) <- c('Comparison', 'PRS type', 'Odds ratio', 'p value', '95% CI')
Tab.S6$Comparison <- rep(c('OSA with EDS VS no OSA', 'OSA without EDS VS no OSA', 
                           'All OSA VS no OSA'),3)
Tab.S6$`Odds ratio` <- round(exp(Tab.S6$`Odds ratio`),2)
Tab.S6$`p value` <- format(Tab.S6$`p value`, digits=1)
kable(Tab.S6, row.names = F)
```

| Comparison                | PRS type | Odds ratio | p value | 95% CI       |
|:--------------------------|:---------|-----------:|:--------|:-------------|
| OSA with EDS VS no OSA    | PRS-ty   |       1.04 | 0.61    | (0.89, 1.21) |
| OSA without EDS VS no OSA | PRS-ty   |       0.98 | 0.55    | (0.9, 1.06)  |
| All OSA VS no OSA         | PRS-ty   |       0.99 | 0.86    | (0.92, 1.07) |
| OSA with EDS VS no OSA    | PRS-EUR  |       0.88 | 0.05    | (0.78, 1)    |
| OSA without EDS VS no OSA | PRS-EUR  |       0.96 | 0.30    | (0.89, 1.04) |
| All OSA VS no OSA         | PRS-EUR  |       0.94 | 0.11    | (0.88, 1.01) |
| OSA with EDS VS no OSA    | PRS-wsum |       0.85 | 0.02    | (0.75, 0.97) |
| OSA without EDS VS no OSA | PRS-wsum |       0.96 | 0.25    | (0.89, 1.03) |
| All OSA VS no OSA         | PRS-wsum |       0.94 | 0.07    | (0.88, 1)    |

``` r
write.csv(Tab.S6, '~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table_S6.csv', row.names = FALSE)
```

# Create Supplementary Table S7-S9

## Read Data

``` r
ahi <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240514_Enet_AHI.csv')
minO2 <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240514_Enet_MinO2.csv')
diab <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240514_Enet_Diab.csv')
hyp <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Lasso_EWAS/20240514_Enet_hyper.csv')
tab2 <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table2.csv')
```

## Table S7

``` r
Tab.S7 <- merge(ahi, tab2[,c(1,4)], by='CpG', all.x=T)
Tab.S7[is.na(Tab.S7)] <- ''
Tab.S7 <- Tab.S7[order(Tab.S7$Phenotypes, decreasing = T),]
Tab.S7$coef_AHI <- round(Tab.S7$coef_AHI, 2)
Tab.S7$Phenotypes <- gsub("AHI", "", Tab.S7$Phenotypes)
Tab.S7$Phenotypes <- gsub("imum", " ", Tab.S7$Phenotypes)
names(Tab.S7)[2:5] <- c('Coefficient', 'CHR', 'Gene Name', 'Overlap') 
write.csv(Tab.S7, '~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table_S7.csv', row.names = FALSE)
kable(head(Tab.S7), row.names = FALSE)
```

| CpG        | Coefficient | CHR | Gene Name   | Overlap            |
|:-----------|------------:|----:|:------------|:-------------------|
| cg00574958 |       -3.70 |  11 | CPT1A;CPT1A | Min SpO2, Diabetes |
| cg08309687 |       -5.93 |  21 |             | Min SpO2, Diabetes |
| cg00816397 |       62.83 |   1 | PFDN2;NIT1  | Min SpO2           |
| cg04103088 |       -4.98 |   4 |             | Min SpO2           |
| cg09048665 |       -0.90 |  16 | WDR90       | Min SpO2           |
| cg10726559 |      -24.91 |  14 | MIR127;RTL1 | Min SpO2           |

## Table S8

``` r
Tab.S8 <- merge(minO2, tab2[,c(1,4)], by='CpG', all.x=T)
Tab.S8[is.na(Tab.S8)] <- ''
Tab.S8 <- Tab.S8[order(Tab.S8$Phenotypes, decreasing = T),]
Tab.S8$coef_MinSpO2 <- round(Tab.S8$coef_MinSpO2, 2)
Tab.S8$Phenotypes <- gsub("MinimumSpO2", "", Tab.S8$Phenotypes)
Tab.S8$Phenotypes <- gsub(",", "", Tab.S8$Phenotypes)
names(Tab.S8)[2:5] <- c('Coefficient', 'CHR', 'Gene Name', 'Overlap') 
write.csv(Tab.S8, '~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table_S8.csv', row.names = FALSE)
kable(head(Tab.S8), row.names = FALSE)
```

| CpG        | Coefficient | CHR | Gene Name   | Overlap      |
|:-----------|------------:|----:|:------------|:-------------|
| cg00574958 |        4.41 |  11 | CPT1A;CPT1A | AHI Diabetes |
| cg08309687 |        0.58 |  21 |             | AHI Diabetes |
| cg00816397 |      -61.31 |   1 | PFDN2;NIT1  | AHI          |
| cg04103088 |        0.02 |   4 |             | AHI          |
| cg09048665 |        4.56 |  16 | WDR90       | AHI          |
| cg10726559 |        5.64 |  14 | MIR127;RTL1 | AHI          |

## Table S9

``` r
Tab.diab <- merge(diab, tab2[,c(1,4)], by='CpG')
Tab.diab$Phenotypes <- gsub("Diabetes", "", Tab.diab$Phenotypes)
Tab.diab <- Tab.diab[order(Tab.diab$Phenotypes, decreasing = T),]
Tab.diab$coef_diab <- round(Tab.diab$coef_diab, 2)
Tab.diab$Trait <- 'Diabetes'
Tab.diab <- Tab.diab[,c(1,6,2:5)]
names(Tab.diab)[3:6] <- c('Coefficient', 'CHR', 'Gene Name', 'Overlap') 

Tab.hyp <- merge(hyp, tab2[,c(1,4)], by='CpG')
Tab.hyp$Phenotypes <- gsub("Hypertension", "", Tab.hyp$Phenotypes)
Tab.hyp <- Tab.hyp[order(Tab.hyp$Phenotypes, decreasing = T),]
Tab.hyp$coef_hyper <- round(Tab.hyp$coef_hyper, 2)
Tab.hyp$Trait <- 'Hypertension'
Tab.hyp <- Tab.hyp[,c(1,6,2:5)]
names(Tab.hyp)[3:6] <- c('Coefficient', 'CHR', 'Gene Name', 'Overlap') 

Tab.S9 <- rbind(Tab.diab, Tab.hyp)

write.csv(Tab.S9, '~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table_S9.csv', row.names = FALSE)
kable(tail(Tab.S9), row.names = FALSE)
```

| CpG        | Trait        | Coefficient | CHR | Gene Name   | Overlap      |
|:-----------|:-------------|------------:|----:|:------------|:-------------|
| cg18852857 | Diabetes     |        0.08 |  22 | ADRBK2      | AHI          |
| cg11607604 | Diabetes     |       -3.72 |  13 | FARP1;FARP1 | MinimumSpO2, |
| cg12450708 | Diabetes     |        0.05 |  10 |             | MinimumSpO2, |
| cg06690548 | Hypertension |       -0.79 |   4 | SLC7A11     | AHI          |
| cg17061862 | Hypertension |       -0.01 |  11 |             | AHI          |
| cg14476101 | Hypertension |       -0.31 |   1 | PHGDH       | MinimumSpO2, |
