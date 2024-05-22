Create table 1
================
Leslie (ZW)
2024-05-09

# Read in the data

``` r
pheno_SOL <- read_csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Data/20240312_pheSol.csv')
```

    ## Rows: 2696 Columns: 138
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (19): Row_names, SOL_ID, CENTER, GENDER, INSULIN_FAST, DIABETES2_INDICA...
    ## dbl (119): ID, HEIGHT, PSU_ID, WEIGHT_FINAL_NORM_OVERALL, AGE, AGG_PHYS, AGG...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Create Table 1

``` r
# Define CRP risk groups and create a variable to get overall summary for the data
pheno_SOL$CRP_group <- as.factor(ifelse(pheno_SOL$LABA91<1, 'Low (<1)', 
                                        ifelse(pheno_SOL$LABA91>3, 'High (>3)', 'Borderline (1-3)')))
pheno_SOL$CRP_group <- relevel(pheno_SOL$CRP_group, ref = 'Low (<1)' )
pheno_SOL$overall<-'Overall'

# Set labels to show in table without changing column names
pheno_SOL$LABA91 <- pheno_SOL$LABA91 %>%
  ff_label("CRP (mg/L)") 
pheno_SOL$GENDER <- pheno_SOL$GENDER %>%
  ff_label("Gender (%)") 
pheno_SOL$AGE <- pheno_SOL$AGE %>%
  ff_label("Age") 
pheno_SOL$HYPERTENSION <- pheno_SOL$HYPERTENSION %>%
  ff_label("Hypertension (%)") 
pheno_SOL$DIABETES2_INDICATOR <- pheno_SOL$DIABETES2_INDICATOR %>%
  ff_label("Diabetes II (%)") 
pheno_SOL$SLPDUR <- pheno_SOL$SLPDUR %>%
  ff_label("Sleep Duration") 
pheno_SOL$SLPA54 <- pheno_SOL$SLPA54 %>%
  ff_label("AHI") 
pheno_SOL$SLPA91 <- pheno_SOL$SLPA91 %>%
  ff_label("Minimum SpO2")  
pheno_SOL$SLPA92 <- pheno_SOL$SLPA92 %>%
  ff_label("Average SpO2") 
pheno_SOL$SLPA97 <- pheno_SOL$SLPA97 %>%
  ff_label("% Time SpO2 <90") 
pheno_SOL$global_cog_score <- pheno_SOL$global_cog_score %>%
  ff_label("Cognitive Baseline") 
pheno_SOL$global_cog_score_change <- pheno_SOL$global_cog_score_change %>%
  ff_label("Cognitive Change") 

# Part of Table 1 with categorical and continuous variables (mean)
Part1 <- c("AGE", "BMI", "GENDER", 'BKGRD1_C7', 'SLPDUR', "HYPERTENSION",
           "DIABETES2_INDICATOR", 'global_cog_score', 'global_cog_score_change')
Tab.P1.1 <- pheno_SOL %>% 
  summary_factorlist("CRP_group", Part1[1:7], na_include=FALSE,cont = "mean", digits = c(1, 1, 3, 1, 0)) %>%
  ff_percent_only() 
```

    ## Note: dependent includes missing data. These are dropped.

``` r
Tab.P1.2 <- pheno_SOL %>% 
  summary_factorlist("CRP_group", Part1[8:9], na_include=FALSE,cont = "mean", digits = c(2, 2, 3, 1, 0)) %>%
  ff_percent_only() 
```

    ## Note: dependent includes missing data. These are dropped.

``` r
Tab.P1 <- rbind(Tab.P1.1, Tab.P1.2)
# Keep only 1 level for variables with 2 levels
keep <- c(1:3,5:13,15,17,18) 
Tab.P1 <- Tab.P1[keep,]
Tab.P1.overall.1 <- pheno_SOL %>% summary_factorlist('overall', Part1[1:7], na_include=FALSE, 
                                                   cont = "mean", digits = c(1, 1, 3, 1, 0)) %>%
  ff_percent_only()
Tab.P1.overall.2 <- pheno_SOL %>% summary_factorlist('overall', Part1[8:9], na_include=FALSE, 
                                                   cont = "mean", digits = c(2, 2, 3, 1, 0)) %>%
  ff_percent_only()
Tab.P1.overall <- rbind(Tab.P1.overall.1, Tab.P1.overall.2)
Tab.P1.overall <- Tab.P1.overall[keep,]
Tab.P1 <- merge(Tab.P1, Tab.P1.overall, by=c('label','levels'))
# Rearrange the table
Tab.P1 <- Tab.P1[c(7,9,13,8,1:3,5,6,4,15,12,14,10,11), c(1,2,6,3:5)]
# Combine label and level to create a final variable
row.names(Tab.P1) <- 1:nrow(Tab.P1)
Tab.P1$label[4] <- ''
Tab.P1$levels[12:13] <- ''
Tab.P1$Variable[c(1,2,11,14,15)] <- paste0(Tab.P1$label[c(1,2,11,14,15)], ' (', Tab.P1$levels[c(1,2,11,14,15)], ')')
Tab.P1$Variable[3] <- paste0(Tab.P1$label[3], ' - ', Tab.P1$levels[3])
Tab.P1$Variable[4:10] <- paste0(Tab.P1$label[4:10], Tab.P1$levels[4:10])
Tab.P1$Variable[12:13] <- Tab.P1$label[12:13]
Tab.P1 <- Tab.P1[,c(7,3:6)]
kable(Tab.P1)
```

| Variable                       | Overall      | Low (\<1)    | Borderline (1-3) | High (\>3)   |
|:-------------------------------|:-------------|:-------------|:-----------------|:-------------|
| Age (Mean (SD))                | 56.6 (7.6)   | 56.5 (7.8)   | 56.4 (7.6)       | 56.7 (7.4)   |
| BMI (Mean (SD))                | 30.2 (5.7)   | 26.8 (3.9)   | 29.3 (4.7)       | 32.8 (6.0)   |
| Gender (%) - Female            | 65.7         | 53.0         | 62.0             | 75.7         |
| Central American               | 9.8          | 10.0         | 9.9              | 9.7          |
| Cuban                          | 15.8         | 14.0         | 14.2             | 18.3         |
| Domician                       | 10.4         | 10.0         | 10.4             | 10.6         |
| Mexican                        | 36.1         | 40.8         | 38.9             | 31.2         |
| Puerto Rican                   | 19.0         | 16.9         | 17.0             | 21.9         |
| South American                 | 6.9          | 6.7          | 7.3              | 6.7          |
| More than one/Other heritage   | 1.9          | 1.5          | 2.3              | 1.6          |
| Sleep Duration (Mean (SD))     | 7.8 (1.4)    | 7.7 (1.3)    | 7.8 (1.4)        | 7.8 (1.5)    |
| Diabetes II (%)                | 30.5         | 22.6         | 28.4             | 36.5         |
| Hypertension (%)               | 44.1         | 35.9         | 42.7             | 49.7         |
| Cognitive Baseline (Mean (SD)) | 0.08 (0.74)  | 0.07 (0.69)  | 0.10 (0.76)      | 0.06 (0.73)  |
| Cognitive Change (Mean (SD))   | -0.18 (0.54) | -0.15 (0.52) | -0.20 (0.53)     | -0.19 (0.55) |

``` r
# Part of Table 1 with continuous variables (median)
Part2 <- c('LABA91', "SLPA54", "SLPA91","SLPA92", 'SLPA97', 'WHIIRS', "ESS")
Tab.P2.1 <- pheno_SOL %>% 
  summary_factorlist("CRP_group", Part2[c(1:4,6,7)], na_include=FALSE, cont = "median") %>%
  ff_percent_only() 
```

    ## Note: dependent includes missing data. These are dropped.

``` r
Tab.P2.2 <- pheno_SOL %>% 
  summary_factorlist("CRP_group", Part2[5], na_include=FALSE, cont = "median", 
                     digits = c(2, 2, 3, 1, 0)) %>%
  ff_percent_only() 
```

    ## Note: dependent includes missing data. These are dropped.

``` r
Tab.P2 <- rbind(Tab.P2.1, Tab.P2.2)
Tab.P2.overall.1 <- pheno_SOL %>% summary_factorlist('overall', Part2[c(1:4,6,7)], 
                                                     na_include=FALSE, cont = "median") %>%
  ff_percent_only()
Tab.P2.overall.2 <- pheno_SOL %>% summary_factorlist('overall', Part2[5], digits = c(2, 2, 3, 1, 0),
                                                     na_include=FALSE, cont = "median") %>%
  ff_percent_only()
Tab.P2.overall <- rbind(Tab.P2.overall.1, Tab.P2.overall.2)
Tab.P2 <- merge(Tab.P2, Tab.P2.overall, by=c('label','levels'))
# Rearrange the table
Tab.P2 <- Tab.P2[c(4,2,6,3,1,5,7), c(1,2,6,3:5)]
# Combine label and level to create a final variable
row.names(Tab.P2) <- 1:nrow(Tab.P2)
Tab.P2$Variable <- paste0(Tab.P2$label, ' (', Tab.P2$levels, ')')
Tab.P2 <- Tab.P2[,c(7,3:6)]

# Combine part 1 and part 2
Tab.1 <- rbind(Tab.P1, Tab.P2)
group_name <- data.frame(Variable=c('Background (%)', 'Sleep traits', 'Comorbidities', 'Cognitive traits'),
                           Overall=rep(NA,4), Low=rep(NA,4), Borderline=rep(NA,4), High=rep(NA,4) )
names(group_name)[3:5] <- names(Tab.1)[3:5]
Tab.1 <- rbind(Tab.1, group_name)
Tab.1 <- Tab.1[c(1:3,16, 23,4:10, 24,17:22,11, 25,12,13, 26,14,15),]
Tab.1[is.na(Tab.1)] <- ''
Tab.1 <- cbind(Tab.1[,1], do.call(cbind,lapply(Tab.1[2:5], function(x) {gsub(' to', ',',x)})))
names(Tab.1)[1] <- '' 

write.csv(Tab.1, '~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table1.csv', row.names = FALSE)
```
