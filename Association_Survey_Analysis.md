Association analysis using Survey regression in HCHS/SOL
================
Leslie (ZW)
2024-05-13

# Model 1

## Data preparation

``` r
# Read data
pheno <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Data/20240629_pheno.csv')

# Classify long and short sleep duration
pheno$cDur <- ifelse(pheno$SLPDUR>9, 'Long', ifelse(pheno$SLPDUR<6, 'Short', 'Norm'))
long <- subset(pheno, cDur%in%c('Long','Norm'))
long$cDur <- relevel(as.factor(long$cDur), ref = 'Norm')
short <- subset(pheno, cDur%in%c('Short','Norm'))
short$cDur <- relevel(as.factor(short$cDur), ref = 'Norm')
# Set reference level for binary variables
pheno$EDS <-relevel(as.factor(pheno$EDS), ref = 'Norm' )
pheno$Insomnia <-relevel(as.factor(pheno$Insomnia), ref = 'Norm' )
pheno$DIABETES2_INDICATOR <-relevel(as.factor(pheno$DIABETES2_INDICATOR), ref = 'Normal' )
pheno$HYPERTENSION <-relevel(as.factor(pheno$HYPERTENSION), ref = 'Normal' )

# Create survey objects
survey_obj <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=pheno)
survey_obj <- subset(survey_obj,  !is.na(lgCrp))
survey_long <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=long)
survey_long <- subset(survey_long,  !is.na(lgCrp))
survey_short <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=short)
survey_short <- subset(survey_short,  !is.na(lgCrp))
```

## Association analysis for MRS_CRP

``` r
tab <- data.frame(pheno=c('lgCrp', 'SLPA54', 'SLPA91', 'SLPA92', 'SLPA97', 'SLPDUR', 
                          'global_cog_score', 'global_cog_score_change', 'EDS','Insomnia',
                          'DIABETES2_INDICATOR', 'HYPERTENSION', 'LongSlp', 'ShortSlp'),
                  coef=rep(1,14), p.val=rep(1,14), lower=rep(1,14), upper=rep(1,14))

for (i in 1:14){
  if (i<9){
    form <- as.formula(paste0(tab$pheno[i], paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5'))) #Zmrs_HEnet
    mod <- svyglm(form, design = survey_obj, family = "gaussian")
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 9:12){
    form <- as.formula(paste0(tab$pheno[i], paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    mod <- svyglm(form, design = survey_obj, family = quasibinomial(link = "logit"))
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 13:14) {
    form <- as.formula(paste0('cDur', paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    if (i==13){
    mod <- svyglm(form, design = survey_long, family = quasibinomial(link = "logit"))
    } else {
      mod <- svyglm(form, design = survey_short, family = quasibinomial(link = "logit"))
    }
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
}
mrs <- tab %>% mutate(group='MRS_CRP')
```

## Association analysis for circulating CRP

``` r
tab <- data.frame(pheno=c('SLPA54', 'SLPA91', 'SLPA92', 'SLPA97', 'SLPDUR', 
                          'global_cog_score', 'global_cog_score_change', 'EDS','Insomnia',
                          'DIABETES2_INDICATOR', 'HYPERTENSION', 'LongSlp', 'ShortSlp'),
                  coef=rep(1,13), p.val=rep(1,13), lower=rep(1,13), upper=rep(1,13))

for (i in 1:13){
  if (i<8){
    form <- as.formula(paste0(tab$pheno[i], paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7'))) #Zmrs_HEnet
    mod <- svyglm(form, design = survey_obj, family = "gaussian")
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 8:11){
    form <- as.formula(paste0(tab$pheno[i], paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7')))
    mod <- svyglm(form, design = survey_obj, family = quasibinomial(link = "logit"))
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 12:13) {
    form <- as.formula(paste0('cDur', paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7')))
    if (i==12){
    mod <- svyglm(form, design = survey_long, family = quasibinomial(link = "logit"))
    } else {
      mod <- svyglm(form, design = survey_short, family = quasibinomial(link = "logit"))
    }
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
}
crp <- tab %>% mutate(group='Blood_CRP')
res <- rbind(mrs, crp)

#write.csv(res,'~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240704_MrsCrp_Svy.csv',row.names = F)
```

## Association analysis for PRS_CRP

``` r
# Create PRS_CRP as simple and weighted sum for HCHS/SOL
mcoef <- read.csv('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Draft/Tables and Figures/Table_S4.csv')
scsx <- read.delim('~/OneDrive - Beth Israel Lahey Health/2023_methCRP/CRP_PRS/constructed_PRSs/HCHS/20240119_CRP_HCHS_std_PRS_CSx_PRSs.txt')
scsx$wsum_all <- scale(scsx$PRS_AFR_std * mcoef$Estimate[1] + scsx$PRS_EUR_std * mcoef$Estimate[2] +
                         scsx$PRS_AMR_std * mcoef$Estimate[3]+ scsx$PRS_EAS_std * mcoef$Estimate[4] +
                         scsx$PRS_SAS_std * mcoef$Estimate[5])
scsx$wsum_hisp <- scale(scsx$PRS_AFR_std * mcoef$Estimate[6] + scsx$PRS_EUR_std * mcoef$Estimate[7] +
                         scsx$PRS_AMR_std * mcoef$Estimate[8] + scsx$PRS_EAS_std * mcoef$Estimate[9] +
                         scsx$PRS_SAS_std * mcoef$Estimate[10])
names(scsx)[1] <- 'SOL_ID'
#write.csv(scsx,'~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/20240523_SOL_PRS_CSx.csv',row.names = F)

# Prepare survey objects
names(scsx)
```

    ##  [1] "SOL_ID"      "PRS_AFR"     "PRS_AMR"     "PRS_EAS"     "PRS_EUR"    
    ##  [6] "PRS_SAS"     "PRS_AFR_std" "PRS_AMR_std" "PRS_EAS_std" "PRS_EUR_std"
    ## [11] "PRS_SAS_std" "wsum_all"    "wsum_hisp"

``` r
pdata <- merge(pheno, scsx[,c(1, 10, 12, 13)], by='SOL_ID') # 2217 samples
long <- subset(pdata, cDur%in%c('Long','Norm'))
long$cDur <- relevel(as.factor(long$cDur), ref = 'Norm')
short <- subset(pdata, cDur%in%c('Short','Norm'))
short$cDur <- relevel(as.factor(short$cDur), ref = 'Norm')

tab <- data.frame(pheno=c('lgCrp', 'SLPA54', 'SLPA91', 'SLPA92', 'SLPA97', 'SLPDUR', 
                          'global_cog_score', 'global_cog_score_change', 'EDS','Insomnia',
                          'DIABETES2_INDICATOR', 'HYPERTENSION', 'LongSlp', 'ShortSlp'),
                  coef=rep(1,14), p.val=rep(1,14), lower=rep(1,14), upper=rep(1,14))

survey_obj <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=pdata)
survey_obj <- subset(survey_obj,  !is.na(lgCrp))
survey_long <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=long)
survey_long <- subset(survey_long,  !is.na(lgCrp))
survey_short <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=short)
survey_short <- subset(survey_short,  !is.na(lgCrp))

# Run the analysis
for (i in 1:14){
  if (i<9){
    form <- as.formula(paste0(tab$pheno[i], paste('~ PRS_EUR_std + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + EV1 + EV2 + EV3 + EV4 + EV5'))) #Zmrs_HEnet
    mod <- svyglm(form, design = survey_obj, family = "gaussian")
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 9:12){
    form <- as.formula(paste0(tab$pheno[i], paste('~ PRS_EUR_std + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    mod <- svyglm(form, design = survey_obj, family = quasibinomial(link = "logit"))
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 13:14) {
    form <- as.formula(paste0('cDur', paste('~ PRS_EUR_std + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    if (i==13){
    mod <- svyglm(form, design = survey_long, family = quasibinomial(link = "logit"))
    } else {
      mod <- svyglm(form, design = survey_short, family = quasibinomial(link = "logit"))
    }
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
}
eur <- tab %>% mutate(group='PRS_EUR')

for (i in 1:14){
  if (i<9){
    form <- as.formula(paste0(tab$pheno[i], paste('~ wsum_all + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + EV1 + EV2 + EV3 + EV4 + EV5'))) #Zmrs_HEnet
    mod <- svyglm(form, design = survey_obj, family = "gaussian")
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 9:12){
    form <- as.formula(paste0(tab$pheno[i], paste('~ wsum_all + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    mod <- svyglm(form, design = survey_obj, family = quasibinomial(link = "logit"))
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 13:14) {
    form <- as.formula(paste0('cDur', paste('~ wsum_all + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    if (i==13){
    mod <- svyglm(form, design = survey_long, family = quasibinomial(link = "logit"))
    } else {
      mod <- svyglm(form, design = survey_short, family = quasibinomial(link = "logit"))
    }
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
}

# Combine results
wsum.all <- tab %>% mutate(group='PRS_wsum_all')

for (i in 1:14){
  if (i<9){
    form <- as.formula(paste0(tab$pheno[i], paste('~ wsum_hisp + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + EV1 + EV2 + EV3 + EV4 + EV5'))) #Zmrs_HEnet
    mod <- svyglm(form, design = survey_obj, family = "gaussian")
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 9:12){
    form <- as.formula(paste0(tab$pheno[i], paste('~ wsum_hisp + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    mod <- svyglm(form, design = survey_obj, family = quasibinomial(link = "logit"))
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 13:14) {
    form <- as.formula(paste0('cDur', paste('~ wsum_hisp + AGE + GENDER + BMI + CENTER + BKGRD1_C7+ EV1 + EV2 + EV3 + EV4 + EV5')))
    if (i==13){
      mod <- svyglm(form, design = survey_long, family = quasibinomial(link = "logit"))
    } else {
      mod <- svyglm(form, design = survey_short, family = quasibinomial(link = "logit"))
    }
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
}

# Combine results
wsum.hisp <- tab %>% mutate(group='PRS_wsum_hisp')
res <- rbind(eur, wsum.all, wsum.hisp)

#write.csv(res,'~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240704_PRS_CRP.csv',row.names = F)
```

# Model 2

## MRS - model 2

``` r
tab <- data.frame(pheno=c('lgCrp', 'SLPA54', 'SLPA91', 'SLPA92', 'SLPA97', 'SLPDUR', 
                          'global_cog_score', 'global_cog_score_change', 'EDS','Insomnia',
                          'DIABETES2_INDICATOR', 'HYPERTENSION', 'LongSlp', 'ShortSlp'),
                  coef=rep(1,14), p.val=rep(1,14), lower=rep(1,14), upper=rep(1,14))

for (i in 1:14){
  if (i<9){
    form <- as.formula(paste0(tab$pheno[i], paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + HYPERTENSION + EV1 + EV2 + EV3 + EV4 + EV5'))) 
    mod <- svyglm(form, design = survey_obj, family = "gaussian")
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 9:12){
    if (i<11){
      form <- as.formula(paste0(tab$pheno[i], paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + HYPERTENSION + EV1 + EV2 + EV3 + EV4 + EV5'))) 
    } else if (i==11){
      form <- as.formula(paste0(tab$pheno[i], paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + HYPERTENSION + EV1 + EV2 + EV3 + EV4 + EV5')))
    } else {
      form <- as.formula(paste0(tab$pheno[i], paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + EV1 + EV2 + EV3 + EV4 + EV5')))
    }
    mod <- svyglm(form, design = survey_obj, family = quasibinomial(link = "logit"))
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 13:14){
    form <- as.formula(paste0('cDur', paste('~ Zmrs_corrected + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + HYPERTENSION + EV1 + EV2 + EV3 + EV4 + EV5')))
    if (i==13){
    mod <- svyglm(form, design = survey_long, family = quasibinomial(link = "logit"))
    } else {
      mod <- svyglm(form, design = survey_short, family = quasibinomial(link = "logit"))
    }
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
}
mrs <- tab %>% mutate(group='MRS_CRP')
```

## CRP - model 2

``` r
tab <- data.frame(pheno=c('SLPA54', 'SLPA91', 'SLPA92', 'SLPA97', 'SLPDUR', 
                          'global_cog_score', 'global_cog_score_change', 'EDS','Insomnia',
                          'DIABETES2_INDICATOR', 'HYPERTENSION', 'LongSlp', 'ShortSlp'),
                  coef=rep(1,13), p.val=rep(1,13), lower=rep(1,13), upper=rep(1,13))

for (i in 1:13){
  if (i<8){
    form <- as.formula(paste0(tab$pheno[i], paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + HYPERTENSION')))
    mod <- svyglm(form, design = survey_obj, family = "gaussian")
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 8:11){
    if (i<10){
      form <- as.formula(paste0(tab$pheno[i], paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + HYPERTENSION + EV1 + EV2 + EV3 + EV4 + EV5'))) 
    } else if (i==10){
      form <- as.formula(paste0(tab$pheno[i], paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + HYPERTENSION + EV1 + EV2 + EV3 + EV4 + EV5')))
    } else {
      form <- as.formula(paste0(tab$pheno[i], paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + EV1 + EV2 + EV3 + EV4 + EV5')))
    }
    mod <- svyglm(form, design = survey_obj, family = quasibinomial(link = "logit"))
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
  if (i %in% 12:13) {
    form <- as.formula(paste0('cDur', paste('~ lgCrp + AGE + GENDER + BMI + CENTER + BKGRD1_C7 + DIABETES2_INDICATOR + HYPERTENSION')))
    if (i==12){
    mod <- svyglm(form, design = survey_long, family = quasibinomial(link = "logit"))
    } else {
      mod <- svyglm(form, design = survey_short, family = quasibinomial(link = "logit"))
    }
    tab$coef[i] <- summary(mod)$coefficients[2,1]
    tab$p.val[i] <- summary(mod)$coefficients[2,4]
    tab$lower[i] <- confint(mod)[2,1]
    tab$upper[i] <- confint(mod)[2,2]
  }
}
crp <- tab %>% mutate(group='Blood_CRP')
res <- rbind(mrs, crp)
#write.csv(res,'~/OneDrive - Beth Israel Lahey Health/2023_methCRP/Results/Survey regression/20240704_MRSCRP_mod2.csv',row.names = FALSE)
```
