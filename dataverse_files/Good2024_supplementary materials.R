## Replication File 2 of 2
## Uses prepped PADD_Agreement Level.csv data; PADD_Delegate Level.csv data; PADD_Agreement Level_Lowest Position.csv data; and PADD_Agreement Level_Multiple Positions.csv data
## "Power Over Presence: 
## Women’s Representation in Comprehensive Peace Negotiations and Gender Provision Outcomes"
## Supplementary Material 
## Elizabeth Good, Northwestern University
## Created: 14/08/2022
## Updated: 01/05/2024

##Analyses performed in R version 2023.06.2+561

#Load libraries
library(tidymodels)
library(corrplot)
library(dplyr)
library(skimr)
library(viridis)
library(stargazer)
library(mitools)
library(mice)
library(miceadds)
library(sandwich)
library(texreg)
library(moderndive)
library(VIM)
library(gridExtra)
library(ggpubr)
library(sjPlot)
library(imputeTS)
library(car)
library(margins)


df <- read.csv("PADD_Agreement Level.csv")
ID <- read.csv("PADD_Delegate Level.csv")


##############################################################################
##############################################################################
## Analyses for Supplementary Material
##############################################################################
##############################################################################

#create binary variables
df$FemSig_Bin <- ifelse(df$FemSig_N > 0, 1, 0)
df$FemNeg_Bin <- ifelse(df$FemNeg_N > 0, 1, 0)
df$FemMed_Bin <- ifelse(df$FemMed_N > 0, 1, 0)
df$FemOb_Bin <- ifelse(df$FemOb_N > 0, 1, 0)

####
## SM - A.2a
####
g1 <- df %>% group_by(Reg) %>%  filter(FemDel_Bin==1) %>% count(FemDel_Bin) %>% select(n) %>% rename(tot_wom = n)
g2 <- df %>% count(Reg) %>% filter(Reg != "Cross-regional")
gf <- full_join(g1,g2)

gf$region <- row.names(gf)
gf_long <- gather(gf, key = var, value = value, tot_wom, n)
plot_regprop <- ggplot(gf_long, aes(x = Reg, y = value, fill = var)) +
  geom_bar(stat = 'identity', position = "dodge") +
  coord_flip() +
  scale_fill_grey(name = "Agreements", labels = c("Total", "Women present"), start = 0.5, end = 0.7) +
  ggtitle("Comprehensive Agreements by Region") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, color = "black", face = "bold"),
        axis.text.x = element_text(color = "black", size = 12),  
        axis.text.y = element_text(color = "black", size = 12),  
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        axis.title.x = element_text(size = 12)) + 
  xlab ("") + ylab("Agreement (#)")

print(plot_regprop)





####
## SM - A.2b
####
g1_sig <- df %>% group_by(Reg) %>%  filter(FemSig_Bin==1) %>% count(FemSig_Bin) %>% rename(tot_wom = n)
g2_sig <- df %>% count(Reg) %>% filter(Reg != "Cross-regional")
gf_sig <- full_join(g1_sig,g2_sig)

gf_sig$region <- row.names(gf_sig)
gf_long_sig <- gather(gf_sig, key = var, value = value, tot_wom, n)
plot_regprop_sig <- ggplot(gf_long_sig, aes(x = Reg, y = value, fill = var)) +
  geom_bar(stat = 'identity', position = "dodge") +
  coord_flip() +
  scale_fill_grey(name = "Agreements", labels = c("Total", "Women signatories present"), start = 0.5, end = 0.7) +
  ggtitle("Women Signatories by Region") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, color = "black", face = "bold"),
        axis.text.x = element_text(color = "black", size = 12), 
        axis.text.y = element_text(color = "black", size = 12), 
        axis.title = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12)) +
  xlab("") + ylab("Agreements (#)") 

print(plot_regprop_sig)





####
## SM - A.3a
####
#load data
df_low <- read.csv("PADD_Agreement Level_Lowest Positions.csv") #Dataset codes delegates who hold multiple positions by their lowest position of power.

#binay measure of all delegates in the lowest position of power
sig_b_glm_low <- glm(GeWom ~ FemSig_N > 0, data = df_low, family = 'binomial')
neg_b_glm_low <- glm(GeWom ~ FemNeg_N > 0, data = df_low, family = 'binomial')
med_b_glm_low <- glm(GeWom ~ FemMed_N > 0, data = df_low, family = 'binomial')
ob_b_glm_low <- glm(GeWom ~ FemOb_N > 0, data = df_low, family = 'binomial')
lg_b_glm_low <- glm(GeWom ~ FemLog_N > 0, data = df_low, family = 'binomial')

#visualization
stargazer(sig_b_glm_low, neg_b_glm_low, med_b_glm_low, ob_b_glm_low, type="text",
          covariate.labels = c("Women Signatories", "Women Negotiators", "Women Mediators", "Women Observers"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = "Provisions for Women")





####
## SM - A.3b
####
#percentage measurement of all delegates in their lowest position of power
sig_og_low <- lm(GeWom ~ FemSig_P, data = df_low)
neg_og_low <- lm(GeWom ~ FemNeg_P, data = df_low)
med_og_low <- lm(GeWom ~ FemMed_P, data = df_low)
ob_og_low <- lm(GeWom ~ FemOb_P, data = df_low)
log_og_low <- lm(GeWom ~ FemLog_P, data = df_low)
adv_og_low <- lm(GeWom ~ FemAdv_P, data = df_low)

#visualization
stargazer(sig_og_low, neg_og_low, med_og_low, ob_og_low, log_og_low, type="text",
          covariate.labels = c("Women Signatories", "Women Negotiators", "Women Mediators", "Women Observers", "Women Staff"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = "Provisions for Women",
          single.row = TRUE)





####
## SM - A.4a
####
#load Data
df_CMB <- read.csv("PADD_Agreement Level_Multiple Positions.csv") #Dataset codes delegates who hold multiple positions in all positions held (double counting 46 delegates across all cases.)

#binay measure of delegates - recorded in all positions held.
sig_b_glm_cmb <- glm(GeWom ~ FemSig_N > 0, data = df_CMB, family = 'binomial')
neg_b_glm_cmb <- glm(GeWom ~ FemNeg_N > 0, data = df_CMB, family = 'binomial')
med_b_glm_cmb <- glm(GeWom ~ FemMed_N > 0, data = df_CMB, family = 'binomial')
ob_b_glm_cmb <- glm(GeWom ~ FemOb_N > 0, data = df_CMB, family = 'binomial')

#visualization
stargazer(sig_b_glm_cmb, neg_b_glm_cmb, med_b_glm_cmb, ob_b_glm_cmb, type="text",
          covariate.labels = c("Women Signatories", "Women Negotiators", "Women Mediators", "Women Observers"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = "Provisions for Women")





####
## SM - A.4b
####
#percentage measurement of all delegates - recorded in all positions held.
sig_og_cmb <- lm(GeWom ~ FemSig_P, data = df_CMB)
neg_og_cmb <- lm(GeWom ~ FemNeg_P, data = df_CMB)
med_og_cmb <- lm(GeWom ~ FemMed_P, data = df_CMB)
ob_og_cmb <- lm(GeWom ~ FemOb_P, data = df_CMB)
log_og_cmb <- lm(GeWom ~ FemLog_P, data = df_CMB)
adv_og_cmb <- lm(GeWom ~ FemAdv_P, data = df_CMB)

#visualization
stargazer(sig_og_cmb, neg_og_cmb, med_og_cmb, ob_og_cmb, type="text",
          covariate.labels = c("Women Signatories (%)", "Women Negotiators (%)", "Women Mediators (%)", "Women Observers (%)"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = "Provisions for Women",
          single.row = TRUE)





####
## SM - A.6a
####

#Correlation Matrices - Gender Equality 
# select variables you want in the matrix
just_codes_ge <- df %>% select("GGGI", "GII", "GDI", "WomParl", "SEP_Fem", "TeenPreg", "WomParl")

# create matrix
data_cor_ge <- cor(just_codes_ge, use = "complete.obs")

#visualize
corrplot(data_cor_ge,            # corrplot graphic without diagonal
         diag = FALSE, 
         type = "lower",
         method = "square", 
         #addCoef.col = "black", 
         tl.col = "black",
         tl.cex = 1.1
)

just_codes_ge_check <- df %>% select("GDI", "WomParl", "SEP_Fem", "TeenPreg", "WomParl")

# create matrix
data_cor_ge_check <- cor(just_codes_ge_check, use = "complete.obs")

#visualize
corrplot(data_cor_ge_check,            # corrplot graphic without diagonal
         diag = FALSE, 
         type = "lower",
         method = "square", 
         #addCoef.col = "black", 
         tl.col = "black",
         tl.cex = 1.1
)

##Correlation Matrices - international involvement
just_codes_II <- df %>% select("UNSCR", "Press_UNSC", "NAP", "ImUN", "ImOth", "ImPK", "NYT_p")

# create matrix
data_cor_II <- cor(just_codes_II, use = "complete.obs")

#visualize
corrplot(data_cor_II,            # corrplot graphic without diagonal
         diag = FALSE, 
         type = "lower",
         method = "square", 
         #addCoef.col = "black", 
         tl.col = "black",
         tl.cex = 1.1
)





####
## SM - A.6b
####

###Converting svac into numeric###
#state Department
df$state_prev_avg <- as.numeric(as.character(df$state_prev_avg))

#amnesty international
df$ai_prev_avg <- as.numeric(as.character(df$ai_prev_avg))

#human rights watch
df$hrw_prev_avg <- as.numeric(as.character(df$hrw_prev_avg))

#female_combatants_exs
df$female_combatants_exs <- as.numeric(as.character(df$female_combatants_exs))


#calculate the VIF for each predictor variable in the model

###model 1 - international involvement
original_II_VIF <- lm(GeWom ~ FemDel_P + UNSCR + Press_UNSC + ImUN + ImOth + NAP, data = df)
vif(original_II_VIF)

####Model 2 - gender inequality
original_GI_VIF <- lm(GeWom ~ FemDel_P + GDI + GGGI + GII + SEP_Fem + TeenPreg, data = df)
vif(original_GI_VIF)

####Model 3 - gender inequality, dropping variables for collinearity
original_GI_VIF_M3 <- lm(GeWom ~ FemDel_P + GDI + SEP_Fem + TeenPreg, data = df)
vif(original_GI_VIF_M3)

###Model 4 - women in politics
original_WomPol_VIF <- lm(GeWom ~ FemDel_P + PolInt_Cmb + WomParl, data = df)
vif(original_WomPol_VIF)

###Model 5 - women in conflict
original_WW_VIF <- lm(GeWom ~ FemDel_P + state_prev_avg + female_combatants_exs, data = df)
vif(original_WW_VIF)

###Model 6 - public awareness
original_PA_VIF <- lm(GeWom ~ FemDel_P + NYT_p + NYT_p_past, data = df)
vif(original_PA_VIF)

###Model 7 - public perception
original_PP_VIF <- lm(GeWom ~ FemDel_P + JobEql_Cmb + LeadPol_Cmb, data = df)
vif(original_PP_VIF)





####
## SM - A.7
####

##create dataframe with relevant variables for imputation
df_imp <- df[, 1:63]

###Removing agreement that negatively reference women (Adadda Agreement, Ethiopia Somalia Ogaden Puntland, 2007)
exclude_row <- 91

# Create a new dataframe excluding rows where the specified variable has the excluded value
df_ex <- df_imp[-exclude_row, ]

###duplicate original data###
df_original <- df_ex

###IMPUTATION###
init = mice(df_ex, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#collinearity check
mice:::find.collinear(df_ex)

#Avoid imputing variables based on relevance, missingness, and collinearity
predM[, c("FemDel_P", "AgtId", "Con", "Dat", "Stage", "StageSub", "FemNeg_P", "FemMed_P", "Log_N", "FemLog_N", "FemLog_P", "Adv_N", "FemAdv_N", "FemAdv_P", "WomCom_N", "WomCom_P", "FemWomCom_N", "FemWomCom_P", "Unkn_N", "FemUnkn_N", "FemUnkn_P", "WomGrpRep_N", "WomGrpRep_P", "FemWomGrp_P")] = 0

meth[c("FemDel_P", "AgtId", "Con", "Dat", "Stage", "StageSub", "FemNeg_P", "FemMed_P", "Log_N", "FemLog_N", "FemLog_P", "Adv_N", "FemAdv_N", "FemAdv_P", "WomCom_N", "WomCom_P", "FemWomCom_N", "FemWomCom_P", "Unkn_N", "FemUnkn_N", "FemUnkn_P", "WomGrpRep_N", "WomGrpRep_P", "FemWomGrp_P")] = ""

#Run multiple (m=5) imputation
set.seed(103)
imputed <- mice(df_ex, method=meth, predictorMatrix=predM, m=5)

###model 1 - non-imputed data, no controls
original <- lm(GeWom ~ FemDel_P, data = df_original)

###model 2 - imputed, international involvement 
modelFit_II <- with(imputed, lm(GeWom ~ FemDel_P + UNSCR + ImUN + ImOth + NAP))

#Visualizing pooled data with original - international involvement 
datlist <- miceadds::mids2datlist(imputed)
model_pooled_II <- with(datlist, lm(GeWom ~ FemDel_P + UNSCR +Press_UNSC + ImUN + ImOth + NAP))
results <- pool(model_pooled_II)

bs_II <- lapply(model_pooled_II, coef)
vs_II <- lapply(model_pooled_II, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_II, vs_II))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_II <- summary(pool_mi(bs_II, vs_II))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_II), 
    coef = model1_pool_II$results, 
    se = model1_pool_II$se, 
    pvalues = model1_pool_II$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_II <- extract.df(model_pooled_II) 

###Model 3 - gender inequality
#Imputed - pooling 
modelFit_GI <- with(imputed, lm(GeWom ~ FemDel_P + GDI + GGGI + GII + SEP_Fem + TeenPreg))

#visualizing gender inequality - pooled data
datlist <- miceadds::mids2datlist(imputed)
model_pooled_GI <- with(datlist, lm(GeWom ~ FemDel_P + GDI + GGGI + GII + SEP_Fem + TeenPreg))
results <- pool(model_pooled_GI)

bs_GI <- lapply(model_pooled_GI, coef)
vs_GI <- lapply(model_pooled_GI, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_GI, vs_GI))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_GI <- summary(pool_mi(bs_GI, vs_GI))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_GI), 
    coef = model1_pool_GI$results, 
    se = model1_pool_GI$se, 
    pvalues = model1_pool_GI$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_GI <- extract.df(model_pooled_GI)

###Model 4 - women in politics
#Imputed - pooling
modelFit_WomPol <- with(imputed,lm(GeWom ~ FemDel_P + PolInt_Cmb + WomParl))

#visualizing women's interest in politics - pooled data
datlist <- miceadds::mids2datlist(imputed)
model_pooled_WP <- with(datlist, lm(GeWom ~ FemDel_P + PolInt_Cmb + WomParl))
results <- pool(model_pooled_WP)

bs_WP <- lapply(model_pooled_WP, coef)
vs_WP <- lapply(model_pooled_WP, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_WP, vs_WP))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_WP <- summary(pool_mi(bs_WP, vs_WP))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_WP), 
    coef = model1_pool_WP$results, 
    se = model1_pool_WP$se, 
    pvalues = model1_pool_WP$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_WP <- extract.df(model_pooled_WP)

###Model 5 - women in war
datlist <- miceadds::mids2datlist(imputed)
model_pooled_WW <- with(datlist, lm(GeWom ~ FemDel_P + state_prev_avg + female_combatants_exs))
results <- pool(model_pooled_WW)

bs_WW <- lapply(model_pooled_WW, coef)
vs_WW <- lapply(model_pooled_WW, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_WW, vs_WW))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_WW <- summary(pool_mi(bs_WW, vs_WW))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_WW), 
    coef = model1_pool_WW$results, 
    se = model1_pool_WW$se, 
    pvalues = model1_pool_WW$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_WW <- extract.df(model_pooled_WW)

###Model 6
datlist <- miceadds::mids2datlist(imputed)
model_pooled_KS <- with(datlist, lm(GeWom ~ FemDel_P + GDI + GGGI + GII + SEP_Fem + TeenPreg + WomParl + NYT_p + UNSCR + Press_UNSC + ImUN + ImOth + NAP + PolInt_Cmb + JobEql_Cmb + LeadPol_Cmb + state_prev_avg + female_combatants_exs))
results <- pool(model_pooled_KS)

bs_KS <- lapply(model_pooled_KS, coef)
vs_KS <- lapply(model_pooled_KS, FUN = function(x){vcovCL(x)}) 
summary(pool_mi(bs_KS, vs_KS))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_KS <- summary(pool_mi(bs_KS, vs_KS))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_KS), 
    coef = model1_pool_KS$results, 
    se = model1_pool_KS$se, 
    pvalues = model1_pool_KS$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_KS <- extract.df(model_pooled_KS)


###Visualizing Table A.7
screenreg(list(original, output_pool_II, output_pool_GI, output_pool_WP, output_pool_WW, output_pool_KS), 
          custom.coef.names = c('(Intercept)', 'Women Delegates (%)', 'UNSC Resolutions', 'UNSC Press', 'UN Signatory', 'Third Party Signatory', 'National Action Plan', 'Gender Development Index', 'Global Gender Gap Index', 'Gender Inequality Index', 'Secondary Education for Women', 'Teen Pregnancy', 'Womens Political Interest', 'Women in Parliament', 'Sexual Violence in Conflict', 'Women Active Combatants', 'New York Times Publications', 'Perception of Employment for Women', 'Perception of Women in Leadership'), custom.model.names = c('Original(1)', 'Imputed(2)', 'Imputed(3)', 'Imputed(4)', 'Imputed(5)', 'Imputed(6)'),
          digits = 3)





####
## SM - A.8
####

#visualize imputed data - Gender inequality index
imp1 <- na_interpolation(df$GII)
impmod1 <- ggplot_na_imputations(df$GII, imp1) +
  labs(
    title = "Imputed Values: Gender Inequality Index",
    subtitle = "Visualization of missing value replacements",
    caption = "Data: United Nations Development Programme"
  )+
  xlab("Peace Agreement")+
  ylab("GII")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(
          hjust = 0.5, size = 15, colour = "black",
          face = "bold"),
        plot.subtitle = element_text(
          hjust = 0.5, size = 10, colour = "black",
          face = "bold"
        ))
impmod1

#visualize imputed data - secondary education enrollment 
imp2 <- na_interpolation(df$SEP_Fem)
impmod2 <- ggplot_na_imputations(df$SEP_Fem, imp2) +
  labs(
    title = "Imputed Values: Secondary Education Enrollment for Girls and Women",
    subtitle = "Visualization of missing value replacements",
    caption = "Data: World Bank"
  )+
  xlab("Peace Agreement")+
  ylab("Enrollment (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(
          hjust = 0.5, size = 12, colour = "black",
          face = "bold"),
        plot.subtitle = element_text(
          hjust = 0.5, size = 10, colour = "black",
          face = "bold",
        ))
impmod2





####
## SM - A.9a
####
#create "critical mass" 10% variable
df$CC_10 <- ifelse(df$FemDel_P > 0.1, 1, 0)

#GLM
CC_10_glm <- glm(GeWom ~ FemDel_Bin + FemDel_P, data = df, family = 'binomial')

#Visualization
stargazer(CC_10_glm, type="text",
          covariate.labels = c("Women Delegates (Binary)", "Women Delegates (Percent)"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = "Provisions for Women")





####
## SM - A.9b
####
#create "critical mass" 10% variable
df$FemSig_CC <- ifelse(df$FemSig_P > 0.1, 1, 0)
df$FemNeg_CC <- ifelse(df$FemNeg_P > 0.1, 1, 0)
df$FemMed_CC <- ifelse(df$FemMed_P > 0.1, 1, 0)
df$FemOb_CC <- ifelse(df$FemOb_P > 0.1, 1, 0)

#Original - position - treats missing data as NA given percentage (0/0)
sig_CC <- lm(GeWom ~ FemSig_CC, data = df)
neg_CC <- lm(GeWom ~ FemNeg_CC, data = df)
med_CC <- lm(GeWom ~ FemMed_CC, data = df)
ob_CC <- lm(GeWom ~ FemOb_CC, data = df)

#visualization
stargazer(sig_CC, neg_CC, med_CC, ob_CC, type="text",
          covariate.labels = c("Women Signatories", "Women Negotiators", "Women Mediators", "Women Observers"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = "Provisions for Women")





####
## SM - A.10
####
#proportional positions held by women using delegate level data 

#Changing character data 
Pos <- c(WomCom = "Women's Committee", Unknown="Unknown", Sig = "Signatory", Ob = "Observer", Neg = "Negotiator", Med = "Mediator", Log = "Logistical Staff", Adv = "Advisor")

ID$Pos_Smp <- as.character(Pos[ID$Pos_Smp])

#Graph 
plot_PosWomProp <- ID %>%
  filter(!is.na(FemTrack1)) %>%
  filter(!is.na(Pos_Smp)) %>% 
  count(Pos_Smp, FemTrack1) %>%       
  group_by(Pos_Smp) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Pos_Smp, pct, fill=as.factor(FemTrack1)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.0f", pct),"%"), ),
            position=position_stack(vjust=0.6), color="black") +
  coord_flip() + 
  scale_fill_grey(name = "Delegates", labels = c("Men", "Women"), start = 0.7, end = 0.9) +
  ggtitle("Track 1 Positions: Proportional by Gender") + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
          axis.title.x = element_text(color = "black", size = 12),
          axis.title.y = element_text(color = "black", size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          legend.text = element_text(size = 12)) + 
  xlab("Position") +ylab("Delegates (%)")

print(plot_PosWomProp)





####
## SM - A.11
####

#Create non-signatory binary variable 
df$FemNonSig_Bin <- ifelse((df$FemNeg_N >0 | df$FemMed_N > 0 | df$FemOb_N > 0) & df$FemSig_N == 0, 1, 0)

#Binary approach to women's role - signatory vs. other vs. total
sig_b_glm <- glm(GeWom ~ FemSig_Bin, data = df, family = 'binomial')
nonsig_b_glm <- glm(GeWom ~ FemNonSig_Bin, data = df, family = 'binomial')
FemDel_Bin_glm <- glm(GeWom ~ FemDel_Bin, data = df, family = 'binomial')


stargazer(FemDel_Bin_glm, sig_b_glm, nonsig_b_glm, type="text",
          covariate.labels = c("All Women Delegates", "Women Signatories", "Women Non-Signatories"),
          dep.var.caption = "Dependent Variable",
          dep.var.labels = "Provisions for Women")

#Interpreting GLMs using marginal effects 
#Women Delegates
margins(FemDel_Bin_glm)
summary(margins(FemDel_Bin_glm))

#Women signatories 
margins(sig_b_glm)
summary(margins(sig_b_glm))

#Women non-signatories 
margins(nonsig_b_glm)
summary(margins(nonsig_b_glm))





####
## SM - A.12
####

df_imp <- df[, 1:63]

#Run multiple (m=5) imputation
set.seed(103)
imputed_long <- mice(df_imp, method=meth, predictorMatrix=predM, m=5)

###model 1 - non-imputed data, no controls
original <- lm(GeWom ~ FemDel_P, data = df)

###model 2 - imputed, international involvement 
modelFit_II <- with(imputed_long, lm(GeWom ~ FemDel_P + UNSCR + ImUN + ImOth + NAP))

#Visualizing pooled data with original - international involvement 
datlist <- miceadds::mids2datlist(imputed_long)
model_pooled_II <- with(datlist, lm(GeWom ~ FemDel_P + UNSCR +Press_UNSC + ImUN + ImOth + NAP))
results <- pool(model_pooled_II)

bs_II <- lapply(model_pooled_II, coef)
vs_II <- lapply(model_pooled_II, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_II, vs_II))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_II <- summary(pool_mi(bs_II, vs_II))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_II), 
    coef = model1_pool_II$results, 
    se = model1_pool_II$se, 
    pvalues = model1_pool_II$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_II <- extract.df(model_pooled_II) 

###Model 3 - gender inequality
#Imputed - pooling 
modelFit_GI <- with(imputed_long, lm(GeWom ~ FemDel_P + GDI + SEP_Fem + TeenPreg))

#visualizing gender inequality - pooled data
datlist <- miceadds::mids2datlist(imputed_long)
model_pooled_GI <- with(datlist, lm(GeWom ~ FemDel_P + GDI + SEP_Fem + TeenPreg))
results <- pool(model_pooled_GI)

bs_GI <- lapply(model_pooled_GI, coef)
vs_GI <- lapply(model_pooled_GI, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_GI, vs_GI))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_GI <- summary(pool_mi(bs_GI, vs_GI))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_GI), 
    coef = model1_pool_GI$results, 
    se = model1_pool_GI$se, 
    pvalues = model1_pool_GI$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_GI <- extract.df(model_pooled_GI)

###Model 4 - women in politics
#Imputed - pooling
modelFit_WomPol <- with(imputed_long,lm(GeWom ~ FemDel_P + PolInt_Cmb + WomParl))

#visualizing women's interest in politics - pooled data
datlist <- miceadds::mids2datlist(imputed_long)
model_pooled_WP <- with(datlist, lm(GeWom ~ FemDel_P + PolInt_Cmb + WomParl))
results <- pool(model_pooled_WP)

bs_WP <- lapply(model_pooled_WP, coef)
vs_WP <- lapply(model_pooled_WP, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_WP, vs_WP))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_WP <- summary(pool_mi(bs_WP, vs_WP))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_WP), 
    coef = model1_pool_WP$results, 
    se = model1_pool_WP$se, 
    pvalues = model1_pool_WP$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_WP <- extract.df(model_pooled_WP)

###Model 5 - women in war
datlist <- miceadds::mids2datlist(imputed_long)
model_pooled_WW <- with(datlist, lm(GeWom ~ FemDel_P + state_prev_avg + female_combatants_exs))
results <- pool(model_pooled_WW)

bs_WW <- lapply(model_pooled_WW, coef)
vs_WW <- lapply(model_pooled_WW, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_WW, vs_WW))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_WW <- summary(pool_mi(bs_WW, vs_WW))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_WW), 
    coef = model1_pool_WW$results, 
    se = model1_pool_WW$se, 
    pvalues = model1_pool_WW$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_WW <- extract.df(model_pooled_WW)

###Model 6
datlist <- miceadds::mids2datlist(imputed_long)
model_pooled_KS <- with(datlist, lm(GeWom ~ FemDel_P + GDI + WomParl + SEP_Fem + TeenPreg + NYT_p + UNSCR + Press_UNSC + ImUN + ImOth + NAP + PolInt_Cmb + JobEql_Cmb + LeadPol_Cmb + state_prev_avg + female_combatants_exs))
results <- pool(model_pooled_KS)

bs_KS <- lapply(model_pooled_KS, coef)
vs_KS <- lapply(model_pooled_KS, FUN = function(x){vcovCL(x)}) 
summary(pool_mi(bs_KS, vs_KS))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_KS <- summary(pool_mi(bs_KS, vs_KS))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_KS), 
    coef = model1_pool_KS$results, 
    se = model1_pool_KS$se, 
    pvalues = model1_pool_KS$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_KS <- extract.df(model_pooled_KS)


###Visualizing Table A.12
screenreg(list(original, output_pool_II, output_pool_GI, output_pool_WP, output_pool_WW, output_pool_KS), 
          custom.coef.names = c('(Intercept)', 'Women Delegates (%)', 'UNSC Resolutions', 'UNSC Press', 'UN Signatory', 'Third Party Signatory', 'National Action Plan', 'Gender Development Index', 'Secondary Education for Women', 'Teen Pregnancy', 'Womens Political Interest', 'Women in Parliament', 'Sexual Violence in Conflict', 'Women Active Combatants', 'New York Times Publications', 'Perception of Employment for Women', 'Perception of Women in Leadership'), custom.model.names = c('Original(1)', 'Imputed(2)', 'Imputed(3)', 'Imputed(4)', 'Imputed(5)', 'Imputed(6)'),
          digits = 3)





####
## SM - A.13
####

GDI_lag <- lm(GeWom ~ FemDel_P + GDI_lag1 + GDI_lag5, data = df)

#Visualization
stargazer(GDI_lag, type="text",
          covariate.labels = c('Women Delegates (%)', "GDI lagged 1yr", "GDI lagged 5yr", 'Constant'),
          dep.var.labels = c('Provisions for Women'))





####
## SM - A.14a
####

#equality language
EqGen <- lm(EqGen ~ FemDel_Bin, data = df)

#protection of civilians language
ProtCiv <- lm(ProtCiv ~ FemDel_Bin, data = df)

#freedom of movement
CprFmov <- lm(CprFmov ~ FemDel_Bin, data = df)

#voting rights
CprVote <- lm(CprVote ~ FemDel_Bin, data = df)

#visualization
stargazer(EqGen, ProtCiv, CprFmov, CprVote, type="text",
          covariate.labels = c("Women Delegates (Percent)"),
          dep.var.labels = c("Equality", "Protection", "Movement", "Vote"))





####
## SM - A.14b
####

#education previsions
SerEdu <- lm(SerEdu ~ FemDel_Bin, data = df)

#property previsions
SerProp <- lm(SerProp ~ FemDel_Bin, data = df)

#work previsions
SerWork <- lm(SerWork ~ FemDel_Bin, data = df)

#health previsions
SerHeal <- lm(SerHeal ~ FemDel_Bin, data = df)

#water previsions
Wat <- lm(Wat ~ FemDel_Bin, data = df)

#visualization
stargazer(SerEdu, SerProp, SerWork, SerHeal, Wat, type="text",
          covariate.labels = c("Women Delegates (Percent)"),
          dep.var.labels = c("Education", "Property", "Work", "Health", "Water"))





####
## SM - A.14c
####

#criminal justice provisions
JusCr <- lm(JusCr ~ FemDel_Bin, data = df)

#provisions for victims
TjVic <- lm(TjVic ~ FemDel_Bin, data = df)

#reparations
TjRep <- lm(TjRep ~ FemDel_Bin, data = df)

#reconciliation
TjNR <- lm(TjNR ~ FemDel_Bin, data = df)

#visualization
stargazer(JusCr, TjVic, TjRep, TjNR, type="text",
          covariate.labels = c("Women Delegates (Binary)"),
          dep.var.labels = c("Justice", "Victims", "Reparations", "Reconciliation"))





####
## SM - A.14d
####

#provisions for children (binary measure of women delegates)
Child_B <- lm(GCh ~ FemDel_Bin, data = df)

#provisions for children (percentage measure of women delegates)
Child_P <- lm(GCh ~ FemDel_P, data = df)

#visualization
stargazer(Child_B, Child_P, type="text",
          covariate.labels = c("Women Delegates (Binary)", "Women Delegates (Percent)", "Constant"),
          dep.var.labels = c("Substantive Provisions for Children"))





####
## SM - A.14e
####

#inclusion of gender-neutral language
Neutral <- lm(GeMeNu ~ FemDel_Bin, data = df)

#provisions for men and boys
Men <- lm(GeMe ~ FemDel_Bin, data = df)

#provisions for families
Family <- lm(GeFa ~ FemDel_Bin, data = df)

#visualization
stargazer(Neutral, Men, Family, type = "text",
          covariate.labels = c("Women Delegates (Binary)"),
          dep.var.labels = c("Gender Neutral", "Men/Boys", "Family"))
