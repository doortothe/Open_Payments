library(dplyr)
library(ggplot2)
library(moments) #kurtosis, skewness
library(ppcor) #pcor
library(usmap) #plot_usmap
library(gridExtra) #grid.arrange

#######################################################
# SET UP DATA
#######################################################

load(file = "MPOS_OP_merge.RData") #MPOS_OP_merge
nrow(MPOS_OP_merge) #1,053,958
sum(MPOS_OP_merge$Number.of.Services) #2,868,851,638
sum(MPOS_OP_merge$Total.Medicare.Allowed.Amount) #125,083,622,447
sum(!is.na(MPOS_OP_merge$NPI_number)) #374,766

#limit to individual providers
MPOS_OP_merge <- dplyr::filter(MPOS_OP_merge, Entity.Type.of.the.Provider=="I")
nrow(MPOS_OP_merge) #991,362 individual providers
sum(MPOS_OP_merge$Number.of.Services) #2,327,445,152 services
sum(MPOS_OP_merge$Total.Medicare.Allowed.Amount) # $107,047,216,808 in Medicare allowed expenses
sum(MPOS_OP_merge$Total_Pay, na.rm = TRUE) # 1,548,394,237
sum(!is.na(MPOS_OP_merge$NPI_number)) #374,766 matched providers

#quantify drug and medical spending
sum(MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount, na.rm=TRUE) # 15,046,696,287
sum(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount, na.rm=TRUE) # 82,331,472,747
mean(MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount, na.rm=TRUE) # 17045.81
mean(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount, na.rm=TRUE) # 93270.1

#limit to the 50 states
data_political <- read.csv(file="Gallup_Political_Scale.csv", header=T)
data_political <- dplyr::select(data_political, 'State','Postal.Code','Conservative.advantage')
state_list <- as.character(data_political$Postal.Code)
state_names <- as.character(data_political$State)
MPOS_OP_merge <- MPOS_OP_merge %>% dplyr::filter(as.character(State.Code.of.the.Provider) %in% state_list)

#total/average medical and drug spending?
sum(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount, na.rm=TRUE) # $81,947,660,719 in medical allowed expenses
sum(MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount, na.rm=TRUE) # $15,018,604,421 in drug allowed expenses
mean(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount, na.rm=TRUE) # $93,673.85 avg medical allowed expenses
mean(MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount, na.rm=TRUE) # $17,167.67 avg drug allowed expenses

#compute log of pay and cost
MPOS_OP_merge$log_Total_Pay <- log(MPOS_OP_merge$Total_Pay)
MPOS_OP_merge$log_Total_Medical_Medicare <- log(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount)
MPOS_OP_merge$log_Total_Drug_Medicare <- log(MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount+1)

#compute number of beneficiaries and costs per beneficiary for each provider
MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount.perBeneficiary <- MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount/MPOS_OP_merge$Number.of.Medicare.Beneficiaries
MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount.perBeneficiary <- MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount/MPOS_OP_merge$Number.of.Medicare.Beneficiaries
MPOS_OP_merge$log_Total_Medical_Medicare_perBeneficiary <- log(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount.perBeneficiary)
MPOS_OP_merge$log_Total_Drug_Medicare_perBeneficiary <- log(MPOS_OP_merge$Total.Drug.Medicare.Allowed.Amount.perBeneficiary+1)
MPOS_OP_merge$log_NumBeneficiaries <- log(MPOS_OP_merge$Number.of.Medicare.Beneficiaries)

#######################################################
# EXPLORATORY ANALYSIS OF TOTAL PAY AND MEDICAL COSTS
#######################################################

#Medical Costs
Medical_Allowed <- na.omit(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount)
skewness(Medical_Allowed)
skewness(log(Medical_Allowed+1))
kurtosis(Medical_Allowed)
kurtosis(log(Medical_Allowed+1))
hist(Medical_Allowed)
hist(log(Medical_Allowed+1))

#Total_Pay
Total_Pay <- na.omit(MPOS_OP_merge$Total_Pay)
skewness(Total_Pay)
skewness(log(Total_Pay))
kurtosis(Total_Pay)
kurtosis(log(Total_Pay))
hist(Total_Pay)
hist(log(Total_Pay))

#######################################################
# PLOTS OF TOTAL PAY VS MEDICAL COSTS
#######################################################

### FIGURE 1 AND SUPPLEMENTARY FIGURE 1

(med_Total_Pay <- median(MPOS_OP_merge$Total_Pay, na.rm=TRUE)) #250.93
(Q99_Total_Pay <- quantile(MPOS_OP_merge$Total_Pay, probs=0.99, na.rm=TRUE)) #73996.66
(Q01_Total_Pay <- quantile(MPOS_OP_merge$Total_Pay, probs=0.01, na.rm=TRUE)) #10.83
(Q75_Total_Pay <- quantile(MPOS_OP_merge$Total_Pay, probs=0.75, na.rm=TRUE)) #939.855
(Q25_Total_Pay <- quantile(MPOS_OP_merge$Total_Pay, probs=0.25, na.rm=TRUE)) #77.02
(Q99_Medical <- quantile(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount, probs=0.99, na.rm=TRUE)) #1884.544
(Q01_Medical <- quantile(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount, probs=0.01, na.rm=TRUE)) #33.77855 
(Q99_Medical_per <- quantile(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount.perBeneficiary, probs=0.99, na.rm=TRUE)) #1884.544
(Q01_Medical_per <- quantile(MPOS_OP_merge$Total.Medical.Medicare.Allowed.Amount.perBeneficiary, probs=0.01, na.rm=TRUE)) #33.77855 

pdf('spaghetti_zoom.pdf')
ggplot(MPOS_OP_merge, aes(x=log_Total_Pay, y=log_Total_Medical_Medicare)) + 
  stat_smooth(geom='line', aes(group=State.Code.of.the.Provider), se=FALSE, method='loess', alpha=0.5) +
  geom_vline(xintercept=c(log(Q25_Total_Pay), log(Q75_Total_Pay)), color='red') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs') +
  coord_cartesian(xlim=c(3,9), ylim=c(10,12.5))
dev.off()

set.seed(23758923)
N <- nrow(MPOS_OP_merge)
samp <- sort(sample(1:N, round(N/10), replace = FALSE))
MPOS_OP_merge_samp <- MPOS_OP_merge[samp,]

#number of observations in spaghetti plots?
sum(!is.na(MPOS_OP_merge$log_Total_Medical_Medicare) & !is.na(MPOS_OP_merge$log_Total_Pay)) #322,109
sum(!is.na(MPOS_OP_merge$log_Total_Medical_Medicare_perBeneficiary) & !is.na(MPOS_OP_merge$log_Total_Pay))  #322109

pdf('spaghetti_points.pdf')
ggplot(MPOS_OP_merge, aes(x=log_Total_Pay, y=log_Total_Medical_Medicare)) + 
  geom_point(data=MPOS_OP_merge_samp, alpha=0.5, shape = ".") +
  stat_smooth(geom='line', aes(group=State.Code.of.the.Provider), se=FALSE, method='loess', alpha=0.5) +
  geom_vline(xintercept=c(log(Q25_Total_Pay), log(Q75_Total_Pay)), color='red') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs') +
  coord_cartesian(xlim=log(c(Q01_Total_Pay,Q99_Total_Pay)), ylim=log(c(Q01_Medical,Q99_Medical)))
dev.off()

pdf('spaghetti_points_perBeneficiary.pdf')
ggplot(MPOS_OP_merge, aes(x=log_Total_Pay, y=log_Total_Medical_Medicare_perBeneficiary)) + 
  geom_point(data=MPOS_OP_merge_samp, alpha=0.5, shape = ".") +
  stat_smooth(geom='line', aes(group=State.Code.of.the.Provider), se=FALSE, method='loess', alpha=0.5) +
  geom_vline(xintercept=c(log(Q25_Total_Pay), log(Q75_Total_Pay)), color='red') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs per Beneficiary') +
  coord_cartesian(xlim=log(c(Q01_Total_Pay,Q99_Total_Pay)), ylim=log(c(Q01_Medical_per,Q99_Medical_per)))
dev.off()

pdf('spaghetti_zoom_perBeneficiary.pdf')
ggplot(MPOS_OP_merge, aes(x=log_Total_Pay, y=log_Total_Medical_Medicare_perBeneficiary)) + 
  stat_smooth(geom='line', aes(group=State.Code.of.the.Provider), se=FALSE, method='loess', alpha=0.5) +
  geom_vline(xintercept=c(log(Q25_Total_Pay), log(Q75_Total_Pay)), color='red') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs per Beneficiary') +
  coord_cartesian(xlim=c(3, 9), ylim=c(5, 6.5))
dev.off()

##########################################################
# LIMIT REGRESSION MODEL TO MIDDLE 50% OF PAYMENTS (LINEAR PART)

#exclude non-matched providers
MPOS_OP_merge <- dplyr::filter(MPOS_OP_merge, !is.na(NPI_number)) 
#limit to middle 50% of payments
MPOS_OP_merge_middle <- MPOS_OP_merge[MPOS_OP_merge$Total_Pay > Q25_Total_Pay,]
MPOS_OP_merge_middle <- MPOS_OP_merge_middle[MPOS_OP_merge_middle$Total_Pay < Q75_Total_Pay,] 

##########################################################
# GET SLOPE ESTIMATE (DV = MEDICAL COSTS)

fit <- lm(log_Total_Medical_Medicare ~ log_Total_Pay + log_NumBeneficiaries + State.Code.of.the.Provider - 1, data=MPOS_OP_merge_middle)
summary(fit) #coefficient of log_Total_Pay = 0.13123
round(confint(fit, parm='log_Total_Pay'),3) #0.127  0.136
length(fit$residuals) #159,389
sum(!is.na(MPOS_OP_merge_middle$log_Total_Medical_Medicare)) #159389

#controlling for drug expenditures
fit2 <- lm(log_Total_Medical_Medicare ~ log_Total_Pay + log_Total_Drug_Medicare + log_NumBeneficiaries + State.Code.of.the.Provider - 1, data=MPOS_OP_merge_middle)
summary(fit2) #coefficient of log_Total_Pay = 0.091063
round(confint(fit2, parm='log_Total_Pay'),3) #0.086  0.096
length(fit2$residuals) #159,389

#correlation between drug and medical costs
cor(MPOS_OP_merge_middle$Total.Medical.Medicare.Allowed.Amount, MPOS_OP_merge_middle$Total.Drug.Medicare.Allowed.Amount, method='spearman', use='pairwise.complete.obs') 
#0.4066702

##########################################################
# GET SLOPE ESTIMATE (DV = DRUG COSTS)

MPOS_OP_merge_middle_drug <- filter(MPOS_OP_merge_middle, Total.Drug.Medicare.Allowed.Amount > 0)
nrow(MPOS_OP_merge_middle_drug) #61574
fit_drug <- lm(log_Total_Drug_Medicare ~ log_Total_Pay + log_NumBeneficiaries + State.Code.of.the.Provider - 1, data=MPOS_OP_merge_middle_drug)
summary(fit_drug) #coefficient of log_Total_Pay = 0.17920
round(confint(fit_drug, parm='log_Total_Pay'),3) #0.156  0.202


##########################################################
# MEDIAN MEDICAL AND DRUG COSTS

median(MPOS_OP_merge_middle$Total_Pay, na.rm=TRUE) #250.93
median(MPOS_OP_merge_middle$Total.Medical.Medicare.Allowed.Amount, na.rm=TRUE) #90,646.08

median(MPOS_OP_merge_middle_drug$Total_Pay, na.rm=TRUE) #317.99
median(MPOS_OP_merge_middle_drug$Total.Drug.Medicare.Allowed.Amount, na.rm=TRUE) #5862.11


##########################################################
# GET SLOPE ESTIMATE WITHIN EACH STATE (DV = TOTAL MEDICAL COSTS)

lmslope_by_state <- NULL
for (i in state_list) {
  print(paste0("On ", i))
  #filter by state, center x and y
  state_data <- filter(MPOS_OP_merge_middle, State.Code.of.the.Provider==i)
  fit <- lm(log_Total_Medical_Medicare ~ log_Total_Pay + log_NumBeneficiaries, data = state_data)
  a <- round(as.numeric(coef(fit)[2]),2)
  CI <- round(confint(fit)[2,], 2)
  lmslope_by_state <- rbind(lmslope_by_state, data.frame(State=i, LM=a, LM_LB=CI[1], LM_UB=CI[2]))
}


#######################################################
# QUANTIFY THE RELATIONSHIP BETWEEN PAY AND COSTS WITH SPEARMAN CORRELATION
#######################################################

#############################
# COMPUTE PARTIAL SPEARMAN CORRELATION

MPOS_OP_merge$State <- as.character(MPOS_OP_merge$State.Code.of.the.Provider)
MPOS_OP_merge <- left_join(MPOS_OP_merge, data_political, by = c("State" = "Postal.Code"))

pcor_by_state <- NULL
count_by_state <- NULL
for (i in state_list) {
  print(paste0("On ", i))
  state_data <- filter(MPOS_OP_merge, State==i)
  state_data <- dplyr::select(state_data, Total_Pay, Total.Medical.Medicare.Allowed.Amount, Number.of.Medicare.Beneficiaries)
  state_data <- na.omit(state_data)

  pcor_i <- pcor(state_data, method='spearman')$estimate[1,2]
  pcor_by_state <- c(pcor_by_state, pcor_i)
  count_by_state <- c(count_by_state, nrow(state_data))
}

pcor_df <- data.frame(State=state_list, pcor=pcor_by_state, sample_size=count_by_state)


#############################
# MAKE U.S. MAP PLOTS OF COR AND CONSERVATIVE ADVANTAGE

### FIGURE 3

data_political_merged <- right_join(pcor_df, data_political, by = c("State" = "Postal.Code"))

names(data_political_merged)[1] <- 'state'
p1 <- plot_usmap(data=data_political_merged, values="pcor") + 
  scale_fill_viridis_c(name='Pay-Cost P-Correlation') +
  theme(legend.position='right')
p2 <- plot_usmap(data=data_political_merged, values="Conservative.advantage") + 
  scale_fill_gradient2(name='Conservative Advantage', low='blue',high='red', mid='white',midpoint=mean(data_political_merged$Conservative.advantage)) +
  theme(legend.position='right')
pdf('maps.pdf', width=8, height=10)
grid.arrange(p1, p2, nrow=2)
dev.off()

#######################################################
# SAVE REGRESSION SLOPES AND PCORS WITHIN EACH STATE

#join with political leaning measures and Spearman correlation
tmp <- left_join(data_political_merged, lmslope_by_state, by=c('state'='State'))
tmp$LM_display <- paste0(tmp$LM, " (", tmp$LM_LB, ", ", tmp$LM_UB, ")")
tmp <- arrange(tmp, Conservative.advantage)
tmp$pcor <- round(tmp$pcor, 2)
dplyr::select(tmp, state, sample_size, LM_display, pcor, Conservative.advantage)

###################################

#Read in ACS economic measures
data_econ <- read.csv('ECON_SMALL.csv', stringsAsFactors=FALSE)
names_long <- data_econ[1,-1]
data_econ <- data_econ[-1,]
names_short <- c('PopulationOver16','UnemploymentRate','MedianHouseholdIncome',
                 'PopulationNonInst','PublicInsuranceCoveragePct','NoInsuranceCoveragePct',
                 'PovertyRate')
names(data_econ) <- c('StateName',names_short)
#Convert variable encoding to numeric
for(i in 2:ncol(data_econ)){ data_econ[,i] <- as.numeric(data_econ[,i]) }
data_econ <- left_join(data_econ, data.frame(State = state_list, StateName = state_names))


#Read in ACS social measures
data_social <- read.csv('SOCIAL_SMALL.csv', stringsAsFactors=FALSE)
names_long <- data_social[1,-1]
data_social <- data_social[-1,]
names_short <- c('PercentHighSchool','PercentBachelors')
names(data_social) <- c('StateName',names_short)
#Convert variable encoding to numeric
for(i in 2:ncol(data_social)){ data_social[,i] <- as.numeric(data_social[,i]) }
data_social <- left_join(data_social, data.frame(State = state_list, StateName = state_names))

#Merge with original vars
names(data_political_merged)[1] <- 'State'
data_political_merged <- left_join(data_political_merged, data_econ, by='State')
data_political_merged <- left_join(data_political_merged, data_social, by='State')
data_political_merged$logPopulationOver16 <- log(data_political_merged$PopulationOver16)

#######################################################
# CONTROL FOR STATE-LEVEL PROVIDER MARKET FACTORS IN POLITICAL MODEL 
#######################################################

data_Specialists <- read.csv('KFF specialist data Simple.csv', stringsAsFactors = FALSE)
data_Specialists$Pct.Specialist <- data_Specialists$Specialist.Physicians/data_Specialists$Total
data_political_merged_health <- left_join(data_political_merged, data_Specialists, by=c('StateName.x' = 'State'))

#######################################################
# CONTROL FOR STATE-LEVEL HEALTH FACTORS IN POLITICAL MODEL
#######################################################

### McKinsey Rankings
data_Health <- read.csv("USHealthNews.csv", stringsAsFactors = FALSE)
data_Health$grp <- 'Q3' #bottom
data_Health$grp[data_Health$RANK <= 33.3] <- 'Q2' #middle
data_Health$grp[data_Health$RANK <= 16.7] <- 'Q1' #top
data_Health$grp <- as.factor(data_Health$grp)
data_political_merged_health <- left_join(data_political_merged_health, data_Health, by=c('StateName.x' = 'STATE'))

#model without health rankings
fit_econ1 <- lm(scale(pcor) ~ scale(Conservative.advantage) + scale(log(PopulationOver16)) + scale(UnemploymentRate) + scale(MedianHouseholdIncome) + scale(PercentHighSchool) + scale(NoInsuranceCoveragePct) + scale(Pct.Specialist) + - 1, data=data_political_merged_health)
summary(fit_econ1)

#model with all variables (including health rankings)
fit_econ <- lm(scale(pcor) ~ scale(Conservative.advantage) + scale(log(PopulationOver16)) + scale(UnemploymentRate) + scale(MedianHouseholdIncome) + scale(PercentHighSchool) + scale(NoInsuranceCoveragePct) + scale(Pct.Specialist) +  grp - 1, data=data_political_merged_health)
summary(fit_econ)
anova(fit_econ1, fit_econ) #F-test on health ranking groups

var_names <- c('Conservative\nAdvantage','Population\nSize (log)','Unemployment\nRate','Median Household\nIncome','HS Graduation\nRate','Percent\nUninsured','Percent\nSpecialist Providers','McKinsey Index:\nTop','McKinsey Index:\nMiddle','McKinsey Index:\nBottom')
CI <- as.data.frame(confint(fit_econ, level=.99))
names(CI) <- c('LB','UB')
CI$estimate <- coefficients(fit_econ)
CI$var <- var_names
CI$var <- factor(CI$var, levels=var_names)

### FIGURE 3

pdf('CI_plot.pdf', width=12, height=5)
ggplot(CI, aes(x=var)) + geom_point(aes(y=estimate)) + geom_errorbar(aes(ymin=LB,ymax=UB)) + geom_hline(yintercept=0, color='red')
dev.off()