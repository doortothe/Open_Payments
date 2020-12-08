library(dplyr)
library(ggplot2)
library(moments) #kurtosis, skewness
library(ppcor) #pcor
library(usmap) #plot_usmap
library(gridExtra) #grid.arrange



MPOS_OP_Merge <- read.csv('MPOS_OP_Merge.csv', stringsAsFactors = FALSE)
nrow(MPOS_OP_Merge) #1001967
sum(MPOS_OP_Merge$Number_of_Services) #2,380,652,850
sum(MPOS_OP_Merge$total_medicare_allowed_amount) #321,033,230
sum(!is.na(MPOS_OP_Merge$National_Provider_Identifier)) #200,023 

# Limit to individual providers
MPOS_OP_Merge <- dplyr::filter(MPOS_OP_Merge, Entity_Type_of_the_Provider=='I')
nrow(MPOS_OP_Merge) #200023
sum(MPOS_OP_Merge$Number_of_Services) #704,649,984
sum(MPOS_OP_Merge$total_medicare_allowed_amount) #321,033,230
sum(!is.na(MPOS_OP_Merge$National_Provider_Identifier)) #200,023

# Quantity drug & medical spending
sum(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #21,061,364
sum(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #20,771,244
mean(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #109.6124
mean(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #103.8484

# Limit to 50 states
political_data <- read.csv(file='Gallup_Political_Scale.csv', header=T)
political_data <- dplyr::select(political_data, 'ï..State', 'Postal.Code', 'Conservative.advantage')
state_list <- as.character(political_data$Postal.Code)
state_names <- as.character(political_data$ï..State)
MPOS_OP_Merge <- MPOS_OP_Merge %>% dplyr::filter(as.character(State_Code_of_the_Provider) %in% state_list)

# total/average medical and drug spending
sum(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #20,937,423
sum(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #20,639,698
mean(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #109.5988
mean(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #103.8632

# compute log of pay and cost
MPOS_OP_Merge$log_total_pay <- log(MPOS_OP_Merge$Sum_Payments)
MPOS_OP_Merge$log_total_medical_medicare <- log(MPOS_OP_Merge$total_medicare_medical_allowed_amount)
MPOS_OP_Merge$log_total_drug_medicare <- log(MPOS_OP_Merge$total_medicare_drug_allowed_amount+1)

# Compute number of beneficiaries and costs per beneficiary for each provider
MPOS_OP_Merge$total_medicare_medical_per_beneficiary <- MPOS_OP_Merge$total_medicare_medical_allowed_amount/MPOS_OP_Merge$Number_of_Medicare_Beneficiaries
MPOS_OP_Merge$total_drug_medicare_per_beneficiary <- MPOS_OP_Merge$total_medicare_drug_allowed_amount/MPOS_OP_Merge$Number_of_Medicare_Beneficiaries
MPOS_OP_Merge$log_total_medical_medicare_per_beneficiary <- log(MPOS_OP_Merge$total_medicare_medical_per_beneficiary)
MPOS_OP_Merge$log_total_drug_medicare_per_beneficiary <- log(MPOS_OP_Merge$total_drug_medicare_per_beneficiary)
MPOS_OP_Merge$log_num_beneficiaries <- log(MPOS_OP_Merge$Number_of_Medicare_Beneficiaries)

#######################################################
# EDA
#######################################################

# Medical costs
medical_allowed <- na.omit(MPOS_OP_Merge$total_medicare_medical_allowed_amount)
skewness(medical_allowed) # 25.73317
skewness(log(medical_allowed+1)) #-0.7045393 (w/o +1) -0.8677353
kurtosis(medical_allowed) #1645.832
kurtosis(log(medical_allowed+1)) # 4.316608 (w/o +1) 4.771537
hist(medical_allowed)
hist(log(medical_allowed+1))

#######################################################
# Plots of total pay vs medical costs
#######################################################

# Figure 1 & supplementary figure 1
(med_total_pay <- median(MPOS_OP_Merge$Sum_Payments, na.rm=TRUE)) #207.395
(Q99_total_pay <- quantile(MPOS_OP_Merge$Sum_Payments, probs=0.99, na.rm=TRUE)) #55414.65 
(Q01_total_pay <- quantile(MPOS_OP_Merge$Sum_Payments, probs=0.01, na.rm=TRUE)) #10.7
(Q75_total_pay <- quantile(MPOS_OP_Merge$Sum_Payments, probs=0.75, na.rm=TRUE)) #729.1175
(Q25_total_pay <- quantile(MPOS_OP_Merge$Sum_Payments, probs=0.25, na.rm=TRUE)) #62.54
(Q99_medical <- quantile(MPOS_OP_Merge$total_medicare_medical_allowed_amount, probs=0.99, na.rm=TRUE)) #698.9083
(Q01_medical <- quantile(MPOS_OP_Merge$total_medicare_medical_allowed_amount, probs=0.01, na.rm=TRUE)) #3
(Q99_medical_per <- quantile(MPOS_OP_Merge$total_medicare_medical_per_beneficiary, probs=0.99, na.rm=TRUE)) #7.676912
(Q01_medical_per <- quantile(MPOS_OP_Merge$total_medicare_medical_per_beneficiary, probs=0.01, na.rm=TRUE)) #0.001327306 

pdf('spaghetti_zoom2.pdf')
ggplot(MPOS_OP_Merge, aes(x=log_total_pay, y=log_total_medical_medicare)) + 
  stat_smooth(geom='line', aes(group=State_Code_of_the_Provider), se=FALSE, method='loess', alpha=0.5) + 
  geom_vline(xintercept = c(log(Q25_total_pay), log(Q75_total_pay)), color='red') + 
  theme_bw() + theme(panel.grid=element_blank()) + 
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs') + 
  coord_cartesian(xlim = c(3,9), ylim = c(10, 12.5))
dev.off()

set.seed(23758923) #oddly specific number?
N <- nrow(MPOS_OP_Merge)
samp <- sort(sample(1:N, round(N/10), replace = FALSE))
MPOS_OP_Merge_samp <- MPOS_OP_Merge[samp, ]

# Number of observations in spaghetti plots?
sum(!is.na(MPOS_OP_Merge$log_total_medical_medicare) & !is.na(MPOS_OP_Merge$log_total_pay)) #198,720
sum(!is.na(MPOS_OP_Merge$log_total_medical_medicare_per_beneficiary) & !is.na(MPOS_OP_Merge$log_total_pay))

pdf('spaghetti_points2.pdf')
ggplot(MPOS_OP_Merge, aes(x=log_total_pay, y=log_total_medical_medicare)) + 
  geom_point(data=MPOS_OP_Merge_samp, alpha=0.5, shape='.') +
  stat_smooth(geom='line', aes(group=State_Code_of_the_Provider), se=FALSE, method = 'loess', alpha=0.5) +
  geom_vline(xintercept = c(log(Q25_total_pay), log(Q75_total_pay)), color='red') + 
  theme_bw() + theme(panel.grid = element_blank()) +
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs') + 
  coord_cartesian(xlim = log(c(Q01_total_pay, Q99_total_pay)), ylim = log(c(Q01_medical, Q99_medical)))
dev.off()

pdf('spaghetti_points_per_beneficiary2.pdf')
ggplot(MPOS_OP_Merge, aes(x=log_total_pay, y=log_total_medical_medicare_per_beneficiary)) + 
  geom_point(data = MPOS_OP_Merge_samp, alpha=0.5, shape='.') + 
  stat_smooth(geom = 'line', aes(group=State_Code_of_the_Provider), se=FALSE, method = 'loess', alpha=0.5) + 
  geom_vline(xintercept = c(log(Q25_total_pay), log(Q75_total_pay)), color='red') + 
  theme_bw() + theme(panel.grid = element_blank()) + 
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs') + 
  coord_cartesian(xlim = log(c(Q01_total_pay, Q99_total_pay)), ylim = log(c(Q01_medical_per, Q99_medical_per)))
dev.off()

##########################################################
# LIMIT REGRESSION MODEL TO MIDDLE 50% OF PAYMENTS (LINEAR PART)

#exclude non-mathced providers
MPOS_OP_Merge <- dplyr::filter(MPOS_OP_Merge, !is.na(National_Provider_Identifier))
# limit to 50% of payments
MPOS_OP_Merge_middle <- MPOS_OP_Merge[MPOS_OP_Merge$Sum_Payments > Q25_total_pay,]
MPOS_OP_Merge_middle <- MPOS_OP_Merge_middle[MPOS_OP_Merge_middle$Sum_Payments < Q75_total_pay,]

##########################################################
# GET SLOPE ESTIMATE (DV = MEDICAL COSTS)
fit <- lm(log_total_medical_medicare ~ log_total_pay + log_num_beneficiaries + State_Code_of_the_Provider - 1, data = MPOS_OP_Merge_middle)
summary(fit) #coefficent of log_total_pay = -0.022691
round(confint(fit, parm = 'log_total_pay'), 3) # -0.032, -0.013
length(fit$residuals) #99,363
sum(!is.na(MPOS_OP_Merge_middle$log_total_medical_medicare)) #99363

# Controlling for drug expenditures
fit2 <- lm(log_total_medical_medicare ~ log_total_pay + log_total_drug_medicare + log_num_beneficiaries + State_Code_of_the_Provider - 1, data = MPOS_OP_Merge_middle)
summary(fit2) #coefficient of log_total_pay = -0.018123
round(confint(fit2, parm = 'log_total_pay'), 3) # -0.028, -0.009
length(fit2$residuals) #95,432

#correlation between drug and medical costs
cor(MPOS_OP_Merge_middle$total_medicare_medical_allowed_amount, MPOS_OP_Merge_middle$total_medicare_drug_allowed_amount, method = 'spearman', use = 'pairwise.complete.obs')
#0.1918653

##########################################################
# GET SLOPE ESTIMATE (DV = DRUG COSTS)

MPOS_OP_Merge_middle_drug <- filter(MPOS_OP_Merge_middle, total_medicare_drug_allowed_amount > 0)
nrow(MPOS_OP_Merge_middle_drug) #95,432
fit_drug <- lm(log_total_drug_medicare ~ log_total_pay + log_num_beneficiaries + State_Code_of_the_Provider - 1, data = MPOS_OP_Merge_middle_drug)
summary(fit_drug) #coefficient of log_total_pay = -0.027310
round(confint(fit_drug, parm = 'log_total_pay'), 3) #-0.037, -0.018

##########################################################
# MEDIAN MEDICAL AND DRUG COSTS

median(MPOS_OP_Merge_middle$Sum_Payments, na.rm = TRUE) #207.4
median(MPOS_OP_Merge_middle$total_medicare_medical_allowed_amount, na.rm = TRUE) #76.34

median(MPOS_OP_Merge_middle_drug$Sum_Payments, na.rm = TRUE) #209.4
median(MPOS_OP_Merge_middle_drug$total_medicare_drug_allowed_amount, na.rm = TRUE) #76.34

##########################################################
# GET SLOPE ESTIMATE WITHIN EACH STATE (DV = TOTAL MEDICAL COSTS)

lmslope_by_state <- NULL
for (i in state_list) {
  print(paste0('On, ', i))
  #filter by state, center x and y
  state_data <- filter(MPOS_OP_Merge_middle, State_Code_of_the_Provider==i)
  fit <- lm(log_total_medical_medicare ~ log_total_pay + log_num_beneficiaries, data = state_data)
  a <- round(as.numeric(coef(fit)[2]), 2)
  CI <- round(confint(fit)[2,], 2)
  lmslope_by_state <- rbind(lmslope_by_state, data.frame(State=i, LM=a, LM_LB=CI[1], LM_UB=CI[2]))
}

#######################################################
# QUANTIFY THE RELATIONSHIP BETWEEN PAY AND COSTS WITH SPEARMAN CORRELATION
#######################################################

#############################
# COMPUTE PARTIAL SPEARMAN CORRELATION
MPOS_OP_Merge$state <- as.character(MPOS_OP_Merge$State_Code_of_the_Provider)
MPOS_OP_Merge <- left_join(MPOS_OP_Merge, political_data, by = c('state' = 'Postal.Code'))

pcor_by_state <- NULL
count_by_state <- NULL
for(i in state_list) {
  print(paste0('On ', i))
  state_data <- filter(MPOS_OP_Merge, state==i)
  state_data <- dplyr::select(state_data, Sum_Payments, total_medicare_medical_allowed_amount, Number_of_Medicare_Beneficiaries)
  state_data <- na.omit(state_data)
  
  pcor_i <- pcor(state_data, method = 'spearman')$estimate[1,2]
  pcor_by_state <- c(pcor_by_state, pcor_i)
  count_by_state <- c(count_by_state, nrow(state_data))
}

pcor_df <- data.frame(state=state_list, pcor=pcor_by_state, sample_size=count_by_state)

#############################
# MAKE U.S. MAP PLOTS OF COR AND CONSERVATIVE ADVANTAGE

### FIGURE 3

political_data_merged <- right_join(pcor_df, political_data, by=c('state' = 'Postal.Code'))

names(political_data_merged)[1] <- 'state'
p1 <- plot_usmap(data = political_data_merged, values = 'pcor') + 
  scale_fill_viridis_c(name='Pay-Cost P-Correlation') + 
  theme(legend.position = 'right')
p2 <- plot_usmap(data = political_data_merged, values = 'Conservative.advantage') +
  scale_fill_gradient2(name='Conservative Advantage', low='blue', high='red', mid='white', midpoint = mean(political_data_merged$Conservative.advantage)) +
  theme(legend.position = 'right')
pdf('maps2.pdf', width = 8, height = 10)
grid.arrange(p1, p2, nrow=2)
dev.off()

#######################################################
# SAVE REGRESSION SLOPES AND PCORS WITHIN EACH STATE

#join with political leaning measures and Spearman correlation
tmp <- left_join(political_data_merged, lmslope_by_state, by=c('state'='State'))
tmp$LM_display <- paste0(tmp$LM, ' (', tmp$LM_LB, ', ', tmp$LM_UB, ')')
tmp <- arrange(tmp, Conservative.advantage)
tmp$pcor <- round(tmp$pcor, 2)
dplyr::select(tmp, state, sample_size, LM_display, pcor, Conservative.advantage)


###################################

#Read in ACS economic measures
econ_data <- read.csv('ECON_SMALL.csv', stringsAsFactors = FALSE)
econ_data <- econ_data[-1,]
short_names <- c('PopulationOver16','UnemploymentRate','MedianHouseholdIncome',
                 'PopulationNonInst','PublicInsuranceCoveragePct','NoInsuranceCoveragePct',
                 'PovertyRate')
names(econ_data) <- c('state_name', short_names)
# Convert variable encoding to numeric
for(i in 2:ncol(econ_data)) { econ_data[,i] <- as.numeric(econ_data[,i]) }
econ_data <- left_join(econ_data, data.frame(state = state_list, state_name = state_names))

#Read in ACS social measures
social_data <- read.csv('SOCIAL_SMALL.csv', stringsAsFactors = FALSE)
social_data <- social_data[-1,]
short_names <- c('PercentHighSchool','PercentBachelors')
names(social_data) <- c('state_name', short_names)
# Convert variable encoding to numeric
for(i in 2:ncol(social_data)){ social_data[,i] <- as.numeric(social_data[,i])}
social_data <- left_join(social_data, data.frame(state = state_list, state_name = state_names))

#Merge with original vars
names(political_data_merged)[1] <- 'state'
political_data_merged <- left_join(political_data_merged, econ_data, by='state')
political_data_merged <- left_join(political_data_merged, social_data, by='state')
political_data_merged$logPopulationOver16 <- log(political_data_merged$PopulationOver16)

#######################################################
# CONTROL FOR STATE-LEVEL PROVIDER MARKET FACTORS IN POLITICAL MODEL 
#######################################################

specialist_data <- read.csv('KFF specialist data Simple.csv', stringsAsFactors = FALSE)
specialist_data$pct_specialist <- specialist_data$Specialist.Physicians / specialist_data$Total
political_data_merged_health <- left_join(political_data_merged, specialist_data, by=c('ï..State'))

#######################################################
# CONTROL FOR STATE-LEVEL HEALTH FACTORS IN POLITICAL MODEL
#######################################################

### McKinsey Rankings
health_data <- read.csv('USHealthNews.csv', stringsAsFactors = FALSE)
health_data$grp <- 'Q3' #bottom
health_data$grp[health_data$ï..RANK <= 33.3] <- 'Q2' #middle
health_data$grp[health_data$ï..RANK <= 16.7] <- 'Q1' #top
health_data$grp <- as.factor(health_data$grp)
political_data_merged_health <- left_join(political_data_merged_health, health_data, by=c('ï..State' = 'STATE'))

#model without health rankings
econ_fit1 <- lm(scale(pcor) ~ scale(Conservative.advantage) + scale(log(PopulationOver16)) + 
                  scale(UnemploymentRate) + scale(MedianHouseholdIncome) + scale(PercentHighSchool) + 
                  scale(NoInsuranceCoveragePct) + scale(pct_specialist) - 1, 
                data = political_data_merged_health)
summary(econ_fit1)

#model with all variables (including health rankings)
econ_fit <- lm(scale(pcor) ~ scale(Conservative.advantage) + scale(log(PopulationOver16)) + 
                 scale(UnemploymentRate) + scale(MedianHouseholdIncome) + scale(PercentHighSchool) + 
                 scale(NoInsuranceCoveragePct) + scale(pct_specialist) +  grp - 1, 
               data = political_data_merged_health)
summary(econ_fit)
anova(econ_fit1, econ_fit) #F-test on health ranking groups

var_names <- c('Conservative\nAdvantage','Population\nSize (log)','Unemployment\nRate','Median Household\nIncome','HS Graduation\nRate','Percent\nUninsured','Percent\nSpecialist Providers','McKinsey Index:\nTop','McKinsey Index:\nMiddle','McKinsey Index:\nBottom')
CI <- as.data.frame(confint(econ_fit, level=.99))
names(CI) <- c('LB', 'UB')
CI$estimate <- coefficients(econ_fit)
CI$var <- var_names
CI$var <- factor(CI$var, levels = var_names)

### Figure 3

pdf('CI_plot2.pdf', width = 12, height = 5)
ggplot(CI, aes(x=var)) + 
  geom_point(aes(y=estimate)) + 
  geom_errorbar(aes(ymin=LB,ymax=UB)) + 
  geom_hline(yintercept=0, color='red')
dev.off()