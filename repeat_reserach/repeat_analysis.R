library(dplyr)
library(ggplot2)
library(moments) #kurtosis, skewness
library(ppcor) #pcor
library(usmap) #plot_usmap
library(gridExtra) #grid.arrange



MPOS_OP_Merge <- read.csv('MPOS_OP_Merge.csv', stringsAsFactors = FALSE)
nrow(MPOS_OP_Merge) #345,743 336,827
sum(MPOS_OP_Merge$Number_of_Services) #1,160,242,264 1,132,044,546
sum(MPOS_OP_Merge$total_medicare_allowed_amount) #534,079,529 520,591,569
sum(!is.na(MPOS_OP_Merge$NPI)) #345,743 336,827

# Limit to individual providers
MPOS_OP_Merge <- dplyr::filter(MPOS_OP_Merge, Entity_Type_of_the_Provider=='I')
nrow(MPOS_OP_Merge)
sum(MPOS_OP_Merge$Number_of_Services) 
sum(MPOS_OP_Merge$total_medicare_allowed_amount)
sum(MPOS_OP_Merge$total_paid) #1,169,272,504 1,093,885,681
sum(!is.na(MPOS_OP_Merge$NPI))

# Quantity drug & medical spending
sum(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #25,600,098 24,865,134 21,061,364
sum(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #508,479,431 495,726,435 20,771,244
mean(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #225.8062 224.1637 109.6124
mean(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #1,470.767 1,471.837 103.8484

# Limit to 50 states
political_data <- read.csv(file='Gallup_Political_Scale.csv', header=T)
political_data <- dplyr::select(political_data, 'ï..State', 'Postal.Code', 'Conservative.advantage')
state_list <- as.character(political_data$Postal.Code)
state_names <- as.character(political_data$ï..State)
MPOS_OP_Merge <- MPOS_OP_Merge %>% dplyr::filter(as.character(State_Code_of_the_Provider) %in% state_list)

# total/average medical and drug spending
sum(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #25,542,384 24,809,246 20,937,423
sum(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #505,667,494 492,977,799 20,639,698
mean(MPOS_OP_Merge$total_medicare_drug_allowed_amount, na.rm=TRUE) #226.0388 224.384 109.5988
mean(MPOS_OP_Merge$total_medicare_medical_allowed_amount, na.rm=TRUE) #1,477.925 1,478.944 103.8632

# compute log of pay and cost
MPOS_OP_Merge$log_total_pay <- log(MPOS_OP_Merge$total_paid)
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
skewness(medical_allowed) #9.884631 9.884422 25.73317
skewness(log(medical_allowed+1)) #-0.4598715 -0.4622274 (w/o +1) -0.4899982 -0.4925424 
kurtosis(medical_allowed) #214.1962 214.7541 1645.832
kurtosis(log(medical_allowed+1)) #3.960915 3.970919 4.316608 (w/o +1) 4.10525 4.116734 4.771537
hist(medical_allowed)
hist(log(medical_allowed+1))

# Total Pay
total_pay <- na.omit(MPOS_OP_Merge$total_paid)
skewness(total_pay) #178.3375 171.9721
skewness(log(total_pay+1)) #0.6934898 0.6958679
kurtosis(total_pay) #43,460.72 44,328.46
kurtosis(log(total_pay+1)) #3.813726 3.790041
hist(total_pay)
hist(log(total_pay+1))

#######################################################
# Plots of total pay vs medical costs
#######################################################

# Figure 1 & supplementary figure 1
(med_total_pay <- median(MPOS_OP_Merge$total_paid, na.rm=TRUE)) #190.98 189.11 207.395
(Q99_total_pay <- quantile(MPOS_OP_Merge$total_paid, probs=0.99, na.rm=TRUE)) #56,499.52 55,217.08 55,414.65 
(Q01_total_pay <- quantile(MPOS_OP_Merge$total_paid, probs=0.01, na.rm=TRUE)) #10.73 10.74 10.7
(Q75_total_pay <- quantile(MPOS_OP_Merge$total_paid, probs=0.75, na.rm=TRUE)) #668.3775 656.915 729.1175
(Q25_total_pay <- quantile(MPOS_OP_Merge$total_paid, probs=0.25, na.rm=TRUE)) #59.22 58.77 62.54
(Q99_medical <- quantile(MPOS_OP_Merge$total_medicare_medical_allowed_amount, probs=0.99, na.rm=TRUE)) #8606.657 8595.322 698.9083
(Q01_medical <- quantile(MPOS_OP_Merge$total_medicare_medical_allowed_amount, probs=0.01, na.rm=TRUE)) #40.55 40.65588 3
(Q99_medical_per <- quantile(MPOS_OP_Merge$total_medicare_medical_per_beneficiary, probs=0.99, na.rm=TRUE)) #15.58803 15.51197 7.676912
(Q01_medical_per <- quantile(MPOS_OP_Merge$total_medicare_medical_per_beneficiary, probs=0.01, na.rm=TRUE)) #0.2265007 0.2266918 0.001327306 

pdf('spaghetti_zoom5.pdf')
ggplot(MPOS_OP_Merge, aes(x=log_total_pay, y=log_total_medical_medicare)) + 
  stat_smooth(geom='line', aes(group=State_Code_of_the_Provider), se=FALSE, method='loess', alpha=0.5) + 
  geom_vline(xintercept = c(log(Q25_total_pay), log(Q75_total_pay)), color='red') + 
  theme_bw() + theme(panel.grid=element_blank()) + 
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs') + 
  coord_cartesian(xlim = c(3,9), ylim = c(6, 7.25))
dev.off()

set.seed(23758923) #oddly specific number?
N <- nrow(MPOS_OP_Merge)
samp <- sort(sample(1:N, round(N/10), replace = FALSE))
MPOS_OP_Merge_samp <- MPOS_OP_Merge[samp, ]

# Number of observations in spaghetti plots?
sum(!is.na(MPOS_OP_Merge$log_total_medical_medicare) & !is.na(MPOS_OP_Merge$log_total_pay)) #342,147 333,331 198,720
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

pdf('spaghetti_zoom_perBeneficiary5.pdf')
ggplot(MPOS_OP_Merge, aes(x=log_total_pay, y=log_total_medical_medicare_per_beneficiary)) + 
  stat_smooth(geom='line', aes(group=State_Code_of_the_Provider), se=FALSE, method='loess', alpha=0.5) +
  geom_vline(xintercept=c(log(Q25_total_pay), log(Q75_total_pay)), color='red') + 
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab('Log Total Open Payments') + ylab('Log Total Medical Costs per Beneficiary') +
  coord_cartesian(xlim=c(3, 9), ylim=c(-0.5, 1.5)) # Different zoom values due to different results
dev.off()

##########################################################
# LIMIT REGRESSION MODEL TO MIDDLE 50% OF PAYMENTS (LINEAR PART)

#exclude non-matched providers
MPOS_OP_Merge <- dplyr::filter(MPOS_OP_Merge, !is.na(NPI))
# limit to 50% of payments
MPOS_OP_Merge_middle <- MPOS_OP_Merge[MPOS_OP_Merge$total_paid > Q25_total_pay,]
MPOS_OP_Merge_middle <- MPOS_OP_Merge_middle[MPOS_OP_Merge_middle$total_paid < Q75_total_pay,]

##########################################################
# GET SLOPE ESTIMATE (DV = MEDICAL COSTS)
fit <- lm(log_total_medical_medicare ~ log_total_pay + log_num_beneficiaries + State_Code_of_the_Provider - 1, data = MPOS_OP_Merge_middle)
summary(fit) #coefficent of log_total_pay = 0.013790 0.012775, -0.022691
round(confint(fit, parm = 'log_total_pay'), 3) # 0.009  0.019 0.008  0.018, -0.032 -0.013
length(fit$residuals) #171,065 166,664 99,363
sum(!is.na(MPOS_OP_Merge_middle$log_total_medical_medicare)) #166,664 99,363

# Controlling for drug expenditures
fit2 <- lm(log_total_medical_medicare ~ log_total_pay + log_total_drug_medicare + log_num_beneficiaries + State_Code_of_the_Provider - 1, data = MPOS_OP_Merge_middle)
summary(fit2) #coefficient of log_total_pay =-0.005502 -0.007545, -0.018123
round(confint(fit2, parm = 'log_total_pay'), 3) #-0.013  0.002, -0.015      0, -0.028 -0.009
length(fit2$residuals) #54,000 52,886 95,432

#correlation between drug and medical costs
cor(MPOS_OP_Merge_middle$total_medicare_medical_allowed_amount, MPOS_OP_Merge_middle$total_medicare_drug_allowed_amount, method = 'spearman', use = 'pairwise.complete.obs')
#-0.01453288 -0.01439348 0.1918653

##########################################################
# GET SLOPE ESTIMATE (DV = DRUG COSTS)

MPOS_OP_Merge_middle_drug <- filter(MPOS_OP_Merge_middle, total_medicare_drug_allowed_amount > 0)
nrow(MPOS_OP_Merge_middle_drug) #54,009 52,894 95,432
fit_drug <- lm(log_total_drug_medicare ~ log_total_pay + log_num_beneficiaries + State_Code_of_the_Provider - 1, data = MPOS_OP_Merge_middle_drug)
summary(fit_drug) #coefficient of log_total_pay = 0.101031 0.100711    -0.027310
round(confint(fit_drug, parm = 'log_total_pay'), 3) #0.079  0.123 0.078  0.123, -0.037, -0.018

##########################################################
# MEDIAN MEDICAL AND DRUG COSTS

median(MPOS_OP_Merge_middle$total_paid, na.rm = TRUE) #191 189.11, 207.4
median(MPOS_OP_Merge_middle$total_medicare_medical_allowed_amount, na.rm = TRUE) #850.4425 852.5313, 76.34

median(MPOS_OP_Merge_middle_drug$total_paid, na.rm = TRUE) #229.39 227.18, 209.4
median(MPOS_OP_Merge_middle_drug$total_medicare_drug_allowed_amount, na.rm = TRUE) #53.93476 53.95227, 76.34

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
  state_data <- dplyr::select(state_data, total_paid, total_medicare_medical_allowed_amount, Number_of_Medicare_Beneficiaries)
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