# We are interested in how the data collected can be used to predict a loan's status. The desired response
# variable is binary. If loan_status is Charged Off or Default, then the reponse variable is 1; otherwise,
# it is 0.

library(tidyverse)
library(ggplot2)
library(eeptools)
library(randomForest)

# Load the data
loan <- read.csv("loan.csv")

# Look at the data
head(loan)

# Preliminary examination of the variables

# Check to see how many values are missing from each column
colSums(is.na(loan))

# id: remove
# member_id: remove
# loan_amnt: keep as is
# funded_amnt: keep as is
# funded_amnt_inv: keep as is
# term: keep as categorical variable
# int_rate: keep as is
# installment: keep as is
# grade/sub_grade: remove grade, keep sub_grade as
# grade information is already present in sub_grade
# emp_title: remove
# emp_length: keep as categorical variable
# home_ownership: keep as categorical variable
# annual_inc: keep as is
# verification_status: keep as categorical variable
# issue_d: convert to age of loan issue date

loan$issue_d_age <- parse_date(loan$issue_d, format="%b-%Y")
loan$issue_d_age <- age_calc(loan$issue_d_age, as.Date("2018-06-06"), units="years")

# loan_status: response variable, convert to binary

loan <- loan %>% mutate(status = ifelse((loan$loan_status == "Charged Off") | (loan$loan_status == "Default"), 1, 0))
loan$status <- as.factor(loan$status)

# pymnt_plan: remove, almost all values are the same

sum(loan$pymnt_plan == "y")

# url: remove
# desc: remove
# purpose: keep as categorical variable, only 14 levels

length(unique(loan$purpose))

# title: remove
# zip_code: remove
# addr_state: keep as categorical variable
# dti: keep as is
# delinq_2yrs: keep as is

# Check to see how many values are 0
sum(loan$delinq_2yrs == 0, na.rm=TRUE)

# earliest_cr_line: convert to age of oldest credit line

loan$earliest_cr_line_age <- parse_date(loan$earliest_cr_line, format="%b-%Y")
# Replace NAs
loan$earliest_cr_line_age <- loan$earliest_cr_line_age %>% replace_na(as.Date("2018-06-06"))
loan$earliest_cr_line_age <- age_calc(loan$earliest_cr_line_age, as.Date("2018-06-06"), units="years")
# Reintroduce NAs
loan$earliest_cr_line_age[loan$earliest_cr_line_age == 0] <- NA
# Check to see how many dates are missing (initially blank text)
sum(is.na(loan$earliest_cr_line_age))

# inq_last_6mths: keep as is
# mths_since_last_delinq: remove, over half of the values are missing

# Rule out that missing values are actually 0s
sum(loan$mths_since_last_delinq == 0, na.rm=TRUE)

# mths_since_last_record: remove, most of the values are missing

# Rule out that missing values are actually 0s
sum(loan$mths_since_last_record == 0, na.rm=TRUE)

# open_acc: keep as is
# pub_rec: keep as is

# Check to see how many values are 0
sum(loan$pub_rec == 0, na.rm=TRUE)

# revol_bal: keep as is
# revol_util: keep as is
# total_acc: keep as is
# initial_list_status: keep as categorical variable

# Check to see how many values are "f"
sum(loan$initial_list_status == "f")

# out_prncp: keep as is
# out_prncp_inv: keep as is
# total_pymnt: keep as is
# total_pymnt_inv: keep as is
# total_rec_prncp: keep as is
# total_rec_int: keep as is
# total_rec_late_fee: keep as is

# Check to see how many values are nonzero
sum(loan$total_rec_late_fee != 0)

# recoveries: remove, only positive when loan_status is charged off,
# and as such shouldn't be used to predict loan_status
# collection_recovery_fee: remove, only positive when loan_status is
# charged off, and as such shouldn't be used to predict loan_status
# last_pymnt_d: convert to time since last payment, replace missing values
# with median time since there are many but not too many

loan$last_pymnt_d_age <- parse_date(loan$last_pymnt_d, format="%b-%Y")
# Replace NAs
loan$last_pymnt_d_age <- loan$last_pymnt_d_age %>% replace_na(as.Date("2018-06-06"))
loan$last_pymnt_d_age <- age_calc(loan$last_pymnt_d_age, as.Date("2018-06-06"), units="years")
# Reintroduce NAs
loan$last_pymnt_d_age[loan$last_pymnt_d_age == 0] <- NA
# Check to see how many dates are missing (initially blank text)
sum(is.na(loan$last_pymnt_d_age))
# Replace NAs with median time
loan$last_pymnt_d_age <- loan$last_pymnt_d_age %>% replace_na(median(loan$last_pymnt_d_age, na.rm=TRUE))

# last_pymnt_amnt: keep as is
# next_pymnt_d: remove, only present when loan_status is current,
# and as such shouldn't be used to predict loan_status
# last_credit_pull_d: convert to time since last credit pull

loan$last_credit_pull_d_age <- parse_date(loan$last_credit_pull_d, format="%b-%Y")
# Replace NAs
loan$last_credit_pull_d_age <- loan$last_credit_pull_d_age %>% replace_na(as.Date("2018-06-06"))
loan$last_credit_pull_d_age <- age_calc(loan$last_credit_pull_d_age, as.Date("2018-06-06"), units="years")
# Reintroduce NAs
loan$last_credit_pull_d_age[loan$last_credit_pull_d_age == 0] <- NA
# Check to see how many dates are missing (initially blank text)
sum(is.na(loan$last_credit_pull_d_age))

# collections_12_mths_ex_med: keep as is

# Check to see how many values are nonzero
sum(loan$collections_12_mths_ex_med != 0, na.rm=TRUE)

# mths_since_last_major_derog: remove, most of the values are missing
# policy_code: remove, all values are the same

sum(loan$policy_code == 1)

# application_type: remove, there are a small number of joint applications

# Get the number of joint applications
sum(loan$application_type == "JOINT")

# annual_inc_joint: remove, there are a small number of joint applications
# dti_joint: remove, there are a small number of joint applications
# verification_status_joint: remove, there are a small number of joint applications
# acc_now_delinq: keep as is

# Check to see how many values are nonzero
sum(loan$acc_now_delinq != 0, na.rm=TRUE)

# tot_coll_amt: keep as a significant number of values are nonzero and non-NA,
# replace NAs by 0 (median)

sum(loan$tot_coll_amt == 0 | is.na(loan$tot_coll_amt), na.rm=TRUE)
loan$tot_coll_amt <- loan$tot_coll_amt %>% replace_na(0)

# tot_cur_bal: keep as less than 10% of the values are missing, replace
# NAs by median

# Check to see how many values are 0
sum(loan$tot_cur_bal == 0, na.rm=TRUE)
loan$tot_cur_bal <- loan$tot_cur_bal %>% replace_na(median(loan$tot_cur_bal, na.rm=TRUE))

# open_acc_6m: remove, most of the values are missing
# open_il_6m: remove, most of the values are missing
# open_il_12m: remove, most of the values are missing
# open_il_24m: remove, most of the values are missing
# mths_since_rcnt_il: remove, most of the values are missing
# total_bal_il: remove, most of the values are missing
# il_util: remove, most of the values are missing
# open_rv_12m: remove, most of the values are missing
# open_rv_24m: remove, most of the values are missing
# max_bal_bc: remove, most of the values are missing
# all_util: remove, most of the values are missing
# total_rev_hi_lim: keep as less than 10% of the values are missing, replace
# NAs by median

loan$total_rev_hi_lim <- loan$total_rev_hi_lim %>% replace_na(median(loan$total_rev_hi_lim, na.rm=TRUE))

# inq_fi: remove, most of the values are missing
# total_cu_tl: remove, most of the values are missing
# inq_last_12m: remove, most of the values are missing 

# Remove unnecessary columns
loan <- loan[, -c(1,2,9,11,16,17,18,19,20,22,23,27,29,30,44,45,46,48,49,51,52,53,54,55,56,
                  60,61,62,63,64,65,66,67,68,69,70,72,73,74)]

# Remove all rows with NAs
loan <- loan[complete.cases(loan),]

# Check correlation for numeric columns
cor(loan[, c(1,2,3,5,6,10,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40)])

# loan_amnt, funded_amnt, funded_amnt_inv, and installment are highly correlated, along with
# out_prncp and out_prncp_inv, and total_pymnt, total_pymnt_inv, and total_rec_prncp. total_acc
# and open_acc have a fairly high correlation at around 0.7, out_prncp and out_prncp_inv have
# a correlation of around 0.65 with loan_amnt, funded_amnt, and funded_amnt_inv, total_rec_int
# has a correlation of almost 0.7 with total_pymnt and total_pymnt_inv, last_pymnt_amnt has
# a correlation of almost 0.75 with total_rec_prncp, and around 0.65 with total_pymnt and
# total_pymnt_inv, revol_bal and total_rev_hi_lim have a high correlation at around 0.8, and
# last_pymnt_d_age has correlations of over 0.7 with issue_d_age and last_credit_pull_d_age.

# Remove funded_amnt, funded_amnt_inv, installment, out_prncp_inv, total_pymnt_inv, and total_rec_prncp
# due to extremely high correlation. total_acc and open_acc contain similar information and have a high
# correlation, and will be combined into a single acc variable. Keep out_prncp as its correlation
# with loan_amnt is not too high and it contains unique information. Remove total_rec_int as it has
# a high correlation with total_pymnt and contains similar information to total_pymnt. Remove
# last_pymnt_amnt as it has high correlations with several variables and is probably not relevant.
# Remove total_rev_hi_lim as it is highly correlated with revol_bal and contains repeated values
# while revol_bal is more specific. Remove last_pymnt_d_age as it has high correlations with two
# other age variables and is probably not important.

loan$acc <- (loan$total_acc + loan$open_acc) / 2
loan <- loan[, -c(2,3,6,17,21,24,26,27,28,30,35,39)]

# Create plots for status, loan_amnt, term, int_rate, sub_grade, annual_inc, purpose, addr_state, dti,
# inq_last_6mths, revol_bal, revol_util, and acc, variables that seem the most important

# First, look at the number of loans that fall in the "good" and "bad" categories of status
loan %>% group_by(status) %>% summarise(ct=n()) %>% mutate(percent=ct/sum(ct)) %>%
  ggplot(aes(x=status, y=percent)) + geom_bar(stat="identity", colour="black", fill="white")

# We see that the vast majority of loans are in the "good" category.

# Now, we create a histogram for the loan amounts
ggplot(loan, aes(loan_amnt)) + geom_histogram(binwidth=4500, color="black", fill="white")

# The loan amounts are right skewed, as expected.

# Now we compare the loan amounts for the two different statuses
ggplot(loan, aes(x=status, y=loan_amnt)) + geom_boxplot()

# The distribution of loan amount appears to be the same for each type of status,
# and so the loan amount by itself does not appear to affect the status.

# Now we examine how total loan amounts have varied over time
ggplot(loan, aes(x=issue_d_age, y=loan_amnt)) + stat_summary(fun.y="sum", geom="bar", colour="black", fill="white")

# As makes sense, the loan amounts have increased over time.

# Now we look at how many loans are in each term
loan %>% group_by(term) %>% summarise(ct=n()) %>% mutate(percent=ct/sum(ct)) %>%
  ggplot(aes(x=term, y=percent)) + geom_bar(stat="identity", colour="black", fill="white")

# We see that about 70% of the loans are for 36-month terms.

# Now we compare the status for each term of loan
ggplot(loan, aes(x=term, fill=status)) + geom_bar(stat="count", position="fill", colour="black") +
  ylab("percent")

# A slightly higher proportion of loans for a 60-month term than for a 36-month term have a "bad" status.

# Now we make a histogram for the interest rate to exmaine its distribution
ggplot(loan, aes(int_rate)) + geom_histogram(binwidth=3, color="black", fill="white")

# The interest rate is right-tailed, as expected.

# Now we compare the status for different interest rates
ggplot(loan, aes(x=status, y=int_rate)) + geom_boxplot()

# We see that loans with "bad" status have higher interest rates, as expected.

# Let's look at the number of loans in each subgrade
loan %>% group_by(sub_grade) %>% summarise(ct=n()) %>% mutate(percent=ct/sum(ct)) %>%
  ggplot(aes(x=sub_grade, y=percent)) + geom_bar(stat="identity", colour="black", fill="white")

# We see that most of the loans are in the "B" and "C" grades.

# Now we compare the status for each subgrade
ggplot(loan, aes(x=sub_grade, fill=status)) + geom_bar(stat="count", position="fill", colour="black") +
  ylab("percent")

# We see that as the subgrade decreases, the proportion of "bad" loans increases, but appears to
# be relatively constant for loans of grade G, actually decreasing slightly from subgrade F5.

# Now we create a histogram for annual income
ggplot(loan, aes(annual_inc)) + geom_histogram(binwidth=1000, color="black", fill="white")
max(loan$annual_inc)
# Remove outliers
high <- 400000:9500000
annual_inc_adj <- loan$annual_inc[!loan$annual_inc %in% high]
# Plot again
ggplot() + aes(annual_inc_adj) + geom_histogram(binwidth=25000, color="black", fill="white")

# The annual income follows an approximate chi-square distribution, as expected.

# Now we compare the status for various annual incomes
ggplot(loan, aes(x=status, y=annual_inc)) + geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits=c(0,200000))

# We see that people with "bad" loans had lower annual incomes, as expected.

# Let's examine the number of loans for each purpose
loan %>% group_by(purpose) %>% summarise(ct=n()) %>% mutate(percent=ct/sum(ct)) %>%
  ggplot(aes(x=purpose, y=percent)) + geom_bar(stat="identity", colour="black", fill="white")

# We see that the vast majority of loans are for debt consolidation, followed by credit cards.

# Now, let's look at the effect of purpose on status
ggplot(loan, aes(x=purpose, fill=status)) + geom_bar(stat="count", position="fill", colour="black") +
  ylab("percent")

# We see that the purpose does affect the status somewhat, with some purposes having a higher
# proportion of "bad" loans.

# Now let's look at the number of loans in each state
loan %>% group_by(addr_state) %>% summarise(ct=n()) %>% mutate(percent=ct/sum(ct)) %>%
  ggplot(aes(x=addr_state, y=percent)) + geom_bar(stat="identity", colour="black", fill="white")

# Interestingly, almost 15% of the loans were applied for in the state of California.

# Now, let's look at the effect of addr_state on status
ggplot(loan, aes(x=addr_state, fill=status)) + geom_bar(stat="count", position="fill", colour="black") +
  ylab("percent")

# For the most part, the state doesn't seem to directly affect the status of the loan.

# Now we plot a histogram for dti to exmaine its distribution
ggplot(loan, aes(dti)) + geom_histogram(binwidth=20, color="black", fill="white")
max(loan$dti)
# Remove outliers
high <- 100:10000
dti_adj <- loan$dti[!loan$dti %in% high]
# Plot again
ggplot() + aes(dti_adj) + geom_histogram(binwidth=4, color="black", fill="white") +
  scale_x_continuous(limits=c(-10,75))

# Excluding outliers, the debt-to-income ratio appears to have a symmetric, approximately
# normal distribution, which is somewhat surprising, as I was expecting something somewhat less
# symmetric.

# Now we compare the status for different dti
ggplot(loan, aes(x=status, y=dti)) + geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits=c(0,75))

# We see that loans with "bad" status have slightly higher dti's, but the difference is minimal,
# and "good" loans actually have some higher dti's than "bad" loans, excluding outliers.

# Now, we create a histogram for inq_last_6mths
ggplot(loan, aes(inq_last_6mths)) + geom_histogram(binwidth=1, color="black", fill="white")

# The number of inquires is right-skewed, as expected.

# Now we compare the number of inquiries for the two different statuses
ggplot(loan, aes(x=status, y=inq_last_6mths)) + geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits=c(0,7.5))

# The number of inquiries in the last 6 months is significantly higher for people with "bad" status loans.
# In fact, for "good" statuses, the median number of inquiries is 0, whereas it is 1 for "bad" statuses.

# Now we plot a histogram for the total revolving balance to exmaine its distribution
ggplot(loan, aes(revol_bal)) + geom_histogram(binwidth=5000, color="black", fill="white")
max(loan$revol_bal)
# Remove outliers
high <- 200000:3000000
revol_bal_adj <- loan$revol_bal[!loan$revol_bal %in% high]
# Plot again
ggplot() + aes(revol_bal_adj) + geom_histogram(binwidth=4000, color="black", fill="white") +
  scale_x_continuous(limits=c(0,200000))

# Excluding outliers, the total credit revolving balance follows an approximate chi-squared
# distribution, as expected.

# Now we compare the status for different values of revol_bal
ggplot(loan, aes(x=status, y=revol_bal)) + geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits=c(0,50000))

# The total balance doesn't have much of a direct effect on status.

# Now, we look at a histogram for revol_util
ggplot(loan, aes(revol_util)) + geom_histogram(binwidth=10, color="black", fill="white")

# The revolving line utilization rate appears somewhat symmetric. I was expecting something
# a bit less symmetric, but it makes sense that there would be a small amount at both tails.

# Now we compare revol_util for the two different statuses
ggplot(loan, aes(x=status, y=revol_util)) + geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits=c(0,150))

# Excluding outliers, loans with "bad" statuses have slightly higher utilization rates, which
# makes sense.

# We decide not to plot tot_cur_bal, as it is probably similar to revol_bal.

# Now, we create a histogram for the number of accounts.
ggplot(loan, aes(acc)) + geom_histogram(binwidth=4, color="black", fill="white")

# The number of accounts appears somewhat chi-square, as expected.

# Now we compare the number of accounts for the two different statuses
ggplot(loan, aes(x=status, y=acc)) + geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits=c(0,50))

# The number of accounts actually appears slightly higher for loans with "good" status.

# We have obtained several relevant insights with these visualizations, which can be used to support
# and communicate results from our models.

# We will now fit and compare two different models to the data.

# I decided to first try a random forest model, which is an improvement over classification trees.
# I chose this model because it usually works well for classification, is flexible, and doesn't
# have many drawbacks. It can handle categorical variables well, which we have many of. Our variables
# are also highly non-normal and nonlinear, and random forests handle this well. Furthermore,
# classification trees suffer from high variance and bagged trees are highly correlated, but random
# forests fix these issues by performing a bootstrap and taking the average of the results, and only
# considering a random subset of variables at each step.

# Split into training and test subsets
samp <- sample(nrow(loan), floor(nrow(loan)*0.9))

loan_train <- loan[samp,]
loan_test <- loan[-samp,]

# Fit a random forest with sqrt(28) variables to consider at each step and predict
# for the test subset
forest_fit <- randomForest(status ~ ., data=loan_train, mtry=floor(sqrt(28)), ntree=100)
forest_pred <- predict(forest_fit, newdata=loan_test)

# Check the accuracy of our model
accuracy <- table(forest_pred, loan_test$status)
1 - (sum(diag(accuracy))/sum(accuracy))

# The misclassification rate is less than 1% (0.00972), a very good result.

# Next, I tried a logistic regression model. This is a very popular method, and is perfectly
# suited for the situation we have, where we are trying to predict a binary response variable.
# Rather than simply outputting a 1 or a 0 for an observation, the logistic regression model gives
# a probability, providing more flexibility. For example, if we wanted to be conservative in
# predicting individuals who are at risk of not paying off their loans, we could assign a prediction
# of 1 for status when the probability provided by the model is greater than, say, 0.2, rather than
# the default of 0.5. I considering using linear discriminant analysis, which is similar to logistic
# regression, but it depends more upon the normality of the predictors, and we have highly non-normal
# data. Also, logistic regression is better equipped to handle a binary response variable.

# We need to add interaction terms for logistic regression. We have that tot_cur_bal and revol_bal
# interact, along with revol_bal and revol_util, int_rate and dti, annual_inc and dti, loan_amnt
# and total_pymnt. There
# could be many interactions here, so we focus on the most revelvant and easily-interpretable ones.

loan$tot_cur_bal_x_revol_bal <- loan$tot_cur_bal * loan$revol_bal
loan$revol_bal_x_revol_util <- loan$revol_bal * loan$revol_util
loan$int_rate_x_dti <- loan$int_rate * loan$dti
loan$annual_inc_x_dti <- loan$annual_inc * loan$dti
loan$loan_amnt_x_total_pymnt <- loan$loan_amnt * loan$total_pymnt

# Split into training and test subsets
loan_train <- loan[samp,]
loan_test <- loan[-samp,]

# Fot a logistic regression model and predict for the test subset
log_fit <- glm(status ~ ., data=loan_train, family=binomial)
log_probs <- predict(log_fit, newdata=loan_test, type="response")
contrasts(loan$status)
log_pred <- rep(0, length(loan_test$status))
log_pred[log_probs > 0.5] <- 1

# Check the accuracy of our model
log_accuracy <- table(log_pred, loan_test$status)
1 - (sum(diag(log_accuracy))/sum(log_accuracy))

# The misclassification rate is just over 1% (0.01025), another excellent result.

# Both models performed very well. The random forest model was slightly more accurate,
# with a misclassification rate of 0.972% compared to 1.025% for the logistic regression model.
# In the absence of other information, the random forest model comes out on top. However, the
# logistic regression model is more flexible since it gives us probabilities, performing only
# slightly worse than the random forest model. If we require more flexibility in our model, for
# example, by predicting status conservatively, then we should choose the logistic regression
# model. However, if we are fine with simply obtaining a 1 or a 0 for status without any degree
# of certainty for each individual observation, the random forest model might be slightly better.
# In short, we need more information as to what kind of model is desired.