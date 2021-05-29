# An R script to assess the impact of child marriage on child under-nutrition.
# Author: Shehyaaz Khan Nayazi

setwd("~/data_science_impact_of_child_marriage")
# load dataset
dataset <- read.csv("dataset/new_trial_diss_clean.csv")
## load WHO data ##
# length-for-age data
height_for_age_boys <- read.csv("dataset/who_growth_standards/length-for-age-0-60-boys-zscore.csv")
height_for_age_girls <- read.csv("dataset/who_growth_standards/length-for-age-0-60-girls-zscore.csv")
# weight-for-age data
weight_for_age_boys <- read.csv("dataset/who_growth_standards/weight-for-age-0-60-boys-zscore.csv")
weight_for_age_girls <- read.csv("dataset/who_growth_standards/weight-for-age-0-60-girls-zscore.csv")
# weight-for-height data
weight_for_height_0_24_boys <- read.csv("dataset/who_growth_standards/weight-for-length-0-24-boys-zscore.csv")
weight_for_height_24_60_boys <- read.csv("dataset/who_growth_standards/weight-for-length-24-60-boys-zscore.csv")
weight_for_height_0_24_girls <- read.csv("dataset/who_growth_standards/weight-for-length-0-24-girls-zscore.csv")
weight_for_height_24_60_girls <- read.csv("dataset/who_growth_standards/weight-for-length-24-60-girls-zscore.csv")

## extract relevant data ##
child_data <- dataset[, c("age_of_child_month", "weight_of_child_kg", "height_of_child_cm", "sex_of_child")]
mother_data <- dataset[, c("age_at_marriage", "education_level", "avg_monthly_income", "anc_visit", "mother_anaemic_preg")]
mother_data$anc_visit <- as.numeric(as.factor(mother_data$anc_visit))
## process child data ##
# Low weight-for-height is known as wasting
# Low height-for-age is known as stunting
# Children with low weight-for-age are known as underweight
# An infant was defined as stunted, wasted or underweight if his or her length-for-age, weight-for-length or weight-for-age z-score, respectively, was less than -2.

# create empty vectors
stunted <- vector(mode = "numeric", length = nrow(child_data))
wasted <- vector(mode = "numeric", length = nrow(child_data))
underweight <- vector(mode = "numeric", length = nrow(child_data))
undernutrition <- vector(mode = "numeric", length = nrow(child_data))
# renaming row names for easy retrievel of data
row.names(height_for_age_boys) <- height_for_age_boys$Month
row.names(height_for_age_girls) <- height_for_age_girls$Month
row.names(weight_for_age_boys) <- weight_for_age_boys$Month
row.names(weight_for_age_girls) <- weight_for_age_girls$Month
row.names(weight_for_height_0_24_boys) <- weight_for_height_0_24_boys$Height..cm.
row.names(weight_for_height_24_60_boys) <- weight_for_height_24_60_boys$Height..cm.
row.names(weight_for_height_0_24_girls) <- weight_for_height_0_24_girls$Height..cm.
row.names(weight_for_height_24_60_girls) <- weight_for_height_24_60_girls$Height..cm.
# determine child under-nutrition status
# sex_of_child = 1 -> girl, 2 -> boy
for(i in 1:nrow(child_data)){
	ht <- child_data[i, "height_of_child_cm"]
	wt <- child_data[i, "weight_of_child_kg"]
	age <- child_data[i, "age_of_child_month"]
	if(child_data[i, "sex_of_child"] == 1){ # girls
		stunted[i] <- ifelse(ht < height_for_age_girls[as.character(age), "X.2.SD"], 1, 0)
		underweight[i] <- ifelse(wt < weight_for_age_girls[as.character(age), "X.2.SD"], 1, 0)
		if(age <= 24){
			wasted[i] <- ifelse(wt < weight_for_height_0_24_girls[as.character(ht), "X.2.SD"], 1, 0)
		}else{
			wasted[i] <- ifelse(wt < weight_for_height_24_60_girls[as.character(ht), "X.2.SD"], 1, 0)
		}
	}else{ # boys
		stunted[i] <- ifelse(ht < height_for_age_boys[as.character(age), "X.2.SD"], 1, 0)
		underweight[i] <- ifelse(wt < weight_for_age_boys[as.character(age), "X.2.SD"], 1, 0)
		if(age <= 24){
			wasted[i] <- ifelse(wt < weight_for_height_0_24_boys[as.character(ht), "X.2.SD"], 1, 0)
		}else{
			wasted[i] <- ifelse(wt < weight_for_height_24_60_boys[as.character(ht), "X.2.SD"], 1, 0)
		}
	}
	undernutrition[i] <- ifelse(stunted[i] || underweight[i] || wasted[i], 1, 0)
}
# add the under-nutrition data to mother_data
mother_data$child_stunted <- stunted
mother_data$child_wasted <- wasted
mother_data$child_underweight <- underweight
mother_data$child_undernutrition <- undernutrition
# add the under-nutrition data to child_data
child_data$stunted <- stunted
child_data$wasted <- wasted
child_data$underweight <- underweight
child_data$undernutrition <- undernutrition
write.csv(child_data, file = "dataset/child_data_with_status.csv") # save child_data

## process mother data ##
# logistic regression model
child_status_logistic <- glm (child_undernutrition ~ age_at_marriage + education_level + avg_monthly_income + anc_visit + mother_anaemic_preg,
                        data=mother_data, family=binomial)
child_status_logistic_stunted <- glm (child_stunted ~ age_at_marriage + education_level + avg_monthly_income + anc_visit + mother_anaemic_preg,
                              data=mother_data, family=binomial)
child_status_logistic_wasted <- glm (child_wasted ~ age_at_marriage + education_level + avg_monthly_income + anc_visit + mother_anaemic_preg,
                              data=mother_data, family=binomial)
child_status_logistic_underweight <- glm (child_underweight ~ age_at_marriage + education_level + avg_monthly_income + anc_visit + mother_anaemic_preg,
                              data=mother_data, family=binomial)
# save model data to file
sink("result/model-data.txt")
summary(child_status_logistic)
summary(child_status_logistic_stunted)
summary(child_status_logistic_wasted)
summary(child_status_logistic_underweight)
sink()

# Plot number of stunted, wasted, underweight children
mat <- matrix(data = 0, nrow = 4, ncol = 3, 
              dimnames = list(c("0-24","24-36","36-48","48-60"),c("Stunted","Wasted","Underweight")))
mat[, "Stunted"] <- c(with(child_data, sum(stunted[age_of_child_month >= 0 & age_of_child_month <= 24]), na.rm = TRUE),
                      with(child_data, sum(stunted[age_of_child_month > 24 & age_of_child_month <= 36]), na.rm = TRUE),
                      with(child_data, sum(stunted[age_of_child_month > 36 & age_of_child_month <= 48]), na.rm = TRUE),
                      with(child_data, sum(stunted[age_of_child_month > 48 & age_of_child_month <= 60]), na.rm = TRUE))
mat[, "Wasted"] <- c(with(child_data, sum(wasted[age_of_child_month >= 0 & age_of_child_month <= 24]), na.rm = TRUE),
                     with(child_data, sum(wasted[age_of_child_month > 24 & age_of_child_month <= 36]), na.rm = TRUE),
                     with(child_data, sum(wasted[age_of_child_month > 36 & age_of_child_month <= 48]), na.rm = TRUE),
                     with(child_data, sum(wasted[age_of_child_month > 48 & age_of_child_month <= 60]), na.rm = TRUE))
mat[, "Underweight"] <- c(with(child_data, sum(underweight[age_of_child_month >= 0 & age_of_child_month <= 24]), na.rm = TRUE),
                          with(child_data, sum(underweight[age_of_child_month > 24 & age_of_child_month <= 36]), na.rm = TRUE),
                          with(child_data, sum(underweight[age_of_child_month > 36 & age_of_child_month <= 48]), na.rm = TRUE),
                          with(child_data, sum(underweight[age_of_child_month > 48 & age_of_child_month <= 60]), na.rm = TRUE))
jpeg("result/under-nutrition-stats.jpg")
barplot(t(mat), beside = TRUE, main = "Under-nutrition status based on age",
        col = c(2,3,4), legend.text = c("Stundted", "Wasted", "Underweight"),
        args.legend = list(x = "top"), xlab = "Child age group(months)", ylim = c(0,30))
dev.off()

# Plot under-nutrition against factors
jpeg("result/under-nutrition-result.jpg")
# Split plot window for multiple plots
layout(mat = matrix(c(1,2,3,4,5,6),nrow = 2,ncol = 3,byrow = TRUE),heights = c(0.5,0.5))
# barplot of under-nutrition against age_at_marriage
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_undernutrition, mother_data$age_at_marriage), 
        names.arg = c("< 18", "> 18"), beside = TRUE, 
        main = "Age at marriage", col = c("green","red"))
# barplot of under-nutrition against education-level
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_undernutrition, mother_data$education_level), 
        names.arg = c("None", "P", "S", "H"), beside = TRUE, 
        main = "Education level", col = c("green","red"))
# barplot of under-nutrition against mother_anaemic_preg
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_undernutrition, mother_data$mother_anaemic_preg), 
        names.arg = c("Yes", "No"), beside = TRUE, 
        main = "Anaemic during Preg.", col = c("green","red"))
# barplot of under-nutrition against avg_monthly_income
par(cex=0.7, mar=c(5,2,2,0.5)) #set margins
barplot(table(mother_data$child_undernutrition, mother_data$avg_monthly_income), 
        names.arg = c("<15", "15-50", "50-100", ">100"), beside = TRUE, 
        main = "Avg. Monthly Income(1000s)", col = c("green","red"))
# barplot of under-nutrition against anc_visit
par(cex=0.7, mar=c(5,2.5,2,0.5)) #set margins
barplot(table(mother_data$child_undernutrition, mother_data$anc_visit), 
        names.arg = c(1:8), beside = TRUE, 
        main = "ANC visits", col = c("green","red"))
# show common legend
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Normal", "Under-nutritioned"), 
       col = c("green","red"), lwd = 5, cex = 1)
dev.off()

# Plot stunted against factors
jpeg("result/stunted-result.jpg")
# Split plot window for multiple plots
layout(mat = matrix(c(1,2,3,4,5,6),nrow = 2,ncol = 3,byrow = TRUE),heights = c(0.5,0.5))
# barplot of stunted against age_at_marriage
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_stunted, mother_data$age_at_marriage), 
        names.arg = c("< 18", "> 18"), beside = TRUE, 
        main = "Age at marriage", col = c("orange", "red"))
# barplot of stunted against education-level
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_stunted, mother_data$education_level), 
        names.arg = c("None", "P", "S", "H"), beside = TRUE, 
        main = "Education level", col = c("orange", "red"))
# barplot of stunted against mother_anaemic_preg
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_stunted, mother_data$mother_anaemic_preg), 
        names.arg = c("Yes", "No"), beside = TRUE, 
        main = "Anaemic during Preg.", col = c("orange", "red"))
# barplot of stunted against avg_monthly_income
par(cex=0.7, mar=c(5,2,2,0.5)) #set margins
barplot(table(mother_data$child_stunted, mother_data$avg_monthly_income), 
        names.arg = c("<15", "15-50", "50-100", ">100"), beside = TRUE, 
        main = "Avg. Monthly Income(1000s)", col = c("orange", "red"))
# barplot of stunted against anc_visit
par(cex=0.7, mar=c(5,2.5,2,0.5)) #set margins
barplot(table(mother_data$child_stunted, mother_data$anc_visit), 
        names.arg = c(1:8), beside = TRUE, 
        main = "ANC visits", col = c("orange", "red"))
# show common legend
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Not stunted", "Stunted"), 
       col=c("orange", "red"), lwd=5, cex=1)
dev.off()

# Plot wasted against factors
jpeg("result/wasted-result.jpg")
# Split plot window for multiple plots
layout(mat = matrix(c(1,2,3,4,5,6),nrow = 2,ncol = 3,byrow = TRUE),heights = c(0.5,0.5))
# barplot of wasted against age_at_marriage
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_wasted, mother_data$age_at_marriage), 
        names.arg = c("< 18", "> 18"), beside = TRUE, 
        main = "Age at marriage", col = c("orange", "red"))
# barplot of wasted against education-level
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_wasted, mother_data$education_level), 
        names.arg = c("None", "P", "S", "H"), beside = TRUE, 
        main = "Education level", col = c("orange", "red"))
# barplot of wasted against mother_anaemic_preg
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_wasted, mother_data$mother_anaemic_preg), 
        names.arg = c("Yes", "No"), beside = TRUE, 
        main = "Anaemic during Preg.", col = c("orange", "red"))
# barplot of wasted against avg_monthly_income
par(cex=0.7, mar=c(5,2,2,0.5)) #set margins
barplot(table(mother_data$child_wasted, mother_data$avg_monthly_income), 
        names.arg = c("<15", "15-50", "50-100", ">100"), beside = TRUE, 
        main = "Avg. Monthly Income(1000s)", col = c("orange", "red"))
# barplot of wasted against anc_visit
par(cex=0.7, mar=c(5,2.5,2,0.5)) #set margins
barplot(table(mother_data$child_wasted, mother_data$anc_visit), 
        names.arg = c(1:8), beside = TRUE, 
        main = "ANC visits", col = c("orange", "red"))
# show common legend
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Not wasted", "Wasted"), 
       col=c("orange", "red"), lwd=5, cex=1)
dev.off()

# Plot underweight against factors
jpeg("result/underweight-result.jpg")
# Split plot window for multiple plots
layout(mat = matrix(c(1,2,3,4,5,6),nrow = 2,ncol = 3,byrow = TRUE),heights = c(0.5,0.5))
# barplot of underweight against age_at_marriage
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_underweight, mother_data$age_at_marriage), 
        names.arg = c("< 18", "> 18"), beside = TRUE, 
        main = "Age at marriage", col = c("orange", "red"))
# barplot of underweight against education-level
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_underweight, mother_data$education_level), 
        names.arg = c("None", "P", "S", "H"), beside = TRUE, 
        main = "Education level", col = c("orange", "red"))
# barplot of underweight against mother_anaemic_preg
par(cex=0.7, mar=c(3,2,2,1)) #set margins
barplot(table(mother_data$child_underweight, mother_data$mother_anaemic_preg), 
        names.arg = c("Yes", "No"), beside = TRUE, 
        main = "Anaemic during Preg.", col = c("orange", "red"))
# barplot of underweight against avg_monthly_income
par(cex=0.7, mar=c(5,2,2,0.5)) #set margins
barplot(table(mother_data$child_underweight, mother_data$avg_monthly_income), 
        names.arg = c("<15", "15-50", "50-100", ">100"), beside = TRUE, 
        main = "Avg. Monthly Income(1000s)", col = c("orange", "red"))
# barplot of underweight against anc_visit
par(cex=0.7, mar=c(5,2.5,2,0.5)) #set margins
barplot(table(mother_data$child_underweight, mother_data$anc_visit), 
        names.arg = c(1:8), beside = TRUE, 
        main = "ANC visits", col = c("orange", "red"))
# show common legend
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Not underweight", "Underweight"), 
       col=c("orange", "red"), lwd=5, cex=1)
dev.off()
