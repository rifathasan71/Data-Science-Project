projectData<- read.csv("C:/Users/User/Downloads/Dataset(Updated)_MIdterm_sectoin(C).csv", header = TRUE, sep=",")
print(projectData)


sum(is.na(projectData))
colSums(is.na(projectData))

projectData[projectData == ""] <- NA
colSums(is.na(projectData))


data_nomissing <- na.omit(projectData)

colSums(is.na(data_nomissing))
print(data_nomissing)


names(projectData)[names(projectData) == "pressurehight"] <- "pressurehigh"
print(projectData)

data_imputation <- projectData

mean_age <- mean(data_imputation$age, na.rm = TRUE)
data_imputation$age[is.na(data_imputation$age)] <- mean_age

mean_impulse <- mean(data_imputation$impluse, na.rm = TRUE)
data_imputation$impluse[is.na(data_imputation$impluse)] <- mean_impulse

mean_pressureH <- mean(data_imputation$pressurehigh, na.rm = TRUE)
data_imputation$pressurehigh[is.na(data_imputation$pressurehigh)] <- mean_pressureH

mean_pressureL <- mean(data_imputation$pressurelow, na.rm = TRUE)
data_imputation$pressurelow[is.na(data_imputation$pressurelow)] <- mean_pressureL


median_age <- median(data_imputation$age, na.rm = TRUE)
data_imputation$age[is.na(data_imputation$age)] <- median_age

median_impulse <- median(data_imputation$impluse, na.rm = TRUE)
data_imputation$impluse[is.na(data_imputation$impluse)] <- median_impulse

median_pressureH <- median(data_imputation$pressurehigh, na.rm = TRUE)
data_imputation$pressurehigh[is.na(data_imputation$pressurehigh)] <- median_pressureH

median_pressureL <- median(data_imputation$pressurelow, na.rm = TRUE)
data_imputation$pressurelow[is.na(data_imputation$pressurelow)] <- median_pressureL

colSums(is.na(data_imputation))

get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_gender <- get_mode(data_imputation$gender)
data_imputation$gender[is.na(data_imputation$gender)] <- mode_gender


mode_glucose <- get_mode(data_imputation$glucose)
data_imputation$glucose[is.na(data_imputation$glucose)] <- mode_glucose

unique(data_imputation$gender)


data_imputation$gender[data_imputation$gender == "femalee"] <- "female"

data_imputation$gender[data_imputation$gender == "malee"] <- "male"

colSums(is.na(data_imputation))





library(naniar)

vis_miss(projectData)
gg_miss_var(projectData)
gg_miss_upset(projectData)





data_no_outlier <- data_nomissing

cap_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  df[[column]][df[[column]] < lower] <- lower
  df[[column]][df[[column]] > upper] <- upper
  
  return(df)
}

data_no_outlier <- cap_outliers(data_no_outlier, "age")
data_no_outlier <- cap_outliers(data_no_outlier, "impluse")
data_no_outlier <- cap_outliers(data_no_outlier, "pressurehigh")
data_no_outlier <- cap_outliers(data_no_outlier, "pressurelow")





data_nomissing$gender <- factor(data_nomissing$gender,levels = c("female", "male"),labels=c(0, 1))

data_nomissing$gender <- factor(data_nomissing$gender,labels = c("female", "male"),levels=c(0, 1))

data_nomissing$glucose <- factor(data_nomissing$glucose,levels = c("Low", "High"),labels=c(0, 1))

data_nomissing$glucose <- factor(data_nomissing$glucose,labels = c("Low", "High"),levels=c(0, 1))



duplicated(data_nomissing)
data_no_duplicate <- unique(data_nomissing)
duplicated(data_no_duplicate)




data_male <- data_imputation %>% filter(gender == "male")

data_female <- data_imputation %>% filter(gender == "female")

data_under_50 <- data_imputation %>% filter(age < 50)

data_40_60 <- data_imputation %>% filter(age >= 40 & age <= 60)

data_high_bp <- data_imputation %>% filter(pressurehigh > 140)

data_positive <- data_imputation %>% filter(class == "positive")

filtered_data <- data_imputation %>% filter(gender == "male", age > 60, pressurehigh > 140)



install.packages("ROSE")
install.packages("dplyr")
library(ROSE)
library(dplyr)


minority <- data_imputation %>% filter(class == "negative")
majority <- data_imputation %>% filter(class == "positive")

cat("Minority size:", nrow(minority), "\n")
cat("Majority size:", nrow(majority), "\n")

set.seed(123)
minority_oversampled <- minority[sample(nrow(minority), nrow(majority), replace = TRUE), ]
data_oversampled <- bind_rows(majority, minority_oversampled)
data_oversampled <- data_oversampled[sample(nrow(data_oversampled)), ]
table(data_oversampled$class)




minority <- data_imputation %>% filter(class == "negative")
majority <- data_imputation %>% filter(class == "positive")

set.seed(123)
majority_undersampled <- majority[sample(nrow(majority), nrow(minority)), ]
data_undersampled <- bind_rows(minority, majority_undersampled)
data_undersampled <- data_undersampled[sample(nrow(data_undersampled)), ]
table(data_undersampled$class)




install.packages("caTools")
library(caTools)

set.seed(123)

split <- sample.split(data_imputation$class, SplitRatio = 0.7)

train_data <- subset(data_imputation, split == TRUE)
test_data <- subset(data_imputation, split == FALSE)

cat("Training size:", nrow(train_data), "\n")
cat("Testing size:", nrow(test_data), "\n")

table(train_data$class)
table(test_data$class)




boxplot(data_nomissing$age, data_no_outlier$age, main = "Age")

boxplot(data_nomissing$impluse, data_no_outlier$impluse, main = "Impluse")

boxplot(data_nomissing$pressurehigh, data_no_outlier$pressurehigh, main = "Pressurehigh")

boxplot(data_nomissing$pressurelow, data_no_outlier$pressurelow, main = "Pressurelow")





male_sub <- subset(data_no_outlier, data_no_outlier$gender == "male")
male_df <- male_sub[c(2,1)]
mean_male <- mean(male_df$age)
median_male <- median(male_df$age)
mod_male <- get_mode(male_df$age)


female_sub <- subset(data_no_outlier, data_no_outlier$gender == "female")
female_df <- female_sub[c(2,1)]
mean_female <- mean(female_df$age)
median_female <- median(female_df$age)
mod_female <- get_mode(female_df$age)


new_mat <- matrix(c(mean_male, median_male, mod_male, mean_female, median_female, mod_female),
                  nrow = 2, ncol = 3, byrow = TRUE)
rownames(new_mat) <- c("Male", "Female")
colnames(new_mat) <- c("Mean", "Median", "Mode")
print(new_mat)






high_sub <- subset(data_no_outlier, data_no_outlier$glucose == "High")
high_df <- high_sub[c(6,1)]
mean_High <- mean(high_df$age)
median_High <- median(high_df$age)
mod_High <- get_mode(high_df$age)

low_sub <- subset(data_no_outlier, data_no_outlier$glucose == "Low")
low_df <- low_sub[c(6,1)]
mean_Low <- mean(low_df$age)
median_Low <- median(low_df$age)
mod_Low <- get_mode(low_df$age)

new_mat <- matrix(c(mean_High, median_High, mod_High, mean_Low, median_Low, mod_Low),
                  nrow = 2, ncol = 3, byrow = TRUE)
rownames(new_mat) <- c("High", "Low")
colnames(new_mat) <- c("Mean", "Median", "Mode")
print(new_mat)





male_sub_2 <- subset(data_no_outlier, data_no_outlier$gender == "male")
male_df_2 <- male_sub_2[c(2,1)]
male_range <- round(max(male_df_2$age) - min(male_df_2$age))
male_var <- round(var(male_df_2$age))
male_sd <- round(sd(male_df_2$age))
male_IQR <- round(IQR(male_df_2$age))

female_sub_2 <- subset(data_no_outlier, data_no_outlier$gender == "female")
female_df_2 <- female_sub_2[c(2,1)]
female_range <- round(max(female_df_2$age) - min(female_df_2$age))
female_var <- round(var(female_df_2$age))
female_sd <- round(sd(female_df_2$age))
fmale_IQR <- round(IQR(female_df_2$age))

mat_com <- matrix(c(male_range, male_var, male_sd, male_IQR, female_range, female_var, female_sd, fmale_IQR),
                  nrow = 2, ncol = 4, byrow = TRUE)
rownames(mat_com) <- c("Male", "Female")
colnames(mat_com) <- c("Range", "IQR", "Variance", "Standard Deviation")
print(mat_com)

