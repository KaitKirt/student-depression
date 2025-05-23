# ---------------------------------------------------------------
# Student Depression Analysis
# ---------------------------------------------------------------
# Dataset: student_depression_dataset.csv (found on Kaggle)
# Goal: Explore how financial stress, work/study hours, and dietary habits relate to depression
# ---------------------------------------------------------------

# ------------------------
# 1. Load and Inspect Data
# ------------------------

data <- read.csv("/Users/kaitlynkirt/Student Depression/student_depression_dataset.csv")
str(data) #this tells me the structure of the data
head(data) #this shows only the first few rows of the dataset
colSums(is.na(data)); #check if there are any 'NA' (missing) values 

# ------------------------
# 2. Data Cleaning & Preparation
# ------------------------

#need to convert the depression column to binary (Yes = 1, No = 0)
data$Depression <- as.factor(data$Depression)
table(data$Depression)

#making dietary habits ordered
unique(data$Dietary.Habits)
data$Dietary.Habits <- factor(data$Dietary.Habits,
                              levels = c('Healthy', 'Moderate', 'Unhealthy','Others'),
                              ordered = TRUE)

data_clean <- na.omit(data) #remove any rows with NA values

data_clean$Financial.Stress <- as.numeric(data_clean$Financial.Stress) #changes values in Financial Stress from characters to numbers

# ------------------------
# 3. Data Visualization
# ------------------------

library(ggplot2) 

#Dietary Habits vs Depression Plot
ggplot(data, aes(x=Dietary.Habits, fill=Depression)) +
  geom_bar(position = 'fill')+
  labs(y="Proportion", title="Dietary Restrictions vs Depression")

#Work Study Hours vs Depression
ggplot(data, aes(x=Depression, y=Work.Study.Hours))+
  geom_boxplot() +
  labs(title="Work Study Hours vs Depression")

#Financial Stress vs Depression
ggplot(data_clean, aes(x=Depression, y=Financial.Stress)) + 
  geom_boxplot() + 
  labs(title="Financial Stress vs Depression")

# ------------------------
# 4. Statistical Testing: T-Tests
# ------------------------

#subsetting for depression groups
yesvalues_fs <- data_clean[data_clean$Depression == 1, "Financial.Stress"]
novalues_fs <- data_clean[data_clean$Depression == 0, "Financial.Stress"]

t.test(yesvalues_fs, novalues_fs) #test statistic is high, means huge difference between yes and no
#p value is very small (reject null), which means very statistically significant
#the mean of x and the mean of y are fairly different

yesvalues_wsh <- data_clean[data_clean$Depression == 1, "Work.Study.Hours"]
novalues_wsh <- data_clean[data_clean$Depression == 0, "Work.Study.Hours"]

t.test(yesvalues_wsh, novalues_wsh) #test statistic is also very high but not as high as financial stress
#reject null hypothesis (significantly significant)
#the mean of x is larger than the mean of y but not by much

# ------------------------
# 5. Predictive Modeling: Logistic Regression
# ------------------------

#predictive modeling test
model <- glm(Depression ~ Dietary.Habits + Work.Study.Hours + Financial.Stress,
             data = data_clean, 
             family = "binomial")
summary(model) #tells me that as work study hours and financial stress increase, so does depression

# ------------------------
# 6. Additional Exploratory Plots
# ------------------------

#Scatter Plot of Work Study Hours vs Financial Stress
plot(data_clean$Work.Study.Hours , data_clean$Financial.Stress,
     xlab = "Work Study Hours", ylab = "Financial Stress",
     main = "Work Study Hours vs. Financial Stress")

#Q-Q Plots to showcase normality
qqnorm(data_clean$Work.Study.Hours,
       main = "Q-Q Plot of Work Study Hours")
qqline(data_clean$Work.Study.Hours, col = "red")

qqnorm(data_clean$Financial.Stress,
       main = "Q-Q Plot of Financial Stress")
qqline(data_clean$Financial.Stress, col="green")
       
#Q-Q Plot to compare Work Study Hours and Financial Stress
qqplot(data_clean$Work.Study.Hours, data_clean$Financial.Stress,
       xlab = "Work Study Hours", ylab = "Financial Stress", 
       main = "QQ-Plot for Comparison between Work and Finances")
       
       