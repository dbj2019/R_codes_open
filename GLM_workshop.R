

######################################Experiment - 1 #################
###install.packages("openxlsx")
###install.packages("car")

#####Load the required library
library(openxlsx)
library(car)
library(emmeans)
library(MASS)
library(lme4)
library(car)


data <- read.xlsx("datasheet.xlsx",
                  sheet ="wing_length_1" )

wing_length_1 <-read_excel("datasheet.xlsx",
                   sheet ="wing_length_1")

####summary data
summary(data)

######structure of the data
str(data)


# ##Q:- Is ave of wing length diff between males and females
# ##Null Hypo:- Ave wing length fo males and females is equal
###Alternate hyp:-Ave wing length fo males and females is not equal
#########Discrete independent variable are called as factors
###In R you need to inform R what is factor
data$Sex<- as.factor(data$Sex)

str(data)
####Generalized linear model (GLM)

####Have an idea about your data 
####your response variable will determine based on the distribution of the data
model<-glm(data=data,wing_length~Sex,family = gaussian(link = "identity"))


###Calculation of the p values
anova_fit<- Anova(model,type = "III",test.statistic = "Wald")
print(anova_fit)
######################################Experiment - 1 #################


####################################Experiment - 2 ###########


data<- read.xlsx("datasheet.xlsx",sheet = "wing_length_2")
data$Food <- as.factor(data$Food)
model<- glm(wing_length~Food,family = gaussian(link = "identity"),
            data=data)
anova_fit <-Anova (model,type="III",test.statistic = "Wald")
print(anova_fit)
#######Post-hoc analysis post-hoc pairwise comparison
install.packages("emmeans")
library(emmeans)
tukey_results <- emmeans(model, pairwise ~ Food,data=data,adjust="tukey")
summary(tukey_results)$contrasts

#############Plot
plot(data$Food,data$wing_length,xlab="food_regimes",ylab="wing_length(mm)")

####################End of the Experiment -2 ##############

###############Start of the Experiment -3  ###############
data<- read.xlsx("datasheet.xlsx",sheet = "wing_length_3")
data$Food <- as.factor(data$Food)
data$Sex <- as.factor(data$Sex)
model<- glm(wing_length ~ Food + Sex + Food:Sex,
            family = gaussian(link = "identity"),
            data=data)
anova_fit <-Anova (model,type="III",test.statistic = "LR")
print(anova_fit)

########### End of Experiment -3 #############
############Star of the experiment- 4########
install.packages("MASS")
library(MASS)
data<- read.xlsx("datasheet.xlsx",sheet = "activity")
data$Food <- as.factor(data$Food)
data$Sex <- as.factor(data$Sex)
model<- glm.nb(Activity_counts~Food*Sex,link = "log",
            data=data)
anova_fit <-Anova (model,type="III",test.statistic = "LR")
print(anova_fit)
#############End of experiment -4#########
############Start of the experiment-5

data<- read.xlsx("datasheet.xlsx",sheet = "eye_colour")
data$Food <- as.factor(data$Food)
data$Sex <- as.factor(data$Sex)
model<- glm.nb(Activity_counts~Food*Sex,link = "log",
               data=data)
anova_fit <-Anova (model,type="III",test.statistic = "LR")
print(anova_fit)

############Experiment- 6 #########






#################Generalized Linear Mixed Models(GLMM)
install.packages("lme4")
library(lme4)
library(car)
data<- read.xlsx("datasheet.xlsx",sheet = "dev_time")
data$Treatment <- as.factor(data$Treatment)
data$Block <- as.factor(data$Block)
model_1<- glmer(Dev_time ~Treatment +(1|Block),family = gaussian(link = "identity"),
               data=data)
anova_fit <-Anova (model_1,type="III",test.statistic = "F")
print(anova_fit)

###Model 1 and 2 comparison
model_2<- glmer(Dev_time ~Treatment +(1+Treatment|Block),family = gaussian(link = "identity"),
              data=data)
comparison<-anova(model_1,model_2)



#############Experiment 7

data<- read.xlsx("datasheet.xlsx",sheet = "social_space")
data$Treatment <- as.factor(data$Treatment)
data$Replicate <- as.factor(data$Replicate)
data$Time <- as.factor(data$Time)

model<-glmer(Nearest_Neighbour_Distance~Treatment*Time+(1|Replicate),
             family = Gamma(link = "log"),data = data)
anova_fit <-Anova (model,type="III",test.statistic = "F")
print(anova_fit)
