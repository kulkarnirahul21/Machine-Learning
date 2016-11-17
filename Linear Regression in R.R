# 4 conditions for linear regression

# Statistical co-relation - done
# Linear relationship - done
# Homoscedasticity - done
# Normality - done

rm(list=ls(all=TRUE))
# we will use a builtin dataset
str(state.x77) # this is not a dataframe
data = as.data.frame(state.x77)
summary(data)
str(data)
colnames(data)
# Data cleaning
colnames(data)[4] = "LifeExp"                   # no spaces in variable names, please
colnames(data)[6] = "HSGrad" 
head(data)
apply(data,2,min)
apply(data,2,max)
apply(data,2,mean)
#
cor(data)                              # correlation matrix (not shown, yet)
round(cor(data), 3)
pairs(data) # Plot correlation matrix.
####################################################################################
# Linear Regression
model1 = lm(LifeExp ~ ., data=data)
summary(model1)
# Linearity check
# A large p-value (> 0.05) indicates weak evidence against the null hypothesis, so you fail to reject the null hypothesis.
# p-values very close to the cutoff (0.05) are considered to be marginal (could go either way). Always report the p-value so your readers can draw their own conclusions.
######################################################################################
model2 = update(model1, .~. -Area)
summary(model2)
# Here Adj-Rsquare value came down and the R square came up -- still insinificant
# ANOVA
anova(model2, model1)
# check the p-alue 0.9649 ...emoving Area has no significance.
####################################################################################
model3 = update(model2, .~. -Illiteracy)
summary(model3)
###############################################################################
model4 = update(model3, .~. -Income)
summary(model4)
# Both Adjusted R square and R square values increased.
###################################################################################
model5 = update(model4, .~. -Density)
summary(model5)
anova(model5, model4)
anova(model5, model1)
###################################################################################
model.int = update(model5, .~.^3) # interaction effect
summary(model.int)


# Stepwise regression
step(model1, direction="backward")

# Prediction Life expentency
predict(model1, list(Population=2000, Murder=10.5, HSGrad=48, Frost=100, income =2))

# Regression diagnostic
par(mfrow=c(2,2))                    # visualize all four graphs at once
plot(model5)
par(mfrow=c(1,1)) 

# Upper left - is a standard residuals plot with which we can evaluate linearity (looks good)
# and homoscedasticity (also good)

# Upper right - is a normal quantile plot of the residuals, which should be normally distributed. 
# We have a little deviation from normality in the tails, but it's not too bad

# The scale-location plot is supposedly a more sensitive way to look for heteroscedasticity, 
# but I never found it very useful, to be honest

# The last plot shows possible high leverage and influential cases.


# Extracting Model elements from the models

names(model5)
model5$coefficients  
model5$resid

