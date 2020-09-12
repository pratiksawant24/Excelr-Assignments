### Simple Linear Regression : Salary Hike ###
#Salary_hike -> Build a prediction model for Salary_hike

ye.sh <- read.csv("C:/PRATIK/Data Science/Assignment/Completed/Simple Linear Regression/Salary_Data.csv")
View(ye.sh)

# Scatter Diagram (Plot x,y)
plot(ye.sh$YearsExperience,ye.sh$Salary)

# Other Exploratory data analysis and Plots
boxplot(ye.sh)
hist(ye.sh$YearsExperience)
hist(ye.sh$Salary)
summary(ye.sh)

# Correlation coefficient value for Years of Experience and Employee Salary Hike
ye <- ye.sh$YearsExperience
sh <- ye.sh$Salary
cor(ye,sh)          # correlation Coeff is 0.9782416

# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.9782416). 
# This has a strong Positive Correlation 

### Simple model without using any transformation
reg <- lm(sh~ye)
summary(reg)       #Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554
confint(reg,level = 0.95)
predict(reg,interval="predict")

### Logarthmic transformation
reg_log <- lm(sh~log(ye))
summary(reg_log)   #Multiple R-squared:  0.8539,	Adjusted R-squared:  0.8487 
confint(reg_log,level = 0.95)
predict(reg_log,interval="predict")

### Exponential model 
reg_exp <- lm(log(sh)~ye)
summary(reg_exp)   #Multiple R-squared:  0.932,	Adjusted R-squared:  0.9295 
confint(reg_exp,level = 0.95)
predict(reg_exp,interval="predict")

### Quadratic model
ye.sh[,"ye_sq"] = ye*ye
quad_mod <- lm(sh~ye+I(ye^2),data=ye.sh)
summary(quad_mod)  #Multiple R-squared:  0.957,	Adjusted R-squared:  0.9538 
confint(quad_mod,level = 0.95)
predict(quad_mod,interval="predict")

### Quadratic model
poly_mod <- lm(sh~ye+I(ye^2)+I(ye^3),data=ye.sh)
summary(poly_mod) #Multiple R-squared:  0.9636,	Adjusted R-squared:  0.9594 
confint(poly_mod,level = 0.95)
predict(poly_mod,interval="predict")

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.9554,0.8487,0.9295,0.9538,0.9594)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)

# Cubic  model gives the best Adjusted R-Squared value
predicted_Value <- predict(poly_mod)
predicted_Value


Final <- cbind(YearsofExp=ye.sh$YearsExperience,Sal_Hike = ye.sh$Salary,Pred_sal_hike=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-sh)^2))
rmse

plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution
