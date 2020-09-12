### Simple Linear Regression : Churn_out_rate ###
#Emp_data -> Build a prediction model for Churn_out_rate 

sh.cr <- read.csv("C:/PRATIK/Data Science/Assignment/Completed/Simple Linear Regression/emp_data.csv")
View(sh.cr)

# Scatter Diagram (Plot x,y)
plot(sh.cr$Salary_hike,sh.cr$Churn_out_rate)

# Other Exploratory data analysis and Plots
boxplot(sh.cr)

hist(sh.cr$Salary_hike)
hist(sh.cr$Churn_out_rate)
summary(sh.cr)

# Correlation coefficient value for Salary Hike and Churn_out_Date

cr <- sh.cr$Churn_out_rate
sh <- sh.cr$Salary_hike
cor(cr,sh)


# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = -0.9117216). 
# This has a strong negative Correlation


### Simple model without using any transformation
reg <- lm(cr~sh)
summary(reg)             #Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 
confint(reg,level = 0.95)
predict(reg,interval="predict")


# we may have to do transformation of variables for better R-squared value
# Applying transformations


### Logarthmic transformation
reg_log <- lm(cr~log(sh))
summary(reg_log)         #Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297 
confint(reg_log,level = 0.95)
predict(reg_log,interval="predict")

### Exponential model 
reg_exp <- lm(log(cr)~sh)
summary(reg_exp)         #Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8577 
confint(reg_exp,level = 0.95)
predict(reg_exp,interval="predict")

### Quadratic model
sh.cr[,"sh_sq"] = sh*sh
quad_mod <- lm(cr~sh+I(sh^2), data = sh.cr)
summary(quad_mod)       #Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9662 
confint(quad_mod,level = 0.95)
predict(quad_mod,interval="predict")

### Cubic model
poly_mod <- lm(cr~sh+I(sh^2)+I(sh^3), data = sh.cr)
summary(poly_mod)       #Multiple R-squared:  0.9893,	Adjusted R-squared:  0.984 
confint(poly_mod,level = 0.95)
predict(poly_mod,interval="predict")


model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.8101,0.8297,0.8577,0.9662,0.984)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)


# Cubic  model gives the best Adjusted R-Squared value
predicted_Value <- predict(poly_mod)
predicted_Value


Final <- cbind(Salary_Hike=sh.cr$Salary_hike,Churn_Rate = sh.cr$Churn_out_rate,Pred_Chr_rate=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-cr)^2))
rmse

plot(poly_mod)

hist(residuals(poly_mod)) # close to normal distribution
