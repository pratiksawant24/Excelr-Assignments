### Simple Linear Regression : Delivery Time ###
# Delivery_time -> Predict delivery time using sorting time 


dl.tm <- read.csv("C:/PRATIK/Data Science/Assignment/Completed/Simple Linear Regression/delivery_time.csv")
View(dl.tm)
dt.st<-dl.tm
View(dt.st)
attach(dt.st)

# Scatter Diagram (Plot x,y)
plot(dt.st$Sorting.Time,dt.st$Delivery.Time)

hist(dt.st$Delivery.Time)
hist(dt.st$Sorting.Time)

boxplot(dt.st)
summary(dt.st)


# Correlation coefficient value for Delivery Time and Sorting Time
dt<- dt.st$Delivery.Time
st<- dt.st$Sorting.Time
cor(dt,st)

# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.8259973). 
# This has a moderate Correlation 

# Simple model without using any transformation
reg <- lm(dt~st)
summary(reg)

# Probability value should be less than 0.05(0.00115)
# The multiple-R-Squared Value is 0.6823 which is lesser than 0.8(In General)
# Adjusted R-Squared Value is 0.6655 
# The Probability Value for F-Statistic is 3.983e-06(Overall Probability Model is also less than 0.05)

confint(reg,level = 0.95)
predict(reg,interval="predict")

# predict(reg,type="prediction")
# Adjusted R-squared value for the above model is 0.6655 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

### Logarthmic transformation
reg_log <- lm(dt~log(st))    # Regression using logarthmic transformation
summary(reg_log)             # Multiple R-squared:  0.6954,	Adjusted R-squared:  0.6794 
confint(reg_log,level = 0.95)
predict(reg_log,interval="predict")

### Exponential model
reg_exp <- lm(log(dt)~st)
summary(reg_exp)             #Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957 
confint(reg_exp,level = 0.95)
predict(reg_exp,interval="predict")

### Quadratic model
dt.st[,"st_sq"] = st*st
quad_mod <- lm(dt~st+I(st^2),data = dt.st)
summary(quad_mod)            #Multiple R-squared:  0.6934,	Adjusted R-squared:  0.6594 
confint(quad_mod,level = 0.95)
predict(quad_mod,interval="predict")

### Cubic model
poly_mod <- lm(dt~st+I(st^2)+I(st^3),data = dt.st)
summary(poly_mod)            #Multiple R-squared:  0.7034,	Adjusted R-squared:  0.6511 
confint(poly_mod,level = 0.95)
predict(poly_mod,interval="predict")

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.6655,0.6794,0.6957,0.6594,0.6511)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)


# Exponential  model gives the best Adjusted R-Squared value
predicted_Value <- exp(predict(reg_exp))
predicted_Value

Final <- cbind(Sorting_Time=dt.st$Sorting.Time ,Delivery_Time = dt.st$Delivery.Time,Predicted_Delivery_time=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-dt)^2))
rmse

plot(reg_exp)

hist(residuals(reg_exp)) # close to normal distribution

