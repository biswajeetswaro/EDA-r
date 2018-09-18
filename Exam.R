#Importing Data
motor_cars <- read.csv(file.choose(), header = T, stringsAsFactors = F)
names(motor_cars)[9] <- c("c_name")
new_car <- read.csv(file.choose(), header = T, stringsAsFactors = F)

#Converting MPG into km/lt
#formula is multiply km/lt = miles per gallon * 0.354
motor_cars$mpg <- motor_cars$mpg*0.354
names(motor_cars)[1]<- c("km/l")

#top 5 acceleration cars
library(dplyr)
top_5<- arrange(motor_cars, desc(motor_cars$acceleration))
top_5 <- top_5[1:5,]
top_5 <- select(top_5, c_name, acceleration)

#HP,CC,CYL
hcc <- select(motor_cars, horsepower, displacement, cylinders)
summary(hcc)
library(ggplot2)
ggplot(data = hcc)+geom_point(mapping=aes(x = hcc$displacement, y = hcc$horsepower))+facet_wrap(~hcc$cylinders)

#Since I converted mpg to km/lt. my na values would be in km/lt only.
library(mice)
md.pattern(motor_cars)

#visualize missing values
library(naniar)
vis_miss(motor_cars)

#Using MICE
temp <- mice(motor_cars, m = 5, maxit = 50, method = "pmm", seed = 300)
temp$imp$`km/lt`
completed <- complete(temp, 5)

#Manual calculation
mtcar1 <- subset(motor_cars, motor_cars$cylinders == 6)
range(mtcar1$`km/l`, na.rm = T)
ggplot(data = mtcar1)+geom_point(mapping = aes(x = mtcar1$`km/l`, y = mtcar1$weight))
ggplot(data = mtcar1)+geom_point(mapping = aes(x = mtcar1$`km/l`, y = mtcar1$horsepower))
#As weight increases the km/l decreases since it has a decent weight i could give it a value around 7
#When hp decreases the km/lt increased
mtcar2 <- subset(motor_cars, motor_cars$cylinders == 8)
range(mtcar2$`km/l`, na.rm = T)
ggplot(data = mtcar2)+geom_point(mapping = aes(x = mtcar2$`km/l`, y = mtcar2$weight))
ggplot(data = mtcar2)+geom_point(mapping = aes(x = mtcar2$`km/l`, y = mtcar2$horsepower))
#I can place value around 6.5 for the above mentioned reason
mtcar3 <- subset(motor_cars, motor_cars$cyl == 4)
range(mtcar3$`km/l`, na.rm = T)
ggplot(data = mtcar3)+geom_point(mapping = aes(x = mtcar3$`km/l`, y = mtcar3$wt))
ggplot(data = mtcar3)+geom_point(mapping = aes(x = mtcar3$`km/l`, y = mtcar3$horsepower))
##I can place value around 7 for the above mentioned reason


#Indian Market
#Since the family is large they prefer large size cars
#Since they have savings mindset they prefer the cars with good milege
#Since the the trafic is hige they prefer Automatic cars.
#Good HP can be an added advantage

#Looking for outliers in Milege
f <- boxplot(new_car$highway.MPG, plot = F)$out
g <- boxplot(new_car$city.mpg, plot = F)$out

#Replacing It with maximum acceptable values
new_car$city.mpg[new_car$city.mpg > 32] <- 32
new_car$highway.MPG[new_car$highway.MPG > 45] <- 45

summary(new_car$city.mpg)
summary(new_car$highway.MPG)


auto_car <- subset(new_car, new_car$Transmission.Type == "AUTOMATIC")

#on summarizing I could see the maximum milege is still the same
summary(auto_car$highway.MPG)
summary(auto_car$city.mpg)

lar_car <- subset(auto_car, auto_car$Vehicle.Size == "Large")

#Milege dropped a bit but still we can hold on to it
summary(lar_car$city.mpg)
summary(lar_car$highway.MPG)

#The maximum milege remains the same
four_door <- subset(lar_car, lar_car$Number.of.Doors == 4)
summary(four_door$highway.MPG)
summary(four_door$city.mpg)

#Got the cars which has decent highway milege
pre <- subset(four_door, four_door$highway.MPG>20)
summary(pre$city.mpg)

#Top 5 makes that should suit for Indian Market
final <- subset(pre, pre$city.mpg > 15)
final1 <- group_by(final, final$Make)
final2 <- summarise(final1, mean_hp = mean(Engine.HP))
final2 <- arrange(final2, desc(final2$mean_hp))

#Maserati
#BMW
#Lexus
#Infiniti
#GMC 
#The above may be the 5 cars which is well-suited for Indian market

#Finding Categories
#We can categorize the dataset from make
#sub-category of make is model
#we can categorize into cylinders and transmission type
make_c <- table(new_car$Make)
model_c <- table(new_car$Model)

#Totally 48 Makes available in a dataset
d <- group_by(new_car, new_car$Make)
s <- summarise(d, len = length(Model))

#915 Models are in the dataset
e <- group_by(new_car, new_car$Model)
w <- summarise(e, len = length(Make))
nrow(w)

#cylinder vs transmission type
cylvstrans <- table(new_car$Engine.Cylinders,new_car$Transmission.Type)

#size and driven_wheels
svswh <- xtabs(~new_car$Vehicle.Size+new_car$Driven_Wheels)

#We can categorise and sub-categorise the dataset using 7 variables
#where first comes the make and the make is subdivided into model and it goes on
#I try to find a mean of hp on each category
cat <- group_by(new_car, new_car$Make, new_car$Model, new_car$Engine.Cylinders, new_car$Transmission.Type, new_car$Driven_Wheels, new_car$Number.of.Doors, new_car$Vehicle.Size)
cat1 <- summarise(cat, mean = mean(Engine.HP)) 
