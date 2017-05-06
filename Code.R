# import data from servo.data file 
servo <- read.table("servo.data", header=FALSE, sep=",")
servo

# Attribute Information:
# 1. motor: A,B,C,D,E 
# 2. screw: A,B,C,D,E 
# 3. pgain: 3,4,5,6 
# 4. vgain: 1,2,3,4,5 
# 5. class: 0.13 to 7.10

# renaming columns accordig to attributes
names(servo) <- c("motor", "screw", "pgain", "vgain", "class")
servo

# verification of attributes 
levels(servo$motor)
levels(servo$screw)
unique(servo$pgain)
unique(servo$vgain)

# checking if class values are in range of 0.13 and 7.10
check = 1
for(i in servo$class){
	if(i >= 0.13 && i <= 7.10){
		check = check + 1	
	}
}
print(paste("all ", check, " values are in range"))

# Correlation Check
with(servo, cor(screw, class))
with(servo, cor(motor, class))
with(servo, cor(pgain, class))
with(servo, cor(vgain, class))

servo.data = as.data.frame.matrix(servo)
servo.data

for (i in 1:nrow(servo.data)){
	if(servo.data$motor[i] == "A"){
		servo.data$motor[i] <- as.numeric(1)
	}else if(servo.data$motor[i] == "B"){
		servo.data$motor[i] <- as.numeric(2)
	}else if(servo.data$motor[i] == "C"){
		servo.data$motor[i] <- as.numeric(3)
	}else if(servo.data$motor[i] == "D"){
		servo.data$motor[i] <- as.numeric(4)
	}else if(servo.data$motor[i] == "E"){
		servo.data$motor[i] <- as.numeric(5)
	}
}
servo.data$motor
for (i in 1:nrow(servo.data)){
	if(servo.data$screw[i] == "A"){
		servo.data$screw[i] <- 1
	}else if(servo.data$screw[i] == "B"){
		servo.data$screw[i] <- 2
	}else if(servo.data$screw[i] == "C"){
		servo.data$screw[i] <- 3
	}else if(servo.data$screw[i] == "D"){
		servo.data$screw[i] <- 4
	}else if(servo.data$screw[i] == "E"){
		servo.data$screw[i] <- 5
	}
}


servo.data$screw
class(servo.data$motor) <- "numeric"
class(servo.data$screw) <- "numeric"

servo$screw = servo
nrow(servo.data)



# regression btw pgain and vgain
lm.out = lm(class ~ pgain + vgain + motor + screw , data=servo.data)
lm.out
summary(lm.out)

# newdata = data.frame(motor = "A", screw = "A", pgain = 6, vgain = 5)

class.predict = predict(lm.out, newdata=subset(servo.data, select=c(-class)))
class.predict

servo.data = as.data.frame.matrix(servo) 
servo.data$predict = class.predict
servo.data









actual <- servo.data$class
actual

predicted <- servo.data$predict
predicted

error <- actual - predicted
error

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
    sqrt(mean(error^2))
}
 
# Function that returns Mean Absolute Error
mae <- function(error)
{
    mean(abs(error))
}

mae(error)
rmse(error)
