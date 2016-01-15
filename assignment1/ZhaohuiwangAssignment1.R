firstName <- "Zhaohui"
lastName <- "Wang"

print(
  paste(
    firstName,
    lastName
  )
)

studentID <- "1461347"
print(studentID)

#Question 1 
#load files in R#
library(foreign)
df.dta<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")

df.csv<-read.csv(
  file=url("https://github.com/EconomiCurtis/econ294A_2015/raw/master/data/NHIS_2007_CSV.csv"))

df.td<-read.table(file=url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt"))
df.Rdata<-load("/Users/babe/Desktop/294A\ R\ /NHIS_2007_RData.RData")
View(NHIS_2007_RData)

#Question2
# df.dta is 188kb; df.csv is 139kb;df.Rdata is 45.3kb;df.ta is 139kb
#df.Rdata is the smallest 
#there will be different varibility given the different type of code.

#Question3
typeof(NHIS_2007_RData)
#list
class(NHIS_2007_RData)
#data frame
length(NHIS_2007_RData)
#9
dim(NHIS_2007_RData)
#4785 observations 9 number of the variables
nrow(NHIS_2007_RData)
#4785
ncol(NHIS_2007_RData)
#9
summary(NHIS_2007_RData)

#Question4
df<-read.dta(file=="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(df)
#1119754 observations and 30 variables
summary(d)
summary(d$rw)
summary(d$NAs)
#Question 5
x<- c(1,2,3,4,5,6,7,4,NULL,NA)
length(x)
#the length is 9 because NA is not inculded
mean(x,na.rm=TRUE)
#the mean without NA is 4

#Question 6
x<-matrix(
  c(1,2,3,4,5,6,7,8,9),
  nrow=3,
  ncol=3,
  byrow=T
)
x
t(x)
eigen(x)
y<-matrix(
  c(1,2,3,3,2,1,2,3,0),
  nrow=3,
  ncol=3,
  byrow=T
  )
y
y.inverse <-solve(y)
y.inverse %*%y

#Question 7
carat<-c(5,2,0.5,1.5,5,NA,3)
cut<-c('"fair"','"good"','"very good"','"good"','"fair"','"Ideal"','"fair"')
clarity<-c('"SI1"','"I1"','"VI1"','"VS1"','"IF"','"VVS2"','NA')
price<-c(850,450,450,NA,750,980,420)
diamonds <-data.frame(carat,cut,clarity,price)
View(diamonds)
#a
summary(diamonds)
#the mean price is 650
#b
diamonds1<-subset(diamonds,(cut=="fair"))
summary(diamonds1)
#the mean price of cut fair is 577.43
#c
diamonds2 <-subset(diamonds,cut=="good"|cut=="very good"|cut=="Ideal")
summary(diamonds2)
#the mean price is 470
#d
diamonds3 <-subset(diamonds,(carat>2))
diamonds4 <-subset(diamonds3,cut=="very good"|cut=="Ideal")
median(diamonds4$price)
#the median price is NA
