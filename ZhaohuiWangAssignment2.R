#Econ 294A
author :"Zhaohui Wang"
data:"winter 2016"
assignment:""

###################
#Quesiong 0
ZhaohuiWangAssignment2 <- list(
  firstName ="Zhaohui",
  lastName="Wang",
  email = "zwang98@ucsc.edu",
  studentID = 1461347
)

######
#Question 1
diamonds <- get(  
  load(
    file = url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")
  )
)
###load R.data
ZhaohuiWangAssignment2$s1a <- nrow(diamonds)
ZhaohuiWangAssignment2$s1b <- ncol(diamonds)
ZhaohuiWangAssignment2$s1c <- names(diamonds)
ZhaohuiWangAssignment2$s1d <-summary(price)

#Question 2
#load data 
NHIS_2007_TSV<- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",
             sep="\t",
             header=T
  )
ZhaohuiWangAssignment2$s2a <- nrow(NHIS_2007_TSV)
ZhaohuiWangAssignment2$s2b <- ncol(NHIS_2007_TSV)
ZhaohuiWangAssignment2$s2c <- colnames(NHIS_2007_TSV)
ZhaohuiWangAssignment2$s2d <- mean(NHIS_2007_TSV[["weight"]])
ZhaohuiWangAssignment2$s2e <- median(NHIS_2007_TSV[["weight"]])
#draw a histogram with the weights. Noticing that there is a gap may be contributed to missing values.
graph <-NHIS_2007_TSV$weight
hist(graph,right = FALSE)
table(graph)
#create a new column setting these weight observations to NA.
NHIS_2007_TSV$weight2<- ifelse(test = NHIS_2007_TSV$weight<996 | NHIS_2007_TSV$weight>999,
                          yes = NHIS_2007_TSV$weight,
                          no = NA)
View(NHIS_2007_TSV)
# open the data we can see there is a new column called weight2 and all the outliner numbers are set to 0
ZhaohuiWangAssignment2$s2f <- mean(NHIS_2007_TSV[["weight2"]],na.rm = T)
ZhaohuiWangAssignment2$s2g <- median(NHIS_2007_TSV[["weight2"]],na.rm = T)
#summary for men
sub.m<- subset(NHIS_2007_TSV,(SEX==1))
sub.w <- subset(NHIS_2007_TSV,(SEX==2))
ZhaohuiWangAssignment2$s2h <- summary(sub.w[["weight2"]])
#summary for women
ZhaohuiWangAssignment2$s2i <- summary(sub.m[["weight2"]])

##Quesion3
#extract even index for a vector 
vec <- c(letters,LETTERS)
ZhaohuiWangAssignment2$s3a <- vec[1:26*2]
ZhaohuiWangAssignment2$s3b <- paste(vec[c(52,8,1)],collapse = "...")
arr <- array(c(letters,LETTERS), dim=c(3,3,3))
View(arr)
ZhaohuiWangAssignment2$s3c <- arr[,1,2]
ZhaohuiWangAssignment2$s3d <- c(arr[2,2,1],arr[2,2,2],arr[2,2,3])
ZhaohuiWangAssignment2$s3e <- paste(arr[2,3,3],arr[2,3,1],arr[1,1,1],sep = "")

