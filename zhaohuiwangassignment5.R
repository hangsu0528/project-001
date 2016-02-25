#econ294 assignment5
print("zhaohuiwang")
print("zwang98@ucsc.edu")
studentID<-1461347
print(studentID)
#question 1 
#part a 
#load ggplot2 
install.packages("ggplot2")
library(ggplot2)
data(diamonds)
graph1<-ggplot(diamonds,
               aes(x=x*y*z,
                   y=price,
                   color=clarity)
               )
graph1+geom_point(aes(color=clarity))+geom_point(aes(size=carat))+scale_x_log10()+scale_y_log10()
#part b 

graph2<-ggplot(diamonds,aes(carat,fill=clarity, ..density..))
graph2+geom_histogram()+facet_grid(cut~.)
#part c

graph3<-ggplot(diamonds,aes(x=cut,price))
graph3+geom_jitter(alpha=0.1)+geom_violin()


