# set the correct directory
#setwd("C:/Users/Nabhan/Dropbox/winter 2017/stad94")
# install and load the xlsx package
install.packages("xlsx")
library(xlsx)
# read the data
laptops=read.xlsx("laptops.xlsx", header=T, sheetIndex=1)
#install and load ggplot 2
install.packages("ggplot2")
library(ggplot2)

#gets rid of the x between the two numbers and input the strings in a list
resolution= strsplit(as.character(laptops$resolution),' x ')
#converts the strings in the list to numeric
res=lapply(resolution,as.numeric)
#function to make the numbers in the list multiply with each other
f=function(x){
  x[1]*x[2]
}
res2=lapply(res,f)
#gets rid of the list
resolution2=unlist(res2)
#displays the first six rows of the newly converted resolution column
head(resolution2)


# boxplot of the brands
ggplot(laptops,aes(x=factor(brand),y=retail_price))+geom_boxplot()

#boxplot of price by platform
ggplot(laptops,aes(x=factor(platform),y=retail_price))+geom_boxplot()

#check for differences within the brands
price0=aov(retail_price~brand,data=laptops)
summary(price0)

#run tukey to see where differences lie
TukeyHSD(price0)

# seperate windows and apple and display the first 6 rows
windows=laptops[1:169,]
apple=laptops[169:211,]
head(windows$platform)
head(apple$platform)

#scatterplot of storage against price separated by platform
ggplot(laptops,aes(x=storage,y=retail_price,colour=platform))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~platform)

#scatterplot of ram against price separated by plaotform
ggplot(laptops,aes(x=ram,y=retail_price,colour=platform))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~platform)

# boxplot of the type of storage against price separated by platform 
ggplot(laptops,aes(x=factor(storage_type),y=retail_price))+geom_boxplot()+facet_wrap(~platform)

#scatterplot and boxplot of processor cores against price separated by platform 
ggplot(laptops,aes(x=processor_cores,y=retail_price,colour=platform))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~platform)
ggplot(laptops,aes(x=factor(processor_cores),y=retail_price,colour=platform))+geom_boxplot()+facet_wrap(~platform)

#scatterplot of processor speed by each platform
ggplot(laptops,aes(x=processor_speed,y=retail_price,colour=platform))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~platform)

#scatterplot of screensize against price separated by platform 
ggplot(laptops,aes(x=screen_size,y=retail_price,colour=platform))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~platform)

#scatterplot of resolution and retail_price by each platform
ggplot(laptops,aes(x=resolution2,y=retail_price,colour=platform))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~platform)

#boxplot of the processors and graphics chipset against price separated by platform 
ggplot(laptops,aes(x=processor_type,y=retail_price,colour=platform))+geom_boxplot()+facet_wrap(~platform)
ggplot(laptops,aes(x=graphics,y=retail_price,colour=platform))+geom_boxplot()+facet_wrap(~platform)

#scatterplot of resolution graphics and screensize 
ggplot(laptops,aes(x=resolution2,y=screen_size,colour=platform))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~platform)
ggplot(laptops,aes(x=graphics,y=resolution2,colour=platform))+geom_boxplot()+facet_wrap(~platform)
ggplot(laptops,aes(x=graphics,y=screen_size,colour=platform))+geom_boxplot()+facet_wrap(~platform)



#created a regression model of the ram and storage for each platform
price1=lm(retail_price~ram+storage,data=windows)
summary(price1)
price2=lm(retail_price~ram+storage,data=apple)
summary(price2)


# changed the model to see if adding an interaction makes significant difference
price3=lm(retail_price~ram*storage,data=windows)
summary(price3)
price4=lm(retail_price~ram*storage,data=apple)
summary(price4)


#checking to see if adding the interaction had any significant improvement on the model
anova(price1,price3)
anova(price2,price4)



price5=lm(retail_price~processor_speed+processor_cores+screen_size+ram+storage+graphics+processor_type+storage_type,data=windows)
summary(price5)

price6=update(price5,.~.-processor_cores) 
summary(price6)
drop1(price6,test="F")

price7=update(price6,.~.-screen_size) 
summary(price7)
drop1(price7,test="F")

price8=update(price7,.~.-storage) 
summary(price8)
drop1(price8,test="F")

summary(price8)


price9=lm(retail_price~processor_speed+processor_cores+screen_size+ram+storage,data=apple)
summary(price9)

price10=update(price9,.~.-processor_cores) 
summary(price10)
drop1(price10,test="F")

