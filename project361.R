
install.packages("tidyverse")
install.packages("UsingR")
install.packages("modelr")
install.packages("readtext")
install.packages("intervals")
install.packages("car")
install.packages("dplyr")

library(tidyverse)
library(UsingR)
library(modelr)
library(readtext)
library(intervals)
library(car)
library(dplyr)

mydataSet<-read_table("Test_1_healthcare_data.txt",col_names = c("id","County","stateAb",
                                                                 "landarea","estpop","pct18to36",
                                                                 "pct65older","V8","V9","V10","pctAdult12yrEd",
                                                                 "pctAdultwBacDegree","pctBelowpov","pctUnemployed",
                                                             "perCapita","totalpersonalincome","geoLoc"),
                      col_types =  cols(
                          id = col_double(),
                          County = col_character(),
                          stateAb = col_character(),
                          landarea = col_double(),
                          estpop = col_double(),
                          pct18to36 = col_double(),
                          pct65older = col_double(),
                          V8 = col_double(),
                          V9 = col_double(),
                          V10 = col_double(),
                          pctAdult12yrEd = col_double(),
                          pctAdultwBacDegree = col_double(),
                          pctBelowpov = col_double(),
                          pctUnemployed = col_double(),
                          perCapita = col_double(),
                          totalpersonalincome = col_double(),
                          geoLoc = col_factor()
                        ))
View(mydataSet)
mydataSet<-mydataSet %>% mutate(geoLoc=fct_recode(geoLoc,
                                       "NE"="1", "Nc"="2", "S"="3", "W"="4"))
View(mydataSet)


scatPlot <- ggplot(mydataSet, aes(x = totalpersonalincome, y = perCapita))+
  geom_point()
scatPlot        #(needs log transformation)
scatPlot1 <- ggplot(mydataSet, aes(x = log(totalpersonalincome), y = perCapita))+
  geom_point()
scatPlot1#(after transformation a linear relationship)
y1<- mydataSet$perCapita
x1<- mydataSet$totalpersonalincome
summary(lm(y1~x1))
eqn<-lm(y1~log(x1))
summary(eqn)
#45 percent variability explained after transformation
fittedValues<-predict(eqn,data.frame(x1))
View(fittedValues)
res<- (y1-fittedValues)
res<-as.matrix(res)
nrow(res)
View(res)
mydataSet<-mydataSet %>% mutate(res)
resPlot <- ggplot(mydataSet, aes(x = totalpersonalincome, y = res))+
  geom_point()
resPlot
#From this residual we can confirm that the SLR is appropraite model




scatPlot2 <- ggplot(mydataSet, aes(x = pctAdultwBacDegree, y = perCapita))+
  geom_point()
scatPlot2
#(linear relationship, now check to see if SLR is apporpraite by using residualplot)
y<- mydataSet$perCapita
x<- mydataSet$pctAdultwBacDegree
eqn1<-lm(y~x)
summary(eqn1)
#48.35% variabilty explained
fittedValues1<-predict(eqn1,data.frame(x))
View(fittedValues1)
res1<- (y-fittedValues1)
res1<-as.matrix(res1)
nrow(res1)
View(res1)
mydataSet<-mydataSet %>% mutate(res1)
resPlot1 <- ggplot(mydataSet, aes(x = pctAdultwBacDegree, y = res1))+
  geom_point()
resPlot1
#From this residual we can confirm that the SLR is appropraite model



scatPlot4 <- ggplot(mydataSet, aes(x = pctUnemployed, y = perCapita))+
  geom_point()
scatPlot4 #(linear but decreasing)
y4<- mydataSet$perCapita
x4<- mydataSet$pctUnemployed
eqn4<-lm(y4~x4)
summary(eqn4)
#10% variability explained
fittedValues4<-predict(eqn4,data.frame(x4))
View(fittedValues4)
res4<- (y4-fittedValues4)
res4<-as.matrix(res4)
nrow(res4)
View(res4)
mydataSet<-mydataSet %>% mutate(res4)
resPlot4 <- ggplot(mydataSet, aes(x = pctUnemployed, y = res4))+
  geom_point()
resPlot4
#From this residual plot 4 we can confirm that the SLR is not the appropraite model




scatPlot5 <- ggplot(mydataSet, aes(x = pctBelowpov, y = perCapita))+
  geom_point()
scatPlot5
y<- mydataSet$perCapita
x5<- mydataSet$pctBelowpov
eqn5<-lm(y~x5)
summary(eqn5)
#36% variability explained 
fittedValues5<-predict(eqn5,data.frame(x5))
View(fittedValues5)
res5<- (y-fittedValues5)
res5<-as.matrix(res5)
View(res5)
mydataSet<-mydataSet %>% mutate(res5)
resPlot5 <- ggplot(mydataSet, aes(x = pctBelowpov, y = res5))+
  geom_point()
resPlot5
#From this residual plot we can infer that it is not random pattern so SLR is not appropraite
#(multiple regression or Non linear regression)



scatPlot6 <- ggplot(mydataSet, aes(x = landArea, y = perCapita))+
  geom_point()
scatPlot6
tscatPlot6 <- ggplot(mydataSet, aes(x = log(landArea), y = perCapita))+
  geom_point()
tscatPlot6
#clustered at each other and even after transformation not helpful
View(mydataSet)




scatPlot7 <- ggplot(mydataSet, aes(x = log(est1990pop), y = perCapita))+
  geom_point()
scatPlot7  #(slightly linear relationship )
y<- mydataSet$perCapita
x7<- mydataSet$est1990pop
eqn7<-lm(y~x7)
summary(eqn7)
#5.5% variablity explained
fittedValues7<-predict(eqn7,data.frame(x7))
View(fittedValues7)
res7<- (y-fittedValues7)
res7<-as.matrix(res7)
View(res7)
mydataSet<-mydataSet %>% mutate(res7)
resPlot7 <- ggplot(mydataSet, aes(x = log(est1990pop), y = res7))+
  geom_point()
resPlot7
#we can say SLR is appropraite from the residual plot

scatPlot8 <- ggplot(mydataSet, aes(x =pct18to36, y = perCapita))+
  geom_point()
scatPlot8
# randomly scattered we can not say there is a relationship


scatPlot9 <- ggplot(mydataSet, aes(x =pct65older, y = perCapita))+
  geom_point()
scatPlot9
# randomly scattered we can not say there is a relationship



scatPlot10 <- ggplot(mydataSet, aes(x = log(V10), y = perCapita))+
  geom_point()
scatPlot10
#(after log transformation serious crimes and percapita showed a slight linear relationship)
y<- mydataSet$perCapita
x10<- mydataSet$V10
eqn10<-lm(y~x10)
summary(eqn10)
#1.3 % variablity explained 
fittedValues10<-predict(eqn10,data.frame(x10))
View(fittedValues10)
res10<- (y-fittedValues10)
res10<-as.matrix(res10)
View(res10)
mydataSet<-mydataSet %>% mutate(res10)
resPlot10<- ggplot(mydataSet, aes(x = log(V10), y = res10))+
  geom_point()
resPlot10
#we can say SLR is appropraite from the residual plot


#pctofBacDegree explained the variablity more than the other explanatory variables 





#SecondPortionoftheProject
#Predicting the per capita according to percentage of bachelor degree in 4 different areas 
#the country




#step1
#Finding the average per capita income at 4 different regions of the country
data1<-as.data.frame(mydataSet)
View(data1)
data1<-data1 %>% select(geoLoc,perCapita,pctUnemployed,pctAdultwBacDegree) %>% 
  group_by(geoLoc) %>% 
  summarise(percapita=mean(perCapita),pctunemployed=mean(pctUnemployed),
            pctadultwBacDegree=mean(pctAdultwBacDegree)) %>% 
  arrange(desc(percapita))
#From this data we can see which region of the country has the highest average per capita income

data2<-as.data.frame(mydataSet)
data2<-data2 %>% select(stateAb,perCapita,pctUnemployed,pctAdultwBacDegree) %>% 
  group_by(stateAb) %>% 
  summarise(percapita=mean(perCapita),pctunemployed=mean(pctUnemployed),
            pctadultwBacDegree=mean(pctAdultwBacDegree)) %>% 
  arrange(desc(percapita))
View(data2)
#From this data we can see which states have the highest per capita income 

#from the scatterplot of percapita vs pct of bachelor degree holder we can infer that there is 
#positive linear relationship

#we want to get a model that can predict the per capita of individual according to 
#the percentage of bachelor degree and what region of the country they are located in
#degree 
y<-mydataSet$perCapita
x1<-mydataSet$pctAdultwBacDegree
x2<-mydataSet$geoLoc
fullModel<-lm(y~x1+x2)
summary(fullModel)
reducedModel<-lm(y~x1)
summary(reducedModel)
coef(lm(y ~ x1))
coef(lm(y ~ x1 + x2))

data <- data.frame(y = y, 
                   x1 = x1,
                   x2 = x2,
                   ey = resid(lm(y ~ x2)),
                   ex1 = resid(lm(x1 ~ x2)))
View(data)

ggplot(data, aes(x = x1, y = y, color = x2)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, color = "black", size = .2)

#based on the multiple regression we found regression constant for each of the four regions 
#so we can use them to predict per capita income based on the percent of the bachelor degree
#holders in that region

#final model 
percapita = 17090 + 0.0162*(B1)+484*(B2)-255.2(B3)+2580(B4)

#Where B1-W B2-NC B3-S B4-NE






