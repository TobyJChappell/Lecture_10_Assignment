library(ggplot2)
#install.packages("dplyr")
library(dplyr)
library(viridis)

diamonds <- read.csv(file = 'diamonds.csv')
diamonds <- diamonds %>% rename(Cut=cut, Clarity=clarity)
diamonds$Cut <- factor(diamonds$Cut,levels=c("Fair","Good","Very Good","Premium","Ideal"))
diamonds$Clarity <- factor(diamonds$Clarity,levels=c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"))
cut_group <- diamonds %>% group_by(Cut) %>%
  summarise(count = n()) %>%
  mutate(per=count/sum(count)) %>% 
  ungroup()

#Proportion of diamonds in the set of each cut

#Side-by-Side Bar
ggplot(data=diamonds,aes(x=Cut,fill=Cut)) +
  geom_bar() +
  ggtitle("Proportion of Diamonds by Cut") +
  ylab(label="Number of Diamonds") +
  scale_fill_viridis_d()
  

#Stacked Bar
ggplot(data=cut_group,aes(x="",y=per,fill=Cut)) +
  geom_bar(stat="identity") +
  xlab("") + 
  ylab("Percent") +
  ggtitle("Proportion of Diamonds by Cut") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d()

#Pie
ggplot(cut_group, aes(x= "", y = per, fill=Cut)) + 
  geom_col() +
  coord_polar("y", start=0) +
  ggtitle("Proportion of Diamonds by Cut") +
  xlab("") +
  ylab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d()
  
#Does proportion change with clarity


