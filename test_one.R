
#문제1
midwest2 <- as.data.frame(ggplot2::midwest)

#문제2
midwest2 <-rename(midwest2,total = poptotal)
midwest2 <-rename(midwest2,asian = popasian)

head(midwest2)
#문제3

midwest2$total_asian_persent <- midwest2$asian/midwest2$total*100
hist(midwest2$total_asian_persent)

View(midwest2)
#문제4
midwest2$asian_group <- ifelse(midwest2$total_asian_persent> mean(midwest2$total_asian_persent),"large",'small')


#문제5

table(midwest2$asian_group)
qplot(midwest2$asian_group)
