setwd("~/Documents/Academic/BA with R/Problem set")
install.packages("DBI")
install.packages("RSQLite")
library(DBI)
library(RSQLite)
library(data.table)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

## Question 1
vote1 <- wpull('vote1')
summary(lm(voteA~log(expendA)+log(expendB)+prtystrA, data=vote1))
C <- log(vote1$expendB) - log(vote1$expendA)
summary(lm(voteA~log(expendA)+C+prtystrA, data=vote1))

## Question 2
lawsch85 <- wpull('lawsch85')
summary(lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank, data=lawsch85))
lawsch85.2 <- lawsch85[complete.cases(lawsch85$LSAT,lawsch85$GPA),]
summary(lm(log(salary)~log(libvol)+log(cost)+rank, data=lawsch85.2))
((0.8417-0.8174)/2)/((1-0.8417)/130) #fstat2  

lawsch85.3 <- lawsch85[complete.cases(lawsch85$LSAT,lawsch85$GPA,lawsch85$clsize,lawsch85$faculty),]
summary(lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank+clsize+faculty, data=lawsch85.3))
summary(lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank, data=lawsch85.3))
((0.844-0.8416)/2)/((1-0.8416)/123) #fstat3

## Question 3
hprice1 <- wpull('hprice1')
summary(lm(log(price)~sqrft+bdrms, data=hprice1))
150*0.0003794 + 0.02888
y <- hprice1$sqrft - 150*hprice1$bdrms
summary(lm(log(price)~y+bdrms, data=hprice1))
confint(lm(log(price)~y+bdrms, data=hprice1))

## Question 4
wage2 <- wpull('wage2')
z <- wage2$exper + wage2$tenure
summary(lm(log(wage)~educ+exper+z, data=wage2))
confint(lm(log(wage)~educ+exper+z, data=wage2))

## Question 5:
subs401K <- wpull('401Ksubs')
subs401K$fsize <- as.numeric(subs401K$fsize ==1)
sum(subs401K$fsize)
(0.84266 - 1)/ 0.09202 
subs401K.1 <- subs401K[which(subs401K$fsize==1)]
summary(lm(nettfa~inc+age,data=subs401K.1))
summary(lm(nettfa~inc,data=subs401K.1))
cor(subs401K.1$inc, subs401K.1$age)

## Question 6:
kielmc <- wpull('kielmc')
y1981 <- kielmc[which(kielmc$y81 ==1)]
summary(lm(log(price)~log(dist), data=y1981))
summary(lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age, data=y1981))
summary(lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age+I(log(intst)^2), data=y1981))
summary(lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age+I(log(intst)^2)+I(log(dist)^2), data=y1981))

## Question 7:
wage1 <- wpull('wage1')
summary(lm(log(wage)~educ+exper+I(exper^2),data=wage1))
100*(0.0409731 - 2*0.0007121*4)
100*(0.0409731 - 2*0.0007121*19) 
0.0409731/(2*0.0007121) 
wage1$y29 <- as.numeric(wage1$exper >28)
sum(wage1$y29)

## Question 8:
wage2 <- wpull('wage2')
summary(lm(log(wage)~educ+exper+exper:educ, data=wage2))
a <- wage2$educ*(wage2$exper-10)
summary(lm(log(wage)~educ+exper+a, data=wage2))
confint(lm(log(wage)~educ+exper+a, data=wage2))

## Question 9:
gpa2 <- wpull('gpa2')
summary(lm(sat~hsize+I(hsize^2), data=gpa2))
19.81/(2*2.13)
summary(lm(log(sat)~hsize+I(hsize^2), data=gpa2))
0.0196029/(2*0.0020872) 

## Question 10:
hprice1 <- wpull('hprice1')
summary(lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1))
-1.29704 + 0.16797*log(20000) + 0.70023*log(2500) + 0.03696*4
exp(5.992921)
summary(lm(price~lotsize+sqrft+bdrms,data=hprice1))

