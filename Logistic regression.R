library(ISLR)
head(Default)
tail(Default)
str(Default)
pairs(Default[c("income", "balance")],
      main="Matrix plot", pch=22,
      bg=c("red", "yellow")
      [unclass(Default$default)])
ndefault=dim(Default[Default$default=="Yes",]);ndefault
pctdefault=ndefault[1]/dim(Default)[1];pctdefault
nodefault=dim(Default[Default$default=="No",]);nodefault
pctnodefault=nodefault[1]/dim(Default)[1]; pctnodefault

avgBalance.nodef = mean(Default[Default$default == "No", "balance"])
avgBalance.nodef
avgBalance.def = mean(Default[Default$default == "Yes", "balance"])
avgBalance.def

avgBalance.noStudent = mean(Default[Default$student == "No", "balance"])
avgBalance.noStudent
avgBalance.Student = mean(Default[Default$student == "Yes", "balance"])
avgBalance.Student

avgIncome.nodef = mean(Default[Default$default == "No", "income"])
avgIncome.nodef
avgIncome.def = mean(Default[Default$default == "Yes", "income"])
avgIncome.def

library(ggplot2)
ggplot(Default, aes(balance, fill=default)) +
  geom_density(alpha=.5) +
  geom_vline(data=Default,
             mapping=aes(xintercept=avgBalance.nodef), color="red") +
  geom_vline(data=Default,
             mapping=aes(xintercept=avgBalance.def), color="dark green")

ggplot(Default, aes(income, fill=default)) +
  geom_density(alpha=.5) +
  geom_vline(data=Default,
             mapping=aes(xintercept=avgIncome.nodef), color="red") +
  geom_vline(data=Default,
             mapping=aes(xintercept=avgIncome.def), color="dark green")


ggplot(Default, aes(balance, fill=student)) +
  geom_density(alpha=.5) +
  geom_vline(data=Default,
             mapping=aes(xintercept=avgBalance.noStudent), color="red") +
  geom_vline(data=Default,
             mapping=aes(xintercept=avgBalance.Student), color="dark green")

ggplot(Default, aes(student, ..count..)) +
  geom_bar(aes(fill=default), position = "dodge")

default.logit=glm(default~balance,family = binomial,data=Default)
summary(default.logit)


a=glm(default~student,family = binomial,data=Default)
summary(a)

default.logit=glm(default~income,family = binomial,data=Default)
summary(default.logit)

default.logit=glm(default~student+balance+income,family = binomial,data=Default)
summary(default.logit)
default.logit=glm(default~.,family = binomial,data=Default)
summary(default.logit)

# marginal default rate
dfrate_Std_b1000 = dim(Default[Default$default=="Yes" & Default$student=="Yes" &
                                 Default$balance < 1000, ])
dfrate_Std_b1000 = dfrate_Std_b1000[1] / ndefault[1] ; dfrate_Std_b1000
dfrate_NoStd_b1000 = dim(Default[Default$default=="Yes" & Default$student=="No" &
                                   Default$balance < 1000, ])
dfrate_NoStd_b1000 = dfrate_NoStd_b1000[1] / ndefault[1] ; dfrate_NoStd_b1000
# same way for "balance < 1500" and "balance < 2000"

# marginal default rate
dfrate_Std_b1500 = dim(Default[Default$default=="Yes" &
                                 Default$student=="Yes" & Default$balance < 1500, ])
dfrate_Std_b1500 = dfrate_Std_b1500[1] / ndefault[1]
dfrate_Std_b1500
dfrate_NoStd_b1500 = dim(Default[Default$default=="Yes" &
                                   Default$student=="No" & Default$balance < 1500, ])
dfrate_NoStd_b1500 = dfrate_NoStd_b1500[1] / ndefault[1]
dfrate_NoStd_b1500

# marginal default rate
dfrate_Std_b2000 = dim(Default[Default$default=="Yes" & Default$student=="Yes" &
                                 Default$balance < 2000, ])
dfrate_Std_b2000 = dfrate_Std_b2000[1] / ndefault[1]
dfrate_Std_b2000
dfrate_NoStd_b2000 = dim(Default[Default$default=="Yes" & Default$student=="No" &
                                   Default$balance < 2000, ])
dfrate_NoStd_b2000 = dfrate_NoStd_b2000[1] / ndefault[1]
dfrate_NoStd_b2000
plot(rbind(dfrate_NoStd_b1000, dfrate_NoStd_b1500, dfrate_NoStd_b2000),
     type="l", col="blue", ylab="default rate",
     main="blue: No student, red: student")
lines(rbind(dfrate_Std_b1000, dfrate_Std_b1500, dfrate_Std_b2000), type="l", col="red")
