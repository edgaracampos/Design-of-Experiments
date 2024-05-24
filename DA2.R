library(data.table)
library(ggplot2)
library(pwr)
library(ggthemes)

#Figure out N
#two-way ANOVA w/interaction
#two factors Drop/Weekly/DualDrop
#            Comedy/Drama/Mystery/Fantasy
#df1 = (r-1)(c-1)
#df2 = rc(n-1)

pwr.f2.test(u = (3-1)*(4-1), v = NULL, f2= (.25)^2, 
            sig.level = 0.05, power = 0.8)

# v = 217.3637
(217.3637/(3*4))+1
# n = 19.113 -> so we will use 20 for each group combination
#N = c * r * n
N = 3*4*20
#uh oh this is higher than the 170 we can use
3*4*20
170/12
N <- 14*12
#n = 14 for practical uses (we cannot use more than 170 shows)
#TOTAL sample size = 168

#planned random assignment
Assignments <-data.table(A = rep(c("Drop", "Weekly", "DualDrop"),
                                 each = 168/3, times = 1),
                         B = rep(c("Comedy","Drama","Mystery", "Fantasy"),
                                 each = 168/(4*3), times = 3))
Assignments <- Assignments[sample(1:168)]
Webflicks$ReleaseSchedule <- Assignments[,A]
Webflicks$Genre <- Assignments[,B]


#import dataset
#assumption check
#1: Independence: can check this off as we used planned random assignment
#2: Normality: 
fit <- aov(Views~ReleaseSchedule * Genre,
           data=Webflicks)
Webflicks$Residuals <-residuals(fit)
qqnorm(Webflicks$Residuals);qqline(Webflicks$Residuals)
#qqplot is straight so we check the normality assumption
#3:Homogeneity:
Webflicks$Predicted <- predict(fit)
plot(Residuals ~ Predicted, data = Webflicks)
#spread is fairly constant, so assumption is likely reasonable
fit <- aov(Views~ReleaseSchedule * Genre,
           data=Webflicks)
summary(fit)
#p of interaction = 0.73336, so we fail to reject null that there is no interaction 
#however, there is evidence that there is difference between the release schedules
#p of Release schedule = 0.00488
#p of Genre = 0.42700 
#only release scheulde is statistically significant

#POST HOC ANALYSIS
Webflicks<- as.data.table(Webflicks)
summary(aov(Views ~ ReleaseSchedule * Genre, 
            data = Webflicks[ReleaseSchedule %in% c("DualDrop","Drop")]))
#DualDrop-Weekly uncorrected p = 0.00597 (corrected = 0.01791)
#DualDrop-Drop uncorrected p = 0.00582 (corrected = 0.01746)
#Drop-Weekly uncorrected p = 0.944 (corrected = 1)

#Dual Drop is different from the other two

#support with EDA ->
ggplot(Webflicks)+geom_boxplot(aes(x = ReleaseSchedule, y = Views, fill = Genre)) 
#Dual Drop is higher than the other two release schedules -> Webflicks should use dual drop
