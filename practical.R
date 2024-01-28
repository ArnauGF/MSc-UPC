#we import the data
setwd("/Users/arnaugarcia/Desktop/Q1/ánalisis tiempo de vida/practical work")
prostate <- read.table("ProstateCancer.txt", skip = 32)
colnames(prostate) <- c("id", "stage", "treat", "time", "delta", "age", 
                        "weight_index", "hist_cardiov", "sbp", "dbp", 
                        "hemoglobin", "tumour_size", "bone_metas")
head(prostate)

#checking if the variables that must be factors are factors
prostate$treat <- factor(prostate$treat)
prostate$hist_cardiov <- factor(prostate$hist_cardiov)
prostate$bone_metas <- factor(prostate$bone_metas)
prostate$stage <- factor(prostate$stage)
#prostate$delta <- factor(prostate$delta)


#we put labels
library(Hmisc)
label(prostate$id)<- "Patient identifier"
label(prostate$stage) <-"Cancer stage"
label(prostate$treat) <-"Estrogen treatment (Placebo, 0.2 mg, 1 mg, or 5 mg of estrogen)"
label(prostate$time)<- "Survival time from study start [months]"
label(prostate$delta) <- "Death indicator (1: yes, 0: no)"
label(prostate$age) <- "Age"
label(prostate$weight_index) <-"Weight index: Weight [kg] - height [cm] + 200"
label(prostate$hist_cardiov) <-"History of cardiovascular disease (1: yes; 0: no)" 
label(prostate$sbp)<-"Systolic blood pressure (divided by 10)"
label(prostate$dbp) <- "Diastolic blood pressure (divided by 10)"
label(prostate$hemoglobin) <- "Hemoglobin level [g/100ml]"
label(prostate$tumour_size) <- "Tumour size [cm^2]"
label(prostate$bone_metas) <- "Bone metastasis (1: yes; 0: no)"



#descriptive analysis:
summary_table <- summary(prostate)
library(knitr)
library(kableExtra)
library(webshot)

styled_table <- kable(summary_table, format = "html") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    font_size = 14
  ) 
save_kable(styled_table, "summary_table3.html")

#we can observe that the minimum of the times is 0! It is, there are times 
#that are 0. We need to change this, because this will give problems
#when fitting parametric models.
#What we will do is to sum 0.5 to these times:
prostate$time[prostate$time==0] <- 0.5

#NON-PARAMETRIC ANALYSIS:
#estimation of the survival (we draw survival with respect treatment, because
#the objective is to analyze the efficacy of estrogen treatments)
library(survival)

sprost <- with(prostate, Surv(time, delta) ~ treat)
#with the following survfit function we can see estimations for the median surv time
survfit(sprost)
#survival without rms (better)
quartz(width = 8)
par(mfrow=c(1,1), font = 2, font.axis = 2, font.lab = 4, las = 1, mar = c(5, 5, 4, 2))
plot(survfit(sprost), xlab = "Time [months]",
     ylab = expression(bolditalic(hat(S)(t))), col = 2:5,
     lty = 1, lwd = 3, yaxs = "i", bty = "l", mark.time=TRUE, xlim=c(0,75))
axis(1, at = seq(0, 70, 5))
axis(2, at = seq(0.1, 0.9, 0.2))
title("Survival functions according to estrogen treatment")
legend("topright", c("0.2 mg", "1 mg",
                     "5 mg", "Placebo"),
       bty = "n", col = 2:5, lty = 1, lwd = 3)
#we compute other survival functions according to:
#historical card disease
quartz(width = 8)
par(mfrow=c(1,3), font = 2, font.axis = 2, font.lab = 4, las = 1, mar = c(5, 5, 4, 2))
plot(survfit(with(prostate, Surv(time, delta) ~ hist_cardiov)), xlab = "Time [months]",
     ylab = expression(bolditalic(hat(S)(t))), col = 2:3,
     lty = 1, lwd = 3, yaxs = "i", bty = "l", xlim=c(0,75))
axis(1, at = seq(0, 70, 5))
axis(2, at = seq(0.1, 0.9, 0.2))
title("Survival functions according to history of cardiovascular disease")
legend("topright", c("No card disease", "Card disease"),
       bty = "n", col = 2:5, lty = 1, lwd = 3)
#bone metas
plot(survfit(with(prostate, Surv(time, delta) ~ bone_metas)), xlab = "Time [months]",
     ylab = expression(bolditalic(hat(S)(t))), col = 2:3,
     lty = 1, lwd = 3, yaxs = "i", bty = "l", xlim=c(0,75))
axis(1, at = seq(0, 70, 5))
axis(2, at = seq(0.1, 0.9, 0.2))
title("Survival functions according to bone metastasis")
legend("topright", c("No bone metas.", "Bone metas."),
       bty = "n", col = 2:5, lty = 1, lwd = 3)
#stage
plot(survfit(with(prostate, Surv(time, delta) ~ stage)), xlab = "Time [months]",
     ylab = expression(bolditalic(hat(S)(t))), col = 2:3,
     lty = 1, lwd = 3, yaxs = "i", bty = "l", xlim = c(0,75))
axis(1, at = seq(0, 70, 5))
axis(2, at = seq(0.1, 0.9, 0.2))
title("Survival functions according to Cancer stage")
legend("topright", c("Stage 3", "Stage 4"),
       bty = "n", col = 2:5, lty = 1, lwd = 3)

#let us do a logrank test or other of the Fleming harrington family (for testing
#differences at different points of the survival)
library(FHtest)
survprost <- with(prostate, Surv(time, delta))
#Now, we will try other Fleming Harrington tests changing the parameters. 
#If we take 𝜌 = 𝜆 = 0, which leads us to the logrank test, we obtain:
FHtestrcc(survprost~prostate$treat, rho = 0, lambda = 0)
#the logrank test can also be done with:
survdiff(Surv(time, delta) ~ treat, prostate)
#we see if we can reject H0: surv functions are equal
10.1 >= qchisq(1-0.05,df=3)
#We try to use other FH tests:
#Now, we take 𝜌 > 0,𝜆 = 0, in order to detect early differences. For instance, we choose rho = 1/2.
FHtestrcc(survprost~prostate$treat, rho = 0.5, lambda = 0)
8>=qchisq(1-0.05,3)
#Now, we take 𝜌 = 0,𝜆 > 0, in order to detect late differences. For instance, we choose 𝜆 = 1/2.
FHtestrcc(survprost~prostate$treat, rho = 0, lambda = 0.5)
13.1>=qchisq(1-0.05,3)
#as we expect, the difference is bigger at the final, we have more late differences
#and the difference is less at the beggining.
#finally test for detect median differences
FHtestrcc(survprost~prostate$treat, rho = 0.5, lambda = 0.5)
11.7>=qchisq(1-0.05,3)


#let us check difference if survivals for the survivals according historical cardiov
#where the difference was clear
survdiff(Surv(time, delta) ~ hist_cardiov, prostate)
19.8>=qchisq(1-0.05,1)
#of course, there are differences

#fit a parametric survival model:
#weibull
survprost <- with(prostate, Surv(time, delta))
weimod <- survreg(survprost ~ stage+treat+age+tumour_size+bone_metas, prostate, 
                  dist = "weibull")
summary(weimod)
#lognormal model
lognormmod <- survreg(survprost ~ stage+treat+age+tumour_size+bone_metas, prostate,
                      dist="lognormal")
summary(lognormmod)
#loglogistic model
loglogmod <- survreg(survprost ~ stage+treat+age+tumour_size+bone_metas, prostate,
                      dist="loglogistic")
summary(loglogmod)

#checking graphically which is the best model
library(GofCens)
#cummulative hazards:
quartz(width = 8)
par(las = 1, font.lab = 4, font.axis = 2, pch = 16)
cumhazPlot(prostate$time, prostate$delta, col = 4, 
           distr = c("weibull", "loglogistic", "lognormal"), font.lab = 4,
           ggplo = TRUE)
?cumhazPlot
#it seems that the weibull is the best in terms of cum hazard
#probabilty plots
#first for weibll
quartz(width = 8)
par(las = 1, font.lab = 4, font.axis = 2, pch = 16)
probPlot(prostate$time, prostate$delta, distr = "weibull", col = 4, ggplo = TRUE)
#loglo
quartz(width = 8)
par(las = 1, font.lab = 4, font.axis = 2, pch = 16)
probPlot(prostate$time, prostate$delta, distr = "loglo", col = 4, ggplo = TRUE)
#lognor
quartz(width = 8)
par(las = 1, font.lab = 4, font.axis = 2, pch = 16)
probPlot(prostate$time, prostate$delta, distr = "lognormal", col = 4, ggplo = TRUE)
#again it seems that weibull is the best model
#thus, we decide to fit a weibull model, now it is time to check wether is the
#best model and select the best variables for modeling
weimod <- survreg(survprost ~ treat+age+tumour_size+bone_metas+hist_cardiov, prostate, 
                  dist = "weibull")
summary(weimod)
AIC(weimod)
#we add, treatment of course, the age of the patient,
#the tumour size, the bone metast, the historic of cardiovascular disease 
#(https://www.ajmc.com/view/correlation-between-prostate-cancer-and-cardiovascular-disease)
#and the weight index
#altgouht stage seems to be an important variable, in our data we only have stage
#3 and 4. It is, advanced stages, and then the variable seems to be not 
#significant when we add to the model

weimod2 <- survreg(survprost~treat+age+tumour_size+bone_metas+hist_cardiov+
                    weight_index, prostate, dist="weibull")
summary(weimod2)
AIC(weimod2)
#we prepare other models and compare in terms of AIC
weimod3 <- survreg(survprost~treat+stage+age+tumour_size+bone_metas+hist_cardiov+
                     weight_index, prostate, dist="weibull")
summary(weimod3)
AIC(weimod3)
weimod4 <- survreg(survprost~treat+stage+age+tumour_size+bone_metas+hist_cardiov+
                     weight_index+hemoglobin, prostate, dist="weibull")
summary(weimod4)
AIC(weimod4)

weimod5 <- survreg(survprost~treat+stage+age+tumour_size+bone_metas+hist_cardiov+
                     weight_index+hemoglobin+sbp+dbp, prostate, dist="weibull")
AIC(weimod5)
#the last is the modek with all the variables
#the model with best AIC is the weimod2
#we add some interaction
weimod6 <- survreg(survprost~treat+stage+age+tumour_size+bone_metas+hist_cardiov+
                     weight_index+treat:hist_cardiov, prostate, dist="weibull")
summary(weimod6)
AIC(weimod6)

weimod7 <- survreg(survprost~treat+stage+age+tumour_size+bone_metas+hist_cardiov+
                     weight_index+stage:hist_cardiov, prostate, dist="weibull")
summary(weimod7)
AIC(weimod7)

weimod8 <- survreg(survprost~treat+age+tumour_size+bone_metas+hist_cardiov+
                     weight_index+bone_metas:hist_cardiov, prostate, dist="weibull")
summary(weimod8)
AIC(weimod8)
#there is no model with interactions with better AIC

#let us see graphically how is the model fit, first we compute the residuals
weimodpred <- predict(weimod2, type = "linear")
length(weimodpred)
#I don't understand why the model only predicts 445 variables while we have 
#450 obs, let us take only 445 obs then
v <- log(prostate$time)[1:445]
u<- prostate$delta[1:445]
res <- (v - weimodpred) / weimod2$scale
resi <- (log(prostate$time) - weimodpred) / weimod$scale



quartz(width = 8)
par(font = 2, font.lab = 4, font.axis = 2, las = 1, oma = c(0, 0, 1, 0),
    mar = c(5, 5, 4, 2))
plot(survfit(Surv(resi, prostate$delta) ~ 1), xlab = "Residuals", lwd = 3,
     ylab = expression(bold(hat(S)(t))), yaxs = "i")
title("Residuals of the Weibull regression model")

# Graphical comparison with the theoretical survival function
survgumb <- function(x) {
  return(exp(-exp(x)))
}

curve(survgumb(x), from = min(res), to = max(res), col = 2, lwd = 3,
      add = TRUE)
legend("bottomleft", c("KM estimate", "95% - CI", "Stand. Gumbel Distribution"),
       col = c(1, 1, 2), lty = c(1, 2, 1), lwd = 3, bty = "n")



#interpretation in relative hazards and AF
#first the hazard ratios with the variable treatment
with(weimod2, exp(-coefficients[2:9]/scale))
#According to the values of the hazard ratios obtained, the instantaneous risk 
#of death for those indivuduals revieving the treatment 1 (1mg), treat 5mg, and 
#placebo is , respectively, is 0.6408723, 0.8741449 and 1.0344170 
#times larger than the individuals recieving treat 0.2mg.

#now AF
with(weimod2, exp(-coefficients[2:9]))
#According to the values of the acceleration factors obtained, the median times
#until death (or any other quantile) of individuals having treat 0.2mg  is 0.6479863,
#0.8770671 and 1.0335489 times larger than the median times of, respectively, 
#of persons having treat 1mg, treat 5mg, treat placebo, with the same value
#in all the other variables.
#past-due delinquency.


#Fit of semiparametric model
#previously to fit any model do the log(time) vs log(-log(surv)) for assess if
#there is a clear crossing
logsprost <- with(prostate, Surv(log(time), delta) ~ treat)
par(font = 2, font.axis = 2, font.lab = 4, las = 1, mar = c(5, 5, 4, 2))
plot(log(-log(survfit(logsprost))), xlab = "log(Time) [months]",
     ylab = expression(bolditalic(log(-log(hat(S)(t))))), col = 2:5,
     lty = 1, lwd = 3, yaxs = "i", bty = "l")
legend("topright", c("0.2 mg", "1 mg",
                     "5 mg", "Placebo"),
       bty = "n", col = 2:5, lty = 1, lwd = 3)

#fit proportional hazard model
cox <- coxph(survprost ~ treat + age + tumour_size + bone_metas + 
               hist_cardiov + weight_index, data = prostate)
summary(cox)
#here we have to interpret the model fitted (see in the sol of assign 3)

#now we compute the relative hazards
#hazard ratio associated with the treatment and histor cardiov
#treat 1mg, no hist cardiov 
with(cox, exp(coefficients[[1]]))
#treat 1mg, yes hist card
with(cox, exp(coefficients[[1]] + coefficients[[7]]))
#treat 5mg, no hist card
with(cox, exp(coefficients[[2]]))
#treat 5mg and yes hist card
with(cox, exp(coefficients[[2]] + coefficients[[7]]))
#treat placebo and no hist card
with(cox, exp(coefficients[[3]]))
#treat placebo and yes hist card
with(cox, exp(coefficients[[3]] + coefficients[[7]]))
#now, we have to interpret these results

#let us now compute the HR of treatment as a function of bones metas
#treat 1mg, no bones meta
with(cox, exp(coefficients[[1]]))
#treat 1mg, yes bones meta
with(cox, exp(coefficients[[1]] + coefficients[[6]]))
#treat 5mg, no bones m
with(cox, exp(coefficients[[2]]))
#treat 5mg and yes bones m
with(cox, exp(coefficients[[2]] + coefficients[[6]]))
#treat placebo and no bones m
with(cox, exp(coefficients[[3]]))
#treat placebo and yes bones m
with(cox, exp(coefficients[[3]] + coefficients[[6]]))
#now, we have to interpret these results

#comparison between treatments:
#treat 0.2 vs the others are compared before, simiply:
with(cox, exp(coefficients[[1]]))
with(cox, exp(coefficients[[2]]))
with(cox, exp(coefficients[[3]]))
#let us compare between the other treatments:
# 5mg VS 1mg
with(cox, exp(-coefficients[[1]]+coefficients[[2]]))
# placebo vs 5mg
with(cox, exp(coefficients[[3]]-coefficients[[2]]))
# placebo vs 1mg
with(cox, exp(coefficients[[3]]-coefficients[[1]]))

#now taking into account bone metas and hist card at the same time
#treat 1mg, yes bones m, yes hist card
with(cox, exp(coefficients[[1]] + coefficients[[6]] + coefficients[[7]]))
#treat 5mg, yes bones m, yes hist card
with(cox, exp(coefficients[[2]] + coefficients[[6]] + coefficients[[7]]))
#treat placebo, yes bones m, yes hist card
with(cox, exp(coefficients[[3]] + coefficients[[6]] + coefficients[[7]]))



#Analysis of the residuals to check godness of fit
#The proportional hazards assumption can be checked with the function cox.zph, which analyses the
#correlation of the Schoenfeld residuals and survival time. Under the proportional 
#hazards assumption, this correlation is expected to be close to 0:
cox.zph(cox)
#in addition the graphical check:
par(mfrow = c(2, 3), font = 2, font.lab = 4, font.axis = 2, las = 1,
    cex.lab = 1.3, cex.axis = 1.2)
plot(cox.zph(cox), lwd = 2)
?cox.zph

#we study if there are influential obs with dfbetas 
dfbet <- residuals(cox, type = "dfbeta")
par(mfrow = c(2, 4), font = 2, font.lab = 4, font.axis = 2, las = 1,
    cex.lab = 1.3, cex.axis = 1.2)
for (i in 1:dim(dfbet)[2]) {
  plot(dfbet[, i], pch = 16, ylab = "")
  title(names(coef(cox))[i])
  axis(1, at = seq(5, 40, 5))
}

#look in deep to an obs which is different than the others in tumour_size var
prostate[dfbet[,5]==min(dfbet[, 5]),]
#it is the patient with id 229, let us see the summary of the data
summary(prostate)
#big tumour size, with 3rd quartile size, recieving 1mg treat (the best)
#die at time 41, which is aprox the mean time, 1st q weight index with 91,
#thid quartile of age, 76 years old

#look into influential obs in hist card var:
prostate[dfbet[,7]==min(dfbet[, 7]),]
#is the same patient

#look into influential obs in bone metas var
prostate[dfbet[,6]==min(dfbet[, 6]),]
#individual with large time 54 almost 3rd quartile, but with 69 years old 1st q,
#hist cardiovascular disease, 1st q weight index with 89, 1st q tumour size with 4


#conclusines rápidas:
#tal y como parecía des de un inicio al pintar las survival functions el tratamiento
#de 1mg es el que mejor funciona, seguido por el de 5mg, luego 0.2 y finalmente placebo 
#que funciona peor, pero no mucho peor que 0.2mg

