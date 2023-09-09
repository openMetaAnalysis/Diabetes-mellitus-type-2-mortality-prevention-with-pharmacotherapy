# rbadgett@kumc.edu
# https://openmetaanalysis.github.io/arrows_plot/

library("Rcmdr")
library("rmeta")
library("nls2")
#op <- par(mfrow = c(1, 2),pty = "s")
#par(din=(10,5))
#split.screen(c(1,3),erase = TRUE) 

erase.screen(1)  
screen(1)
devAskNewPage(ask = FALSE)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("../data")
getwd()

all_trials<- read.table("all trials.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
save(all_trials, file = "all_trials.rda")
load("all_trials.rda")
attach("all_trials.rda")

included <- subset(all_trials, subset=(Study.name != "UKPDS 34 all rx vs diet"))
attach(included)
included$Event_rate_exp <- 100 * exp_events / exp_total
included$Event_rate_con <- 100 * control_events / control_total
included$surrogate_diff_p_rx <- included$control_biomarker_post - included$exp_biomarker_post
included$OR <- (included$exp_events * (included$control_total - included$control_events)) / ((included$exp_total - included$exp_events) * included$control_events)
included$x <- exp_biomarker_post; included$y <-  included$Event_rate_exp; included$x2 <- included$control_biomarker_post; included$y2 <- included$Event_rate_con
#included=cbind(included,OR)
attach(included)

#-----------------------------------------------------------------------
#Arrows plot -------
par(mfrow=c(1,1), oma = c(3, 0, 0, 0))

topic = "Aggressive treatment of diabetes"
outcome = "overall mortality"
biomarker = "HbA1c"
timeframe = "annualized"

par(yaxt = "n")
plot(control_biomarker_post/duration, Event_rate_con, axes=F,xlab="", ylab = "",main=topic,xlim=c(6,10),ylim=c(0,10))
par(yaxt = "s")
#mtext(side=3,line=3,"Aggressive treatment of diabetes.", cex=1.2,font=2, adj=0)
axis(2,c(0,10,20,30,40,50), tick = TRUE,c("0%","10%","20%","30%","40%","50%"))
axis(2,c(0,10,20), tick = TRUE,c("0%","10%","20%"))
axis(1,c(6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10), tick = TRUE,c("6.0","6.5","7.0","7.5","8.0","8.5","9.0","9.5","10"))
box(which="plot")

##Grid lines
i = 0
for(i in 0:5)
	{
	abline(h=i*10, v=0, col = "gray90")
	}
i=1
for(i in 6:10)
	{
	abline(h=0, v=i, col = "gray90")
	}
mtext(side=2,line=3,paste("Clinical outcome (", outcome, ")", sep=""), font=2)
mtext(side=2,line=2,timeframe, font=1)
mtext(side=1,line=2, paste("Biomarker (", biomarker, ") at study end", sep=""), font=2)

##Study labels
for(i in 1: length(included$Study.name))
{
if (included$Study.name[i] == "Dargie" | included$Study.name[i] == "AleCardio")
  {
  #Put these lower
  #Long label
  #text(x=control_biomarker_post, y=Event_rate.con,labels=paste(Study.name, "(OR =",format(OR,digits=2),")"), cex=0.65, pos=4,adj=0,font=1,col='black')
  #Short label
  included$Study.name[i]
  text(x=control_biomarker_post[i], y=Event_rate_con[i] - 0.5,labels=Study.name[i], cex=0.65, pos=4,adj=0,font=1,col='black')
  }
else
  {
  #Short label
	#Put these higher
	if (included$Study.name[i] == "SAVOR-TIMI 53" | included$Study.name[i] == "UKPDS 34 all rx vs diet")
		{
		text(x=control_biomarker_post[i], y=Event_rate_con[i] + 0.5,labels=Study.name[i], cex=0.65, pos=4,adj=0,font=1,col='black')
		}
	else
		{
		text(x=control_biomarker_post[i], y=Event_rate_con[i],labels=Study.name[i], cex=0.65, pos=4,adj=0,font=1,col='black')
		}
  }
}

## draw arrows from point to point :
#s <- seq(length(x)+1)

included_OR <- meta.DSL(included [["exp_total"]], included [["control_total"]], included [["exp_events"]], included [["control_events"]],names=Study.name,conf.level=0.95)
summary(included_OR)

lower_CI=summary(included_OR)$ors[,2]
upper_CI=summary(included_OR)$ors[,3]
significant= (lower_CI>1 | upper_CI<1)
n_study=length(significant)
linetype=array(0,c(n_study,1))
linewidth=1
#BMJ: linewidth=2
for(i in 1: n_study)
	{
	if(significant[i]>0)
		linetype[i]= 1
	else 
		linetype[i]= 3 #BMJ: linetype[i]= 2
	}

s_OR_greater_than_one_data= which(included$OR>=1)

#BMJ: arrows(x2[s_OR_greater_than_one_data], y2[s_OR_greater_than_one_data],x[s_OR_greater_than_one_data],y[s_OR_greater_than_one_data], lwd=linewidth,length=0.2,col='red',lty=linetype[s_OR_greater_than_one_data])
arrows(x2[s_OR_greater_than_one_data], y2[s_OR_greater_than_one_data],x[s_OR_greater_than_one_data],y[s_OR_greater_than_one_data], lwd=linewidth,length=0.1,col='red',lty=linetype[s_OR_greater_than_one_data])

s_OR_less_than_one_data= which(included$OR<1)

#BMJ: arrows(x2[s_OR_less_than_one_data], y2[s_OR_less_than_one_data],x[s_OR_less_than_one_data],y[s_OR_less_than_one_data], lwd=linewidth,length=0.2,col="darkgreen", lty=linetype[s_OR_less_than_one_data])
arrows(x2[s_OR_less_than_one_data], y2[s_OR_less_than_one_data],x[s_OR_less_than_one_data],y[s_OR_less_than_one_data], lwd=linewidth,length=0.1,col="darkgreen", lty=linetype[s_OR_less_than_one_data])

#Notes
mtext(side=1,line=3,cex=0.9,adj=0,"Notes:", font=2)
mtext(side=1,line=5,cex=0.9,adj=0, "1. For each study, the arrow starts with the results of the control group and ends with the \nresults of the treatment group", font=1)
mtext(side=1,line=6,cex=0.9,adj=0,"2. Solid arrows indicate statistically significant change in clinical outcome.", font=1)
#mtext(side=1,line=8,cex=0.9,adj=0,"3. P-best is the p-value assuming a relative risk reduction from treatment of 0.1. P-worst is \nthe P-value assuming that the\nrelative benefit of treat is 1.0", font=1)

#-----------------------------------
# Meta-regression ------
erase.screen(2)  
screen(2)

library(rmeta)
temp_OR <- meta.DSL(temp [["exp_total"]], temp [["control_total"]], temp [["exp_events"]], temp [["control_events"]],names=Study.name)
summary(temp_OR)
studyweights <- 1 / (temp_OR$tau2 + temp_OR$selogs^2)
y <- temp_OR$logs

temp_mod <- lm(y ~ exp_HbA1c.post, data = temp , weights = studyweights)
summary(temp_mod)
attach(temp)
par(yaxt = "n")
plot(y ~ x, data = temp, main="Meta-regression", xlab="", ylab="",ylim=c(-2,2),axes=F,xaxs="r",xlim=c(6,10.7))
box()
text(x=x, y=y,labels=paste(Study.name), cex=0.65, pos=4,adj=0,font=1,col='black')
par(yaxt = "s")
abline(lm(y ~ x, data = temp, weights = studyweights))
abline(h=0, v=0, col = "gray90")
abline(h=1, v=0, col = "gray90")
abline(h=-1, v=0, col = "gray90")
abline(a=0, b=0,lty="dotted")
axis(1,c(6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10), tick = TRUE,c("6.0","6.5","7.0","7.5","8.0","8.5","9.0","9.5","10"))
box()
abline(h=0, v=6, col = "gray90")
abline(h=0, v=7, col = "gray90")
abline(h=0, v=8, col = "gray90")
abline(h=0, v=9, col = "gray90")
abline(h=0, v=10, col = "gray90")
mtext(side=1,line=2,"Surrogate outcome (HbA1c) at study end", font=2)
mtext(side=2,line=3,"Odds ratio transformed to natural log (Ln)", font=2)
mtext(side=2,line=2,"(0 indicates odds ratio = 1)")
mtext(side=3,line=0.5,"(Ln odds ratio below 0 favors treatment)")
#-----------------------------------
# Forest plot
erase.screen(3)  
screen(3)

plot(temp_OR,main="Forest plot",xlim=c(-3,3))
e <- meta.DSL(n.trt, n.ctrl, inf.trt, inf.ctrl, data=temp_OR,names=Name, subset=c(13,6,3,12,4,11,1,14,8,10,2))

