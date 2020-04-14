plot_size=2.3
#setwd("C:/Users/Shuxi ZENG/Dropbox/Third Year/OW_Randomization/results_0327/binary")

pdf("bin_MCSD.pdf",height=2.8*plot_size,width=4*plot_size)

m <- matrix(c(1,4,7,10,2,5,8,11,3,6,9,12,13,13,13,13),nrow = 4,ncol = 4,byrow = TRUE)

layout(mat = m,heights = c(0.5,0.5,0.5,0.15))
h.degree=0
pt=0.5
mis.specified=0
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)




h.degree=0
pt=0.5
mis.specified=1
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)


h.degree=0
pt=0.5
mis.specified=0
rare.degree=-0.85
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)


h.degree=0
pt=0.5
mis.specified=1
rare.degree=-0.85
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)


par(mar = c(0,0,1,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("top",inset=0,title="Relative efficiency to UNADJ", col=c("black","purple","blue","red"),lwd=1.5,
       lty=1,pch=c(1,3,6,2),legend=c("IPW","LR","AIPW","OW"),horiz=TRUE)


dev.off()






plot_size=2.3
pdf("bin_MCSD_variation.pdf",height=2.8*plot_size,width=4*plot_size)
#par(mfcol=c(3,4),mar = c(4, 4.5, 2, 1))
m <- matrix(c(1,4,7,10,2,5,8,11,3,6,9,12,13,13,13,13),nrow = 4,ncol = 4,byrow = TRUE)

layout(mat = m,heights = c(0.5,0.5,0.5,0.15))

h.degree=0.75
pt=0.5
mis.specified=0
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)



h.degree=0
pt=0.7
mis.specified=0
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)


h.degree=0
pt=0.5
mis.specified=0
rare.degree=-1.386
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-2.197
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_DIFF[,1],(MC_SD_DIFF[,2]/MC_SD_DIFF[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_RISK[,1],(MC_SD_RISK[,2]/MC_SD_RISK[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=1,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,4])^2,type='o',col="red",pch=2,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,5])^2,type='o',col="purple",pch=3,lwd=2)
lines(MC_SD_ODDS[,1],(MC_SD_ODDS[,2]/MC_SD_ODDS[,6])^2,type='o',col="blue",pch=6,lwd=2)
abline(h=1,lty=2,lwd=2)


par(mar = c(0,0,1,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("top",inset=0,title="Relative efficiency to UNADJ", col=c("black","purple","blue","red"),lwd=1.5,
       lty=1,pch=c(1,3,6,2),legend=c("IPW","LR","AIPW","OW"),horiz=TRUE)


dev.off()


pdf("non-covergence.pdf",width=4,height=4)
h.degree=0
pt=0.5
mis.specified=0
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
plot(NON_CONVER[,1],NON_CONVER[,2]/2000,type='l',lwd=2,ylim=c(0,1),
     main=" ",ylab="Probability",cex.main=2,cex.lab=1.5,
     xlab="Sample size")

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-0.85
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
lines(NON_CONVER[,1],NON_CONVER[,2]/2000,lwd=2,lty=2)

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-1.386
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
lines(NON_CONVER[,1],NON_CONVER[,2]/2000,lwd=2,lty=6)

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-2.197
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
lines(NON_CONVER[,1],NON_CONVER[,2]/2000,lwd=2,lty=4)
legend("topright",lty=c(1,2,6,4),lwd=2,legend=c("u=0.5","u=0.3","u=0.2","u=0.1"))
dev.off()