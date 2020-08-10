plot_size=2.3
n_index=c(1:16)
pdf("bin_MCSD.pdf",height=2.8*plot_size,width=4*plot_size)

m <- matrix(c(1,4,7,10,2,5,8,11,3,6,9,12,13,13,13,13),nrow = 4,ncol = 4,byrow = TRUE)

layout(mat = m,heights = c(0.5,0.5,0.5,0.15))
h.degree=0
pt=0.5
mis.specified=0
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(a)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(a)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(a)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)




h.degree=0
pt=0.5
mis.specified=1
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(b)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(b)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(b",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)


h.degree=0
pt=0.5
mis.specified=0
rare.degree=-0.85
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(c)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(c)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(c)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch=15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch=19,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch=17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)


h.degree=0
pt=0.5
mis.specified=1
rare.degree=-0.85
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(d)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(d)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(d)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)


par(mar = c(0,0,1,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("top",inset=0,title="Relative efficiency to UNADJ", col=c("black","purple","red"),lwd=1.5,
       lty=1,pch=c(15,17,19),legend=c("IPW","LR","OW"),horiz=TRUE)


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
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(e)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(e)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(e)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)



h.degree=0
pt=0.7
mis.specified=0
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(f)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(f)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(f)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)


h.degree=0
pt=0.5
mis.specified=0
rare.degree=-1.386
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(g)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(g)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(g)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-2.197
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RD])),
     # main=substitute(paste("Standard deviation, risk difference, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0.5,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_DIFF[n_index,1],(MC_SD_DIFF[n_index,2]/MC_SD_DIFF[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,3])^2,type='o',
     xlab="",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[RR])),
     # main=substitute(paste("Standard deviation, log risk ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_RISK[n_index,1],(MC_SD_RISK[n_index,2]/MC_SD_RISK[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)

par(mar = c(4, 4.5, 2, 1))
plot(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,3])^2,type='o',
     xlab="Sample size",
     ylab="",
     #     ylab=expression(paste('SD'[DIF],' /SD')),
     main=expression(paste("(h)",tau[OR])),
     # main=substitute(paste("Standard deviation, log odds ratio, ",u,"=",rare,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(rare=round(1/(1+exp(-rare.degree)),2),h.degree=h.degree,s_string=s_string)),
     ylim=c(0,1.6),cex.main=2,cex.lab=1.5,
     col="black",pch = 15,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,4])^2,type='o',col="red",pch = 16,lwd=1.5)
lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,5])^2,type='o',col="purple",pch = 17,lwd=1.5)
# lines(MC_SD_ODDS[n_index,1],(MC_SD_ODDS[n_index,2]/MC_SD_ODDS[n_index,6])^2,type='o',col="blue",pch=6,lwd=1.5)
abline(h=1,lty=2,lwd=1.5)


par(mar = c(0,0,1,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("top",inset=0,title="Relative efficiency to UNADJ", col=c("black","purple","red"),lwd=1.5,
       lty=1,pch=c(15,17,19),legend=c("IPW","LR","OW"),horiz=TRUE)


dev.off()


pdf("non-covergence.pdf",width=4,height=4)
h.degree=0
pt=0.5
mis.specified=0
rare.degree=0
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
plot(NON_CONVER[n_index,1],NON_CONVER[n_index,2]/2000,type='l',lwd=1.5,ylim=c(0,1),
     main=" ",ylab="Probability",cex.main=2,cex.lab=1.5,
     xlab="Sample size")

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-0.85
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
lines(NON_CONVER[n_index,1],NON_CONVER[n_index,2]/2000,lwd=1.5,lty=2)

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-1.386
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
lines(NON_CONVER[n_index,1],NON_CONVER[n_index,2]/2000,lwd=1.5,lty=6)

h.degree=0
pt=0.5
mis.specified=0
rare.degree=-2.197
load(paste(rare.degree,h.degree,pt,mis.specified,"binary_result.RData",sep="_"))
lines(NON_CONVER[n_index,1],NON_CONVER[n_index,2]/2000,lwd=1.5,lty=4)
legend("topright",lty=c(1,2,6,4),lwd=1.5,legend=c("u=0.5","u=0.3","u=0.2","u=0.1"))
dev.off()