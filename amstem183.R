reginfo <- function(n, kk, ll, mm) {
  #lmm[nn] <<- mm
  #line <- sprintf("%d) %8.8s  %8.8s  %s", n, summary(mm)$r.squared, summary(mm)$adj.r.squared, ll)
  line <- sprintf("%2d) %9.4f  %9.4f  %9.4f %4d/%3d %10.3e  %s", n,
    as.numeric(summary(mm)$r.squared),
    as.numeric(summary(mm)$adj.r.squared),
    summary(mm)$fstatistic[1],
    summary(mm)$fstatistic[2],
    summary(mm)$fstatistic[3],
    anova(mm)$'Pr(>F)'[1], ll)
  return(line)
}

getslope <- function(jj, year1, year2) {
  #kk <- jj[jj$emp_imm > 0,]
  kk <- jj[!is.na(jj$emp_imm),]
  kk <- kk[kk$year >= year1 & kk$year <= year2,]
  #kk <- jj[jj$year >= year1 & jj$year <= year2,]
  mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
  ret <- sprintf("%7.4f", mm$coef[2])
  #print(paste(ret, year1, year2))
  return(ret)
}

getslopes <- function(n, jj, year0, year1, year2, year3) {
  line <- sprintf("%4d", year1)
  if (year1 >= year0)
  {
    for (yr in year0:year1)
    {
      line <- paste(line, "       ")
    }
  }
  for (yr in year2:year3)
  {
    line <- paste(line, getslope(jj, year1, yr))
  }
  line <- sprintf("%s  %4d", line, year1)
}

getjob <- function(jj, year1, year2) {
  #kk <- jj[jj$emp_imm > 0,]
  kk <- jj[!is.na(jj$emp_imm),]
  kk <- kk[kk$year >= year1 & kk$year <= year2,]
  #kk <- jj[jj$year >= year1 & jj$year <= year2,]
  mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
  pp <- sum(kk$emp_native) / sum(kk$emp_imm) * mm$coef[2] * 100
  ret <- sprintf("%7.1f", pp)
  #print(paste(ret, year1, year2))
  return(ret)
}

getjobs <- function(n, jj, year0, year1, year2, year3) {
  line <- sprintf("%4d", year1)
  if (year1 >= year0)
  {
    for (yr in year0:year1)
    {
      line <- paste(line, "       ")
    }
  }
  for (yr in year2:year3)
  {
    line <- paste(line, getjob(jj, year1, yr))
  }
  line <- sprintf("%s  %4d", line, year1)
}

library(foreign)
library(plyr)

dd <- read.dta("public_h1b.dta")
dd <- dd[dd$year != 2007,] # DROP 2007 DUE TO ERRORS
d0 <- dd
dd <- dd[dd$year >= 2001,] # NO H1B DATA FOR 2000
source("amjobsg.R")

dd$emp_imm <- dd$h1b
dd$imm_level <- dd$emp_imm / dd$emp_total
dd$ln_imm_level <- log(dd$imm_level)
dd$nat_level <- dd$emp_native / dd$pop_native
dd$ln_nat_level <- log(dd$nat_level)

dd$pop_native_t <- with(dd, ave(pop_native, year, FUN=sum))
dd$weight = dd$pop_native / dd$pop_native_t
dd$fyear <- as.factor(dd$year)
dd$floc <- as.factor(dd$fips)
dd$emp_imm_1000 <- dd$emp_imm / 1000

labyears="2001-2010" # CHANGE FOR YEARS
labyears2="2002-2010"

print("H1B AND NATIVES: NUMBER EMPLOYED AND LEVELS, BY YEAR")
print("")
ss <- ddply(dd, "year", summarise, emp_imm=sum(emp_imm), emp_native=sum(emp_native), emp_total=sum(emp_total), pop_native=sum(pop_native))
ss$imm_level <- 100* ss$emp_imm / ss$emp_total
ss$nat_level <- 100* ss$emp_native / ss$pop_native
ss$ln_imm_level <- log(ss$imm_level)
ss$ln_nat_level <- log(ss$nat_level)
print(ss)

print("")
print("                           JOBS    CORREL                                     ")
print(" N  INTERCEPT    SLOPE   CREATED    COEF   DESCRIPTION                        ")
print("--  ---------  --------  -------  -------  -----------------------------------")
lst = c("1")
kk <- dd
print(paste(labyears, ", ALL DATA", sep=""))
nn <- 1
ll <- "ln_nat_level ~ ln_imm_level"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level)))
jj <- sum(dd$emp_native) / sum(dd$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "ln_nat_level ~ ln_imm_level + fyear"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear)))
jj <- sum(dd$emp_native) / sum(dd$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "ln_nat_level ~ ln_imm_level + fyear + floc"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc)))
jj <- sum(dd$emp_native) / sum(dd$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
jj <- sum(d0$emp_native) / sum(dd$emp_imm) * 0.011 * 100 # 182.5 replicates study
#jj <- sum(dd$emp_native) / sum(dd$emp_imm) * 0.011 * 100 # 164.2 corrected with 0.011
#jj <- sum(dd$emp_native) / sum(dd$emp_imm) * mm$coef[2] * 100 # 171.6 actual
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

#USING ZAVODNY'S FORMULA
print("USING STUDY'S FORMULA")
year1 <- 2001
year2 <- 2010
kk <- dd[dd$year >= year1 & dd$year <= year2,]
nn <- nn+1
ll <- paste(year1,"-",year2,", study's data with corrected job count", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
jj <- sum(kk$emp_native) / sum(kk$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

year1 <- 2002
year2 <- 2010
kk <- dd[dd$year >= year1 & dd$year <= year2,]
nn <- nn+1
ll <- paste(year1,"-",year2,", skip bad data for 2001", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
jj <- sum(kk$emp_native) / sum(kk$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

year1 <- 2002
year2 <- 2008
kk <- dd[dd$year >= year1 & dd$year <= year2,]
nn <- nn+1
ll <- paste(year1,"-",year2,", skip worse years of job loss", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
jj <- sum(kk$emp_native) / sum(kk$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

year1 <- 2003
year2 <- 2006
kk <- dd[dd$year >= year1 & dd$year <= year2,]
nn <- nn+1
ll <- paste(year1,"-",year2,", skip all years of job loss", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
jj <- sum(kk$emp_native) / sum(kk$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

year1 <- 2003
year2 <- 2005
kk <- dd[dd$year >= year1 & dd$year <= year2,]
nn <- nn+1
ll <- paste(year1,"-",year2,", longest span of growing h1b level", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
jj <- sum(kk$emp_native) / sum(kk$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

kk <- dd
if (max(kk$year) >= 2010)
{
print("")
print("SLOPE BETWEEN GIVEN YEARS (using same regression as was used to obtain 183 job finding)")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print("year    2003    2004    2005    2006    2007    2008    2009    2010  year")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print( getslopes(1, kk, 2002, 2001, 2003, 2010))
print( getslopes(1, kk, 2002, 2002, 2004, 2010))
print( getslopes(1, kk, 2002, 2003, 2005, 2010))
print( getslopes(1, kk, 2002, 2004, 2006, 2010))
print( getslopes(1, kk, 2002, 2005, 2007, 2010))
print( getslopes(1, kk, 2002, 2006, 2008, 2010))
print( getslopes(1, kk, 2002, 2007, 2009, 2010))
print( getslopes(1, kk, 2002, 2008, 2010, 2010))
print("")
print("JOBS GAINED/LOST BETWEEN GIVEN YEARS (using same regression as was used to obtain 183 job finding)")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print("year    2003    2004    2005    2006    2007    2008    2009    2010  year")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print( getjobs(1, kk, 2002, 2001, 2003, 2010))
print( getjobs(1, kk, 2002, 2002, 2004, 2010))
print( getjobs(1, kk, 2002, 2003, 2005, 2010))
print( getjobs(1, kk, 2002, 2004, 2006, 2010))
print( getjobs(1, kk, 2002, 2005, 2007, 2010))
print( getjobs(1, kk, 2002, 2006, 2008, 2010))
print( getjobs(1, kk, 2002, 2007, 2009, 2010))
print( getjobs(1, kk, 2002, 2008, 2010, 2010))
}

#forlab1 <- "Employed Foreign STEM Workers with Adv US Degrees"
#forlab2 <- "Foreign Share of Total Employment (STEM w/ Adv US Deg)"
forlab1 <- "Thousands of H-1B Certified Workers"
forlab2 <- "H-1B Certification Level"

x11()
# Graph Native versus H-1B Certified Workers
kk <- dd
nn <- 1
with(kk, plot(emp_native/1000000 ~ emp_imm_1000, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Millions of Employed Native Workers", xlab=forlab1))
title(main=paste("Figure ", nn, ": Native versus H-1B Certified Workers, ", labyears, sep=""))
with(kk, text(emp_imm_1000, emp_native/1000000, labels=year-2000, cex=0.5, pos=4))
legend("bottomright", inset=0, title="State",
c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"),
cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE)
grid()
readline("Press enter to continue, escape to exit")

labyears <- "2002-2010"
x11()
# Graph Native versus H-1B Certified Workers
kk <- dd[dd$year >= 2002,]
nn <- nn+1
with(kk, plot(emp_native/1000000 ~ emp_imm_1000, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Millions of Employed Native Workers", xlab=forlab1))
title(main=paste("Figure ", nn, ": Native versus H-1B Certified Workers, ", labyears, sep=""))
with(kk, text(emp_imm_1000, emp_native/1000000, labels=year-2000, cex=0.5, pos=4))
legend("bottomright", inset=0, title="State",
c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"),
cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE)
grid()
readline("Press enter to continue, escape to exit")

kk <- dd
labyears <- "2001-2010"
x11()
# Graph Native Employment Level vs. H-1B Certification Level
nn <- nn+1
with(kk, plot(nat_level ~ imm_level, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Native Worker Employment Level", xlab=forlab2, xlim=c(0,0.04)))
title(main=paste("Figure ", nn, ": Native Employment Level vs. H-1B Certification Level\n", labyears, sep=""))
#with(kk, text(h1b_level_emp, emp_pop, labels=kk$ss, cex=0.5, pos=4))
legend("bottomright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
grid()
#lma <- (with(kk, lm(emp_pop ~ h1b_level_emp)))
#abline(lma)
readline("Press enter to continue, escape to exit")

x11()
# Graph Natural Logs of Native Employment Rate vs. H-1B Certification Level
nn <- nn+1
with(kk, plot(ln_nat_level ~ ln_imm_level, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. H-1B Certification Level\n", labyears, sep=""))
legend("bottomright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
grid()
#legend("bottomright", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
#  c("w/o weighting","with weighting"))
#lma <- (with(kk, lm(lnemprate_native ~ lnemp_immshare)))
#abline(mm3, col="red", lty=2)
#abline(mm4)
readline("Press enter to continue, escape to exit")

x11()
# Graph Natural Logs of Native Employment Rate vs. H-1B Certification Level
nn <- nn+1
with(kk, plot(ln_nat_level ~ ln_imm_level, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. H-1B Certification Level\n", labyears, sep=""))
legend("bottomright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
grid()
legend("bottomleft", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
  c("w/o weighting","with weighting"))
lma <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc)))
abline(lma, col="red", lty=2)
lmb <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
abline(lmb)
readline("Press enter to continue, escape to exit")

kk <- dd[dd$year >= 2003 & dd$year <= 2006,]
labyears <- "2003-2006"
x11()
# Graph Natural Logs of Native Employment Rate vs. H-1B Certification Level
nn <- nn+1
with(kk, plot(ln_nat_level ~ ln_imm_level, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. H-1B Certification Level\n", labyears, sep=""))
legend("bottomright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
grid()
#legend("bottomright", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
#  c("w/o weighting","with weighting"))
#lma <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
#abline(lma)
readline("Press enter to continue, escape to exit")

kk <- dd[dd$year >= 2003 & dd$year <= 2006,]
labyears <- "2003-2006"
x11()
# Graph Natural Logs of Native Employment Rate vs. H-1B Certification Level
nn <- nn+1
with(kk, plot(ln_nat_level ~ ln_imm_level, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. H-1B Certification Level\n", labyears, sep=""))
legend("bottomright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
grid()
legend("bottomleft", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
  c("w/o weighting","with weighting"))
lma <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc)))
abline(lma, col="red", lty=2)
lmb <- (with(kk, lm(ln_nat_level ~ ln_imm_level + fyear + floc, weights=weight)))
abline(lmb)
