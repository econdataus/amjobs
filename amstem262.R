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
  ii <- jj[jj$year >= year1 & jj$year <= year2,]
  kk <- ii[ii$emp_imm > 0,]
  kk <- kk[kk$emp_imm2 > 0,]
  #kk <- kk[!is.na(kk$emp_imm),]
  mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
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
  ii <- jj[jj$year >= year1 & jj$year <= year2,]
  kk <- ii[ii$emp_imm > 0,]
  kk <- kk[kk$emp_imm2 > 0,]
  #kk <- kk[!is.na(kk$emp_imm),]
  mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
  pp <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
  #pp <- (sum(ii$emp_native) / sum(ii$emp_imm)) * (floor(1000 * mm$coef[2]) / 10) # trucated slope
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

getjob_trunc <- function(jj, year1, year2) {
  ii <- jj[jj$year >= year1 & jj$year <= year2,]
  kk <- ii[ii$emp_imm > 0,]
  kk <- kk[kk$emp_imm2 > 0,]
  #kk <- kk[!is.na(kk$emp_imm),]
  mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
  #pp <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
  pp <- (sum(ii$emp_native) / sum(ii$emp_imm)) * (floor(1000 * mm$coef[2]) / 10) # trucated slope
  ret <- sprintf("%7.1f", pp)
  #ret <- sprintf("%7.2f", pp) # 2 decimal places so that 262 shown
  #print(paste(ret, year1, year2))
  return(ret)
}

getjobs_trunc <- function(n, jj, year0, year1, year2, year3) {
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
    line <- paste(line, getjob_trunc(jj, year1, yr))
  }
  line <- sprintf("%s  %4d", line, year1)
}

library(foreign)
library(plyr)

dd <- read.table("morg13.txt") # CHANGE FOR YEARS
source("amjobsg.R")

dd$emp_imm <- dd$emp_edus_stem_grad
dd$imm_level <- dd$emp_imm / dd$emp_total
dd$ln_imm_level <- log(dd$imm_level)
dd$nat_level <- dd$emp_native / dd$pop_native
dd$ln_nat_level <- log(dd$nat_level)

dd$emp_imm2 <- dd$emp_nedus_stem_grad
dd$imm_level2 <- dd$emp_imm2 / dd$emp_total
dd$ln_imm_level2 <- log(dd$imm_level2)

dd$pop_native_t <- with(dd, ave(pop_native, year, FUN=sum))
dd$weight = dd$pop_native / dd$pop_native_t
dd$fyear <- as.factor(dd$year)
dd$floc <- as.factor(dd$state)
dd$emp_imm_1000 <- dd$emp_imm / 1000

labyears="2000-2013" # CHANGE FOR YEARS
#labyears2="2001-2013"

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
labyears="2000-2007" # CHANGE FOR YEARS
year1 <- 2000
year2 <- 2007
ii <- dd[dd$year >= year1 & dd$year <= year2,]
kk <- ii[ii$emp_imm > 0,] 
kk <- kk[kk$emp_imm2 > 0,] 
print(paste(labyears, ", ALL DATA", sep=""))
nn <- 1
ll <- "ln_nat_level ~ ln_imm_level + ln_imm_level2"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight"
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * 0.004 * 100 # 262.99 replicates study
#jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100 # 293.44 actual
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

#USING ZAVODNY'S FORMULA
print("USING STUDY'S FORMULA")
year1 <- 2000
year2 <- 2007
ii <- dd[dd$year >= year1 & dd$year <= year2,]
kk <- ii[ii$emp_imm > 0,] 
kk <- kk[kk$emp_imm2 > 0,] 
nn <- nn+1
ll <- paste(year1,"-",year2,", study's data with corrected job count", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

year1 <- 2002
year2 <- 2005
ii <- dd[dd$year >= year1 & dd$year <= year2,]
kk <- ii[ii$emp_imm > 0,] 
kk <- kk[kk$emp_imm2 > 0,] 
nn <- nn+1
ll <- paste(year1,"-",year2,", during first span of increasing immigrant level", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

year1 <- 2006
year2 <- 2009
ii <- dd[dd$year >= year1 & dd$year <= year2,]
kk <- ii[ii$emp_imm > 0,] 
kk <- kk[kk$emp_imm2 > 0,] 
nn <- nn+1
ll <- paste(year1,"-",year2,", during second  span of increasing immigrant level", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

year1 <- 2002
year2 <- 2009
ii <- dd[dd$year >= year1 & dd$year <= year2,]
kk <- ii[ii$emp_imm > 0,] 
kk <- kk[kk$emp_imm2 > 0,] 
nn <- nn+1
ll <- paste(year1,"-",year2,", during increasing immigrant level (except 2005-06)", sep="")
mm <- (with(kk, lm(ln_nat_level ~ ln_imm_level + ln_imm_level2 + fyear + floc, weights=weight)))
jj <- sum(ii$emp_native) / sum(ii$emp_imm) * mm$coef[2] * 100
print(sprintf("%2d) %9.4f %9.4f %8.1f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], jj, with(kk, cor(ln_imm_level, ln_nat_level)), ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

#kk <- dd[dd$emp_imm > 0,] 
#kk <- kk[kk$emp_imm2 > 0,]
kk <- dd
if (max(kk$year) >= 2013)
{
print("")
print("SLOPE BETWEEN GIVEN YEARS (using same regression as was used to obtain 262 job finding)           ")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print("year    2003    2004    2005    2006    2007    2008    2009    2010    2011    2012    2013  year")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print( getslopes(1, kk, 2001, 2000, 2003, 2013))
print( getslopes(1, kk, 2001, 2001, 2004, 2013))
print( getslopes(1, kk, 2001, 2002, 2005, 2013))
print( getslopes(1, kk, 2001, 2003, 2006, 2013))
print( getslopes(1, kk, 2001, 2004, 2007, 2013))
print( getslopes(1, kk, 2001, 2005, 2008, 2013))
print( getslopes(1, kk, 2001, 2006, 2009, 2013))
print( getslopes(1, kk, 2001, 2007, 2010, 2013))
print( getslopes(1, kk, 2001, 2008, 2011, 2013))
print( getslopes(1, kk, 2001, 2009, 2012, 2013))
print( getslopes(1, kk, 2001, 2010, 2013, 2013))
print("")
print("NATIVE JOBS GAINED/LOST BETWEEN GIVEN YEARS (using same regression as was used to obtain 262 job finding but with truncation errors)")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print("year    2003    2004    2005    2006    2007    2008    2009    2010    2011    2012    2013  year")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print( getjobs_trunc(1, kk, 2001, 2000, 2003, 2013))
print( getjobs_trunc(1, kk, 2001, 2001, 2004, 2013))
print( getjobs_trunc(1, kk, 2001, 2002, 2005, 2013))
print( getjobs_trunc(1, kk, 2001, 2003, 2006, 2013))
print( getjobs_trunc(1, kk, 2001, 2004, 2007, 2013))
print( getjobs_trunc(1, kk, 2001, 2005, 2008, 2013))
print( getjobs_trunc(1, kk, 2001, 2006, 2009, 2013))
print( getjobs_trunc(1, kk, 2001, 2007, 2010, 2013))
print( getjobs_trunc(1, kk, 2001, 2008, 2011, 2013))
print( getjobs_trunc(1, kk, 2001, 2009, 2012, 2013))
print( getjobs_trunc(1, kk, 2001, 2010, 2013, 2013))
print("")
print("NATIVE JOBS GAINED/LOST PER EACH 100 STEM WORKERS WITH ADVANCED US DEGREES BETWEEN GIVEN YEARS (using study's methodology with no errors)")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print("year    2003    2004    2005    2006    2007    2008    2009    2010    2011    2012    2013  year")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print( getjobs(1, kk, 2001, 2000, 2003, 2013))
print( getjobs(1, kk, 2001, 2001, 2004, 2013))
print( getjobs(1, kk, 2001, 2002, 2005, 2013))
print( getjobs(1, kk, 2001, 2003, 2006, 2013))
print( getjobs(1, kk, 2001, 2004, 2007, 2013))
print( getjobs(1, kk, 2001, 2005, 2008, 2013))
print( getjobs(1, kk, 2001, 2006, 2009, 2013))
print( getjobs(1, kk, 2001, 2007, 2010, 2013))
print( getjobs(1, kk, 2001, 2008, 2011, 2013))
print( getjobs(1, kk, 2001, 2009, 2012, 2013))
print( getjobs(1, kk, 2001, 2010, 2013, 2013))
}

forlab1 <- "Employed Foreign STEM Workers with Adv US Degrees"
forlab2 <- "Foreign Share of Total Employment (STEM w/ Adv US Deg)"
labyears="2000-2013" # CHANGE FOR YEARS

x11()
# Graph Natural Logs of Native Employment Rate vs. Immigrant Share
kk <- dd
nn <- 1
with(kk, plot(ln_nat_level ~ ln_imm_level, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. Immigrant Share\n", labyears, sep=""))
#legend("bottomright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
#  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
grid()
#legend("bottomright", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
#  c("w/o weighting","with weighting"))
#lma <- (with(kk, lm(lnemprate_native ~ lnemp_immshare)))
#abline(mm3, col="red", lty=2)
#abline(mm4)
readline("Press enter to continue, escape to exit")
