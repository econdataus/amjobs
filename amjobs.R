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
  jj$lnimmshare_emp_stem_n_grad <- log(jj$immshare_emp_stem_n_grad)
  kk <- jj[jj$immshare_emp_stem_e_grad > 0,]
  kk <- kk[kk$immshare_emp_stem_n_grad > 0,]
  kk <- kk[kk$year >= year1 & kk$year <= year2,]
  mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + lnimmshare_emp_stem_n_grad + fyear + fstate,
                     weights=weight_native)))
  ret <- sprintf("%7.4f", mm$coef[2])
  #print(paste(ret, year1, year2))
  return(ret)
}

getslopes <- function(n, jj, year1, year2, year3) {
  line <- sprintf("%4d", year1)
  if (year1 > 2000)
  {
    for (yr in 2001:year1)
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

plotstate1 <- function(nn, st, state) {
  x11()
  # Graph Natural Logs of Native Employment Rate vs. Immigrant Share Minus Effects of Year for State
  #nn <- nn+1
  kk = dd[dd$st == st,]
  kk$yPos <- 4
  with(kk, plot(lnemprate_native_yr ~ lnemp_immshare, cex=0.5, pch=3, col="black",
    ylab="Log of Native Employment Rate (minus effects of year)", xlab=paste("Log of", forlab2)))
  with(kk, text(lnemprate_native_yr ~ lnemp_immshare, labels=kk$yLab, cex=0.6, pos=yPos, offset=0.3, col="black"))
  title(main=paste("Figure ", nn, ": Native Employment Rate Adjusted vs. Immigrant Share\n", state, ", ", labyears, sep=""))
  grid()
  lm1 <- (with(kk, lm(lnemprate_native_yr ~ lnimmshare_emp_stem_e_grad)))
  abline(lm1)
  readline("Press enter to continue, escape to exit")
}

plotstate <- function(nn, st, state) {
  x11()
  # Graph Natural Logs of Native Employment Rate vs. Immigrant Share Minus Effects of Year for State
  #nn <- nn+1
  kk = dd[dd$st == st,]
  yy <- append(kk$lnemprate_native, kk$lnemprate_native_yr)
  xx <- append(kk$lnemp_immshare, kk$lnemp_immshare)
  kk$yPos  <- 4
  kk$yCol  <- "deepskyblue"
  kk$yCol2 <- "black"
  ccol <- append(kk$yCol, kk$yCol2)
  with(kk, plot(yy ~ xx, cex=0.5, pch=3, col=ccol,
    ylab="Log of Native Employment Rate (minus effects of year)", xlab=paste("Log of", forlab2)))
  with(kk, text(yy ~ xx, labels=append(kk$yLab,kk$yLab), cex=0.6, pos=yPos, offset=0.3, col=ccol))
  title(main=paste("Figure ", nn, ": Native Employment Rate Adjusted vs. Immigrant Share\n", state, ", ", labyears, sep=""))
  legend("bottomleft", inset=0, cex=0.5, lty=c(1,2), col=c("black","deepskyblue"), horiz=FALSE, c("adjusted by year","non-adjusted"))
  grid()
  lm1 <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad)))
  abline(lm1, col="deepskyblue", lty=2)
  lm2 <- (with(kk, lm(lnemprate_native_yr ~ lnimmshare_emp_stem_e_grad)))
  abline(lm2)
  readline("Press enter to continue, escape to exit")
}

# FOLLOWING VARIABLES NEED TO BE DEFINED BEFORE CALLING amjobs.R
# dd
# dd$emp_imm
# labyears
# forlab1
# forlab2
# dd$lf_native (may be redefined)

dd[is.na(dd)] <- 0
dd$statefip <- dd$state

#print(summary(dd))
print("dim(dd)")
print(dim(dd))
source("amjobsg.R")

# Create emprate_native and its log
dd$emprate_native   <- dd$emp_native / dd$lf_native * 100
dd$lnemprate_native <- log(dd$emprate_native)
source(paste("amjobs_coef", ".R", sep=filex))

# Create immshares
dd$immshare_emp_stem_e_grad   <- dd$emp_edus_stem_grad  / dd$emp_total * 100
ee <- dd[dd$immshare_emp_stem_e_grad <= 0,]
print(dim(ee))
dd$immshare_emp_stem_n_grad   <- dd$emp_nedus_stem_grad / dd$emp_total * 100
ee <- dd[dd$immshare_emp_stem_n_grad <= 0,]
print(dim(ee))
dd$immshare_emp_stem_grad     <- dd$immshare_emp_stem_e_grad + dd$immshare_emp_stem_n_grad
ee <- dd[dd$immshare_emp_stem_grad <= 0,]
print(dim(ee))
dd$immshare_emp_immig_grad    <- dd$emp_immig_grad  / dd$emp_total * 100
ee <- dd[dd$immshare_emp_immig_grad <= 0,]
print(dim(ee))
dd$immshare_emp_immig_coll    <- dd$emp_immig_coll  / dd$emp_total * 100
ee <- dd[dd$immshare_emp_immig_coll <= 0,]
print(dim(ee))
dd$immshare_emp_nostem_grad   <- dd$immshare_emp_immig_grad - dd$immshare_emp_stem_grad
ee <- dd[dd$immshare_emp_nostem_grad <= 0,]
print(dim(ee))
dd$immshare_emp_only_coll     <- dd$immshare_emp_immig_coll - dd$immshare_emp_immig_grad
ee <- dd[dd$immshare_emp_only_coll <= 0,]
print(dim(ee))
dd$immshare_emp_immig         <- dd$emp_immig / dd$emp_total * 100
ee <- dd[dd$immshare_emp_immig <= 0,]
print(dim(ee))
dd$immshare_emp_no_coll       <- dd$immshare_emp_immig - dd$immshare_emp_immig_coll
ee <- dd[dd$immshare_emp_no_coll <= 0,]
print(dim(ee))

# Calculate weight_native and create fyear and fstate
dd$sum_lf_native <- with(dd, ave(lf_native, year, FUN=sum))
dd$weight_native <- dd$lf_native / dd$sum_lf_native
dd$fyear  <- as.factor(dd$year)
dd$fstate <- as.factor(dd$state)
dd$lnimmshare_emp_stem_e_grad <- log(dd$immshare_emp_stem_e_grad)

# UNCOMMENT FOR 2000-2007 JOBS CREATED
# Calculate additional native jobs created
print("ADDITIONAL JOBS AMONG US NATIVE CREATED BY 100 FOREIGN-BORN WORKERS IN...")
print(sprintf("%9.4f  STEM fields with advanced degrees from US universities",
  sum(dd$emp_native)/sum(dd$emp_edus_stem_grad)*0.004*100))
print(sprintf("%9.4f  STEM fields with advanced degrees from any universities",
  sum(dd$emp_native)/(sum(dd$emp_edus_stem_grad)+sum(dd$emp_nedus_stem_grad))*0.003*100))
print(sprintf("%9.4f  any field with advanced degrees from any universities",
  sum(dd$emp_native)/(sum(dd$emp_edus_grad)+sum(dd$emp_nedus_grad))*0.008*100))

# UNCOMMENT FOR 2000-2007 JOBS CREATED (RANGE)
# Calculate additional native jobs created (minimum and maximum values)
print("ADDITIONAL JOBS AMONG US NATIVE CREATED BY 100 FOREIGN-BORN WORKERS IN... (RANGE)")
print(sprintf("%9.4f-%9.4f  STEM fields with advanced degrees from US universities",
  sum(dd$emp_native)/sum(dd$emp_edus_stem_grad)*0.0035*100,
  sum(dd$emp_native)/sum(dd$emp_edus_stem_grad)*0.0045*100))
print(sprintf("%9.4f-%9.4f  STEM fields with advanced degrees from any universities",
  sum(dd$emp_native)/(sum(dd$emp_edus_stem_grad)+sum(dd$emp_nedus_stem_grad))*0.0025*100,
  sum(dd$emp_native)/(sum(dd$emp_edus_stem_grad)+sum(dd$emp_nedus_stem_grad))*0.0033*100))
print(sprintf("%9.4f-%9.4f  any field with advanced degrees from any universities",
  sum(dd$emp_native)/(sum(dd$emp_edus_grad)+sum(dd$emp_nedus_grad))*0.0075*100,
  sum(dd$emp_native)/(sum(dd$emp_edus_grad)+sum(dd$emp_nedus_grad))*0.0085*100))

# Save dd0 and exclude points with zero emp_imm
dd$emp_immshare <- dd$emp_imm / dd$emp_total * 100
dd0 <- dd
dd <- dd0[dd0$emp_imm > 0,]
dd$lnemp_immshare <- log(dd$emp_immshare)

print("dim(dd0)")
print(dim(dd0))

# Include just California to create ddca
ddca = dd[dd$statefip == 6,]
print("dim(ddca)")
print(dim(ddca))

print("Minimum non-zero count of foreign-born workers in STEM fields with advanced degrees from US universities")
print(min(dd$emp_imm))
print("                          CORREL                                              ")
print(" N  INTERCEPT    SLOPE     COEF   P-VALUE  Y VARIABLE ~ X VARIABLE [, WEIGHTS]")
print("--  ---------  --------  -------  -------  -----------------------------------")

lst = c("1")
print(paste(labyears, "ALL DATA", sep=", "))
nn <- 1
ll <- "emprate_native ~ immshare_emp_stem_e_grad"
kk <- dd0
mm <- (with(kk, lm(emprate_native ~ immshare_emp_stem_e_grad)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(emprate_native, immshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

print(paste(labyears, "EXCLUDING POINTS WITH ZERO FOREIGN WORKERS IN STEM WITH ADVANCED US DEGREES", sep=", "))
nn <- nn+1
ll <- "emprate_native ~ immshare_emp_stem_e_grad"
kk <- dd
mm <- (with(kk, lm(emprate_native ~ immshare_emp_stem_e_grad)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(emprate_native, immshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm2 <- mm
nn <- nn+1
ll <- "lnemprate_native ~ lnimmshare_emp_stem_e_grad"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm3 <- mm
nn <- nn+1
ll <- "lnemprate_native ~ lnimmshare_emp_stem_e_grad, weights=weight_native"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm4 <- mm
  
print("                          CORREL                                              ")
print(" N  INTERCEPT    SLOPE     COEF   P-VALUE  DESCRIPTION                        ")
print("--  ---------  --------  -------  -------  -----------------------------------")
print(paste(labyears, "WEIGHTED WITH DUMMY VARIABLES", sep=", "))
nn <- nn+1
ll <- "without dummy variables"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm5 <- mm
nn <- nn+1
ll <- "with year dummy variables only"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fyear, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm6 <- mm
nn <- nn+1
ll <- "with state dummy variables only"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fstate, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm7 <- mm
nn <- nn+1
ll <- "with year and state dummy variables"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fyear
  + AK+AR+AZ+CA+CO+CT+DC+DE+FL+GA+HI+IA+ID+IL+IN+KS+KY+LA+MA+MD+ME+MI+MN+MO+MS+MT+NC+ND+NE+NH+NJ+NM+NV+NY+OH+OK+OR+PA+RI+SC+SD+TN+TX+UT+VA+VT+WA+WI, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm8 <- mm

print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE ADJUSTED TO REMOVE EFFECTS OF YEAR AND STATE", sep=", "))
nn <- nn+1
ll <- "with year and state dummy variables, unweighted"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fyear + fstate)))
print(sprintf("%2d) %9.4f %9.5f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm9 <- mm
# use coefficients from previous regression to create amjobs_coefXX.R which sets lnemprate_native2
#readline("Press enter to continue, escape to exit")
nn <- nn+1
ll <- "native employment rate adjusted to remove effects of year and state"
mm <- (with(kk, lm(lnemprate_native2 ~ lnimmshare_emp_stem_e_grad)))
print(sprintf("%2d) %9.4f %9.5f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native2, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm10 <- mm
nn <- nn+1
ll <- "native employment rate adjusted to remove effects of year and state, weighted"
mm <- (with(kk, lm(lnemprate_native2 ~ lnimmshare_emp_stem_e_grad, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.5f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native2, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm11 <- mm

print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE ADJUSTED TO REMOVE EFFECTS OF YEAR ONLY", sep=", "))
nn <- nn+1
ll <- "with year dummy variable, unweighted"
mm <- (with(kk, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fyear)))
print(sprintf("%2d) %9.4f %9.5f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm9yr <- mm
# use coefficients from previous regression to create amjobs_coyrXX.R which sets lnemprate_native_yr
#readline("Press enter to continue, escape to exit")
nn <- nn+1
ll <- "native employment rate adjusted to remove effects of year"
mm <- (with(kk, lm(lnemprate_native_yr ~ lnimmshare_emp_stem_e_grad)))
print(sprintf("%2d) %9.4f %9.5f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm10yr <- mm
nn <- nn+1
ll <- "native employment rate adjusted to remove effects of year, weighted"
mm <- (with(kk, lm(lnemprate_native_yr ~ lnimmshare_emp_stem_e_grad, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.5f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnimmshare_emp_stem_e_grad)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
mm11yr <- mm

print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE (YEAR-ADJUSTED) VS IMMIGRANT SHARE, BY STATE", sep=", "))
nn <- nn+1
ll <- "California"
kk = dd[dd$st == "CA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Connecticut"
kk = dd[dd$st == "CT",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "District of Columbia"
kk = dd[dd$st == "DC",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Florida"
kk = dd[dd$st == "FL",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Georgia"
kk = dd[dd$st == "GA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Illinois"
kk = dd[dd$st == "IL",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Maryland"
kk = dd[dd$st == "MD",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Massachusetts"
kk = dd[dd$st == "MA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Michigan"
kk = dd[dd$st == "MI",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "New Jersey"
kk = dd[dd$st == "NJ",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "New York"
kk = dd[dd$st == "NY",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Ohio"
kk = dd[dd$st == "OH",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Oregon"
kk = dd[dd$st == "OR",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Pennsylvania"
kk = dd[dd$st == "PA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Texas"
kk = dd[dd$st == "TX",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Virginia"
kk = dd[dd$st == "VA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Washington"
kk = dd[dd$st == "WA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ lnemp_immshare)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, lnemp_immshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

print("                          CORREL                                                             ")
print(" N  INTERCEPT    SLOPE     COEF   P-VALUE  T.R.C    OLS   Y VARIABLE ~ X VARIABLE [, WEIGHTS]")
print("--  ---------  --------  -------  -------  -----  ------  -----------------------------------")
print(paste(labyears, "OLS WITH YEAR, STATE, AND SPECIFIED GROUP OF FOREIGN WORKERS", sep=", "))

nn <- nn+1
ll <- "Advanced US degree and in STEM occupation"
dd0$share <- dd0$immshare_emp_stem_e_grad
kk <- dd0[dd0$share > 0,]
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  3.3.1   0.004  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
#readline("Press enter to continue, escape to exit")

nn <- nn+1
ll <- "Advanced foreign degree and in STEM occupation"
dd0$share <- dd0$immshare_emp_stem_n_grad # REPLACE X-VAR
kk <- dd0[dd0$share > 0,]
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("%d) %9.4f %9.4f %8.4f %8.4f  3.3.3 -0.0002  %s", # REPLACE N, TRC, OLS AND LABEL
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "Advanced US|foreign degree and in STEM occupation"
dd0$share <- dd0$immshare_emp_stem_e_grad
kk <- dd0[dd0$share > 0,]
kk <- kk[kk$immshare_emp_stem_n_grad > 0,]
kk$lnshare <- log(kk$share)
kk$lnimmshare_emp_stem_n_grad <- log(kk$immshare_emp_stem_n_grad)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + lnimmshare_emp_stem_n_grad + fyear + fstate, weights=weight_native)))
print(sprintf("%2da)%9.4f %9.4f %8.4f %8.4f  3.3.1   0.004  Advanced US degree and in STEM occupation",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1]))
print(sprintf("%2db)%9.4f %9.4f %8.4f %8.4f  3.3.3 -0.0002  Advanced foreign degree and in STEM occupation",
  nn, mm$coef[1], mm$coef[3], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[2]))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "Advanced degree and in STEM occupation"
dd0$share <- dd0$immshare_emp_stem_grad
kk <- dd0[dd0$share > 0,]
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("%d) %9.4f %9.4f %8.4f %8.4f  1.4.1   0.004  Advanced degree and in STEM occupation",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1]))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "Advanced degree"
dd0$share <- dd0$immshare_emp_immig_grad
kk <- dd0[dd0$share > 0,]
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  1.3.1   0.011  Advanced degree",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1]))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "Bachelor's degree or higher"
dd0$share <- dd0$immshare_emp_immig_coll
kk <- dd0[dd0$share > 0,]
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("%d) %9.4f %9.4f %8.4f %8.4f  1.2.1   0.008  Bachelor's degree or higher",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1]))
lst[nn] <- reginfo(20, nn, ll, mm)

nn <- nn+1
ll <- "Advanced degree and NOT in STEM occupation"
dd0$share <- dd0$immshare_emp_nostem_grad
kk <- dd0[dd0$share > 0,]
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("%d) %9.4f %9.4f %8.4f %8.4f  .....  ......  Advanced degree and NOT in STEM occupation",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1]))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "Bachelor's degree only"
dd0$share <- dd0$immshare_emp_only_coll
kk <- dd0[dd0$share > 0,]
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("%d) %9.4f %9.4f %8.4f %8.4f  .....  ......  Bachelor's degree only",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1]))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "OLS with Year, State, and 4 Subsets"
print("                          CORREL                                              ")
print(" N  INTERCEPT    SLOPE     COEF   P-VALUE  DESCRIPTION                        ")
print("--  ---------  --------  -------  -------  -----------------------------------")
print(paste(labyears, "OLS WITH YEAR, STATE, AND 4 SUBSETS OF FOREIGN WORKERS WITH BACHELOR'S DEGREE OR HIGHER", sep=", "))
dd0$share <- dd0$immshare_emp_stem_e_grad
kk <- dd0[dd0$share > 0,]
kk <- kk[kk$immshare_emp_stem_n_grad > 0,]
kk <- kk[kk$immshare_emp_nostem_grad > 0,]
kk$lnshare <- log(kk$share)
kk$lnimmshare_emp_stem_n_grad <- log(kk$immshare_emp_stem_n_grad)
kk$lnimmshare_emp_nostem_grad <- log(kk$immshare_emp_nostem_grad)
kk$lnimmshare_emp_only_coll   <- log(kk$immshare_emp_only_coll)
mm <- (with(kk, lm(lnemprate_native ~ lnshare + lnimmshare_emp_stem_n_grad + lnimmshare_emp_nostem_grad + lnimmshare_emp_only_coll
  + fyear + fstate, weights=weight_native)))
print(sprintf("%2da)%9.4f %9.4f %8.4f %8.4f  Advanced US degree and in STEM occupation",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1]))
print(sprintf("%2db)%9.4f %9.4f %8.4f %8.4f  Advanced foreign degree and in STEM occupation",
  nn, mm$coef[1], mm$coef[3], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[2]))
print(sprintf("%2dc)%9.4f %9.4f %8.4f %8.4f  Advanced degree and NOT in STEM occupation",
  nn, mm$coef[1], mm$coef[4], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[3]))
print(sprintf("%2dd)%9.4f %9.4f %8.4f %8.4f  Bachelor's degree only",
  nn, mm$coef[1], mm$coef[5], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[4]))
lst[nn] <- reginfo(nn, kk, ll, mm)

nn <- nn+1
ll <- "OLS with Year and State only"
#dd0$share <- dd0$immshare_emp_stem_e_grad
#kk <- dd0[dd0$share > 0,]
kk <- dd0
kk$lnshare <- log(kk$share)
mm <- (with(kk, lm(lnemprate_native ~ fyear + fstate, weights=weight_native)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native, lnshare)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

if (max(dd0$year) >= 2013)
{
print("")
print("SLOPE BETWEEN GIVEN YEARS (using same regression as was used to obtain 2.62 job finding)  ")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print("year    2004    2005    2006    2007    2008    2009    2010    2011    2012    2013  year")
print("----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ----")
print( getslopes(1, dd0, 2000, 2004, 2013))
print( getslopes(1, dd0, 2001, 2005, 2013))
print( getslopes(1, dd0, 2002, 2006, 2013))
print( getslopes(1, dd0, 2003, 2007, 2013))
print( getslopes(1, dd0, 2004, 2008, 2013))
print( getslopes(1, dd0, 2005, 2009, 2013))
print( getslopes(1, dd0, 2006, 2010, 2013))
print( getslopes(1, dd0, 2007, 2011, 2013))
print( getslopes(1, dd0, 2008, 2012, 2013))
print( getslopes(1, dd0, 2009, 2013, 2013))
}

print("                          CORREL                                              ")
print(" N  INTERCEPT    SLOPE     COEF   P-VALUE  DESCRIPTION                        ")
print("--  ---------  --------  -------  -------  -----------------------------------")
print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE (YEAR-ADJUSTED) VS YEAR, BY STATE", sep=", "))
nn <- nn+1
ll <- "California"
kk = dd[dd$st == "CA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Connecticut"
kk = dd[dd$st == "CT",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "District of Columbia"
kk = dd[dd$st == "DC",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Florida"
kk = dd[dd$st == "FL",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Georgia"
kk = dd[dd$st == "GA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Illinois"
kk = dd[dd$st == "IL",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Maryland"
kk = dd[dd$st == "MD",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Massachusetts"
kk = dd[dd$st == "MA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Michigan"
kk = dd[dd$st == "MI",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "New Jersey"
kk = dd[dd$st == "NJ",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "New York"
kk = dd[dd$st == "NY",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Ohio"
kk = dd[dd$st == "OH",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Oregon"
kk = dd[dd$st == "OR",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Pennsylvania"
kk = dd[dd$st == "PA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Texas"
kk = dd[dd$st == "TX",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Virginia"
kk = dd[dd$st == "VA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Washington"
kk = dd[dd$st == "WA",]
mm <- (with(kk, lm(lnemprate_native_yr ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemprate_native_yr, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

print(paste(labyears, "IMMIGRANT SHARE VS YEAR, BY STATE", sep=", "))
nn <- nn+1
ll <- "California"
kk = dd[dd$st == "CA",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Connecticut"
kk = dd[dd$st == "CT",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "District of Columbia"
kk = dd[dd$st == "DC",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Florida"
kk = dd[dd$st == "FL",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Georgia"
kk = dd[dd$st == "GA",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Illinois"
kk = dd[dd$st == "IL",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Maryland"
kk = dd[dd$st == "MD",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Massachusetts"
kk = dd[dd$st == "MA",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Michigan"
kk = dd[dd$st == "MI",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "New Jersey"
kk = dd[dd$st == "NJ",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "New York"
kk = dd[dd$st == "NY",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Ohio"
kk = dd[dd$st == "OH",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Oregon"
kk = dd[dd$st == "OR",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Pennsylvania"
kk = dd[dd$st == "PA",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Texas"
kk = dd[dd$st == "TX",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Virginia"
kk = dd[dd$st == "VA",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)
nn <- nn+1
ll <- "Washington"
kk = dd[dd$st == "WA",]
mm <- (with(kk, lm(lnemp_immshare ~ year)))
print(sprintf("%2d) %9.4f %9.4f %8.4f %8.4f  %s",
  nn, mm$coef[1], mm$coef[2], with(kk, cor(lnemp_immshare, year)), anova(mm)$'Pr(>F)'[1], ll))
lst[nn] <- reginfo(nn, kk, ll, mm)

print("")
print("     MULTIPLE   ADJUSTED      F-                                    ")
print(" N  R-SQUARED  R-SQUARED  STATISTIC  DF1/DF2   P-VALUE   DESCRIPTION")
print("--  ---------  ---------  ---------  -------  ---------  -----------")
print(paste(labyears, "ALL DATA", sep=", "))
print(lst[1])
print(paste(labyears, "EXCLUDING POINTS WITH ZERO FOREIGN WORKERS IN STEM WITH ADVANCED US DEGREES", sep=", "))
print(lst[2:4])
print(paste(labyears, "WEIGHTED WITH DUMMY VARIABLES", sep=", "))
print(lst[5:8])
print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE ADJUSTED TO REMOVE EFFECTS OF YEAR AND STATE", sep=", "))
print(lst[9:11])
print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE ADJUSTED TO REMOVE EFFECTS OF YEAR ONLY", sep=", "))
print(lst[12:14])
print("")
print("      MULTIPLE   ADJUSTED      F-                                    ")
print("  N  R-SQUARED  R-SQUARED  STATISTIC  DF1/DF2   P-VALUE   DESCRIPTION")
print("---  ---------  ---------  ---------  -------  ---------  -----------")
print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE (YEAR-ADJUSTED) VS IMMIGRANT SHARE, BY STATE", sep=", "))
print(lst[15:31])
print("")
print("     MULTIPLE   ADJUSTED      F-                                    ")
print(" N  R-SQUARED  R-SQUARED  STATISTIC  DF1/DF2   P-VALUE   DESCRIPTION")
print("--  ---------  ---------  ---------  -------  ---------  -----------")
print(paste(labyears, "OLS WITH YEAR, STATE, AND SPECIFIED GROUP OF FOREIGN WORKERS", sep=", "))
print(lst[32:39])
print(paste(labyears, "OLS WITH YEAR, STATE, AND 4 SUBSETS OF FOREIGN WORKERS WITH BACHELOR'S DEGREE OR HIGHER", sep=", "))
print(lst[40:41])
print("      MULTIPLE   ADJUSTED      F-                                    ")
print("  N  R-SQUARED  R-SQUARED  STATISTIC  DF1/DF2   P-VALUE   DESCRIPTION")
print("---  ---------  ---------  ---------  -------  ---------  -----------")
print(paste(labyears, "NATIVE WORKER EMPLOYMENT RATE (YEAR-ADJUSTED) VS YEAR, BY STATE", sep=", "))
print(lst[42:58])
print(paste(labyears, "IMMIGRANT SHARE VS YEAR, BY STATE", sep=", "))
print(lst[59:75])

# Graph Employed Natives vs. Employed Immigrants (foreign STEM US adv. degrees)
nn <- 1
with(dd, plot(emp_native/1000000 ~ emp_imm, cex=0.5, pch=dd$sPch, col=dd$sCol,
  ylab="Millions of Employed Native Workers", xlab=forlab1))
#title(main=paste("Foreign STEM Workers,", labyears))
title(main=paste("Figure ", nn, ": Foreign STEM Workers, ", labyears, sep=""))
	#with(dd, text(emp_imm, emp_native, labels=ss, cex=0.5, pos=4))
legend("bottomright", inset=0, title="State",
c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"),
cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE)
grid()
readline("Press enter to continue, escape to exit")

x11()
# Graph Native Employment Rate vs. Immigrant Share (with values of 0)
nn <- nn+1
with(dd0, plot(emprate_native ~ emp_immshare, cex=0.5, pch=dd0$sPch, col=dd0$sCol,
  ylab="Native Worker Employment Rate", xlab=forlab2))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. Immigrant Share\n(with values of 0), ", labyears, sep=""))
legend("topright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
lma <- (with(dd0, lm(emprate_native ~ emp_immshare)))
grid()
abline(lma)
readline("Press enter to continue, escape to exit")

x11()
# Graph Natural Logs of Native Employment Rate vs. Immigrant Share (without values of 0)
nn <- nn+1
with(dd, plot(lnemprate_native ~ lnemp_immshare, cex=0.5, pch=dd$sPch, col=dd$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. Immigrant Share\n(without values of 0), ", labyears, sep=""))
legend("bottomleft", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
legend("bottomright", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
  c("w/o weighting","with weighting"))
#lma <- (with(dd, lm(lnemprate_native ~ lnemp_immshare)))
grid()
abline(mm3, col="red", lty=2)
abline(mm4)
readline("Press enter to continue, escape to exit")

x11()
# Graph Natural Logs of Native Employment Rate vs. Immigrant Share, Weighted with Dummy Variables
nn <- nn+1
with(dd, plot(lnemprate_native ~ lnemp_immshare, cex=0.5, pch=dd$sPch, col=dd$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. Immigrant Share\n(weighted, with dummy variables), ", labyears, sep=""))
legend("bottomleft", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
legend("bottomright", inset=0, cex=0.5, lty=c(1,2,3,4), col=c("black","blue","green3","red"), horiz=FALSE,
  c("without dummy variables","with year dummy variables","with state dummy variables","with year and state dummy variables"))
grid()
abline(mm5)
abline(mm6, col="blue", lty=2)
abline(mm7, col="green3", lty=3)
abline(mm8, col="red", lty=4)
readline("Press enter to continue, escape to exit")

x11()
# Graph Natural Logs of Native Employment Rate vs. Immigrant Share Minus Effects of Year and State
nn <- nn+1
with(dd, plot(lnemprate_native2 ~ lnemp_immshare, cex=0.5, pch=dd$sPch, col=dd$sCol,
  ylab="Log of Native Employment Rate (minus effects of year and state)", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. Immigrant Share\n(adjusted for year and state), ", labyears, sep=""))
legend("bottomleft", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
legend("bottomright", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
  c("w/o weighting","with weighting"))
#lma <- (with(dd, lm(lnemprate_native ~ lnemp_immshare)))
grid()
abline(mm10, col="red", lty=2)
abline(mm11)
readline("Press enter to continue, escape to exit")

x11()
# Graph Natural Logs of Native Employment Rate vs. Immigrant Share Minus Effects of Year
nn <- nn+1
kk <- dd
with(kk, plot(lnemprate_native_yr ~ lnemp_immshare, cex=0.5, pch=kk$sPch, col=kk$sCol,
  ylab="Log of Native Employment Rate (minus effects of year)", xlab=paste("Log of", forlab2)))
title(main=paste("Figure ", nn, ": Native Employment Rate vs. Immigrant Share\n(adjusted for year), ", labyears, sep=""))
legend("bottomleft", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
legend("bottomright", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
  c("w/o weighting","with weighting"))
grid()
lm1 <- (with(kk, lm(lnemprate_native_yr ~ lnimmshare_emp_stem_e_grad)))
lm2 <- (with(kk, lm(lnemprate_native_yr ~ lnimmshare_emp_stem_e_grad, weights=weight_native)))
abline(lm1, col="red", lty=2)
abline(lm2)
readline("Press enter to continue, escape to exit")

#2000-2007
nn <- nn+1
plotstate(nn, "CA", "California")
nn <- nn+1
plotstate(nn, "DC", "District of Columbia")
nn <- nn+1
plotstate(nn, "FL", "Florida")
nn <- nn+1
plotstate(nn, "IL", "Illinois")
nn <- nn+1
plotstate(nn, "MI", "Michigan")
nn <- nn+1
plotstate(nn, "NJ", "New Jersey")
nn <- nn+1
plotstate(nn, "NY", "New York")
nn <- nn+1
plotstate(nn, "OR", "Oregon")
nn <- nn+1
plotstate(nn, "CT", "Connecticut")
nn <- nn+1
plotstate(nn, "GA", "Georgia")
nn <- nn+1
plotstate(nn, "MD", "Maryland")
nn <- nn+1
plotstate(nn, "MA", "Massachusetts")
nn <- nn+1
plotstate(nn, "OH", "Ohio")
nn <- nn+1
plotstate(nn, "PA", "Pennsylvania")
nn <- nn+1
plotstate(nn, "TX", "Texas")
nn <- nn+1
plotstate(nn, "VA", "Virginia")
nn <- nn+1
plotstate(nn, "WA", "Washington")

#2000-2013
nn <- nn+1
plotstate(nn, "CA", "California")
nn <- nn+1
plotstate(nn, "DC", "District of Columbia")
nn <- nn+1
plotstate(nn, "FL", "Florida")
nn <- nn+1
plotstate(nn, "MA", "Massachusetts")
nn <- nn+1
plotstate(nn, "MI", "Michigan")
nn <- nn+1
plotstate(nn, "OH", "Ohio")
nn <- nn+1
plotstate(nn, "PA", "Pennsylvania")
nn <- nn+1
plotstate(nn, "TX", "Texas")
