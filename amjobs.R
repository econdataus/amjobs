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

print(paste(labyears, "ALL DATA", sep=", "))
lm1 <- (with(dd0, lm(emprate_native ~ immshare_emp_stem_e_grad)))
print(sprintf(" 1) %9.4f %9.4f %8.4f %8.4f  emprate_native ~ immshare_emp_stem_e_grad",
  lm1$coef[1], lm1$coef[2], with(dd0, cor(emprate_native, immshare_emp_stem_e_grad)),
  anova(lm1)$'Pr(>F)'[1])) # OR summary(lm1)$coefficient[2,4]

print(paste(labyears, "EXCLUDING POINTS WITH ZERO FOREIGN WORKERS IN STEM WITH ADVANCED US DEGREES", sep=", "))
lm2 <- (with(dd, lm(emprate_native ~ immshare_emp_stem_e_grad)))
print(sprintf(" 2) %9.4f %9.4f %8.4f %8.4f  emprate_native ~ immshare_emp_stem_e_grad",
  lm2$coef[1], lm2$coef[2], with(dd, cor(emprate_native, immshare_emp_stem_e_grad)), anova(lm2)$'Pr(>F)'[1]))
lm3 <- (with(dd, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad)))
print(sprintf(" 3) %9.4f %9.4f %8.4f %8.4f  lnemprate_native ~ lnimmshare_emp_stem_e_grad",
  lm3$coef[1], lm3$coef[2], with(dd, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(lm3)$'Pr(>F)'[1]))
lm4 <- (with(dd, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad, weights=weight_native)))
print(sprintf(" 4) %9.4f %9.4f %8.4f %8.4f  lnemprate_native ~ lnimmshare_emp_stem_e_grad, weights=weight_native",
  lm4$coef[1], lm4$coef[2], with(dd, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(lm4)$'Pr(>F)'[1]))

print(paste(labyears, "WEIGHTED WITH DUMMY VARIABLES", sep=", "))
lm5 <- (with(dd, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad, weights=weight_native)))
print(sprintf(" 5) %9.4f %9.4f %8.4f %8.4f  without dummy variables",
  lm5$coef[1], lm5$coef[2], with(dd, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(lm5)$'Pr(>F)'[1]))
lm6 <- (with(dd, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fyear, weights=weight_native)))
print(sprintf(" 6) %9.4f %9.4f %8.4f %8.4f  with year dummy variables only",
  lm6$coef[1], lm6$coef[2], with(dd, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(lm6)$'Pr(>F)'[1]))
lm7 <- (with(dd, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fstate, weights=weight_native)))
print(sprintf(" 7) %9.4f %9.4f %8.4f %8.4f  with state dummy variables only",
  lm7$coef[1], lm7$coef[2], with(dd, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(lm7)$'Pr(>F)'[1]))
lm8 <- (with(dd, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad + fyear 
  + AK+AR+AZ+CA+CO+CT+DC+DE+FL+GA+HI+IA+ID+IL+IN+KS+KY+LA+MA+MD+ME+MI+MN+MO+MS+MT+NC+ND+NE+NH+NJ+NM+NV+NY+OH+OK+OR+PA+RI+SC+SD+TN+TX+UT+VA+VT+WA+WI, weights=weight_native)))
print(sprintf(" 8) %9.4f %9.4f %8.4f %8.4f  with year and state dummy variables",
  lm8$coef[1], lm8$coef[2], with(dd, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(lm8)$'Pr(>F)'[1]))

print(paste(labyears, "CALIFORNIA", sep=", "))
lma <- (with(ddca, lm(emprate_native ~ immshare_emp_stem_e_grad)))
print(sprintf(" 9) %9.4f %9.4f %8.4f %8.4f  emprate_native ~ immshare_emp_stem_e_grad",
  lma$coef[1], lma$coef[2], with(ddca, cor(emprate_native, immshare_emp_stem_e_grad)), anova(lma)$'Pr(>F)'[1]))
lma <- (with(ddca, lm(lnemprate_native ~ lnimmshare_emp_stem_e_grad)))
print(sprintf("10) %9.4f %9.4f %8.4f %8.4f  lnemprate_native ~ lnimmshare_emp_stem_e_grad",
  lma$coef[1], lma$coef[2], with(ddca, cor(lnemprate_native, lnimmshare_emp_stem_e_grad)), anova(lma)$'Pr(>F)'[1]))
lma <- (with(ddca, lm(emprate_native ~ year)))
print(sprintf("11) %9.4f %9.4f %8.4f %8.4f  emprate_native ~ year",
  lma$coef[1], lma$coef[2], with(ddca, cor(emprate_native, year)), anova(lma)$'Pr(>F)'[1]))
lma <- (with(ddca, lm(lnemprate_native ~ year)))
print(sprintf("12) %9.4f %9.4f %8.4f %8.4f  lnemprate_native ~ year",
  lma$coef[1], lma$coef[2], with(ddca, cor(lnemprate_native, year)), anova(lma)$'Pr(>F)'[1]))
lma <- (with(ddca, lm(immshare_emp_stem_e_grad ~ year)))
print(sprintf("13) %9.4f %9.4f %8.4f %8.4f  immshare_emp_stem_e_grad ~ year",
  lma$coef[1], lma$coef[2], with(ddca, cor(immshare_emp_stem_e_grad, year)), anova(lma)$'Pr(>F)'[1]))
lma <- (with(ddca, lm(lnimmshare_emp_stem_e_grad ~ year)))
print(sprintf("14) %9.4f %9.4f %8.4f %8.4f  lnimmshare_emp_stem_e_grad ~ year",
  lma$coef[1], lma$coef[2], with(ddca, cor(lnimmshare_emp_stem_e_grad, year)), anova(lma)$'Pr(>F)'[1]))

print("                          CORREL                                                             ")
print(" N  INTERCEPT    SLOPE     COEF   P-VALUE  T.R.C    OLS   Y VARIABLE ~ X VARIABLE [, WEIGHTS]")
print("--  ---------  --------  -------  -------  -----  ------  -----------------------------------")
print(paste(labyears, "OLS WITH YEAR, STATE, AND SPECIFIED GROUP OF FOREIGN WORKERS", sep=", "))
dd0$share <- dd0$immshare_emp_stem_e_grad
tt <- dd0[dd0$share > 0,]
tt <- tt[tt$immshare_emp_stem_n_grad > 0,]
tt$lnshare <- log(tt$share)
tt$lnimmshare_emp_stem_n_grad <- log(tt$immshare_emp_stem_n_grad)
lm <- (with(tt, lm(lnemprate_native ~ lnshare + lnimmshare_emp_stem_n_grad + fyear + fstate, weights=weight_native)))
print(sprintf("15a)%9.4f %9.4f %8.4f %8.4f  3.3.1   0.004  Advanced US degree and in STEM occupation",
  lm$coef[1], lm$coef[2], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[1]))
print(sprintf("15b)%9.4f %9.4f %8.4f %8.4f  3.3.3 -0.0002  Advanced foreign degree and in STEM occupation",
  lm$coef[1], lm$coef[3], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[2]))

dd0$share <- dd0$immshare_emp_stem_grad
tt <- dd0[dd0$share > 0,]
tt$lnshare <- log(tt$share)
lm <- (with(tt, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("16) %9.4f %9.4f %8.4f %8.4f  1.4.1   0.004  Advanced degree and in STEM occupation",
  lm$coef[1], lm$coef[2], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[1]))

dd0$share <- dd0$immshare_emp_immig_grad
tt <- dd0[dd0$share > 0,]
tt$lnshare <- log(tt$share)
lm <- (with(tt, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("17) %9.4f %9.4f %8.4f %8.4f  1.3.1   0.011  Advanced degree",
  lm$coef[1], lm$coef[2], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[1]))

dd0$share <- dd0$immshare_emp_immig_coll
tt <- dd0[dd0$share > 0,]
tt$lnshare <- log(tt$share)
lm <- (with(tt, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("18) %9.4f %9.4f %8.4f %8.4f  1.2.1   0.008  Bachelor's degree or higher",
  lm$coef[1], lm$coef[2], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[1]))

dd0$share <- dd0$immshare_emp_nostem_grad
tt <- dd0[dd0$share > 0,]
tt$lnshare <- log(tt$share)
lm <- (with(tt, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("19) %9.4f %9.4f %8.4f %8.4f  .....  ......  Advanced degree and NOT in STEM occupation",
  lm$coef[1], lm$coef[2], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[1]))

dd0$share <- dd0$immshare_emp_only_coll
tt <- dd0[dd0$share > 0,]
tt$lnshare <- log(tt$share)
lm <- (with(tt, lm(lnemprate_native ~ lnshare + fyear + fstate, weights=weight_native)))
print(sprintf("20) %9.4f %9.4f %8.4f %8.4f  .....  ......  Bachelor's degree only",
  lm$coef[1], lm$coef[2], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[1]))

print("                          CORREL                                              ")
print(" N  INTERCEPT    SLOPE     COEF   P-VALUE  Y VARIABLE ~ X VARIABLE [, WEIGHTS]")
print("--  ---------  --------  -------  -------  -----------------------------------")
print(paste(labyears, "OLS WITH YEAR, STATE, AND 4 SUBSETS OF FOREIGN WORKERS WITH BACHELOR'S DEGREE OR HIGHER", sep=", "))
dd0$share <- dd0$immshare_emp_stem_e_grad
tt <- dd0[dd0$share > 0,]
tt <- tt[tt$immshare_emp_stem_n_grad > 0,]
tt <- tt[tt$immshare_emp_nostem_grad > 0,]
tt$lnshare <- log(tt$share)
tt$lnimmshare_emp_stem_n_grad <- log(tt$immshare_emp_stem_n_grad)
tt$lnimmshare_emp_nostem_grad <- log(tt$immshare_emp_nostem_grad)
tt$lnimmshare_emp_only_coll   <- log(tt$immshare_emp_only_coll)
lm <- (with(tt, lm(lnemprate_native ~ lnshare + lnimmshare_emp_stem_n_grad + lnimmshare_emp_nostem_grad + lnimmshare_emp_only_coll
  + fyear + fstate, weights=weight_native)))
print(sprintf("21a)%9.4f %9.4f %8.4f %8.4f  Advanced US degree and in STEM occupation",
  lm$coef[1], lm$coef[2], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[1]))
print(sprintf("21b)%9.4f %9.4f %8.4f %8.4f  Advanced foreign degree and in STEM occupation",
  lm$coef[1], lm$coef[3], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[2]))
print(sprintf("21c)%9.4f %9.4f %8.4f %8.4f  Advanced degree and NOT in STEM occupation",
  lm$coef[1], lm$coef[4], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[3]))
print(sprintf("21d)%9.4f %9.4f %8.4f %8.4f  Bachelor's degree only",
  lm$coef[1], lm$coef[5], with(tt, cor(lnemprate_native, lnshare)), anova(lm)$'Pr(>F)'[4]))

# zg1: Graph employed natives vs. immigration share (foreign STEM US adv. degrees)
with(dd, plot(emp_native/1000000 ~ emp_imm, cex=0.5, pch=dd$sPch, col=dd$sCol,
  ylab="Millions of Employed Native Workers", xlab=forlab1))
title(main=paste("Foreign STEM Workers,", labyears))
	#with(dd, text(emp_imm, emp_native, labels=ss, cex=0.5, pos=4))
legend("bottomright", inset=0, title="State",
c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"),
cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE)
grid()
readline("Press enter to continue, escape to exit")

x11()
# zg2: Graph Native Employment vs. Immigrant Share
with(dd0, plot(emprate_native ~ emp_immshare, cex=0.5, pch=dd0$sPch, col=dd0$sCol,
  ylab="Native Worker Employment Rate", xlab=forlab2))
title(main=paste("Native Employment Rate vs. Immigrant Share\n(with values of 0),", labyears))
legend("topright", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
lma <- (with(dd0, lm(emprate_native ~ emp_immshare)))
grid()
abline(lma)
readline("Press enter to continue, escape to exit")

x11()
# zg3: Graph Natural Logs of Native Employment Rate vs. Immigrant Share
with(dd, plot(lnemprate_native ~ lnemp_immshare, cex=0.5, pch=dd$sPch, col=dd$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Native Employment Rate vs. Immigrant Share\n(without values of 0),", labyears))
legend("bottomleft", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
  c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
legend("bottomright", inset=0, cex=0.5, lty=c(2,1), col=c("red","black"), horiz=FALSE,
  c("w/o weighting","with weighting"))
#lma <- (with(dd, lm(lnemprate_native ~ lnemp_immshare)))
grid()
abline(lm3, col="red", lty=2)
abline(lm4)
readline("Press enter to continue, escape to exit")

x11()
# zg4: Graph Natural Logs of Native Employment Rate vs. Immigrant Share, Weighted
with(dd, plot(lnemprate_native ~ lnemp_immshare, cex=0.5, pch=dd$sPch, col=dd$sCol,
  ylab="Log of Native Worker Employment Rate", xlab=paste("Log of", forlab2)))
title(main=paste("Native Employment Rate vs. Immigrant Share\n(weighted, without values of 0),", labyears))
legend("bottomleft", inset=0, cex=0.5, pch=gg$pch, col=gg$col, horiz=FALSE,
c("California","Connecticut","DC","Florida","Georgia","Illinois","Maryland","Massachusetts","Michigan","New Jersey","New York","Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington"))
legend("bottomright", inset=0, cex=0.5, lty=c(1,2,3,4), col=c("black","blue","green3","red"), horiz=FALSE,
  c("without dummy variables","with year dummy variables","with state dummy variables","with year and state dummy variables"))
grid()
abline(lm5)
abline(lm6, col="blue", lty=2)
abline(lm7, col="green3", lty=3)
abline(lm8, col="red", lty=4)
readline("Press enter to continue, escape to exit")

x11()
# zg5: Graph Native Employment Rate vs. Year in California
with(ddca, plot(emprate_native ~ year, cex=0.5, pch=ddca$sPch, col=ddca$sCol,
  ylab="Native Worker Employment Rate", xlab="Year"))
title(main=paste("Native Employment Rate vs. Year in California,", labyears))
grid()
lma <- (with(ddca, lm(emprate_native ~ year)))
abline(lma)
readline("Press enter to continue, escape to exit")

x11()
# zg6: Foreign STEM Share vs. Year in California
with(ddca, plot(emp_immshare ~ year, cex=0.5, pch=ddca$sPch, col=ddca$sCol,
  ylab=forlab2, xlab="Year"))
title(main=paste("Foreign STEM Share vs. Year in California,", labyears))
legend("bottomright", inset=0, cex=0.5, lty=1, col=c("black","red"), horiz=FALSE,
c("with all years","without 2002"))
grid()
lma <- (with(ddca, lm(emp_immshare ~ year)))
abline(lma)

# Skip 2002 as an outlier
ddcam02 = ddca[ddca$year != 2002,]
lma <- (with(ddcam02, lm(emp_immshare ~ year)))
abline(lma, col="red")
