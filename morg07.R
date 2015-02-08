get_statefip <- function(state) {
	statefip <- 0
	statefip[state == "AK"] <- 02
	statefip[state == "AL"] <- 01
	statefip[state == "AR"] <- 05
	statefip[state == "AS"] <- 60
	statefip[state == "AZ"] <- 04
	statefip[state == "CA"] <- 06
	statefip[state == "CO"] <- 08
	statefip[state == "CT"] <- 09
	statefip[state == "DC"] <- 11
	statefip[state == "DE"] <- 10
	statefip[state == "FL"] <- 12
	statefip[state == "GA"] <- 13
	statefip[state == "GU"] <- 66
	statefip[state == "HI"] <- 15
	statefip[state == "IA"] <- 19
	statefip[state == "ID"] <- 16
	statefip[state == "IL"] <- 17
	statefip[state == "IN"] <- 18
	statefip[state == "KS"] <- 20
	statefip[state == "KY"] <- 21
	statefip[state == "LA"] <- 22
	statefip[state == "MA"] <- 25
	statefip[state == "MD"] <- 24
	statefip[state == "ME"] <- 23
	statefip[state == "MI"] <- 26
	statefip[state == "MN"] <- 27
	statefip[state == "MO"] <- 29
	statefip[state == "MS"] <- 28
	statefip[state == "MT"] <- 30
	statefip[state == "NC"] <- 37
	statefip[state == "ND"] <- 38
	statefip[state == "NE"] <- 31
	statefip[state == "NH"] <- 33
	statefip[state == "NJ"] <- 34
	statefip[state == "NM"] <- 35
	statefip[state == "NV"] <- 32
	statefip[state == "NY"] <- 36
	statefip[state == "OH"] <- 39
	statefip[state == "OK"] <- 40
	statefip[state == "OR"] <- 41
	statefip[state == "PA"] <- 42
	statefip[state == "PR"] <- 72
	statefip[state == "RI"] <- 44
	statefip[state == "SC"] <- 45
	statefip[state == "SD"] <- 46
	statefip[state == "TN"] <- 47
	statefip[state == "TX"] <- 48
	statefip[state == "UT"] <- 49
	statefip[state == "VA"] <- 51
	statefip[state == "VI"] <- 78
	statefip[state == "VT"] <- 50
	statefip[state == "WA"] <- 53
	statefip[state == "WI"] <- 55
	statefip[state == "WV"] <- 54
	statefip[state == "WY"] <- 56
	return(statefip)
}
get_statefip2 <- function(state) {
	statefip <- 0
	statefip[state==63] <- 1
	statefip[state==94] <- 2
	statefip[state==86] <- 4
	statefip[state==71] <- 5
	statefip[state==93] <- 6
	statefip[state==84] <- 8
	statefip[state==16] <- 9
	statefip[state==51] <- 10
	statefip[state==53] <- 11
	statefip[state==59] <- 12
	statefip[state==58] <- 13
	statefip[state==95] <- 15
	statefip[state==82] <- 16
	statefip[state==33] <- 17
	statefip[state==32] <- 18
	statefip[state==42] <- 19
	statefip[state==47] <- 20
	statefip[state==61] <- 21
	statefip[state==72] <- 22
	statefip[state==11] <- 23
	statefip[state==52] <- 24
	statefip[state==14] <- 25
	statefip[state==34] <- 26
	statefip[state==41] <- 27
	statefip[state==64] <- 28
	statefip[state==43] <- 29
	statefip[state==81] <- 30
	statefip[state==46] <- 31 
	statefip[state==88] <- 32 
	statefip[state==12] <- 33 
	statefip[state==22] <- 34 
	statefip[state==85] <- 35 
	statefip[state==21] <- 36 
	statefip[state==56] <- 37 
	statefip[state==44] <- 38 
	statefip[state==31] <- 39 
	statefip[state==73] <- 40 
	statefip[state==92] <- 41 
	statefip[state==23] <- 42 
	statefip[state==15] <- 44 
	statefip[state==57] <- 45 
	statefip[state==45] <- 46 
	statefip[state==62] <- 47 
	statefip[state==74] <- 48 
	statefip[state==87] <- 49 
	statefip[state==13] <- 50 
	statefip[state==54] <- 51 
	statefip[state==91] <- 53 
	statefip[state==55] <- 54 
	statefip[state==35] <- 55 
	statefip[state==83] <- 56
	return(statefip)
}
# create likely year of immigration (codes are intervaled in CPS, so this is inexact)
get_yrimig <- function(peinusyr) {
	yrimig <- 0
	yrimig[as.integer(peinusyr) ==  1] <- 0 # Not In Universe
	yrimig[as.integer(peinusyr) ==  2] <- 0 # Not Foreign Born
	yrimig[as.integer(peinusyr) ==  3] <- 1950
	yrimig[as.integer(peinusyr) ==  4] <- 1954
	yrimig[as.integer(peinusyr) ==  5] <- 1962
	yrimig[as.integer(peinusyr) ==  6] <- 1967
	yrimig[as.integer(peinusyr) ==  7] <- 1972
	yrimig[as.integer(peinusyr) ==  8] <- 1977
	yrimig[as.integer(peinusyr) ==  9] <- 1980
	yrimig[as.integer(peinusyr) == 10] <- 1982
	yrimig[as.integer(peinusyr) == 11] <- 1984
	yrimig[as.integer(peinusyr) == 12] <- 1986
	yrimig[as.integer(peinusyr) == 13] <- 1988
	yrimig[as.integer(peinusyr) == 14] <- 1990
	yrimig[as.integer(peinusyr) == 15] <- 1992
	yrimig[as.integer(peinusyr) == 16] <- 1994
	yrimig[as.integer(peinusyr) == 17] <- 1996
	yrimig[as.integer(peinusyr) == 18] <- 1998
	yrimig[as.integer(peinusyr) == 19] <- 2000
	yrimig[as.integer(peinusyr) == 20] <- 2002
	yrimig[as.integer(peinusyr) == 21] <- 2004
	yrimig[as.integer(peinusyr) == 22] <- 2006
	yrimig[as.integer(peinusyr) == 23] <- 2008
	yrimig[as.integer(peinusyr) == 24] <- 2010
	return(yrimig)
}
read_morg <- function(aa, year, file) {
	print(sprintf("START READ OF %s", file))
	flush.console()
	mm <- read.dta(file)
	dim0 <- dim(mm)[2]
	mm$weight <- mm$weight / 3
	print(sprintf("PROCESS %s", file))
	mm = mm[mm$age >= 16 & mm$age <= 64,]
	mm$statefip <- get_statefip(mm$state)
	#mm <- mm[mm$statefip == 6,] #limit for testing to CA
	mm$immig    <- as.integer(as.integer(mm$prcitshp) >= 4)
	mm$lesshs   <- as.integer(as.integer(mm$grade92) < 9)
	mm$hs       <- as.integer(as.integer(mm$grade92) == 9)
	mm$somecoll <- as.integer(as.integer(mm$grade92) >= 10 & as.integer(mm$grade92) <= 12)
	mm$coll     <- as.integer(as.integer(mm$grade92) == 13)
	mm$grad     <- as.integer(as.integer(mm$grade92) >= 14)
	mm$emp      <- as.integer(as.integer(mm$lfsr94) <=2 & as.integer(mm$class94) <= 5)
	mm$stem     <- as.integer(as.integer(mm$docc00) >= 4 & as.integer(mm$docc00) <= 6)
	#gen ageimmig=age-(year-yrimmig)
	#gen educus=((ageimmig<21 & coll==1) | (ageimmig<25 & grad==1))
	mm$ageimmig <- mm$age - (year - get_yrimig(mm$peinusyr))
	mm$ageimmig[mm$ageimmig < 0] <- 0
	mm$educus <- as.integer((mm$ageimmig<21 & mm$coll==1) | (mm$ageimmig<25 & mm$grad==1))
	count_nas(mm, dim0+1)
	count_stem_nas(mm)
#mm[is.na(mm)] <- 0
#mm$lfsr94[is.na(mm$lfsr94)] <- 0
mm$stem[is.na(mm$stem)] <- 0
nn <- aggregate(mm$weight, by=list("immig"=mm$immig,"state"=mm$statefip,"emp"=mm$emp,"lesshs"=mm$lesshs,
	  "hs"=mm$hs,"somecoll"=mm$somecoll,"coll"=mm$coll,"grad"=mm$grad,"educus"=mm$educus,"stem"=mm$stem),FUN=sum, na.rm=FALSE)
	nn$year <- year
	aa <- rbind(aa, nn)
	assign('aa',aa,envir=.GlobalEnv)
	#aa <<- aa
	#readline("Press enter to continue, escape to exit")
}
read_morg2 <- function(aa, year, file) {
	print(sprintf("START READ OF %s", file))
	flush.console()
	mm <- read.dta(file)
	dim0 <- dim(mm)[2]
	mm$weight <- mm$weight / 3
	print(sprintf("PROCESS %s", file))
	mm = mm[mm$age >= 16 & mm$age <= 64,]
	mm$statefip <- get_statefip2(mm$state) # call statefip2 instead of statefip
	#mm <- mm[mm$statefip == 6,] #limit for testing to CA
	mm$immig    <- as.integer(as.integer(mm$prcitshp) >= 4)
	mm$lesshs   <- as.integer(as.integer(mm$grade92) < 39) # mm$grade92 is +30
	mm$hs       <- as.integer(as.integer(mm$grade92) == 39)
	mm$somecoll <- as.integer(as.integer(mm$grade92) >= 40 & as.integer(mm$grade92) <= 42)
	mm$coll     <- as.integer(as.integer(mm$grade92) == 43)
	mm$grad     <- as.integer(as.integer(mm$grade92) >= 44)
	mm$emp      <- as.integer(as.integer(mm$lfsr94) <=2 & as.integer(mm$class94) <= 5)
	mm$stem     <- as.integer(as.integer(mm$docc00) >= 4 & as.integer(mm$docc00) <= 6)
	#gen ageimmig=age-(year-yrimmig)
	#gen educus=((ageimmig<21 & coll==1) | (ageimmig<25 & grad==1))
	mm$ageimmig <- mm$age - (year - get_yrimig(mm$peinusyr))
	mm$ageimmig[mm$ageimmig < 0] <- 0
	mm$educus <- as.integer((mm$ageimmig<21 & mm$coll==1) | (mm$ageimmig<25 & mm$grad==1))
	count_nas(mm, dim0+1)
	count_stem_nas(mm)
#mm[is.na(mm)] <- 0
#mm$lfsr94[is.na(mm$lfsr94)] <- 0
mm$stem[is.na(mm$stem)] <- 0
nn <- aggregate(mm$weight, by=list("immig"=mm$immig,"state"=as.integer(mm$statefip),"emp"=mm$emp,"lesshs"=mm$lesshs,
	  "hs"=mm$hs,"somecoll"=mm$somecoll,"coll"=mm$coll,"grad"=mm$grad,"educus"=mm$educus,"stem"=mm$stem),FUN=sum, na.rm=FALSE)
	nn$year <- year
	aa <- rbind(aa, nn)
	assign('aa',aa,envir=.GlobalEnv)
	#aa <<- aa
	#readline("Press enter to continue, escape to exit")
}
count_stem_nas <- function(mm) {
	stem0 <- mm[is.na(mm$stem) & is.na(mm$lfsr94),]
	stem1 <- mm[is.na(mm$stem) & !is.na(mm$lfsr94) & as.integer(mm$lfsr94)==1,]
	stem2 <- mm[is.na(mm$stem) & !is.na(mm$lfsr94) & as.integer(mm$lfsr94)==2,]
	stem3 <- mm[is.na(mm$stem) & !is.na(mm$lfsr94) & as.integer(mm$lfsr94)==3,]
	stem4 <- mm[is.na(mm$stem) & !is.na(mm$lfsr94) & as.integer(mm$lfsr94)==4,]
	stem5 <- mm[is.na(mm$stem) & !is.na(mm$lfsr94) & as.integer(mm$lfsr94)==5,]
	stem6 <- mm[is.na(mm$stem) & !is.na(mm$lfsr94) & as.integer(mm$lfsr94)==6,]
	stem7 <- mm[is.na(mm$stem) & !is.na(mm$lfsr94) & as.integer(mm$lfsr94)==7,]
	if (dim(stem0)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==NA", dim(stem0)[1]))
	if (dim(stem1)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==1 (Employed-At Work)", dim(stem1)[1]))
	if (dim(stem2)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==2 (Employed-Absent)", dim(stem2)[1]))
	if (dim(stem3)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==3 (Unemployed-On Layoff)", dim(stem3)[1]))
	if (dim(stem4)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==4 (Unemployed-Looking)", dim(stem4)[1]))
	if (dim(stem5)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==5 (Retired-Not In Labor Force)", dim(stem5)[1]))
	if (dim(stem6)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==6 (Disabled-Not In Labor Force)", dim(stem6)[1]))
	if (dim(stem7)[1] > 0) print(sprintf("%7d NAs in stem, lfsr94==7 (Other-Not In Labor Force)", dim(stem7)[1]))
}
count_nas <- function(df1, start) {
  print(sprintf("%7d TOTAL ROWS", dim(df1)[1]))
  for (i in start:dim(df1)[2]) {
    cnt <- sum(is.na(df1[,i]))
    if (cnt > 0) {
      print(sprintf("%7d NAs in %s", cnt, names(df1)[i]))
    }    
  }
  cnt <- sum(is.na(df1$lfsr94))
  if (cnt > 0) print(sprintf("%7d NAs in lfsr94", cnt))    
}
count_nas_all <- function(df1) {
  print(sprintf("%7d : TOTAL ROWS", dim(df1)[1]))
  for (i in 1:dim(df1)[2]) {
    cnt <- sum(is.na(df1[,i]))
    if (cnt > 0) print(sprintf("%7d : %s", cnt, names(df1)[i]))
  }
  print(sprintf("%7d : TOTAL MISSING", sum(is.na(df1))))
}
diff_dfs <- function(df1, df2) {
  total <- 0
  print(sprintf("    MAX  %5g : TOTAL ROWS", dim(df1)[1]))
  for (i in 1:dim(df1)[2]) {
    diff <- df1[,i] - df2[,i]
    cnt <- sum(abs(diff) > 0.001)
    max <- max(abs(diff))
    if (cnt > 0) print(sprintf("%7g  %5d : %s", max, cnt, names(df1)[i]))
    total <- total + cnt
  }
  print(sprintf("%7g  %5d : TOTAL DIFFERENCES", max(abs(df1-df2)), total))
}
aggdf <- function(df) {
  agg <- aggregate(list(x=df$x), by=list(year=df$year, state=df$state), FUN=sum, na.rm=FALSE)
  return(agg)
}
mergedf <- function(df1, df2) {
  mrg <- merge(df1, df2, by=c("year","state"), all=TRUE)
  return(mrg)
}

# CHANGE TO READ MORG FILE FOR EACH YEAR
aa <- NULL
read_morg(aa, 2000, "morg00.dta")
read_morg(aa, 2001, "morg01.dta") 
read_morg(aa, 2002, "morg02.dta") 
read_morg(aa, 2003, "morg03.dta") 
read_morg(aa, 2004, "morg04.dta") 
read_morg(aa, 2005, "morg05.dta") 
read_morg(aa, 2006, "morg06.dta") 
read_morg2(aa, 2007, "morg07.dta") 
#read_morg2(aa, 2008, "morg08.dta") 
#read_morg2(aa, 2009, "morg09.dta") 
#read_morg2(aa, 2010, "morg10.dta") 
#read_morg2(aa, 2011, "morg11.dta") 
#read_morg2(aa, 2012, "morg12.dta") 
#read_morg2(aa, 2013, "morg13.dta") 

print(sprintf("ALL MORG FILE READ"))
#readline("Press enter to continue, escape to exit")
print(sprintf("START OF AGGREGATE AND MERGE INTO FINAL FILES"))

# COMBINE SOME SMALLEST GROUPS TO CREATE GROUPS TO STUDY (maybe more than 1 per year and state)
pop_nedus_stem_grad <- aa[aa$immig==1             & aa$educus==0 & aa$stem==1 &  aa$grad==1,]
emp_nedus_stem_grad <- aa[aa$immig==1 & aa$emp==1 & aa$educus==0 & aa$stem==1 &  aa$grad==1,]
pop_nedus_stem_coll <- aa[aa$immig==1             & aa$educus==0 & aa$stem==1 & (aa$grad==1 | aa$coll==1),]
emp_nedus_stem_coll <- aa[aa$immig==1 & aa$emp==1 & aa$educus==0 & aa$stem==1 & (aa$grad==1 | aa$coll==1),]
pop_edus_stem_grad  <- aa[aa$immig==1             & aa$educus==1 & aa$stem==1 &  aa$grad==1,]
emp_edus_stem_grad  <- aa[aa$immig==1 & aa$emp==1 & aa$educus==1 & aa$stem==1 &  aa$grad==1,]
pop_edus_stem_coll  <- aa[aa$immig==1             & aa$educus==1 & aa$stem==1 & (aa$grad==1 | aa$coll==1),]
emp_edus_stem_coll  <- aa[aa$immig==1 & aa$emp==1 & aa$educus==1 & aa$stem==1 & (aa$grad==1 | aa$coll==1),]
pop_nedus_grad      <- aa[aa$immig==1             & aa$educus==0              &  aa$grad==1,]
pop_edus_grad       <- aa[aa$immig==1             & aa$educus==1              &  aa$grad==1,]
pop_nedus_coll      <- aa[aa$immig==1             & aa$educus==0              & (aa$grad==1 | aa$coll==1),]
pop_edus_coll       <- aa[aa$immig==1             & aa$educus==1              & (aa$grad==1 | aa$coll==1),]
emp_nedus_grad      <- aa[aa$immig==1 & aa$emp==1 & aa$educus==0              &  aa$grad==1,]
emp_edus_grad       <- aa[aa$immig==1 & aa$emp==1 & aa$educus==1              &  aa$grad==1,]
emp_nedus_coll      <- aa[aa$immig==1 & aa$emp==1 & aa$educus==0              & (aa$grad==1 | aa$coll==1),]
emp_edus_coll       <- aa[aa$immig==1 & aa$emp==1 & aa$educus==1              & (aa$grad==1 | aa$coll==1),]
emp_native_coll     <- aa[aa$immig==0 & aa$emp==1                             & (aa$grad==1 | aa$coll==1),]
emp_native_grad     <- aa[aa$immig==0 & aa$emp==1                             &  aa$grad==1,]
emp_immig_coll      <- aa[aa$immig==1 & aa$emp==1                             & (aa$grad==1 | aa$coll==1),]
emp_immig_grad      <- aa[aa$immig==1 & aa$emp==1                             &  aa$grad==1,]
emp_total_coll      <- aa[              aa$emp==1                             & (aa$grad==1 | aa$coll==1),]
emp_total_grad      <- aa[              aa$emp==1                             &  aa$grad==1,]
emp_native          <- aa[aa$immig==0 & aa$emp==1,]
#lf_native           <- aa[aa$immig==0 & aa$lf==1,]
emp_immig           <- aa[aa$immig==1 & aa$emp==1,]
emp_total           <- aa[              aa$emp==1,]
pop_native_coll     <- aa[aa$immig==0                                         & (aa$grad==1 | aa$coll==1),]
pop_native_grad     <- aa[aa$immig==0                                         &  aa$grad==1,]
pop_immig_coll      <- aa[aa$immig==1                                         & (aa$grad==1 | aa$coll==1),]
pop_immig_grad      <- aa[aa$immig==1                                         &  aa$grad==1,]
pop_native          <- aa[aa$immig==0,]
pop_immig           <- aa[aa$immig==1,]
pop_total           <- aa

# AGGREGATE SO THAT THERE IS 1 PER YEAR AND STATE
pop_nedus_stem_grad <- aggdf(pop_nedus_stem_grad)      
emp_nedus_stem_grad <- aggdf(emp_nedus_stem_grad)      
pop_nedus_stem_coll <- aggdf(pop_nedus_stem_coll)
emp_nedus_stem_coll <- aggdf(emp_nedus_stem_coll)
pop_edus_stem_grad  <- aggdf(pop_edus_stem_grad)
emp_edus_stem_grad  <- aggdf(emp_edus_stem_grad)
pop_edus_stem_coll  <- aggdf(pop_edus_stem_coll)
emp_edus_stem_coll  <- aggdf(emp_edus_stem_coll)
pop_nedus_grad      <- aggdf(pop_nedus_grad)
pop_edus_grad       <- aggdf(pop_edus_grad)
pop_nedus_coll      <- aggdf(pop_nedus_coll)
pop_edus_coll       <- aggdf(pop_edus_coll)
emp_nedus_grad      <- aggdf(emp_nedus_grad)
emp_edus_grad       <- aggdf(emp_edus_grad)
emp_nedus_coll      <- aggdf(emp_nedus_coll)
emp_edus_coll       <- aggdf(emp_edus_coll)
emp_native_coll     <- aggdf(emp_native_coll)
emp_native_grad     <- aggdf(emp_native_grad)
emp_immig_coll      <- aggdf(emp_immig_coll)
emp_immig_grad      <- aggdf(emp_immig_grad)
emp_total_coll      <- aggdf(emp_total_coll)
emp_total_grad      <- aggdf(emp_total_grad)
emp_native          <- aggdf(emp_native)
#lf_native           <- aggdf(lf_native)
emp_immig           <- aggdf(emp_immig)
emp_total           <- aggdf(emp_total)
pop_native_coll     <- aggdf(pop_native_coll)
pop_native_grad     <- aggdf(pop_native_grad)
pop_immig_coll      <- aggdf(pop_immig_coll)
pop_immig_grad      <- aggdf(pop_immig_grad)
pop_native          <- aggdf(pop_native)
pop_immig           <- aggdf(pop_immig)
pop_total           <- aggdf(pop_total)
minweight <- aggregate(list(minweight=aa$x), by=list(year=aa$year, state=aa$state), FUN=min, na.rm=FALSE)
maxweight <- aggregate(list(maxweight=aa$x), by=list(year=aa$year, state=aa$state), FUN=max, na.rm=FALSE)
avgweight <- aggregate(list(avgweight=aa$x), by=list(year=aa$year, state=aa$state), FUN=mean, na.rm=FALSE)

# CHANGE COLUMN NAMES FROM X TO VARIABLE NAME
colnames(pop_nedus_stem_grad)[colnames(pop_nedus_stem_grad)=="x"] <- "pop_nedus_stem_grad"
colnames(emp_nedus_stem_grad)[colnames(emp_nedus_stem_grad)=="x"] <- "emp_nedus_stem_grad"
colnames(pop_nedus_stem_coll)[colnames(pop_nedus_stem_coll)=="x"] <- "pop_nedus_stem_coll"
colnames(emp_nedus_stem_coll)[colnames(emp_nedus_stem_coll)=="x"] <- "emp_nedus_stem_coll"
colnames(pop_edus_stem_grad) [colnames(pop_edus_stem_grad) =="x"] <- "pop_edus_stem_grad"
colnames(emp_edus_stem_grad) [colnames(emp_edus_stem_grad) =="x"] <- "emp_edus_stem_grad"
colnames(pop_edus_stem_coll) [colnames(pop_edus_stem_coll) =="x"] <- "pop_edus_stem_coll"
colnames(emp_edus_stem_coll) [colnames(emp_edus_stem_coll) =="x"] <- "emp_edus_stem_coll"
colnames(pop_nedus_grad)     [colnames(pop_nedus_grad)     =="x"] <- "pop_nedus_grad"
colnames(pop_edus_grad)      [colnames(pop_edus_grad)      =="x"] <- "pop_edus_grad"
colnames(pop_nedus_coll)     [colnames(pop_nedus_coll)     =="x"] <- "pop_nedus_coll"
colnames(pop_edus_coll)      [colnames(pop_edus_coll)      =="x"] <- "pop_edus_coll"
colnames(emp_nedus_grad)     [colnames(emp_nedus_grad)     =="x"] <- "emp_nedus_grad"
colnames(emp_edus_grad)      [colnames(emp_edus_grad)      =="x"] <- "emp_edus_grad"
colnames(emp_nedus_coll)     [colnames(emp_nedus_coll)     =="x"] <- "emp_nedus_coll"
colnames(emp_edus_coll)      [colnames(emp_edus_coll)      =="x"] <- "emp_edus_coll"
colnames(emp_native_coll)    [colnames(emp_native_coll)    =="x"] <- "emp_native_coll"
colnames(emp_native_grad)    [colnames(emp_native_grad)    =="x"] <- "emp_native_grad"
colnames(emp_immig_coll)     [colnames(emp_immig_coll)     =="x"] <- "emp_immig_coll"
colnames(emp_immig_grad)     [colnames(emp_immig_grad)     =="x"] <- "emp_immig_grad"
colnames(emp_total_coll)     [colnames(emp_total_coll)     =="x"] <- "emp_total_coll"
colnames(emp_total_grad)     [colnames(emp_total_grad)     =="x"] <- "emp_total_grad"
colnames(emp_native)         [colnames(emp_native)         =="x"] <- "emp_native"
#colnames(lf_native)          [colnames(lf_native)          =="x"] <- "lf_native"
colnames(emp_immig)          [colnames(emp_immig)          =="x"] <- "emp_immig"
colnames(emp_total)          [colnames(emp_total)          =="x"] <- "emp_total"
colnames(pop_native_coll)    [colnames(pop_native_coll)    =="x"] <- "pop_native_coll"
colnames(pop_native_grad)    [colnames(pop_native_grad)    =="x"] <- "pop_native_grad"
colnames(pop_immig_coll)     [colnames(pop_immig_coll)     =="x"] <- "pop_immig_coll"
colnames(pop_immig_grad)     [colnames(pop_immig_grad)     =="x"] <- "pop_immig_grad"
colnames(pop_native)         [colnames(pop_native)         =="x"] <- "pop_native"
colnames(pop_immig)          [colnames(pop_immig)          =="x"] <- "pop_immig"
colnames(pop_total)          [colnames(pop_total)          =="x"] <- "pop_total"    

# MERGE ALL VARIABLES INTO CC
cc <- pop_nedus_stem_grad              #differs
cc <- mergedf(cc, emp_nedus_stem_grad)      
cc <- mergedf(cc, pop_nedus_stem_coll) #differs
cc <- mergedf(cc, emp_nedus_stem_coll)
cc <- mergedf(cc, pop_edus_stem_grad)  #differs
cc <- mergedf(cc, emp_edus_stem_grad)
cc <- mergedf(cc, pop_edus_stem_coll)  #differs
cc <- mergedf(cc, emp_edus_stem_coll)
cc <- mergedf(cc, pop_nedus_grad) #differs
cc <- mergedf(cc, pop_edus_grad)  #differs
cc <- mergedf(cc, pop_nedus_coll) #differs
cc <- mergedf(cc, pop_edus_coll)  #differs
cc <- mergedf(cc, emp_nedus_grad) #differs
cc <- mergedf(cc, emp_edus_grad)  #differs
cc <- mergedf(cc, emp_nedus_coll) #differs
cc <- mergedf(cc, emp_edus_coll)  #differs
cc <- mergedf(cc, emp_native_coll)
cc <- mergedf(cc, emp_native_grad)
cc <- mergedf(cc, emp_immig_coll)
cc <- mergedf(cc, emp_immig_grad)
cc <- mergedf(cc, emp_total_coll)
cc <- mergedf(cc, emp_total_grad)
cc <- mergedf(cc, emp_native)
#cc <- mergedf(cc, lf_native)
cc <- mergedf(cc, emp_immig)
cc <- mergedf(cc, emp_total)
cc <- mergedf(cc, pop_native_coll)
cc <- mergedf(cc, pop_native_grad)
cc <- mergedf(cc, pop_immig_coll)
cc <- mergedf(cc, pop_immig_grad)
cc <- mergedf(cc, pop_native)
cc <- mergedf(cc, pop_immig)
cc <- mergedf(cc, pop_total)
cc <- mergedf(cc, minweight)
cc <- mergedf(cc, maxweight)
cc <- mergedf(cc, avgweight)

cc <- cc[with(cc, order(state,year)),]
count_nas_all(cc)
cc[is.na(cc)] <- 0

# UNCOMMENT TO COMPARE DATA THRU 2007 TO ORIGINAL DATA 
pp <- read.dta("public.dta")
pp <- pp[pp$year <= 2007,]
#cc <- cc[pp$year <= 2007,] # UNCOMMENT IF cc not 2000-2007
pp$minweight <- cc$minweight
pp$maxweight <- cc$maxweight
pp$avgweight <- cc$avgweight
print("")
diff_dfs(pp, cc)

#readline("Press enter to continue, escape to exit")
print("CREATE FINAL FILES morg07.txt and morg07.csv") # CHANGE FOR YEARS
write.table(cc, "morg07.txt") # CHANGE FOR YEARS
write.table(cc, "morg07.csv", sep=',') # CHANGE FOR YEARS
