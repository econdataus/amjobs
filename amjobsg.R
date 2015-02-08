dd$y01 <- 0
dd$y02 <- 0
dd$y03 <- 0
dd$y04 <- 0
dd$y05 <- 0
dd$y06 <- 0
dd$y07 <- 0
dd$y01[dd$year == 2001] <- 1
dd$y02[dd$year == 2002] <- 1
dd$y03[dd$year == 2003] <- 1
dd$y04[dd$year == 2004] <- 1
dd$y05[dd$year == 2005] <- 1
dd$y06[dd$year == 2006] <- 1
dd$y07[dd$year == 2007] <- 1

dd$AK <- 0
dd$AR <- 0
dd$AZ <- 0
dd$CA <- 0
dd$CO <- 0
dd$CT <- 0
dd$DC <- 0
dd$DE <- 0
dd$FL <- 0
dd$GA <- 0
dd$HI <- 0
dd$IA <- 0
dd$ID <- 0
dd$IL <- 0
dd$IN <- 0
dd$KS <- 0
dd$KY <- 0
dd$LA <- 0
dd$MA <- 0
dd$MD <- 0
dd$ME <- 0
dd$MI <- 0
dd$MN <- 0
dd$MO <- 0
dd$MS <- 0
dd$MT <- 0
dd$NC <- 0
dd$ND <- 0
dd$NE <- 0
dd$NH <- 0
dd$NJ <- 0
dd$NM <- 0
dd$NV <- 0
dd$NY <- 0
dd$OH <- 0
dd$OK <- 0
dd$OR <- 0
dd$PA <- 0
dd$RI <- 0
dd$SC <- 0
dd$SD <- 0
dd$TN <- 0
dd$TX <- 0
dd$UT <- 0
dd$VA <- 0
dd$VT <- 0
dd$WA <- 0
dd$WI <- 0
dd$WV <- 0
dd$WY <- 0

#dd$AL[dd$statefip == 01] <- 1 # AL IS DEFAULT
dd$AK[dd$statefip == 02] <- 1
dd$AR[dd$statefip == 05] <- 1
dd$AZ[dd$statefip == 04] <- 1
dd$CA[dd$statefip == 06] <- 1
dd$CO[dd$statefip == 08] <- 1
dd$CT[dd$statefip == 09] <- 1
dd$DC[dd$statefip == 11] <- 1
dd$DE[dd$statefip == 10] <- 1
dd$FL[dd$statefip == 12] <- 1
dd$GA[dd$statefip == 13] <- 1
dd$HI[dd$statefip == 15] <- 1
dd$IA[dd$statefip == 19] <- 1
dd$ID[dd$statefip == 16] <- 1
dd$IL[dd$statefip == 17] <- 1
dd$IN[dd$statefip == 18] <- 1
dd$KS[dd$statefip == 20] <- 1
dd$KY[dd$statefip == 21] <- 1
dd$LA[dd$statefip == 22] <- 1
dd$MA[dd$statefip == 25] <- 1
dd$MD[dd$statefip == 24] <- 1
dd$ME[dd$statefip == 23] <- 1
dd$MI[dd$statefip == 26] <- 1
dd$MN[dd$statefip == 27] <- 1
dd$MO[dd$statefip == 29] <- 1
dd$MS[dd$statefip == 28] <- 1
dd$MT[dd$statefip == 30] <- 1
dd$NC[dd$statefip == 37] <- 1
dd$ND[dd$statefip == 38] <- 1
dd$NE[dd$statefip == 31] <- 1
dd$NH[dd$statefip == 33] <- 1
dd$NJ[dd$statefip == 34] <- 1
dd$NM[dd$statefip == 35] <- 1
dd$NV[dd$statefip == 32] <- 1
dd$NY[dd$statefip == 36] <- 1
dd$OH[dd$statefip == 39] <- 1
dd$OK[dd$statefip == 40] <- 1
dd$OR[dd$statefip == 41] <- 1
dd$PA[dd$statefip == 42] <- 1
dd$RI[dd$statefip == 44] <- 1
dd$SC[dd$statefip == 45] <- 1
dd$SD[dd$statefip == 46] <- 1
dd$TN[dd$statefip == 47] <- 1
dd$TX[dd$statefip == 48] <- 1
dd$UT[dd$statefip == 49] <- 1
dd$VA[dd$statefip == 51] <- 1
dd$VT[dd$statefip == 50] <- 1
dd$WA[dd$statefip == 53] <- 1
dd$WI[dd$statefip == 55] <- 1
dd$WV[dd$statefip == 54] <- 1
dd$WY[dd$statefip == 56] <- 1
dd$ss=""
dd$ss[dd$statefip == 02] <- "AK"
dd$ss[dd$statefip == 01] <- "AL"
dd$ss[dd$statefip == 05] <- "AR"
dd$ss[dd$statefip == 60] <- "AS"
dd$ss[dd$statefip == 04] <- "AZ"
dd$ss[dd$statefip == 06] <- "CA"
dd$ss[dd$statefip == 08] <- "CO"
dd$ss[dd$statefip == 09] <- "CT"
dd$ss[dd$statefip == 11] <- "DC"
dd$ss[dd$statefip == 10] <- "DE"
dd$ss[dd$statefip == 12] <- "FL"
dd$ss[dd$statefip == 13] <- "GA"
dd$ss[dd$statefip == 66] <- "GU"
dd$ss[dd$statefip == 15] <- "HI"
dd$ss[dd$statefip == 19] <- "IA"
dd$ss[dd$statefip == 16] <- "ID"
dd$ss[dd$statefip == 17] <- "IL"
dd$ss[dd$statefip == 18] <- "IN"
dd$ss[dd$statefip == 20] <- "KS"
dd$ss[dd$statefip == 21] <- "KY"
dd$ss[dd$statefip == 22] <- "LA"
dd$ss[dd$statefip == 25] <- "MA"
dd$ss[dd$statefip == 24] <- "MD"
dd$ss[dd$statefip == 23] <- "ME"
dd$ss[dd$statefip == 26] <- "MI"
dd$ss[dd$statefip == 27] <- "MN"
dd$ss[dd$statefip == 29] <- "MO"
dd$ss[dd$statefip == 28] <- "MS"
dd$ss[dd$statefip == 30] <- "MT"
dd$ss[dd$statefip == 37] <- "NC"
dd$ss[dd$statefip == 38] <- "ND"
dd$ss[dd$statefip == 31] <- "NE"
dd$ss[dd$statefip == 33] <- "NH"
dd$ss[dd$statefip == 34] <- "NJ"
dd$ss[dd$statefip == 35] <- "NM"
dd$ss[dd$statefip == 32] <- "NV"
dd$ss[dd$statefip == 36] <- "NY"
dd$ss[dd$statefip == 39] <- "OH"
dd$ss[dd$statefip == 40] <- "OK"
dd$ss[dd$statefip == 41] <- "OR"
dd$ss[dd$statefip == 42] <- "PA"
dd$ss[dd$statefip == 72] <- "PR"
dd$ss[dd$statefip == 44] <- "RI"
dd$ss[dd$statefip == 45] <- "SC"
dd$ss[dd$statefip == 46] <- "SD"
dd$ss[dd$statefip == 47] <- "TN"
dd$ss[dd$statefip == 48] <- "TX"
dd$ss[dd$statefip == 49] <- "UT"
dd$ss[dd$statefip == 51] <- "VA"
dd$ss[dd$statefip == 78] <- "VI"
dd$ss[dd$statefip == 50] <- "VT"
dd$ss[dd$statefip == 53] <- "WA"
dd$ss[dd$statefip == 55] <- "WI"
dd$ss[dd$statefip == 54] <- "WV"
dd$ss[dd$statefip == 56] <- "WY"
dd$st <- dd$ss

leg <- c("California","Connecticut","DC","Florida","Georgia","Illinois",
  "Maryland","Massachusetts","Michigan","New Jersey","New York",
  "Ohio","Oregon","Pennsylvania","Texas","Virginia","Washington")
col <- c(2,3,4,5,6,9,10,11,12,13,14,17,18,19,20,21,22)
pch <- c(0,1,2,4,5,6,9,10,11,12,13,14,17,18,19,20,21)
gg <- data.frame(leg=leg,col=col,pch=pch)
#print(head(gg))

dd$sCol="black"
dd$sCol[dd$ss == "CA"] <- gg$col[1]
dd$sCol[dd$ss == "CT"] <- gg$col[2]
dd$sCol[dd$ss == "DC"] <- gg$col[3]
dd$sCol[dd$ss == "FL"] <- gg$col[4]
dd$sCol[dd$ss == "GA"] <- gg$col[5]
dd$sCol[dd$ss == "IL"] <- gg$col[6]
dd$sCol[dd$ss == "MD"] <- gg$col[7]
dd$sCol[dd$ss == "MA"] <- gg$col[8]
dd$sCol[dd$ss == "MI"] <- gg$col[9]
dd$sCol[dd$ss == "NJ"] <- gg$col[10]
dd$sCol[dd$ss == "NY"] <- gg$col[11]
dd$sCol[dd$ss == "OH"] <- gg$col[12]
dd$sCol[dd$ss == "OR"] <- gg$col[13]
dd$sCol[dd$ss == "PA"] <- gg$col[14]
dd$sCol[dd$ss == "TX"] <- gg$col[15]
dd$sCol[dd$ss == "VA"] <- gg$col[16]
dd$sCol[dd$ss == "WA"] <- gg$col[17]

dd$sPch=3
dd$sPch[dd$ss == "CA"] <- gg$pch[1]
dd$sPch[dd$ss == "CT"] <- gg$pch[2]
dd$sPch[dd$ss == "DC"] <- gg$pch[3]
dd$sPch[dd$ss == "FL"] <- gg$pch[4]
dd$sPch[dd$ss == "GA"] <- gg$pch[5]
dd$sPch[dd$ss == "IL"] <- gg$pch[6]
dd$sPch[dd$ss == "MD"] <- gg$pch[7]
dd$sPch[dd$ss == "MA"] <- gg$pch[8]
dd$sPch[dd$ss == "MI"] <- gg$pch[9]
dd$sPch[dd$ss == "NJ"] <- gg$pch[10]
dd$sPch[dd$ss == "NY"] <- gg$pch[11]
dd$sPch[dd$ss == "OH"] <- gg$pch[12]
dd$sPch[dd$ss == "OR"] <- gg$pch[13]
dd$sPch[dd$ss == "PA"] <- gg$pch[14]
dd$sPch[dd$ss == "TX"] <- gg$pch[15]
dd$sPch[dd$ss == "VA"] <- gg$pch[16]
dd$sPch[dd$ss == "WA"] <- gg$pch[17]

dd$ss[dd$ss == "CA"] <- ""
dd$ss[dd$ss == "CT"] <- ""
dd$ss[dd$ss == "DC"] <- ""
dd$ss[dd$ss == "FL"] <- ""
dd$ss[dd$ss == "GA"] <- ""
dd$ss[dd$ss == "IL"] <- ""
dd$ss[dd$ss == "MD"] <- ""
dd$ss[dd$ss == "MA"] <- ""
dd$ss[dd$ss == "MI"] <- ""
dd$ss[dd$ss == "NJ"] <- ""
dd$ss[dd$ss == "NY"] <- ""
dd$ss[dd$ss == "OH"] <- ""
dd$ss[dd$ss == "OR"] <- ""
dd$ss[dd$ss == "PA"] <- ""
dd$ss[dd$ss == "TX"] <- ""
dd$ss[dd$ss == "VA"] <- ""
dd$ss[dd$ss == "WA"] <- ""
