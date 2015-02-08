# Read public.dta for 2000-2010 to create data.frame ddorg
#library(foreign) # needed for read.dta
ddorg <- read.table("morg13.txt") # CHANGE FOR YEARS
dd = ddorg[ddorg$year < 2014,] # CHANGE FOR YEARS
labyears="2000-2013" # CHANGE FOR YEARS
dd$lf_native <- dd$pop_native # use bad emprate_native

# Select immigration group for graphs
dd$emp_imm <- dd$emp_edus_stem_grad
forlab1 <- "Employed Foreign STEM Workers with Adv US Degrees"
forlab2 <- "Foreign Share of Total Employment (STEM w/ Adv US Deg)"

#dd$emp_imm <- dd$emp_edus_stem_grad+dd$emp_nedus_stem_grad
#forlab1 <- "Employed Foreign STEM Workers with Advanced Degrees"
#forlab2 <- "Foreign Share of Total Employment (STEM w/ Adv Deg)"

#dd$emp_imm <- dd$emp_edus_grad+dd$emp_nedus_grad
#forlab1 <- "Employed Foreign Workers with Advanced Degrees"
#forlab2 <- "Foreign Share of Total Employment (all w/ Adv Deg)"

source("amjobs.R")
