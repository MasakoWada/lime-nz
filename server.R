## Version 5
## Update 09 Feb 2023 [mw]

library(shiny); library(ggplot2); library(reactable); library(plotly)
library(xlsx); library(reshape2); library(scales); library(plyr)


## Read the parameter definition matrix
#para <- read.csv("./shinyapp/param.csv")
# para <- read.csv("./param.csv")
# para$name <- paste(para$var1, para$var2, sep = "_")

file <- "disease_impact_calculator_v5.xlsm"
para1 <- read.xlsx(paste0("dat/", file), sheetName = "Disease parameter", as.data.frame = TRUE)
para2 <- read.xlsx(paste0("dat/", file), sheetName = "Dairy parameter", as.data.frame = TRUE)
para3 <- read.xlsx(paste0("dat/", file), sheetName = "Beef parameter", as.data.frame = TRUE)
para4 <- read.xlsx(paste0("dat/", file), sheetName = "Sheep parameter", as.data.frame = TRUE)
dfn <- read.xlsx(paste0("dat/", file), sheetName = "Farm type", as.data.frame = TRUE)

para <- reshape2::melt(para1, id = c("ID", "Section", "Parameter", "Unit"))
names(para) <- tolower(names(para))
para$no <- paste0("no", para$id)

levels(para$variable) <- gsub("\\.|\\.\\.", " ", levels(para$variable))

para$tabname <- factor(para$section, levels = unique(para$section))
levels(para$tabname) <- c("Morbidity/mortality", "Morbidity/mortality", "Reproduction/milk", "Reproduction/milk", "Suboptimal sales/cull", "Intervention")

para$tabno <- as.numeric(para$tabname)
pa <- para[para$variable == para$variable[1],]

## Paste descriptive data
dfn$peak.cow.no <- as.numeric(c(para2[1,4:15], rep(NA, 17)))
dfn$open.cattle.su <- as.numeric(c(rep(NA, 12), para3[1,4:20]))
dfn$open.sheep.su <- as.numeric(c(rep(NA, 12), para4[1,4:20]))
dfn$effective.area.ha <- as.numeric(c(para2[32,4:15], para3[42,4:20]))
dfn$sheep.cattle.su.ratio <- as.numeric(c(rep(NA, 12), para3[43,4:20]))

ts <- 15
## ===================================================================================================
## Functions (copy from farm_gross_margin.R)
input_dairy <- function(par1, par2){
  
  par1[is.na(par1)] <- 0
  
  par2 <- t(par2[,4:15])
  
  ## Iteration number (stochastic)
  nit <- 1
  
  ## Dairy parameters
  inp0 <- data.frame(
    
    hsize = par2[,1],
    r.mort1 = par2[,2],
    r.mort2 = par2[,3],
    r.mort3 = par2[,4],
    
    r.empty = par2[,5],
    r.abort = par2[,6],
    r.repop = par2[,7],
    
    p.bobby = par2[,8],
    
    # r.cull1 = par2[,9],
    # r.cull2 = par2[,10],
    
    p.heifer = par2[,11],
    
    r.ms = par2[,13],
    val.ms = par2[,14],
    
    val.sell1 = par2[,15],
    val.sell2 = par2[,16],
    val.sellx = par2[,17],
    
    val.cull1 = par2[,18],
    val.cull2 = par2[,19],
    val.cull3 = par2[,20],
    
    exp.lab1 = par2[,21],
    exp.lab2 = par2[,22],
    exp.lab3 = par2[,23],
    
    exp.ah1 = par2[,24],
    
    exp.breed = par2[,27],
    
    exp.feed1 = par2[,28],
    exp.feed2 = par2[,29],
    exp.feed3 = par2[,31],
    
    exp.other = par2[,30],
    
    net.adj = par2[,33],
    val.sell3 = par2[,34],
    
    ref.exp = par2[,35],
    ref.inc = par2[,36],
    ref.profit = par2[,37]
    
  )
  
  inp0$r.fail <- inp0$r.empty + (1 - inp0$r.empty) * inp0$r.abort
  inp0$r.cull1 <- inp0$r.repop - inp0$r.mort1
  inp0$r.cull2 <- inp0$r.cull1
  
  ## Mid-year average adjustment
  f1 <- 1/(1-(inp0$r.cull1 + inp0$r.mort1)/2)
  inp0$r.ms <- inp0$r.ms * f1
  inp0$exp.lab1 <- inp0$exp.lab1 * f1
  inp0$exp.feed1 <- inp0$exp.feed1 * f1
  inp0$exp.feed2 <- inp0$exp.feed2 * f1
  inp0$exp.feed3 <- inp0$exp.feed2 + inp0$exp.feed3
  
  ## Denominator adjustment factor
  f2 <- 1/(1+2*inp0$r.repop)
  inp0$exp.ah1 <- inp0$exp.ah1 * f2
  
  ## Disease scenario
  inp1 <- inp0
  
  inp1$prev <- par1[1,"value"]/100
  
  inp1$r.mort1 <- inp0$r.mort1 + par1[2,"value"]/100
  inp1$r.mort2 <- inp0$r.mort2 + par1[2,"value"]/100
  inp1$r.mort3 <- inp0$r.mort3 + par1[3,"value"]/100
  
  inp1$r.abort <- inp0$r.abort + par1[4,"value"]/100
  inp1$r.empty <- inp0$r.empty + par1[5,"value"]/100
  inp1$r.fail <- inp1$r.empty + (1 - inp1$r.empty) * inp1$r.abort
  
  inp1$r.cull1 <- inp1$r.fail + (inp0$r.cull1 - inp0$r.fail) + par1[8,"value"]/100
  inp1$r.cull2 <- inp1$r.cull1
  
  inp1$r.ms <- inp0$r.ms * (1 - par1[6,"value"]/100)
  
  inp1$val.sell1 <- inp0$val.sell1 * (1 - par1[7,"value"]/100)
  inp1$val.sell2 <- inp0$val.sell2 * (1 - par1[7,"value"]/100)
  inp1$val.sell3 <- inp0$val.sell3 * (1 - par1[7,"value"]/100)
  inp1$val.sellx <- inp0$val.sellx * (1 - par1[7,"value"]/100)
  
  inp1$val.cull1 <- inp0$val.cull1 * (1 - par1[7,"value"]/100)
  inp1$val.cull2 <- inp0$val.cull2 * (1 - par1[7,"value"]/100)
  inp1$val.cull3 <- inp0$val.cull3 * (1 - par1[7,"value"]/100)
  
  inp1$exp.ah1 <- inp0$exp.ah1 + (par1[11,"value"] * par1[10,"value"]/100)
  
  list(inp0, inp1) 
}

calc_dairy <- function(inp){
  
  inp0 <- inp[[1]]
  inp1 <- inp[[2]]
  
  fclass <- factor(row.names(inp0), levels = row.names(inp0))
  
  ## ===================================================================================================
  ## Baseline herd (no disease)
  gm0 <- data.frame(class = fclass)
  
  ## Initial numbers
  gm0$n1 <- inp0$hsize
  # gm0$n2 <- gm0$n1 * inp0$r.repop / (1 - inp0$r.empty) / (1 - inp0$r.mort2)
  gm0$n2 <- gm0$n1 * inp0$r.repop / (1 - inp0$r.cull2 - inp0$r.mort2)
  gm0$n3 <- gm0$n1 * (1 - inp0$r.cull1 - inp0$r.mort1)
  
  
  ## Number died
  gm0$die1 <- gm0$n1 * inp0$r.mort1
  gm0$die2 <- gm0$n2 * inp0$r.mort2
  gm0$die3 <- gm0$n3 * inp0$r.mort3
  
  ## Number culled
  gm0$cull1 <- gm0$n1 * inp0$r.cull1
  gm0$cull2 <- gm0$n2 * inp0$r.cull2
  
  ## Calves
  ## Dairy heifer calves (starting number) (ha)
  ## Bobby calves (cull3)
  
  gm0$n3ha <- gm0$n3 * inp0$p.heifer
  gm0$n3hb <- gm0$n3ha * (1 - inp0$r.mort3)
  gm0$cull3 <- gm0$n3 * inp0$p.bobby * (1 - inp0$r.mort3)
  
  ## Number sold
  ## Beef x dairy calves
  gm0$sell3 <- gm0$n3 * (1 - inp0$p.bobby - inp0$p.heifer) * (1 - inp0$r.mort3)
  
  
  ## Closing number (before purchase)
  gm0$n1. <- gm0$n1 - gm0$die1 - gm0$cull1
  gm0$n2. <- gm0$n2 - gm0$die2 - gm0$cull2
  gm0$n3. <- gm0$n3ha * (1 - inp0$r.mort3)
  
  # ## Number purchased
  gm0$buy1 <- gm0$n1 * inp0$r.repop - gm0$n2.
  gm0$buy2 <- gm0$n2 - gm0$n3hb
  
  ## Season average number
  gm0$n1x <- (gm0$n1 + gm0$n1.)/2
  gm0$n2x <- (gm0$n2 + gm0$n2.)/2
  gm0$n3x <- (gm0$n3 - gm0$cull3) * (1 - inp0$r.mort3/2)
  
  ## Milk production
  gm0$ms <- gm0$n1x * inp0$r.ms
  
  ## Expenses
  gm0$exp.feed <- inp0$exp.feed1 * gm0$n1x + inp0$exp.feed2 * gm0$n2x + inp0$exp.feed3 * gm0$n3x
  gm0$exp.breed <- inp0$exp.breed * (gm0$n1 + gm0$n2)
  gm0$exp.ah <- inp0$exp.ah1 * (gm0$n1x + gm0$n2x + gm0$n3x)
  gm0$exp.lab <- inp0$exp.lab1 * gm0$n1x + inp0$exp.lab2 * gm0$n2x + inp0$exp.lab3 * gm0$n3x
  gm0$exp.stock <- gm0$buy1 * inp0$val.sell1 + gm0$buy2 * inp0$val.sell2
  gm0$exp.other <- inp0$exp.other
  gm0$exp.opp <- 0
  gm0$exp.purchase <- gm0$exp.stock + gm0$exp.opp
  gm0$exp.total <- gm0$exp.feed + gm0$exp.breed + gm0$exp.ah + gm0$exp.lab + gm0$exp.other
  
  ## Income
  gm0$inc.ms <- inp0$val.ms * gm0$ms
  gm0$inc.stock1 <- inp0$val.cull1 * gm0$cull1
  gm0$inc.stock2 <- inp0$val.cull2 * gm0$cull2
  gm0$inc.stock3 <- inp0$val.cull3 * gm0$cull3 + inp0$val.sellx * gm0$sell3
  gm0$inc.stock <- gm0$inc.stock1 + gm0$inc.stock2 + gm0$inc.stock3
  
  gm0$inc.stock.net <- gm0$inc.stock - gm0$exp.stock
  gm0$inc.total <- gm0$inc.ms + gm0$inc.stock.net
  
  
  ## Farm level summary
  gm0$profit <- gm0$inc.total - gm0$exp.total + inp0$net.adj
  
  ## ===================================================================================================
  ## Infected herd - non-infected component (gm10)
  gm10 <- data.frame(class = fclass)
  
  ## Initial numbers
  gm10$n1 <- inp0$hsize * (1 - inp1$prev)
  gm10$n2 <- gm0$n2 * (1 - inp1$prev)
  gm10$n3 <- gm10$n1 * (1 - inp0$r.cull1 - inp0$r.mort1) * (1 - inp1$prev)
  
  
  ## Number died
  gm10$die1 <- gm10$n1 * inp0$r.mort1
  gm10$die2 <- gm10$n2 * inp0$r.mort2
  gm10$die3 <- gm10$n3 * inp0$r.mort3
  
  ## Number culled
  gm10$cull1 <- gm10$n1 * inp0$r.cull1
  gm10$cull2 <- gm10$n2 * inp0$r.cull2
  
  ## Calves
  ## Dairy heifer calves (starting number) (ha)
  ## Bobby calves (cull3)
  gm10$n3ha <- gm10$n3 * inp0$p.heifer
  gm10$n3hb <- gm10$n3ha * (1 - inp0$r.mort3)
  gm10$cull3 <- gm10$n3 * inp0$p.bobby * (1 - inp0$r.mort3)
  
  ## Number sold
  ## Beef x dairy calves
  gm10$sell3 <- gm10$n3 * (1 - inp0$p.bobby - inp0$p.heifer) * (1 - inp0$r.mort3)
  
  
  ## ===================================================================================================
  ## Infected herd - infected component (gm11)
  gm11 <- data.frame(class = fclass)
  
  ## Initial numbers
  gm11$n1 <- inp0$hsize * inp1$prev
  gm11$n2 <- gm0$n2 * inp1$prev
  gm11$n3 <- gm11$n1 * (1 - inp1$r.cull1 - inp1$r.mort1) + gm10$n1 * (1 - inp0$r.cull1 - inp0$r.mort1) * inp1$prev
  
  
  ## Number died
  gm11$die1 <- gm11$n1 * inp1$r.mort1
  gm11$die2 <- gm11$n2 * inp1$r.mort2
  gm11$die3 <- gm11$n3 * inp1$r.mort3
  
  
  ## Number culled
  gm11$cull1 <- gm11$n1 * inp1$r.cull1
  gm11$cull2 <- gm11$n2 * inp1$r.cull2
  
  ## Calves
  ## Dairy heifer calves (starting number) (ha)
  ## Bobby calves (cull3)
  gm11$n3ha <- gm11$n3 * inp0$p.heifer
  gm11$n3hb <- gm11$n3ha * (1 - inp1$r.mort3)
  gm11$cull3 <- gm11$n3 * inp0$p.bobby * (1 - inp1$r.mort3)
  
  ## Beef x dairy calves
  gm11$sell3 <- gm11$n3 * (1 - inp0$p.bobby - inp0$p.heifer) * (1 - inp1$r.mort3)
  
  # ## ====================================================================================================
  # ## Closing number
  gm10$n1. <- gm10$n1 - gm10$die1 - gm10$cull1
  gm10$n2. <- gm10$n2 - gm10$die2 - gm10$cull2
  gm10$n3. <- gm10$n3 - gm10$die3 - gm10$cull3 - gm10$sell3
  
  gm11$n1. <- gm11$n1 - gm11$die1 - gm11$cull1
  gm11$n2. <- gm11$n2 - gm11$die2 - gm11$cull2
  gm11$n3. <- gm11$n3 - gm11$die3 - gm11$cull3 - gm11$sell3
  
  
  ## Season average number
  gm10$n1x <- gm10$n1 - (gm10$die1 + gm10$cull1)/2
  gm10$n2x <- gm10$n2 - (gm10$die2 + gm10$cull2)/2
  gm10$n3x <- (gm10$n3 - gm10$cull3) * (1 - inp0$r.mort3/2)
  
  gm11$n1x <- gm11$n1 - (gm11$die1 + gm11$cull1)/2
  gm11$n2x <- gm11$n2 - (gm11$die2 + gm11$cull2)/2
  gm11$n3x <- (gm11$n3 - gm11$cull3) * (1 - inp1$r.mort3/2)
  
  
  ## Milk production
  gm10$ms <- gm10$n1x * inp0$r.ms
  gm11$ms <- gm11$n1x * inp1$r.ms
  
  
  ## Income
  gm10$inc.ms <- inp0$val.ms * gm10$ms
  gm10$inc.stock1 <- inp0$val.cull1 * gm10$cull1
  gm10$inc.stock2 <- inp0$val.cull2 * gm10$cull2
  gm10$inc.stock3 <- inp0$val.cull3 * gm10$cull3 + inp0$val.sellx * gm10$sell3
  gm10$inc.stock <- gm10$inc.stock1 + gm10$inc.stock2 + gm10$inc.stock3
  gm10$inc.total <- gm10$inc.ms + gm10$inc.stock
  
  gm11$inc.ms <- inp1$val.ms * gm11$ms
  gm11$inc.stock1 <- inp1$val.cull1 * gm11$cull1
  gm11$inc.stock2 <- inp1$val.cull2 * gm11$cull2
  gm11$inc.stock3 <- inp1$val.cull3 * gm11$cull3 + inp1$val.sellx * gm11$sell3
  gm11$inc.stock <- gm11$inc.stock1 + gm11$inc.stock2 + gm11$inc.stock3
  gm11$inc.total <- gm11$inc.ms + gm11$inc.stock
  
  
  ## Expenses
  gm10$exp.feed <- inp0$exp.feed1 * gm10$n1x + inp0$exp.feed2 * gm10$n2x + inp0$exp.feed3 * gm10$n3x
  gm10$exp.breed <- inp0$exp.breed * (gm10$n1 + gm10$n2)
  gm10$exp.ah <- inp0$exp.ah1 * (gm10$n1x + gm10$n2x + gm10$n3x)
  gm10$exp.lab <- inp0$exp.lab1 * gm10$n1x + inp0$exp.lab2 * gm10$n2x + inp0$exp.lab3 * gm10$n3x
  
  
  gm11$exp.feed <- inp0$exp.feed1 * gm11$n1x + inp0$exp.feed2 * gm11$n2x + inp0$exp.feed3 * gm11$n3x
  gm11$exp.breed <- inp1$exp.breed * (gm11$n1 + gm11$n2)
  gm11$exp.ah <- inp1$exp.ah1 * (gm11$n1x + gm11$n2x + gm11$n3x)
  gm11$exp.lab <- inp1$exp.lab1 * gm11$n1x + inp1$exp.lab2 * gm11$n2x + inp1$exp.lab3 * gm11$n3x
  
  ## ====================================================================================================
  ## Combined
  gm1 <- gm10
  
  gm1$n1 <- gm10$n1 + gm11$n1
  gm1$n2 <- gm10$n2 + gm11$n2
  gm1$n3 <- gm10$n3 + gm11$n3
  
  gm1$die1 <- gm10$die1 + gm11$die1
  gm1$die2 <- gm10$die2 + gm11$die2
  gm1$die3 <- gm10$die3 + gm11$die3
  
  gm1$cull1 <- gm10$cull1 + gm11$cull1
  gm1$cull2 <- gm10$cull2 + gm11$cull2
  gm1$cull3 <- gm10$cull3 + gm11$cull3
  
  gm1$sell3 <- gm10$sell3 + gm11$sell3
  
  
  # ## Number purchased
  gm1$buy1 <- gm0$n1 * inp0$r.repop - (gm10$n2. + gm11$n2.)
  gm1$buy2 <- gm1$n2 - (gm10$n3hb + gm11$n3hb)
  
  
  gm1$n1. <- gm10$n1. + gm11$n1.
  gm1$n2. <- gm10$n2. + gm11$n2.
  gm1$n3. <- gm10$n3. + gm11$n3.
  
  gm1$n1x <- gm10$n1x + gm11$n1x
  gm1$n2x <- gm10$n2x + gm11$n2x
  gm1$n3x <- gm10$n3x + gm11$n3x
  
  gm1$ms <- gm10$ms + gm11$ms 
  
  ## Expenses
  gm1$exp.feed <- gm10$exp.feed + gm11$exp.feed
  gm1$exp.breed <- gm10$exp.breed + gm11$exp.breed
  gm1$exp.ah <- gm10$exp.ah + gm11$exp.ah
  gm1$exp.lab <- gm10$exp.lab + gm11$exp.lab
  gm1$exp.stock <- gm1$buy1 * inp0$val.sell1 + gm1$buy2 * inp0$val.sell2
  gm1$exp.other <- inp0$exp.other
  
  ## Opportunity cost of less livestock
  gm1$exp.opp1 <- inp0$val.sell1 * (gm0$n1. - gm1$n1.)
  gm1$exp.opp2 <- inp0$val.sell2 * (gm0$n2. - gm1$n2.)
  gm1$exp.opp3 <- inp0$val.sell3 * (gm0$n3. - gm1$n3.)
  gm1$exp.opp <- gm1$exp.opp1 + gm1$exp.opp2 + gm1$exp.opp3
  
  gm1$exp.purchase <- gm1$exp.stock + gm1$exp.opp
  
  gm1$exp.total <- gm1$exp.feed + gm1$exp.breed + gm1$exp.ah + gm1$exp.lab + gm1$exp.other
  
  gm1$exp.other <- inp0$exp.other
  
  ## Income
  gm1$inc.ms <- gm10$inc.ms + gm11$inc.ms
  gm1$inc.stock1 <- gm10$inc.stock1 + gm11$inc.stock1
  gm1$inc.stock2 <- gm10$inc.stock2 + gm11$inc.stock2
  gm1$inc.stock3 <- gm10$inc.stock3 + gm11$inc.stock3
  gm1$inc.stock <- gm10$inc.stock + gm11$inc.stock
  
  gm1$inc.stock.net <- gm1$inc.stock - gm1$exp.purchase
  gm1$inc.total <- gm1$inc.ms + gm1$inc.stock.net
  
  
  ## Farm level summary
  gm1$profit <- gm1$inc.total - gm1$exp.total + inp0$net.adj
  
  
  ## ===================================================================================================
  ## Partial budget
  pb <- data.frame(class = fclass)
  pb$profit <- gm1$profit - gm0$profit
  pb$profit.pct <- 100 * pb$profit/gm0$profit
  
  pb$inc.total <- gm1$inc.total - gm0$inc.total
  pb$inc.ms <- gm1$inc.ms - gm0$inc.ms
  pb$inc.stock <- gm1$inc.stock - gm0$inc.stock
  pb$inc.stock1 <- gm1$inc.stock1 - gm0$inc.stock1
  pb$inc.stock2 <- gm1$inc.stock2 - gm0$inc.stock2
  pb$inc.stock3 <- gm1$inc.stock3 - gm0$inc.stock3
  
  pb$exp.feed <- gm1$exp.feed - gm0$exp.feed
  pb$exp.breed <- gm1$exp.breed - gm0$exp.breed
  pb$exp.ah <- gm1$exp.ah - gm0$exp.ah
  pb$exp.lab <- gm1$exp.lab - gm0$exp.lab
  pb$exp.stock <- gm1$exp.stock - gm0$exp.stock
  pb$exp.opp1 <- gm1$exp.opp1
  pb$exp.opp2 <- gm1$exp.opp2
  pb$exp.opp3 <- gm1$exp.opp3
  pb$exp.opp <- gm1$exp.opp
  pb$exp.purchase <- gm1$exp.purchase - gm0$exp.purchase
  pb$exp.total <- gm1$exp.total - gm0$exp.total
  
  pb$inc.total.pct <- 100 * pb$inc.total/gm0$inc.total
  pb$inc.ms.pct <- 100 * pb$inc.ms/gm0$inc.ms
  pb$inc.stock.pct <- 100 * pb$inc.stock/gm0$inc.stock
  pb$inc.stock1.pct <- 100 * pb$inc.stock1/gm0$inc.stock1
  pb$inc.stock2.pct <- 100 * pb$inc.stock2/gm0$inc.stock2
  pb$inc.stock3.pct <- 100 * pb$inc.stock3/gm0$inc.stock3
  
  pb$exp.feed.pct <- 100 * pb$exp.feed/gm0$exp.feed
  pb$exp.breed.pct <- 100 * pb$exp.breed/gm0$exp.breed
  pb$exp.ah.pct <- 100 * pb$exp.ah/gm0$exp.ah
  pb$exp.lab.pct <- 100 * pb$exp.lab/gm0$exp.lab
  pb$exp.stock.pct <- 100 * pb$exp.stock/gm0$exp.stock
  pb$exp.opp1 <- gm1$exp.opp1
  pb$exp.opp2 <- gm1$exp.opp2
  pb$exp.opp3 <- gm1$exp.opp3
  pb$exp.opp <- gm1$exp.opp
  pb$exp.purchase.pct <- 100 * pb$exp.purchase/gm0$exp.purchase
  pb$exp.total.pct <- 100 * pb$exp.total/gm0$exp.total
  
  ## Reference
  pb$diff.exp <- gm0$exp.total - inp0$ref.exp
  pb$diff.inc <- gm0$inc.total - inp0$ref.inc
  pb$diff.profit <- gm0$profit - inp0$ref.profit
  
  pb$error.exp <- 100 * (gm0$exp.total - inp0$ref.exp)/inp0$ref.exp
  pb$error.inc <- 100 * (gm0$inc.total - inp0$ref.inc)/inp0$ref.inc
  pb$error.profit <- 100 * (gm0$profit - inp0$ref.profit)/inp0$ref.profit
  
  pb$unit.loss <- pb$profit/gm0$n1
  
  out <- list(gm0, gm1, pb)
  names(out) <- c("Non-infected farm", "Infected farm", "Partial Budget")
  return(out)
  
}

input_beef <- function(par1, par2){
  
  par1[is.na(par1)] <- 0
  par2[is.na(par2)] <- 0
  
  par2 <- t(par2[,4:20])
  
  ## Iteration number (stochastic)
  nit <- 1
  
  ## Beef parameters
  inp0 <- data.frame(
    
    open = par2[,1],
    hsize = par2[,2],
    n.calves = par2[,3],
    
    # r.repop = par2[,7],
    # r.empty = par2[,5],
    # r.abort = par2[,6],
    
    r.mort1 = par2[,6]/100,
    r.mort2 = par2[,5]/100,
    
    n.store.h0 = par2[,10],
    n.store.h1 = par2[,16],
    n.store.s0 = par2[,8],
    n.store.s1 = par2[,14],
    n.store.b0 = par2[,12],
    n.store.b1 = par2[,18],
    
    n.prime.c = par2[,28],
    n.prime.h1 = par2[,22],
    n.prime.h2 = par2[,24],
    n.prime.s1 = par2[,20],
    n.prime.s2 = par2[,26],
    n.prime.b = par2[,30],
    
    val.store.c = par2[,36],
    val.store.h0 = par2[,11],
    val.store.h1 = par2[,17],
    val.store.s0 = par2[,9],
    val.store.s1 = par2[,15],
    val.store.b0 = par2[,13],
    val.store.b1 = par2[,19],
    
    val.prime.c = par2[,29],
    val.prime.h1 = par2[,23],
    val.prime.h2 = par2[,25],
    val.prime.s1 = par2[,21],
    val.prime.s2 = par2[,27],
    val.prime.b = par2[,31],
    val.prime.h0 = NA,
    val.prime.s0 = NA,
    val.prime.b0 = NA,
    
    
    
    # val.avg.calf = par2[,7],
    # val.avg.h1 = par2[,32],
    # val.avg.h2 = par2[,33],
    # val.avg.s1 = par2[,34],
    # val.avg.s2 = par2[,35],
    # val.avg.c = par2[,36],
    # val.avg.b = par2[,37],
    
    exp.lab = par2[,38],
    
    exp.ah = par2[,39],
    
    exp.feed = par2[,40],
    exp.graze = par2[,44],
    
    exp.other = par2[,41],
    
    ref.inc = par2[,45],
    ref.exp = par2[,46],
    ref.profit = par2[,45] - par2[,46]
    
    
  )
  
  ## Adjustment
  inp0$hsize <- ifelse(inp0$hsize > inp0$n.prime.c, inp0$hsize, inp0$n.prime.c)
  # inp0$val.store.h0[inp0$val.store.h0==0] <- median(inp0$val.store.h0[inp0$val.store.h0!=0])
  # inp0$val.store.s0[inp0$val.store.s0==0] <- median(inp0$val.store.s0[inp0$val.store.s0!=0])
  # inp0$val.store.b0[inp0$val.store.b0==0] <- median(inp0$val.store.b0[inp0$val.store.b0!=0])
  
  
  # inp0$r.cull1 <- (1 - inp0$r.mort1) * (inp0$r.empty + inp0$r.abort)
  # inp0$r.sell1 <- apply(cbind(inp0$r.repop - inp0$r.mort1 - inp0$r.cull1, 0), 1, max)
  
  
  
  ## Disease scenario
  inp1 <- inp0
  
  inp1$prev <- par1[1,"value"]/100
  
  inp1$r.mort1 <- inp0$r.mort1 + par1[2,"value"]/100
  inp1$r.mort2 <- inp0$r.mort2 + par1[3,"value"]/100
  
  inp1$r.abort <- par1[4,"value"]/100 # no baseline rate
  inp1$r.empty <- par1[5,"value"]/100 # no baseline rate
  
  inp1$r.cull <- par1[8,"value"]/100 # no baseline rate
  inp1$r.under <- par1[9,"value"]/100 # no baseline rate
  
  inp1$val.store.h0 <- inp1$val.store.h0 * (1 - par1[7,"value"]/100)
  inp1$val.store.h1 <- inp1$val.store.h1 * (1 - par1[7,"value"]/100)
  inp1$val.store.s0 <- inp1$val.store.s0 * (1 - par1[7,"value"]/100)
  inp1$val.store.s1 <- inp1$val.store.s1 * (1 - par1[7,"value"]/100)
  inp1$val.store.b0 <- inp1$val.store.b0 * (1 - par1[7,"value"]/100)
  inp1$val.store.b1 <- inp1$val.store.b1 * (1 - par1[7,"value"]/100)
  
  inp1$val.prime.c <- inp1$val.prime.c * (1 - par1[7,"value"]/100)
  inp1$val.prime.h1 <- inp1$val.prime.h1 * (1 - par1[7,"value"]/100)
  inp1$val.prime.h2 <- inp1$val.prime.h2 * (1 - par1[7,"value"]/100)
  inp1$val.prime.s1 <- inp1$val.prime.s1 * (1 - par1[7,"value"]/100)
  inp1$val.prime.s2 <- inp1$val.prime.s2 * (1 - par1[7,"value"]/100)
  inp1$val.prime.b <- inp1$val.prime.b * (1 - par1[7,"value"]/100)
  
  inp1$exp.ah.extra <- par1[11,"value"] * par1[10,"value"]/100
  
  list(inp0, inp1) 
}

calc_beef <- function(inp){
  
  inp0 <- inp[[1]]
  inp1 <- inp[[2]]
  
  fclass <- factor(row.names(inp0), levels = row.names(inp0))
  
  ## ===================================================================================================
  ## Baseline herd (no disease)
  gm0 <- data.frame(class = fclass)
  
  ## Sell (store)
  gm0$sell.h0 <- inp0$n.store.h0
  gm0$sell.h1 <- inp0$n.store.h1
  gm0$sell.s0 <- inp0$n.store.s0
  gm0$sell.s1 <- inp0$n.store.s1
  gm0$sell.b0 <- inp0$n.store.b0
  gm0$sell.b1 <- inp0$n.store.b1
  gm0$sell.c <- 0
  gm0$sell.b <- 0
  gm0$sell.h2 <- 0
  gm0$sell.s2 <- 0
  
  ## Cull (prime)
  gm0$cull.c <- inp0$n.prime.c
  gm0$cull.h1 <- inp0$n.prime.h1
  gm0$cull.h2 <- inp0$n.prime.h2
  gm0$cull.s1 <- inp0$n.prime.s1
  gm0$cull.s2 <- inp0$n.prime.s2
  gm0$cull.b <- inp0$n.prime.b
  gm0$cull.h0 <- 0
  gm0$cull.s0 <- 0
  gm0$cull.b0 <- 0
  gm0$cull.b1 <- 0
  
  ## Initial number, death
  ## (assuming the number leaving the herd = the number entering the herd)
  
  ## Starting from MA cows
  gm0$n.c <- inp0$hsize
  gm0$die.c <- gm0$n.c * inp0$r.mort1
  
  ## R3 heifers
  gm0$n.h2 <- (gm0$sell.h2 + gm0$cull.h2)/(1 - inp0$r.mort1)
  gm0$die.h2 <- gm0$n.h2 * inp0$r.mort1
  
  ## R2 heifers
  gm0$n.h1 <- ((gm0$die.c + gm0$sell.c + gm0$cull.c) + (gm0$sell.h1 + gm0$cull.h1) + gm0$n.h2)/(1 - inp0$r.mort1)
  gm0$die.h1 <- gm0$n.h1 * inp0$r.mort1
  
  ## Heifer calves/R1 heifers
  gm0$buy.h0 <- (gm0$n.h1 + gm0$sell.h0 - inp0$n.calves/2)/(1 - inp0$r.mort2)
  gm0$buy.h0 <- ifelse(gm0$buy.h0 < 0, 0, gm0$buy.h0)
  gm0$n.h0 <- inp0$n.calves/2 + gm0$buy.h0
  gm0$die.h0 <- gm0$n.h0 * inp0$r.mort2
  
  ## Starting from R3 steers
  gm0$n.s2 <- (gm0$sell.s2 + gm0$cull.s2)/(1 - inp0$r.mort1)
  gm0$die.s2 <- gm0$n.s2 * inp0$r.mort1
  
  ## R2 steers
  gm0$n.s1 <- (gm0$n.s2 + gm0$sell.s1 + gm0$cull.s1)/(1 - inp0$r.mort1)
  gm0$die.s1 <- gm0$n.s1 * inp0$r.mort1
  
  ## Steer calves / R0 steers
  gm0$buy.s0 <- (gm0$n.s1 + gm0$sell.s0 - inp0$n.calves * 0.5)/(1 - inp0$r.mort2)
  gm0$buy.s0 <- ifelse(gm0$buy.s0 < 0, 0, gm0$buy.s0)
  gm0$n.s0 <- inp0$n.calves * 0.5 + gm0$buy.s0
  gm0$die.s0 <- gm0$n.s0 * inp0$r.mort2
  
  ## Starting from breeding bulls
  # gm0$n.b <- (gm0$sell.b + gm0$cull.b)/(1 - inp0$r.mort1 - 0.10)
  gm0$n.b <- (gm0$sell.b + gm0$cull.b)/(1 - inp0$r.mort1)
  gm0$die.b <- gm0$n.b * inp0$r.mort1
  
  ## R2 bulls
  gm0$n.b1 <- (gm0$n.b + gm0$sell.b1 + gm0$cull.b1)/(1 - inp0$r.mort1)
  gm0$die.b1 <- gm0$n.b1 * inp0$r.mort1
  
  ## Bull calves / R0 bulls
  gm0$buy.b0 <- (gm0$n.b1 + gm0$sell.b0 + gm0$cull.b0)/(1 - inp0$r.mort2)
  gm0$n.b0 <- gm0$buy.b0
  gm0$die.b0 <- gm0$n.b0 * inp0$r.mort2
  
  ## Calving rate
  gm0$r.calv <- inp0$n.calves/gm0$n.c
  
  ## Sales rate; proportion of prime
  gm0$r.prod.c <- (gm0$sell.c + gm0$cull.c)/gm0$n.c
  gm0$r.prod.h0 <- (gm0$sell.h0 + gm0$cull.h0)/gm0$n.h0
  gm0$r.prod.h1 <- (gm0$sell.h1 + gm0$cull.h1)/gm0$n.h1
  gm0$r.prod.h2 <- (gm0$sell.h2 + gm0$cull.h2)/gm0$n.h2
  gm0$r.prod.s0 <- (gm0$sell.s0 + gm0$cull.s0)/gm0$n.s0
  gm0$r.prod.s1 <- (gm0$sell.s1 + gm0$cull.s1)/gm0$n.s1
  gm0$r.prod.s2 <- (gm0$sell.s2 + gm0$cull.s2)/gm0$n.s2
  gm0$r.prod.b <- (gm0$sell.b + gm0$cull.b)/gm0$n.b
  gm0$r.prod.b0 <- (gm0$sell.b0 + gm0$cull.b0)/gm0$n.b0
  gm0$r.prod.b1 <- (gm0$sell.b1 + gm0$cull.b1)/gm0$n.b1
  
  gm0$r.prime.c <- gm0$cull.c/(gm0$sell.c + gm0$cull.c)
  gm0$r.prime.h1 <- gm0$cull.h1/(gm0$sell.h1 + gm0$cull.h1)
  gm0$r.prime.h2 <- gm0$cull.h2/(gm0$sell.h2 + gm0$cull.h2)
  gm0$r.prime.s1 <- gm0$cull.s1/(gm0$sell.s1 + gm0$cull.s1)
  gm0$r.prime.s2 <- gm0$cull.s2/(gm0$sell.s2 + gm0$cull.s2)
  gm0$r.prime.b <- gm0$cull.b/(gm0$sell.b + gm0$cull.b)
  gm0$r.prime.b1 <- rep(0, nrow(gm0))
  gm0$r.prime.h0 <- rep(0, nrow(gm0))
  gm0$r.prime.s0 <- rep(0, nrow(gm0))
  gm0$r.prime.b0 <- rep(0, nrow(gm0))
  
  ## Other buys (default 0)
  gm0$buy.c <- 0
  gm0$buy.h2 <- 0
  gm0$buy.h1 <- 0
  gm0$buy.s2 <- 0
  gm0$buy.s1 <- 0
  gm0$buy.b <- 0
  gm0$buy.b1 <- 0
  
  ## Closing number
  gm0$n.c. <- gm0$n.c - gm0$die.c - gm0$sell.c - gm0$cull.c
  gm0$n.h2. <- gm0$n.h2 - gm0$die.h2 - gm0$sell.h2 - gm0$cull.h2
  gm0$n.h1. <- gm0$n.h1 - gm0$die.h1 - gm0$sell.h1 - gm0$cull.h1
  gm0$n.h0. <- gm0$n.h0 - gm0$die.h0 - gm0$sell.h0 - gm0$cull.h0
  gm0$n.s2. <- gm0$n.s2 - gm0$die.s2 - gm0$sell.s2 - gm0$cull.s2
  gm0$n.s1. <- gm0$n.s1 - gm0$die.s1 - gm0$sell.s1 - gm0$cull.s1
  gm0$n.s0. <- gm0$n.s0 - gm0$die.s0 - gm0$sell.s0 - gm0$cull.s0
  gm0$n.b. <- gm0$n.b - gm0$die.b - gm0$sell.b - gm0$cull.b
  gm0$n.b1. <- gm0$n.b1 - gm0$die.b1 - gm0$sell.b1 - gm0$cull.b1
  gm0$n.b0. <- gm0$n.b0 - gm0$die.b0 - gm0$sell.b0 - gm0$cull.b0
  
  ## Season average number
  gm0$n.c.x <- (gm0$n.c + gm0$n.c.)/2
  gm0$n.h0.x <- (gm0$n.h0 + gm0$n.h0.)/2
  gm0$n.h1.x <- (gm0$n.h1 + gm0$n.h1.)/2
  gm0$n.h2.x <- (gm0$n.h2 + gm0$n.h2.)/2
  gm0$n.s0.x <- (gm0$n.s0 + gm0$n.s0.)/2
  gm0$n.s1.x <- (gm0$n.s1 + gm0$n.s1.)/2
  gm0$n.s2.x <- (gm0$n.s2 + gm0$n.s2.)/2
  gm0$n.b0.x <- (gm0$n.b0 + gm0$n.b0.)/2
  gm0$n.b1.x <- (gm0$n.b1 + gm0$n.b1.)/2
  gm0$n.b.x <- (gm0$n.b + gm0$n.b.)/2
  gm0$n.calves.x <- gm0$n.h0.x + gm0$n.s0.x + gm0$n.b0.x
  gm0$su.x <- gm0$n.c.x * 5.5 + gm0$n.h2.x * 5.5 + gm0$n.h1.x * 4.5 + gm0$n.h0.x * 3.5 +
    gm0$n.s2.x * 5.5 + gm0$n.s1.x * 5 + gm0$n.s0.x * 4.5 + 
    gm0$n.b.x * 5.5 + gm0$n.b1.x * 5.5 + gm0$n.b0.x * 4.5
  
  ## Income
  gm0$inc.stock.c <- rowSums(cbind(inp0$val.prime.c * gm0$cull.c, inp0$val.store.c * gm0$sell.c), na.rm = TRUE)
  gm0$inc.stock.h0 <- rowSums(cbind(inp0$val.prime.h0 * gm0$cull.h0, inp0$val.store.h0 * gm0$sell.h0), na.rm = TRUE)
  gm0$inc.stock.h1 <- rowSums(cbind(inp0$val.prime.h1 * gm0$cull.h1, inp0$val.store.h1 * gm0$sell.h1), na.rm = TRUE)
  gm0$inc.stock.h2 <- rowSums(cbind(inp0$val.prime.h2 * gm0$cull.h2, inp0$val.store.h2 * gm0$sell.h2), na.rm = TRUE)
  gm0$inc.stock.s0 <- rowSums(cbind(inp0$val.prime.s0 * gm0$cull.s0, inp0$val.store.s0 * gm0$sell.s0), na.rm = TRUE)
  gm0$inc.stock.s1 <- rowSums(cbind(inp0$val.prime.s1 * gm0$cull.s1, inp0$val.store.s1 * gm0$sell.s1), na.rm = TRUE)
  gm0$inc.stock.s2 <- rowSums(cbind(inp0$val.prime.s2 * gm0$cull.s2, inp0$val.store.s2 * gm0$sell.s2), na.rm = TRUE)
  gm0$inc.stock.b <- rowSums(cbind(inp0$val.prime.b * gm0$cull.b, inp0$val.store.b * gm0$sell.b), na.rm = TRUE)
  gm0$inc.stock.b0 <- rowSums(cbind(inp0$val.prime.b0 * gm0$cull.b0, inp0$val.store.b0 * gm0$sell.b0), na.rm = TRUE)
  gm0$inc.stock.b1 <- rowSums(cbind(inp0$val.prime.b1 * gm0$cull.b1, inp0$val.store.b1 * gm0$sell.b1), na.rm = TRUE)
  
  # gm0$inc.stock.h <- gm0$inc.stock.h1 + gm0$inc.stock.h2
  # gm0$inc.stock.s <- gm0$inc.stock.s1 + gm0$inc.stock.s2
  # gm0$inc.stock.b <- gm0$inc.stock.b1 + gm0$inc.stock.b
  gm0$inc.stock.calves <- gm0$inc.stock.h0 + gm0$inc.stock.s0 + gm0$inc.stock.b0
  gm0$inc.stock.r2 <- gm0$inc.stock.h1 + gm0$inc.stock.s1 + gm0$inc.stock.b1
  gm0$inc.stock.r3 <- gm0$inc.stock.h2 + gm0$inc.stock.s2
  
  gm0$inc.stock <- rowSums(gm0[,paste0("inc.stock.", c("c","r2","r3","b","calves"))], na.rm = TRUE)
  gm0$inc.total <- gm0$inc.stock
  
  ## Mid-year adjustment factor
  f <- inp0$open/gm0$su.x
  
  ## Expenses
  gm0$exp.feed <- inp0$exp.graze * gm0$su.x * f#+ inp0$exp.feed * gm0$n.calves.x
  gm0$exp.ah <- inp0$exp.ah * gm0$su.x * f
  gm0$exp.lab <- inp0$exp.lab * gm0$su.x * f
  gm0$exp.stock <- (gm0$buy.h0 * inp0$val.store.h0 + gm0$buy.s0 * inp0$val.store.s0 + gm0$buy.b0 * inp0$val.store.b0) * 0.7
  # gm0$exp.other <- inp0$exp.other
  gm0$exp.opp <- 0
  gm0$exp.purchase <- gm0$exp.stock + gm0$exp.opp
  
  ## Net income
  gm0$inc.stock.net <- gm0$inc.stock - gm0$exp.stock
  gm0$inc.total <- gm0$inc.stock.net
  
  gm0$exp.total <- gm0$exp.feed + gm0$exp.ah + gm0$exp.lab #+ gm0$exp.other
  
  ## Farm level summary
  gm0$profit <- gm0$inc.total - gm0$exp.total
  
  ## ===================================================================================================
  ## Infected herd (combined)
  gm1 <- gm0
  
  ## Infected herd - non-infected component (gm10)
  gm10 <- data.frame(class = fclass)
  
  ## Infected herd - infected component (gm11)
  gm11 <- data.frame(class = fclass)
  
  ## Initial numbers (non-infected)
  gm10$n.c <- gm1$n.c * (1 - inp1$prev)
  gm10$n.h1 <- gm1$n.h1 * (1 - inp1$prev)
  gm10$n.h2 <- gm1$n.h2 * (1 - inp1$prev)
  gm10$n.s1 <- gm1$n.s1 * (1 - inp1$prev)
  gm10$n.s2 <- gm1$n.s2 * (1 - inp1$prev)
  gm10$n.b1 <- gm1$n.b1 * (1 - inp1$prev)
  gm10$n.b <- gm1$n.b * (1 - inp1$prev)
  
  ## Initial numbers (infected)
  gm11$n.c <- gm1$n.c * inp1$prev
  gm11$n.h1 <- gm1$n.h1 * inp1$prev
  gm11$n.h2 <- gm1$n.h2 * inp1$prev
  gm11$n.s1 <- gm1$n.s1 * inp1$prev
  gm11$n.s2 <- gm1$n.s2 * inp1$prev
  gm11$n.b1 <- gm1$n.b1 * inp1$prev
  gm11$n.b <- gm1$n.b * inp1$prev
  
  ## Calves born
  gm11$born.calves <- gm11$n.c * gm0$r.calv * (1 - inp1$r.abort) * (1 - inp1$r.empty) + gm10$n.c * gm0$r.calv * inp1$prev
  gm11$born.h0 <- gm11$born.calves * 0.5
  gm11$born.s0 <- gm11$born.calves * 0.5
  gm11$born.b0 <- 0
  
  gm10$born.calves <- gm10$n.c * gm0$r.calv * (1 - inp1$prev)
  gm10$born.h0 <- gm10$born.calves * 0.5
  gm10$born.s0 <- gm10$born.calves * 0.5
  gm10$born.b0 <- gm10$born.calves * 0
  
  gm1$born.calves <- gm11$born.calves + gm10$born.calves
  gm1$born.h0 <- gm11$born.h0 + gm10$born.h0
  gm1$born.s0 <- gm11$born.s0 + gm10$born.s0
  gm1$born.b0 <- gm11$born.b0 + gm10$born.b0
  
  ## Calves buy
  gm1$buy.h0 <- gm1$n.h0 - gm1$born.h0
  gm1$buy.s0 <- gm1$n.s0 - gm1$born.s0
  gm1$buy.b0 <- gm1$n.b0 - gm1$born.b0
  
  gm10$buy.h0 <- gm1$buy.h0 * (1 - inp1$prev)
  gm10$buy.s0 <- gm1$buy.s0 * (1 - inp1$prev)
  gm10$buy.b0 <- gm1$buy.b0 * (1 - inp1$prev)
  
  gm11$buy.h0 <- gm1$buy.h0 * inp1$prev
  gm11$buy.s0 <- gm1$buy.s0 * inp1$prev
  gm11$buy.b0 <- gm1$buy.b0 * inp1$prev
  
  
  ## Calf numbers infected
  gm11$n.h0 <- gm11$born.h0 + gm11$buy.h0
  gm11$n.s0 <- gm11$born.s0 + gm11$buy.s0
  gm11$n.b0 <- gm11$born.b0 + gm11$buy.b0
  
  gm10$n.h0 <- gm10$born.h0 + gm10$buy.h0
  gm10$n.s0 <- gm10$born.s0 + gm10$buy.s0
  gm10$n.b0 <- gm10$born.b0 + gm10$buy.b0
  
  
  ## Number died (non-infected)
  gm10$die.c <- gm10$n.c *  inp0$r.mort1
  gm10$die.h1 <- gm10$n.h1 *  inp0$r.mort1
  gm10$die.h2 <- gm10$n.h2 *  inp0$r.mort1
  gm10$die.s1 <- gm10$n.s1 *  inp0$r.mort1
  gm10$die.s2 <- gm10$n.s2 *  inp0$r.mort1
  gm10$die.b1 <- gm10$n.b1 *  inp0$r.mort1
  gm10$die.b <- gm10$n.b *  inp0$r.mort1
  gm10$die.h0 <- gm10$n.h0 * inp0$r.mort2
  gm10$die.s0 <- gm10$n.s0 * inp0$r.mort2
  gm10$die.b0 <- gm10$n.b0 * inp0$r.mort2
  
  ## Number died (infected)
  gm11$die.c <- gm11$n.c *  inp1$r.mort1
  gm11$die.h1 <- gm11$n.h1 *  inp1$r.mort1
  gm11$die.h2 <- gm11$n.h2 *  inp1$r.mort1
  gm11$die.s1 <- gm11$n.s1 *  inp1$r.mort1
  gm11$die.s2 <- gm11$n.s2 *  inp1$r.mort1
  gm11$die.b1 <- gm11$n.b1 *  inp1$r.mort1
  gm11$die.b <- gm11$n.b *  inp1$r.mort1
  gm11$die.h0 <- gm11$n.h0 * inp1$r.mort2
  gm11$die.s0 <- gm11$n.s0 * inp1$r.mort2
  gm11$die.b0 <- gm11$n.b0 * inp1$r.mort2
  
  ## Combined
  gm1$die.c <- gm10$die.c + gm11$die.c
  gm1$die.h0 <- gm10$die.h0 + gm11$die.h0
  gm1$die.h1 <- gm10$die.h1 + gm11$die.h1
  gm1$die.h2 <- gm10$die.h2 + gm11$die.h2
  gm1$die.s0 <- gm10$die.s0 + gm11$die.s0
  gm1$die.s1 <- gm10$die.s1 + gm11$die.s1
  gm1$die.s2 <- gm10$die.s2 + gm11$die.s2
  gm1$die.b0 <- gm10$die.b0 + gm11$die.b0
  gm1$die.b1 <- gm10$die.b1 + gm11$die.b1
  gm1$die.b <- gm10$die.b + gm11$die.b
  
  
  ## Number sold (store) (non-infected)
  gm10$sell.c <- gm10$n.c * gm0$r.prod.c * (1-gm0$r.prime.c)
  gm10$sell.h0 <- gm10$n.h0 * gm0$r.prod.h0 * (1-gm0$r.prime.h0)
  gm10$sell.h1 <- gm10$n.h1 * gm0$r.prod.h1 * (1-gm0$r.prime.h1)
  gm10$sell.h2 <- gm10$n.h2 * gm0$r.prod.h2 * (1-gm0$r.prime.h2)
  gm10$sell.s0 <- gm10$n.s0 * gm0$r.prod.s0 * (1-gm0$r.prime.s0)
  gm10$sell.s1 <- gm10$n.s1 * gm0$r.prod.s1 * (1-gm0$r.prime.s1)
  gm10$sell.s2 <- gm10$n.s2 * gm0$r.prod.s2 * (1-gm0$r.prime.s2)
  gm10$sell.b0 <- gm10$n.b0 * gm0$r.prod.b0 * (1-gm0$r.prime.b0)
  gm10$sell.b1 <- gm10$n.b1 * gm0$r.prod.b1 * (1-gm0$r.prime.b1)
  gm10$sell.b <- gm10$n.b * gm0$r.prod.b * (1-gm0$r.prime.b)
  
  ## Number sold (prime) (non-infected)
  gm10$cull.c <- gm10$n.c * gm0$r.prod.c * gm0$r.prime.c
  gm10$cull.h0 <- gm10$n.h0 * gm0$r.prod.h0 * gm0$r.prime.h0
  gm10$cull.h1 <- gm10$n.h1 * gm0$r.prod.h1 * gm0$r.prime.h1
  gm10$cull.h2 <- gm10$n.h2 * gm0$r.prod.h2 * gm0$r.prime.h2
  gm10$cull.s0 <- gm10$n.s0 * gm0$r.prod.s0 * gm0$r.prime.s0
  gm10$cull.s1 <- gm10$n.s1 * gm0$r.prod.s1 * gm0$r.prime.s1
  gm10$cull.s2 <- gm10$n.s2 * gm0$r.prod.s2 * gm0$r.prime.s2
  gm10$cull.b0 <- gm10$n.b0 * gm0$r.prod.b0 * gm0$r.prime.b0
  gm10$cull.b1 <- gm10$n.b1 * gm0$r.prod.b1 * gm0$r.prime.b1
  gm10$cull.b <- gm10$n.b * gm0$r.prod.b * gm0$r.prime.b
  
  ## ===================================================================================================
  
  ## Selling/culling rate (infected)
  gm11$r.prod.c <- apply(cbind(1,gm0$r.prod.c + inp1$r.cull), 1, min)
  gm11$r.prod.h0 <- apply(cbind(1,gm0$r.prod.h0 + inp1$r.cull), 1, min)
  gm11$r.prod.h1 <- apply(cbind(1,gm0$r.prod.h1 + inp1$r.cull), 1, min)
  gm11$r.prod.h2 <- apply(cbind(1,gm0$r.prod.h2 + inp1$r.cull), 1, min)
  gm11$r.prod.s0 <- apply(cbind(1,gm0$r.prod.s0 + inp1$r.cull), 1, min)
  gm11$r.prod.s1 <- apply(cbind(1,gm0$r.prod.s1 + inp1$r.cull), 1, min)
  gm11$r.prod.s2 <- apply(cbind(1,gm0$r.prod.s2 + inp1$r.cull), 1, min)
  gm11$r.prod.b0 <- apply(cbind(1,gm0$r.prod.b0 + inp1$r.cull), 1, min)
  gm11$r.prod.b1 <- apply(cbind(1,gm0$r.prod.b1 + inp1$r.cull), 1, min)
  gm11$r.prod.b <- apply(cbind(1,gm0$r.prod.b + inp1$r.cull), 1, min)
  
  ## Proportion of prime
  gm11$r.prime.c <- gm0$r.prime.c * (1 - inp1$r.under)
  gm11$r.prime.h0 <- gm0$r.prime.h0 * (1 - inp1$r.under)
  gm11$r.prime.h1 <- gm0$r.prime.h1 * (1 - inp1$r.under)
  gm11$r.prime.h2 <- gm0$r.prime.h2 * (1 - inp1$r.under)
  gm11$r.prime.s0 <- gm0$r.prime.s0 * (1 - inp1$r.under)
  gm11$r.prime.s1 <- gm0$r.prime.s1 * (1 - inp1$r.under)
  gm11$r.prime.s2 <- gm0$r.prime.s2 * (1 - inp1$r.under)
  gm11$r.prime.b0 <- gm0$r.prime.b0 * (1 - inp1$r.under)
  gm11$r.prime.b1 <- gm0$r.prime.b1 * (1 - inp1$r.under)
  gm11$r.prime.b <- gm0$r.prime.b * (1 - inp1$r.under)
  
  ## Number sold (store)
  gm11$sell.c <- gm11$n.c * gm11$r.prod.c * (1-gm11$r.prime.c)
  gm11$sell.h0 <- gm11$n.h0 * gm11$r.prod.h0 * (1-gm11$r.prime.h0)
  gm11$sell.h1 <- gm11$n.h1 * gm11$r.prod.h1 * (1-gm11$r.prime.h1)
  gm11$sell.h2 <- gm11$n.h2 * gm11$r.prod.h2 * (1-gm11$r.prime.h2)
  gm11$sell.s0 <- gm11$n.s0 * gm11$r.prod.s0 * (1-gm11$r.prime.s0)
  gm11$sell.s1 <- gm11$n.s1 * gm11$r.prod.s1 * (1-gm11$r.prime.s1)
  gm11$sell.s2 <- gm11$n.s2 * gm11$r.prod.s2 * (1-gm11$r.prime.s2)
  gm11$sell.b0 <- gm11$n.b0 * gm11$r.prod.b0 * (1-gm11$r.prime.b0)
  gm11$sell.b1 <- gm11$n.b1 * gm11$r.prod.b1 * (1-gm11$r.prime.b1)
  gm11$sell.b <- gm11$n.b * gm11$r.prod.b * (1-gm11$r.prime.b)
  
  ## Number sold (prime)
  gm11$cull.c <- gm11$n.c * gm11$r.prod.c * gm11$r.prime.c
  gm11$cull.h0 <- gm11$n.h0 * gm11$r.prod.h0 * gm11$r.prime.h0
  gm11$cull.h1 <- gm11$n.h1 * gm11$r.prod.h1 * gm11$r.prime.h1
  gm11$cull.h2 <- gm11$n.h2 * gm11$r.prod.h2 * gm11$r.prime.h2
  gm11$cull.s0 <- gm11$n.s0 * gm11$r.prod.s0 * gm11$r.prime.s0
  gm11$cull.s1 <- gm11$n.s1 * gm11$r.prod.s1 * gm11$r.prime.s1
  gm11$cull.s2 <- gm11$n.s2 * gm11$r.prod.s2 * gm11$r.prime.s2
  gm11$cull.b0 <- gm11$n.b0 * gm11$r.prod.b0 * gm11$r.prime.b0
  gm11$cull.b1 <- gm11$n.b1 * gm11$r.prod.b1 * gm11$r.prime.b1
  gm11$cull.b <- gm11$n.b * gm11$r.prod.b * gm11$r.prime.b
  
  
  ## Combined
  gm1$sell.c <- gm10$sell.c + gm11$sell.c
  gm1$sell.h0 <- gm10$sell.h0 + gm11$sell.h0
  gm1$sell.h1 <- gm10$sell.h1 + gm11$sell.h1
  gm1$sell.h2 <- gm10$sell.h2 + gm11$sell.h2
  gm1$sell.s0 <- gm10$sell.s0 + gm11$sell.s0
  gm1$sell.s1 <- gm10$sell.s1 + gm11$sell.s1
  gm1$sell.s2 <- gm10$sell.s2 + gm11$sell.s2
  gm1$sell.b0 <- gm10$sell.b0 + gm11$sell.b0
  gm1$sell.b1 <- gm10$sell.b1 + gm11$sell.b1
  gm1$sell.b <- gm10$sell.b + gm11$sell.b
  
  gm1$cull.c <- gm10$cull.c + gm11$cull.c
  gm1$cull.h0 <- gm10$cull.h0 + gm11$cull.h0
  gm1$cull.h1 <- gm10$cull.h1 + gm11$cull.h1
  gm1$cull.h2 <- gm10$cull.h2 + gm11$cull.h2
  gm1$cull.s0 <- gm10$cull.s0 + gm11$cull.s0
  gm1$cull.s1 <- gm10$cull.s1 + gm11$cull.s1
  gm1$cull.s2 <- gm10$cull.s2 + gm11$cull.s2
  gm1$cull.b0 <- gm10$cull.b0 + gm11$cull.b0
  gm1$cull.b1 <- gm10$cull.b1 + gm11$cull.b1
  gm1$cull.b <- gm10$cull.b + gm11$cull.b
  ## ===================================================================================================
  
  ## Season average number
  gm10$n.c.x <- gm10$n.c - (gm10$die.c + gm10$cull.c + gm10$sell.c)/2
  gm10$n.h0.x <- gm10$n.h0 - (gm10$die.h0 + gm10$cull.h0 + gm10$sell.h0)/2
  gm10$n.h1.x <- gm10$n.h1 - (gm10$die.h1 + gm10$cull.h1 + gm10$sell.h1)/2
  gm10$n.h2.x <- gm10$n.h2 - (gm10$die.h2 + gm10$cull.h2 + gm10$sell.h2)/2
  gm10$n.s0.x <- gm10$n.s0 - (gm10$die.s0 + gm10$cull.s0 + gm10$sell.s0)/2
  gm10$n.s1.x <- gm10$n.s1 - (gm10$die.s1 + gm10$cull.s1 + gm10$sell.s1)/2
  gm10$n.s2.x <- gm10$n.s2 - (gm10$die.s2 + gm10$cull.s2 + gm10$sell.s2)/2
  gm10$n.b0.x <- gm10$n.b0 - (gm10$die.b0 + gm10$cull.b0 + gm10$sell.b0)/2
  gm10$n.b1.x <- gm10$n.b1 - (gm10$die.b1 + gm10$cull.b1 + gm10$sell.b1)/2
  gm10$n.b.x <- gm10$n.b - (gm10$die.b + gm10$cull.b + gm10$sell.b)/2
  gm10$n.calves.x <- gm10$n.h0.x + gm10$n.s0.x + gm10$n.b0.x
  gm10$su.x <- gm10$n.c.x * 5.5 + gm10$n.h2.x * 5.5 + gm10$n.h1.x * 4.5 + gm10$n.h0.x * 3.5 +
    gm10$n.s2.x * 5.5 + gm10$n.s1.x * 5 + gm10$n.s0.x * 4.5 + 
    gm10$n.b.x * 5.5 + gm10$n.b1.x * 5.5 + gm10$n.b0.x * 4.5
  
  gm11$n.c.x <- gm11$n.c - (gm11$die.c + gm11$cull.c + gm11$sell.c)/2
  gm11$n.h0.x <- gm11$n.h0 - (gm11$die.h0 + gm11$cull.h0 + gm11$sell.h0)/2
  gm11$n.h1.x <- gm11$n.h1 - (gm11$die.h1 + gm11$cull.h1 + gm11$sell.h1)/2
  gm11$n.h2.x <- gm11$n.h2 - (gm11$die.h2 + gm11$cull.h2 + gm11$sell.h2)/2
  gm11$n.s0.x <- gm11$n.s0 - (gm11$die.s0 + gm11$cull.s0 + gm11$sell.s0)/2
  gm11$n.s1.x <- gm11$n.s1 - (gm11$die.s1 + gm11$cull.s1 + gm11$sell.s1)/2
  gm11$n.s2.x <- gm11$n.s2 - (gm11$die.s2 + gm11$cull.s2 + gm11$sell.s2)/2
  gm11$n.b0.x <- gm11$n.b0 - (gm11$die.b0 + gm11$cull.b0 + gm11$sell.b0)/2
  gm11$n.b1.x <- gm11$n.b1 - (gm11$die.b1 + gm11$cull.b1 + gm11$sell.b1)/2
  gm11$n.b.x <- gm11$n.b - (gm11$die.b + gm11$cull.b + gm11$sell.b)/2
  gm11$n.calves.x <- gm11$n.h0.x + gm11$n.s0.x + gm11$n.b0.x
  gm11$su.x <- gm11$n.c.x * 5.5 + gm11$n.h2.x * 5.5 + gm11$n.h1.x * 4.5 + gm11$n.h0.x * 3.5 +
    gm11$n.s2.x * 5.5 + gm11$n.s1.x * 5 + gm11$n.s0.x * 4.5 + 
    gm11$n.b.x * 5.5 + gm11$n.b1.x * 5.5 + gm11$n.b0.x * 4.5
  
  gm1$n.c.x <- gm10$n.c.x + gm11$n.c.x
  gm1$n.h0.x <- gm10$n.h0.x + gm11$n.h0.x
  gm1$n.h1.x <- gm10$n.h1.x + gm11$n.h1.x
  gm1$n.h2.x <- gm10$n.h2.x + gm11$n.h2.x
  gm1$n.s0.x <- gm10$n.s0.x + gm11$n.s0.x
  gm1$n.s1.x <- gm10$n.s1.x + gm11$n.s1.x
  gm1$n.s2.x <- gm10$n.s2.x + gm11$n.s2.x
  gm1$n.b0.x <- gm10$n.b0.x + gm11$n.b0.x
  gm1$n.b1.x <- gm10$n.b1.x + gm11$n.b1.x
  gm1$n.b.x <- gm10$n.b.x + gm11$n.b.x
  gm1$n.calves.x <- gm1$n.h0.x + gm1$n.s0.x + gm1$n.b0.x
  gm1$su.x <- gm1$n.c.x * 5.5 + gm1$n.h2.x * 5.5 + gm1$n.h1.x * 4.5 + gm1$n.h0.x * 3.5 +
    gm1$n.s2.x * 5.5 + gm1$n.s1.x * 5 + gm1$n.s0.x * 4.5 + 
    gm1$n.b.x * 5.5 + gm1$n.b1.x * 5.5 + gm1$n.b0.x * 4.5
  
  ## Closing number
  gm1$n.c. <- gm1$n.c - (gm1$die.c + gm1$cull.c + gm1$sell.c)
  gm1$n.h0. <- gm1$n.h0 - (gm1$die.h0 + gm1$cull.h0 + gm1$sell.h0)
  gm1$n.h1. <- gm1$n.h1 - (gm1$die.h1 + gm1$cull.h1 + gm1$sell.h1)
  gm1$n.h2. <- gm1$n.h2 - (gm1$die.h2 + gm1$cull.h2 + gm1$sell.h2)
  gm1$n.s0. <- gm1$n.s0 - (gm1$die.s0 + gm1$cull.s0 + gm1$sell.s0)
  gm1$n.s1. <- gm1$n.s1 - (gm1$die.s1 + gm1$cull.s1 + gm1$sell.s1)
  gm1$n.s2. <- gm1$n.s2 - (gm1$die.s2 + gm1$cull.s2 + gm1$sell.s2)
  gm1$n.b0. <- gm1$n.b0 - (gm1$die.b0 + gm1$cull.b0 + gm1$sell.b0)
  gm1$n.b1. <- gm1$n.b1 - (gm1$die.b1 + gm1$cull.b1 + gm1$sell.b1)
  gm1$n.b. <- gm1$n.b - (gm1$die.b + gm1$cull.b + gm1$sell.b)
  gm1$n.calves. <- gm1$n.h0. + gm1$n.s0. + gm1$n.b0.
  
  ## Income
  gm10$inc.stock.c <- rowSums(cbind(inp0$val.prime.c * gm10$cull.c, inp0$val.store.c * gm10$sell.c), na.rm = TRUE)
  gm10$inc.stock.h0 <- rowSums(cbind(inp0$val.prime.h0 * gm10$cull.h0, inp0$val.store.h0 * gm10$sell.h0), na.rm = TRUE)
  gm10$inc.stock.h1 <- rowSums(cbind(inp0$val.prime.h1 * gm10$cull.h1, inp0$val.store.h1 * gm10$sell.h1), na.rm = TRUE)
  gm10$inc.stock.h2 <- rowSums(cbind(inp0$val.prime.h2 * gm10$cull.h2, inp0$val.store.h2 * gm10$sell.h2), na.rm = TRUE)
  gm10$inc.stock.s0 <- rowSums(cbind(inp0$val.prime.s0 * gm10$cull.s0, inp0$val.store.s0 * gm10$sell.s0), na.rm = TRUE)
  gm10$inc.stock.s1 <- rowSums(cbind(inp0$val.prime.s1 * gm10$cull.s1, inp0$val.store.s1 * gm10$sell.s1), na.rm = TRUE)
  gm10$inc.stock.s2 <- rowSums(cbind(inp0$val.prime.s2 * gm10$cull.s2, inp0$val.store.s2 * gm10$sell.s2), na.rm = TRUE)
  gm10$inc.stock.b <- rowSums(cbind(inp0$val.prime.b * gm10$cull.b, inp0$val.store.b * gm10$sell.b), na.rm = TRUE)
  gm10$inc.stock.b0 <- rowSums(cbind(inp0$val.prime.b0 * gm10$cull.b0, inp0$val.store.b0 * gm10$sell.b0), na.rm = TRUE)
  gm10$inc.stock.b1 <- rowSums(cbind(inp0$val.prime.b1 * gm10$cull.b1, inp0$val.store.b1 * gm10$sell.b1), na.rm = TRUE)
  
  # gm10$inc.stock.h <- gm10$inc.stock.h1 + gm10$inc.stock.h2
  # gm10$inc.stock.s <- gm10$inc.stock.s1 + gm10$inc.stock.s2
  # gm10$inc.stock.b <- gm10$inc.stock.b1 + gm10$inc.stock.b
  gm10$inc.stock.calves <- gm10$inc.stock.h0 + gm10$inc.stock.s0 + gm10$inc.stock.b0
  gm10$inc.stock.r2 <- gm10$inc.stock.h1 + gm10$inc.stock.s1 + gm10$inc.stock.b1
  gm10$inc.stock.r3 <- gm10$inc.stock.h2 + gm10$inc.stock.s2
  
  gm10$inc.stock <- rowSums(gm10[,paste0("inc.stock.", c("c","r2","r3","b","calves"))], na.rm = TRUE)
  # gm10$inc.total <- gm10$inc.stock
  
  
  gm11$inc.stock.c <- rowSums(cbind(inp1$val.prime.c * gm11$cull.c, inp1$val.store.c * gm11$sell.c), na.rm = TRUE)
  gm11$inc.stock.h0 <- rowSums(cbind(inp1$val.prime.h0 * gm11$cull.h0, inp1$val.store.h0 * gm11$sell.h0), na.rm = TRUE)
  gm11$inc.stock.h1 <- rowSums(cbind(inp1$val.prime.h1 * gm11$cull.h1, inp1$val.store.h1 * gm11$sell.h1), na.rm = TRUE)
  gm11$inc.stock.h2 <- rowSums(cbind(inp1$val.prime.h2 * gm11$cull.h2, inp1$val.store.h2 * gm11$sell.h2), na.rm = TRUE)
  gm11$inc.stock.s0 <- rowSums(cbind(inp1$val.prime.s0 * gm11$cull.s0, inp1$val.store.s0 * gm11$sell.s0), na.rm = TRUE)
  gm11$inc.stock.s1 <- rowSums(cbind(inp1$val.prime.s1 * gm11$cull.s1, inp1$val.store.s1 * gm11$sell.s1), na.rm = TRUE)
  gm11$inc.stock.s2 <- rowSums(cbind(inp1$val.prime.s2 * gm11$cull.s2, inp1$val.store.s2 * gm11$sell.s2), na.rm = TRUE)
  gm11$inc.stock.b <- rowSums(cbind(inp1$val.prime.b * gm11$cull.b, inp1$val.store.b * gm11$sell.b), na.rm = TRUE)
  gm11$inc.stock.b0 <- rowSums(cbind(inp1$val.prime.b0 * gm11$cull.b0, inp1$val.store.b0 * gm11$sell.b0), na.rm = TRUE)
  gm11$inc.stock.b1 <- rowSums(cbind(inp1$val.prime.b1 * gm11$cull.b1, inp1$val.store.b1 * gm11$sell.b1), na.rm = TRUE)
  
  # gm11$inc.stock.h <- gm11$inc.stock.h1 + gm11$inc.stock.h2
  # gm11$inc.stock.s <- gm11$inc.stock.s1 + gm11$inc.stock.s2
  # gm11$inc.stock.b <- gm11$inc.stock.b1 + gm11$inc.stock.b
  gm11$inc.stock.calves <- gm11$inc.stock.h0 + gm11$inc.stock.s0 + gm11$inc.stock.b0
  gm11$inc.stock.r2 <- gm11$inc.stock.h1 + gm11$inc.stock.s1 + gm11$inc.stock.b1
  gm11$inc.stock.r3 <- gm11$inc.stock.h2 + gm11$inc.stock.s2
  
  gm11$inc.stock <- rowSums(gm11[,paste0("inc.stock.", c("c","r2","r3","b","calves"))], na.rm = TRUE)
  # gm11$inc.total <- gm11$inc.stock
  
  ## Expenses
  gm10$exp.feed <- inp0$exp.graze * gm10$su.x * f# + inp0$exp.feed * gm10$n.calves.x
  gm10$exp.ah <- inp0$exp.ah * gm10$su.x * f
  gm10$exp.lab <- inp0$exp.lab * gm10$su.x * f
  gm10$exp.stock <- (gm10$buy.h0 * inp0$val.store.h0 + gm10$buy.s0 * inp0$val.store.s0 + gm10$buy.b0 * inp0$val.store.b0) * 0.7
  gm10$exp.opp <- 0
  gm10$exp.total <- gm10$exp.feed + gm10$exp.ah + gm10$exp.lab + gm10$exp.stock + gm10$exp.opp
  
  gm11$exp.feed <- inp1$exp.graze * gm11$su.x * f# + inp1$exp.feed * gm11$n.calves.x
  gm11$exp.ah <- inp0$exp.ah * gm11$su.x * f + inp1$exp.ah.extra * rowSums(gm11[,grep("^n\\..$|^n\\..[0-2]$", names(gm11), value = TRUE)])
  gm11$exp.lab <- inp1$exp.lab * gm11$su.x * f
  gm11$exp.stock <- (gm11$buy.h0 * inp0$val.store.h0 + gm11$buy.s0 * inp0$val.store.s0 + gm11$buy.b0 * inp0$val.store.b0) * 0.7
  # gm11$exp.total <- gm11$exp.feed + gm11$exp.ah + gm11$exp.lab + gm11$exp.stock + gm11$exp.opp
  
  ## Farm level summary
  gm1$inc.stock.c <- gm10$inc.stock.c + gm11$inc.stock.c
  # gm1$inc.stock.h <- gm10$inc.stock.h + gm11$inc.stock.h
  # gm1$inc.stock.s <- gm10$inc.stock.s + gm11$inc.stock.s
  gm1$inc.stock.b <- gm10$inc.stock.b + gm11$inc.stock.b
  gm1$inc.stock.calves <- gm10$inc.stock.calves + gm11$inc.stock.calves
  gm1$inc.stock.r2 <- gm10$inc.stock.r2 + gm11$inc.stock.r2
  gm1$inc.stock.r3 <- gm10$inc.stock.r3 + gm11$inc.stock.r3
  
  gm1$inc.stock <- gm10$inc.stock + gm11$inc.stock
  
  gm1$exp.feed <- gm10$exp.feed + gm11$exp.feed
  gm1$exp.ah <- gm10$exp.ah + gm11$exp.ah
  gm1$exp.lab <- gm10$exp.lab + gm11$exp.lab
  gm1$exp.stock <- gm10$exp.stock + gm11$exp.stock
  # gm1$exp.opp <- gm10$exp.opp + gm11$exp.opp
  gm1$exp.opp.c <- inp0$val.store.c * (gm0$n.c. - gm1$n.c.)
  gm1$exp.opp.h1 <- inp0$val.prime.h1 * (gm0$n.h1. - gm1$n.h1.)
  gm1$exp.opp.h2 <- inp0$val.prime.h2 * (gm0$n.h2. - gm1$n.h2.)
  gm1$exp.opp.s1 <- inp0$val.prime.s1 * (gm0$n.s1. - gm1$n.s1.)
  gm1$exp.opp.s2 <- inp0$val.prime.s2 * (gm0$n.s2. - gm1$n.s2.)
  gm1$exp.opp.b1 <- inp0$val.store.b1 * (gm0$n.b1. - gm1$n.b1.)
  gm1$exp.opp.h0 <- inp0$val.store.h0 * (gm0$n.h0. - gm1$n.h0.)
  gm1$exp.opp.s0 <- inp0$val.store.s0 * (gm0$n.s0. - gm1$n.s0.)
  gm1$exp.opp.b0 <- inp0$val.store.b0 * (gm0$n.b0. - gm1$n.b0.)
  gm1$exp.opp.b <- inp0$val.store.b1 * (gm0$n.b. - gm1$n.b.)
  gm1$exp.opp <- gm1$exp.opp.c + gm1$exp.opp.h0 + gm1$exp.opp.h1 + gm1$exp.opp.h2 +
    gm1$exp.opp.s0 + gm1$exp.opp.s1 + gm1$exp.opp.s2 +
    gm1$exp.opp.b0 + gm1$exp.opp.b1 + gm1$exp.opp.b
  
  # gm1$exp.other <- inp0$exp.other
  gm1$exp.purchase <- gm1$exp.stock + gm1$exp.opp
  
  
  gm1$inc.stock.net <- gm1$inc.stock - gm1$exp.purchase
  gm1$inc.total <- gm1$inc.stock.net
  
  
  gm1$exp.total <- gm1$exp.feed + gm1$exp.ah + gm1$exp.lab #+ gm1$exp.other
  
  gm1$profit <- gm1$inc.total - gm1$exp.total
  
  ## ===================================================================================================
  ## Partial budget
  pb <- data.frame(class = fclass)
  pb$profit <- gm1$profit - gm0$profit
  pb$profit.pct <- 100 * pb$profit/gm0$profit
  
  pb$inc.total <- gm1$inc.total - gm0$inc.total
  pb$inc.stock <- gm1$inc.stock - gm0$inc.stock
  pb$inc.stock.net <- gm1$inc.stock.net - gm0$inc.stock.net
  pb$inc.stock.c <- gm1$inc.stock.c - gm0$inc.stock.c
  # pb$inc.stock.h <- gm1$inc.stock.h - gm0$inc.stock.h
  # pb$inc.stock.s <- gm1$inc.stock.s - gm0$inc.stock.s
  pb$inc.stock.b <- gm1$inc.stock.b - gm0$inc.stock.b
  pb$inc.stock.r2 <- gm1$inc.stock.r2 - gm0$inc.stock.r3
  pb$inc.stock.r3 <- gm1$inc.stock.r2 - gm0$inc.stock.r3
  pb$inc.stock.calves <- gm1$inc.stock.calves - gm0$inc.stock.calves
  
  pb$exp.feed <- gm1$exp.feed - gm0$exp.feed
  pb$exp.ah <- gm1$exp.ah - gm0$exp.ah
  pb$exp.lab <- gm1$exp.lab - gm0$exp.lab
  pb$exp.stock <- gm1$exp.stock - gm0$exp.stock
  pb$exp.opp.c <- gm1$exp.opp.c
  # pb$exp.opp.h <- gm1$exp.opp.h
  # pb$exp.opp.s <- gm1$exp.opp.s
  pb$exp.opp.r2 <- gm1$exp.opp.r2
  pb$exp.opp.r3 <- gm1$exp.opp.r3
  pb$exp.opp.b <- gm1$exp.opp.b
  pb$exp.opp.calves <- gm1$exp.opp.calves
  pb$exp.opp <- gm1$exp.opp
  pb$exp.purchase <- gm1$exp.purchase - gm0$exp.purchase
  pb$exp.total <- gm1$exp.total - gm0$exp.total
  
  pb$inc.total.pct <- 100 * pb$inc.total/gm0$inc.total
  pb$inc.stock.pct <- 100 * pb$inc.stock/gm0$inc.stock
  pb$inc.stock.net.pct <- 100 * pb$inc.stock.net/gm0$inc.stock.net
  pb$inc.stock.c.pct <- 100 * pb$inc.stock.c/gm0$inc.stock.c
  pb$inc.stock.r2.pct <- 100 * pb$inc.stock.r2/gm0$inc.stock.r2
  pb$inc.stock.r3.pct <- 100 * pb$inc.stock.r3/gm0$inc.stock.r3
  pb$inc.stock.calves.pct <- 100 * pb$inc.stock.calves/gm0$inc.stock.calves
  
  pb$exp.feed.pct <- 100 * pb$exp.feed/gm0$exp.feed
  pb$exp.ah.pct <- 100 * pb$exp.ah/gm0$exp.ah
  pb$exp.lab.pct <- 100 * pb$exp.lab/gm0$exp.lab
  pb$exp.stock.pct <- 100 * pb$exp.stock/gm0$exp.stock
  pb$exp.purchase.pct <- 100 * pb$exp.purchase/gm0$exp.purchase
  pb$exp.total.pct <- 100 * pb$exp.total/gm0$exp.total
  
  ## Reference
  pb$diff.inc <- gm0$inc.total - inp0$ref.inc
  pb$diff.exp <- gm0$exp.total - inp0$ref.exp
  pb$diff.profit <- gm0$profit - inp0$ref.profit
  
  pb$error.inc <- 100 * (gm0$inc.total - inp0$ref.inc)/inp0$ref.inc
  pb$error.exp <- 100 * (gm0$exp.total - inp0$ref.exp)/inp0$ref.exp
  pb$error.profit <- 100 * (gm0$profit - inp0$ref.profit)/inp0$ref.profit
  
  pb$unit.loss <- pb$profit/gm0$n.c
  
  out <- list(gm0, gm1, pb)
  names(out) <- c("Non-infected farm", "Infected farm", "Partial Budget")
  return(out)
  
}

input_sheep <- function(par1, par2){
  
  par1[is.na(par1)] <- 0
  par2[is.na(par2)] <- 0
  
  par2 <- t(par2[,4:20])
  
  ## Iteration number (stochastic)
  nit <- 1
  
  ## Sheep parameters
  inp0 <- data.frame(
    
    
    hsize = par2[,2],
    su = par2[,1],
    n.lamb = par2[,3],
    
    # r.repop = par2[,7],
    # r.empty = par2[,5],
    # r.abort = par2[,6],
    r.wool = par2[,8],
    r.lamb.ewe = par2[,4]/100,
    r.lamb.hgt = par2[,24],
    p.hgtlamb = par2[,5]/100,
    
    r.mort1 = par2[,7]/100,
    r.mort2 = par2[,6]/100,
    
    
    n.cull.ewe = par2[,16],
    n.cull.hgt = par2[,14],
    
    n.prime.lamb = par2[,27],
    n.store.lamb = par2[,28],
    p.prime.lamb = par2[,27]/(par2[,27] + par2[,28]),
    
    val.wool = par2[,9],
    
    val.store.hgt = par2[,18],
    val.store.lamb = par2[,13],
    
    val.prime.ewe = par2[,17],
    val.prime.hgt = par2[,15],
    val.prime.lamb = par2[,11],
    
    val.avg.ram = par2[,19],
    
    exp.shear = par2[,10]/100,
    
    exp.lab = par2[,20],
    
    exp.ah = par2[,21],
    
    exp.feed = par2[,23],
    
    exp.other = par2[,22],
    
    ref.inc = par2[,25],
    ref.exp = par2[,29],
    ref.profit = par2[,25] - par2[,29]
    
  )
  
  ## Hogget mortality
  inp0$r.mort3 <- (inp0$r.mort1 + inp0$r.mort2)/2
  
  ## Disease scenario
  inp1 <- inp0
  
  inp1$prev <- par1[1,"value"]/100
  
  inp1$r.mort1 <- inp0$r.mort1 + par1[2,"value"]/100
  inp1$r.mort2 <- inp0$r.mort2 + par1[3,"value"]/100
  inp1$r.mort3 <- par1[2,"value"]/100 + (inp0$r.mort1 + inp0$r.mort2)/2
  
  inp1$r.abort <- par1[4,"value"]/100 # no baseline rate
  inp1$r.empty <- par1[5,"value"]/100 # no baseline rate
  
  inp1$r.lamb.ewe <- inp0$r.lamb.ewe * (1 - inp1$r.abort) * (1 - inp1$r.empty)
  inp1$r.lamb.hgt <- inp0$r.lamb.hgt * (1 - inp1$r.abort) * (1 - inp1$r.empty)
  
  inp1$r.cull <- par1[8,"value"]/100 # no baseline rate
  inp1$r.under <- par1[9,"value"]/100 # no baseline rate
  
  inp1$p.prime.lamb <- inp0$p.prime.lamb * (1 - inp1$r.under)
  
  inp1$val.store.hgt <- inp1$val.store.hgt * (1 - par1[7,"value"]/100)
  inp1$val.store.lamb <- inp1$val.store.lamb * (1 - par1[7,"value"]/100)
  
  inp1$val.prime.ewe <- inp1$val.prime.ewe * (1 - par1[7,"value"]/100)
  inp1$val.prime.hgt <- inp1$val.prime.hgt * (1 - par1[7,"value"]/100)
  inp1$val.prime.lamb <- inp1$val.prime.lamb * (1 - par1[7,"value"]/100)
  
  inp1$exp.ah.extra <- par1[11,"value"] * par1[10,"value"]/100
  
  list(inp0, inp1) 
}

calc_sheep <- function(inp){
  
  inp0 <- inp[[1]]
  inp1 <- inp[[2]]
  
  fclass <- factor(row.names(inp0), levels = row.names(inp0))
  
  ## ===================================================================================================
  ## Baseline herd (no disease)
  gm0 <- data.frame(class = fclass)
  
  ## Initial number, death
  ## ewe = 1 SU, hogget = 0.7 SU
  gm0$n.ewe <- inp0$hsize
  gm0$n.hgt <- (inp0$su - (gm0$n.ewe * 1))/0.7
  gm0$n.lamb <- inp0$n.lamb
  gm0$su <- gm0$n.ewe
  
  
  ## Death
  gm0$die.ewe <- gm0$n.ewe * inp0$r.mort1
  
  ## Cull (prime)
  gm0$cull.ewe <- inp0$n.cull.ewe
  gm0$cull.hgt <- inp0$n.cull.hgt
  gm0$cull.lamb <- inp0$n.prime.lamb
  
  ## Sell (store)
  gm0$sell.ewe <- 0
  gm0$sell.hgt <- 0
  gm0$sell.lamb <- inp0$n.store.lamb
  
  ## Replacement ewe lambs (minimum necessary & actual number of replacement)
  gm0$n.lamb.ewe <- gm0$n.lamb * 0.5
  gm0$n.lamb.ewe.min <- gm0$n.hgt/(1 - inp0$r.mort2)
  gm0$n.lamb.ewe.dif <- gm0$n.lamb.ewe - gm0$n.lamb.ewe.min
  gm0$n.lamb.ewe.rep <- ifelse(gm0$n.lamb.ewe.dif > 0, gm0$n.lamb.ewe.min, gm0$n.lamb.ewe)
  gm0$n.lamb.meat <- (gm0$cull.lamb + gm0$sell.lamb)/(1 - inp0$r.mort2)
  gm0$n.lamb.spl <- gm0$n.lamb - gm0$n.lamb.ewe.rep - gm0$n.lamb.meat
  
  ## Hogget numbers
  gm0$n.hgt.mated <- (gm0$n.lamb * inp0$p.hgtlamb)/inp0$r.lamb.hgt
  # gm0$p.hgt.mated <- gm0$n.hgt.mated/gm0$n.hgt
  gm0$n.hgt.rep <- (gm0$cull.ewe + gm0$die.ewe) / (1 - inp0$r.mort3)
  gm0$n.hgt.adj <- gm0$n.hgt.rep + gm0$cull.hgt/(1 - inp0$r.mort3)
  gm0$n.ewe.spl <- gm0$n.hgt - gm0$n.hgt.adj
  
  
  ## Buy lambs (at the start of the season)
  ## Buying 1 breeding ram per 100 ewes to mate
  gm0$buy.lamb.rep <- ifelse(gm0$n.lamb.ewe.dif < 0, -gm0$n.lamb.ewe.dif, 0)
  gm0$buy.lamb.meat <- ifelse(gm0$n.lamb.spl < 0, -gm0$n.lamb.spl, 0)
  gm0$buy.hgt <- ifelse(gm0$n.ewe.spl<0,-gm0$n.ewe.spl,0)
  gm0$buy.ram <- (gm0$n.ewe + gm0$n.hgt.mated)/100
  
  
  gm0$n.lamb.meat.adj <- gm0$n.lamb/2 + gm0$buy.lamb.meat + ifelse(gm0$n.lamb.ewe.dif < 0, 0, gm0$n.lamb.ewe.dif)
  gm0$p.cull.ewe <- gm0$cull.ewe/gm0$n.ewe
  gm0$p.cull.hgt <- gm0$cull.hgt/(gm0$n.hgt + gm0$buy.hgt)
  
  
  ## Death
  # gm0$die.ewe <- gm0$n.ewe * inp0$r.mort1
  gm0$die.hgt <- (gm0$n.hgt + gm0$buy.hgt) * inp0$r.mort3
  gm0$die.lamb.rep <- (gm0$n.lamb.ewe.rep + gm0$buy.lamb.rep) * inp0$r.mort2
  gm0$die.lamb.meat <- gm0$n.lamb.meat.adj * inp0$r.mort2
  gm0$die.lamb <- gm0$die.lamb.rep + gm0$die.lamb.meat
  
  ## Closing number
  gm0$n.ewe. <- gm0$n.ewe - gm0$die.ewe - gm0$sell.ewe - gm0$cull.ewe
  gm0$n.hgt. <- gm0$n.hgt + gm0$buy.hgt - gm0$die.hgt - gm0$sell.hgt - gm0$cull.hgt
  gm0$n.lamb.rep. <- gm0$n.lamb.ewe.rep + gm0$buy.lamb.rep - gm0$die.lamb.rep
  gm0$n.lamb.meat. <- gm0$n.lamb.meat.adj - gm0$die.lamb.meat - gm0$cull.lamb - gm0$sell.lamb
  
  ## Season average number
  gm0$n.ewe.x <- (gm0$n.ewe + gm0$n.ewe.)/2
  gm0$n.hgt.x <- gm0$n.hgt + gm0$buy.hgt - (gm0$die.hgt + gm0$sell.hgt + gm0$cull.hgt)/2
  gm0$n.lamb.x <- gm0$n.lamb + gm0$buy.lamb.rep + gm0$buy.lamb.meat - (gm0$die.lamb + gm0$sell.lamb + gm0$cull.lamb)/2
  gm0$su.x <- gm0$n.ewe.x * 1 + gm0$n.hgt.x * 0.7 + gm0$n.lamb.x * 0.7
  
  ## Wool production
  gm0$wool <- inp0$r.wool * (gm0$n.ewe.x + gm0$n.hgt.x)
  
  ## Income
  gm0$inc.stock.ewe <- rowSums(cbind(inp0$val.prime.ewe * gm0$cull.ewe, inp0$val.store.ewe * gm0$sell.ewe), na.rm = TRUE)
  gm0$inc.stock.hgt <- rowSums(cbind(inp0$val.prime.hgt * gm0$cull.hgt, inp0$val.store.hgt * gm0$sell.hgt), na.rm = TRUE)
  gm0$inc.stock.lamb <- rowSums(cbind(inp0$val.prime.lamb * gm0$cull.lamb, inp0$val.store.lamb * gm0$sell.lamb), na.rm = TRUE)
  
  gm0$inc.stock <- rowSums(gm0[,paste0("inc.stock.", c("ewe","hgt","lamb"))], na.rm = TRUE)
  
  gm0$inc.wool <- gm0$wool * inp0$val.wool/100
  
  
  ## Mid year average modification factor
  f <- inp0$su / gm0$su.x
  
  
  ## Expenses
  gm0$exp.wool <- gm0$su.x * inp0$exp.shear * f
  gm0$exp.ah <- inp0$exp.ah * gm0$su.x * f
  gm0$exp.feed <- inp0$exp.feed * gm0$su.x * f
  gm0$exp.lab <- inp0$exp.lab * gm0$su.x * f
  gm0$exp.stock <- (gm0$buy.lamb.rep + gm0$buy.lamb.meat) * inp0$val.store.lamb + gm0$buy.hgt * inp0$val.store.hgt + gm0$buy.ram * inp0$val.avg.ram
  # gm0$exp.other <- inp0$exp.other
  gm0$exp.opp <- 0
  gm0$exp.purchase <- gm0$exp.stock + gm0$exp.opp
  gm0$exp.total <- gm0$exp.wool + gm0$exp.ah + gm0$exp.feed + gm0$exp.lab #+ gm0$exp.other
  
  gm0$inc.stock.net <- gm0$inc.stock - gm0$exp.purchase
  gm0$inc.total <- gm0$inc.stock.net + gm0$inc.wool
  
  ## Farm level summary
  gm0$profit <- gm0$inc.total - gm0$exp.total
  
  ## ===================================================================================================
  ## Infected herd (combined)
  gm1 <- gm0
  
  ## Infected herd - non-infected component (gm10)
  gm10 <- data.frame(class = fclass)
  
  ## Infected herd - infected component (gm11)
  gm11 <- data.frame(class = fclass)
  
  ## Initial numbers (non-infected)
  gm10$n.ewe <- gm1$n.ewe * (1 - inp1$prev)
  gm10$n.hgt <- gm1$n.hgt * (1 - inp1$prev)
  
  ## Initial numbers (infected)
  gm11$n.ewe <- gm1$n.ewe * inp1$prev
  gm11$n.hgt <- gm1$n.hgt * inp1$prev
  
  
  gm10$n.hgt.mated <- gm10$n.hgt * gm0$n.hgt.mated/gm0$n.hgt
  gm11$n.hgt.mated <- gm11$n.hgt * gm0$n.hgt.mated/gm0$n.hgt * (1 - inp1$r.under)
  gm1$n.hgt.mated <- gm10$n.hgt.mated + gm11$n.hgt.mated
  
  ## Lamb born
  gm11$n.lamb <- (gm11$n.ewe * inp1$r.lamb.ewe) + (gm11$n.hgt.mated * inp1$r.lamb.hgt) +
    (gm10$n.ewe * inp0$r.lamb.ewe * inp1$prev) +
    (gm10$n.hgt.mated * inp0$r.lamb.hgt * inp1$prev)
  
  gm10$n.lamb <- (gm10$n.ewe * inp0$r.lamb.ewe + gm10$n.hgt.mated * inp0$r.lamb.hgt) * (1 - inp1$prev)
  gm1$n.lamb <- gm11$n.lamb + gm10$n.lamb
  
  ## Death
  gm10$die.ewe <- gm10$n.ewe * inp0$r.mort1
  gm11$die.ewe <- gm11$n.ewe * inp1$r.mort1
  gm1$die.ewe <- gm10$die.ewe + gm11$die.ewe
  
  
  ## Replacement ewe lambs (minimum necessary & actual number of replacement)
  gm10$n.lamb.ewe <- gm10$n.lamb * 0.5
  gm11$n.lamb.ewe <- gm11$n.lamb * 0.5
  gm1$n.lamb.ewe <- gm10$n.lamb.ewe + gm11$n.lamb.ewe
  
  gm1$n.lamb.ewe.dif <- gm1$n.lamb.ewe - gm1$n.lamb.ewe.min
  gm1$n.lamb.ewe.rep <- ifelse(gm1$n.lamb.ewe.dif > 0, gm1$n.lamb.ewe.min, gm1$n.lamb.ewe)
  gm1$n.lamb.spl <- gm1$n.lamb - gm1$n.lamb.ewe.rep - gm1$n.lamb.meat
  
  
  ## Buy lambs (at the start of the season)
  gm1$buy.lamb.rep <- ifelse(gm1$n.lamb.ewe.dif < 0, -gm1$n.lamb.ewe.dif, 0)
  gm1$buy.lamb.meat <- ifelse(gm1$n.lamb.spl < 0, -gm1$n.lamb.spl, 0)
  gm1$buy.hgt <- ifelse(gm1$n.ewe.spl<0,-gm1$n.ewe.spl,0)
  gm10$buy.hgt <- gm1$buy.hgt * (1 - inp1$prev)
  gm11$buy.hgt <- gm1$buy.hgt * inp1$prev
  gm0$buy.ram <- (gm1$n.ewe + gm1$n.hgt.mated)/100
  
  gm1$n.lamb.meat.adj <- gm1$n.lamb/2 + gm1$buy.lamb.meat + ifelse(gm1$n.lamb.ewe.dif < 0, 0, gm1$n.lamb.ewe.dif)
  gm10$n.lamb.meat.adj <- gm1$buy.lamb.meat * (1 - inp1$prev) + (gm1$n.lamb/2 + ifelse(gm1$n.lamb.ewe.dif < 0, 0, gm1$n.lamb.ewe.dif)) * gm10$n.lamb/gm1$n.lamb
  gm11$n.lamb.meat.adj <- gm1$buy.lamb.meat * inp1$prev + (gm1$n.lamb/2 + ifelse(gm1$n.lamb.ewe.dif < 0, 0, gm1$n.lamb.ewe.dif)) * gm11$n.lamb/gm1$n.lamb
  
  gm1$n.lamb.rep.adj <- gm1$n.lamb.ewe.rep + gm1$buy.lamb.rep
  gm10$n.lamb.rep.adj <- gm1$n.lamb.ewe.rep * gm10$n.lamb/gm1$n.lamb + gm1$buy.lamb.rep * (1 - inp1$prev)
  gm11$n.lamb.rep.adj <- gm1$n.lamb.ewe.rep * gm11$n.lamb/gm1$n.lamb + gm1$buy.lamb.rep * inp1$prev
  
  
  ## Cull (prime)
  gm10$cull.ewe <- gm10$n.ewe * gm0$p.cull.ewe
  gm10$cull.hgt <- (gm10$n.hgt + gm10$buy.hgt) * gm0$p.cull.hgt
  gm10$cull.lamb <- gm10$n.lamb.meat.adj * (1 - inp0$r.mort2) * inp0$p.prime.lamb
  
  gm11$cull.ewe <- gm11$n.ewe * ifelse(gm0$p.cull.ewe + inp1$r.cull>1,1,gm0$p.cull.ewe + inp1$r.cull)
  gm11$cull.hgt <- (gm11$n.hgt + gm11$buy) * ifelse(gm0$p.cull.hgt + inp1$r.cull>1,1,gm0$p.cull.hgt + inp1$r.cull)
  gm11$cull.lamb <- gm11$n.lamb.meat.adj * (1 - inp1$r.mort2) * inp1$p.prime.lamb
  
  gm1$cull.ewe <- gm10$cull.ewe + gm11$cull.ewe
  gm1$cull.hgt <- gm10$cull.hgt + gm11$cull.hgt
  gm1$cull.lamb <- gm10$cull.lamb + gm11$cull.lamb
  
  
  # gm10$n.hgt.rep <- gm10$n.hgt - gm10$die.hgt - gm10$cull.hgt
  # gm10$n.ewe.spl <- gm10$n.hgt.rep - gm10$die.ewe - gm10$cull.ewe
  # 
  # gm11$n.hgt.rep <- gm11$n.hgt - gm11$die.hgt - gm11$cull.hgt
  # gm11$n.ewe.spl <- gm11$n.hgt.rep - gm11$die.ewe - gm11$cull.ewe
  # 
  # gm1$n.hgt.rep <- gm11$n.hgt.rep + gm10$n.hgt.rep
  # gm1$n.ewe.spl <- gm11$n.ewe.spl + gm10$n.ewe.spl
  
  ## Sell (store)
  gm10$sell.ewe <- 0
  gm11$sell.ewe <- 0
  gm1$sell.ewe <- 0
  
  gm1$sell.hgt <- 0
  gm10$sell.hgt <- 0
  gm11$sell.hgt <- 0
  
  gm10$sell.lamb <- gm10$n.lamb.meat.adj * (1 - inp0$r.mort2) * (1 - inp0$p.prime.lamb)
  gm11$sell.lamb <- gm11$n.lamb.meat.adj * (1 - inp1$r.mort2) * (1 - inp1$p.prime.lamb)
  gm1$sell.lamb <- gm10$sell.lamb + gm11$sell.lamb
  
  
  
  ## Death
  gm10$die.hgt <- (gm10$n.hgt + gm10$buy.hgt) * inp0$r.mort3
  gm10$die.lamb.rep <- gm10$n.lamb.rep.adj * inp0$r.mort2
  gm10$die.lamb.meat <- gm10$n.lamb.meat.adj * inp0$r.mort2
  
  gm11$die.hgt <- (gm11$n.hgt + gm11$buy.hgt) * inp1$r.mort3
  gm11$die.lamb.rep <- gm11$n.lamb.rep.adj * inp1$r.mort2
  gm11$die.lamb.meat <- gm11$n.lamb.meat.adj * inp1$r.mort2
  
  gm1$die.hgt <- gm10$die.hgt + gm11$die.hgt
  gm1$die.lamb.rep <- gm10$die.lamb.rep + gm11$die.lamb.rep
  gm1$die.lamb.meat <- gm10$die.lamb.meat + gm11$die.lamb.meat
  
  ## ===================================================================================================
  
  ## Season average number
  gm10$n.ewe.x <- gm10$n.ewe - (gm10$die.ewe + gm10$cull.ewe + gm10$sell.ewe)/2
  gm10$n.hgt.x <- gm10$n.hgt + gm10$buy.hgt - (gm10$die.hgt + gm10$cull.hgt + gm10$sell.hgt)/2
  gm10$n.lamb.x <- gm10$n.lamb.meat.adj + gm10$n.lamb.rep.adj - (gm10$die.lamb.meat + gm10$die.lamb.rep + gm10$cull.lamb + gm10$sell.lamb)/2
  gm10$su.x <- gm10$n.ewe.x * 1 + gm10$n.hgt.x * 0.7 + gm10$n.lamb.x * 0.7
  
  gm11$n.ewe.x <- gm11$n.ewe - (gm11$die.ewe + gm11$cull.ewe + gm11$sell.ewe)/2
  gm11$n.hgt.x <- gm11$n.hgt + gm11$buy.hgt - (gm11$die.hgt + gm11$cull.hgt + gm11$sell.hgt)/2
  gm11$n.lamb.x <- gm11$n.lamb.meat.adj + gm11$n.lamb.rep.adj - (gm11$die.lamb.meat + gm11$die.lamb.rep + gm11$cull.lamb + gm11$sell.lamb)/2
  gm11$su.x <- gm11$n.ewe.x * 1 + gm11$n.hgt.x * 0.7 + gm11$n.lamb.x * 0.7
  
  gm1$n.ewe.x <- gm10$n.ewe.x + gm11$n.ewe.x
  gm1$n.hgt.x <- gm10$n.hgt.x + gm11$n.hgt.x
  gm1$n.lamb.x <- gm10$n.lamb.x + gm11$n.lamb.x
  gm1$su.x <- gm11$su.x + gm10$su.x
  
  
  
  ## Closing number
  gm1$n.ewe. <- gm1$n.ewe - (gm1$die.ewe + gm1$cull.ewe + gm1$sell.ewe)
  gm1$n.hgt. <- gm1$n.hgt - (gm1$die.hgt + gm1$cull.hgt + gm1$sell.hgt) + gm1$buy.hgt
  gm1$n.lamb.rep. <- gm1$n.lamb.rep.adj - gm1$die.lamb.rep
  gm1$n.lamb.meat. <- gm1$n.lamb.meat.adj - (gm1$die.lamb.meat + gm1$cull.lamb + gm1$sell.lamb)
  
  
  ## Wool production
  gm10$wool <- inp0$r.wool * (gm10$n.ewe.x + gm10$n.hgt.x)
  gm11$wool <- inp0$r.wool * (gm11$n.ewe.x + gm11$n.hgt.x)
  gm1$wool <- gm10$wool + gm11$wool
  
  ## Income
  gm10$inc.stock.ewe <- rowSums(cbind(inp0$val.prime.ewe * gm10$cull.ewe, inp0$val.store.ewe * gm10$sell.ewe), na.rm = TRUE)
  gm10$inc.stock.hgt <- rowSums(cbind(inp0$val.prime.hgt * gm10$cull.hgt, inp0$val.store.hgt * gm10$sell.hgt), na.rm = TRUE)
  gm10$inc.stock.lamb <- rowSums(cbind(inp0$val.prime.lamb * gm10$cull.lamb, inp0$val.store.lamb * gm10$sell.lamb), na.rm = TRUE)
  
  gm10$inc.stock <- rowSums(gm10[,paste0("inc.stock.", c("ewe","hgt","lamb"))], na.rm = TRUE)
  gm10$inc.wool <- gm10$wool * inp0$val.wool/100
  # gm10$inc.total <- gm10$inc.stock + gm10$inc.wool
  
  
  gm11$inc.stock.ewe <- rowSums(cbind(inp1$val.prime.ewe * gm11$cull.ewe, inp1$val.store.ewe * gm11$sell.ewe), na.rm = TRUE)
  gm11$inc.stock.hgt <- rowSums(cbind(inp1$val.prime.hgt * gm11$cull.hgt, inp1$val.store.hgt * gm11$sell.hgt), na.rm = TRUE)
  gm11$inc.stock.lamb <- rowSums(cbind(inp1$val.prime.lamb * gm11$cull.lamb, inp1$val.store.lamb * gm11$sell.lamb), na.rm = TRUE)
  
  gm11$inc.stock <- rowSums(gm11[,paste0("inc.stock.", c("ewe","hgt","lamb"))], na.rm = TRUE)
  gm11$inc.wool <- gm11$wool * inp0$val.wool/100
  # gm11$inc.total <- gm11$inc.stock + gm11$inc.wool
  
  gm1$inc.stock <- gm11$inc.stock + gm10$inc.stock
  gm1$inc.stock.ewe <- gm11$inc.stock.ewe + gm10$inc.stock.ewe
  gm1$inc.stock.hgt <- gm11$inc.stock.hgt + gm10$inc.stock.hgt
  gm1$inc.stock.lamb <- gm11$inc.stock.lamb + gm10$inc.stock.lamb
  gm1$inc.wool <- gm11$inc.wool + gm10$inc.wool
  # gm1$inc.total <- gm11$inc.total + gm10$inc.total
  
  ## Expenses
  gm1$exp.stock <- (gm1$buy.lamb.rep + gm1$buy.lamb.meat) * inp0$val.store.lamb + gm1$buy.hgt * inp0$val.store.hgt + gm1$buy.ram * inp0$val.avg.ram
  gm1$exp.opp.ewe <- inp0$val.prime.ewe * ifelse(gm0$n.ewe. - gm1$n.ewe.>0,gm0$n.ewe. - gm1$n.ewe.,0)
  gm1$exp.opp.hgt <- inp0$val.prime.hgt * ifelse(gm0$n.hgt. - gm1$n.hgt.>0, gm0$n.hgt. - gm1$n.hgt., 0)
  gm1$exp.opp.lamb <- inp0$val.prime.lamb * ifelse(gm0$n.lamb.rep. - gm1$n.lamb.rep.>0, gm0$n.lamb.rep. - gm1$n.lamb.rep., 0)
  gm1$exp.opp <- gm1$exp.opp.ewe + gm1$exp.opp.hgt + gm1$exp.opp.lamb
  gm1$exp.purchase <- gm1$exp.stock + gm1$exp.opp
  
  
  
  gm1$exp.wool <- inp0$exp.shear * gm1$su.x * f
  gm1$exp.ah <- (inp0$exp.ah * gm1$su.x * f) + inp1$exp.ah.extra * (gm11$n.ewe + gm11$n.hgt + gm11$buy.hgt + gm11$n.lamb.rep.adj + gm11$n.lamb.meat.adj)
  gm1$exp.feed <- inp0$exp.feed * gm1$su.x * f
  gm1$exp.lab <- inp0$exp.lab * gm1$su.x * f
  
  
  gm1$inc.stock.net <- gm1$inc.stock - gm1$exp.purchase
  gm1$inc.total <- gm1$inc.stock.net + gm1$inc.wool
  
  gm1$exp.total <- gm1$exp.wool + gm1$exp.ah + gm1$exp.feed + gm1$exp.lab #+ inp0$exp.other
  
  gm1$profit <- gm1$inc.total - gm1$exp.total
  
  ## ===================================================================================================
  ## Partial budget
  pb <- data.frame(class = fclass)
  pb$profit <- gm1$profit - gm0$profit
  pb$profit.pct <- 100 * pb$profit/gm0$profit
  
  pb$inc.total <- gm1$inc.total - gm0$inc.total
  pb$inc.stock <- gm1$inc.stock - gm0$inc.stock
  pb$inc.stock.ewe <- gm1$inc.stock.ewe - gm0$inc.stock.ewe
  pb$inc.stock.hgt <- gm1$inc.stock.hgt - gm0$inc.stock.hgt
  pb$inc.stock.lamb <- gm1$inc.stock.lamb - gm0$inc.stock.lamb
  pb$inc.wool <- gm1$inc.wool - gm0$inc.wool
  
  pb$exp.wool <- gm1$exp.wool - gm0$exp.wool
  pb$exp.feed <- gm1$exp.feed - gm0$exp.feed
  pb$exp.ah <- gm1$exp.ah - gm0$exp.ah
  pb$exp.lab <- gm1$exp.lab - gm0$exp.lab
  pb$exp.stock <- gm1$exp.stock - gm0$exp.stock
  pb$exp.opp.ewe <- gm1$exp.opp.ewe
  pb$exp.opp.hgt <- gm1$exp.opp.hgt
  pb$exp.opp.lamb <- gm1$exp.opp.lamb
  pb$exp.opp <- gm1$exp.opp
  pb$exp.purchase <- gm1$exp.purchase - gm0$exp.purchase
  pb$exp.total <- gm1$exp.total - gm0$exp.total
  
  pb$inc.total.pct <- 100 * pb$inc.total/gm0$inc.total
  pb$inc.stock.pct <- 100 * pb$inc.stock/gm0$inc.stock
  pb$inc.stock.ewe.pct <- 100 * pb$inc.stock.ewe/gm0$inc.stock.ewe
  pb$inc.stock.hgt.pct <- 100 * pb$inc.stock.hgt/gm0$inc.stock.hgt
  pb$inc.stock.lamb.pct <- 100 * pb$inc.stock.lamb/gm0$inc.stock.lamb
  pb$inc.wool.pct <- 100 * pb$inc.wool/gm0$inc.wool
  
  pb$exp.wool.pct <- 100 * pb$exp.wool/gm0$exp.wool
  pb$exp.feed.pct <- 100 * pb$exp.feed/gm0$exp.feed
  pb$exp.ah.pct <- 100 * pb$exp.ah/gm0$exp.ah
  pb$exp.lab.pct <- 100 * pb$exp.lab/gm0$exp.lab
  pb$exp.stock.pct <- 100 * pb$exp.stock/gm0$exp.stock
  pb$exp.purchase.pct <- 100 * pb$exp.purchase/gm0$exp.purchase
  pb$exp.total.pct <- 100 * pb$exp.total/gm0$exp.total
  
  
  ## Reference
  pb$diff.inc <- gm0$inc.total - inp0$ref.inc
  pb$diff.exp <- gm0$exp.total - inp0$ref.exp
  pb$diff.profit <- gm0$profit - inp0$ref.profit
  
  pb$error.inc <- 100 * (gm0$inc.total - inp0$ref.inc)/inp0$ref.inc
  pb$error.exp <- 100 * (gm0$exp.total - inp0$ref.exp)/inp0$ref.exp
  pb$error.profit <- 100 * (gm0$profit - inp0$ref.profit)/inp0$ref.profit
  
  pb$unit.loss <- pb$profit/gm0$n.ewe
  
  out <- list(gm0, gm1, pb)
  names(out) <- c("Non-infected farm", "Infected farm", "Partial Budget")
  return(out)
  
}

terminology <- function(original, species = "dairy", full = TRUE){
  
  if(species == "dairy"){
    code <- c('n1', 'n2', 'n3',
              'die1', 'die2', 'die3', 'buy1', 'buy2', 'buy3', 'cull1', 'cull2', 'cull3',
              'sell3', 'ms', 
              'profit','inc.total', 'exp.total',
              'inc.ms', 'inc.stock', 'inc.stock1', 'inc.stock2', 'inc.stock3', 'inc.stock.net', 
              'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.purchase', 'exp.other',
              'profit.pct', 'inc.total.pct', 'inc.ms.pct', 'inc.stock.pct', 'inc.stock1.pct', 'inc.stock2.pct', 'inc.stock3.pct',
              'exp.feed.pct', 'exp.breed.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct',
              "unit.loss",
              'error.inc', 'error.exp', 'error.profit')
    
    label <- c('Initial number (MA)', 'Initial number (R2)', 'Number of calves born',
               'Death (MA)', 'Death (R2)', 'Death (calves/R1)',
               'Purchase (MA)', 'Purchase (R2)', 'Purchase (R1)',
               'Cull (MA)', 'Cull (R2)', 'Cull (bobby)',
               'Sell (beefx)', 'Milk solids (kg)',
               'Profit before tax (cash operating surplus)', 'Total income (net dairy cash income)', 'Total expense (farm working expenses)',
               "Income: milk", "Income: livestock sales", "Income: cows sales", "Income: heifers sales", "Income: calves/R1 sales", "Income: net livestock sales",
               "Expense: feed", "Expense: breeding", "Expense: animal health", "Expense: labour",  "Expense: stock purchase", "Expense: other",
               "Profit", "Total income", "Milk", "Livestock sales",  "MA cow sales", "R2 heifer sales", "Calf/R1 sales",
               "Feed", "Breeding", "Animal health", "Labour", "Livestock purchase", "Total expense",
               "Loss per MA cow",
               '% Error (total income)', '% Error (total expense)', '% Error (farm profit)')
    
  } else if (species == "beef"){
    code <- c('n.c', 'n.h2', 'n.h1', 'n.h0', 'n.s2', 'n.s1', 'n.s0', "n.b", 'n.b1', 'n.b0',
              'die.c', 'die.h2', 'die.h1', 'die.h0', 'die.s2', 'die.s1', 'die.s0', "die.b", 'die.b1', 'die.b0',
              'buy.c', 'buy.h2', 'buy.h1', 'buy.h0', 'buy.s2', 'buy.s1', 'buy.s0', "buy.b", 'buy.b1', 'buy.b0',
              'cull.c', 'cull.h2', 'cull.h1', 'cull.h0', 'cull.s2', 'cull.s1', 'cull.s0', "cull.b", 'cull.b1', 'cull.b0',
              'sell.c', 'sell.h2', 'sell.h1', 'sell.h0', 'sell.s2', 'sell.s1', 'sell.s0', "sell.b", 'sell.b1', 'sell.b0',
              'profit', 'inc.total', 'exp.total',
              'inc.stock', 'inc.stock.c', 'inc.stock.r2', 'inc.stock.r3', 'inc.stock.b', 'inc.stock.calves', 'inc.stock.net',
              'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase', 'exp.other',
              'profit.pct', 'inc.total.pct', 'inc.stock.pct', 'inc.stock.c.pct', 'inc.stock.r2.pct', 'inc.stock.r3.pct', 'inc.stock.b.pct', 'inc.stock.calves.pct',
              'exp.feed.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct',
              "unit.loss",
              'error.inc', 'error.exp', 'error.profit')
    
    
    label <- c('Initial number (+3y cows)', 'Initial number (R3 heifers)', 'Initial number (R2 heifers)', 'Number of calves born/bought (heifers)',
               'Initial number (R3 steers)', 'Initial number (R2 steers)', 'Number of calves born/bought (steers)',
               'Initial number (+2y bulls)', 'Initial number (R2 bulls)', 'Number of calves born/bought (bulls)',
               
               'Death (+3y cows)', 'Death (R3 heifers)', 'Death (R2 heifers)', 'Death (heifer calves)',
               'Death (R3 steers)', 'Death (R2 steers)', 'Death (steer calves)',
               'Death (+2y bulls)', 'Death (R2 bulls)', 'Death (bull calves)',
               
               'Purchase (+3y cows)', 'Purchase (R3 heifers)', 'Purchase (R2 heifers)', 'Purchase (heifer calves)',
               'Purchase (R3 steers)', 'Purchase (R2 steers)', 'Purchase (steer calves)',
               'Purchase (+2y bulls)', 'Purchase (R2 bulls)', 'Purchase (bull calves)',
               
               'Cull (+3y cows)', 'Cull (R3 heifers)', 'Cull (R2 heifers)', 'Cull (heifer calves)',
               'Cull (R3 steers)', 'Cull (R2 steers)', 'Cull (steer calves)',
               'Cull (+2y bulls)', 'Cull (R2 bulls)', 'Cull (bull calves)',
               
               'Sell (+3y cows)', 'Sell (R3 heifers)', 'Sell (R2 heifers)', 'Sell (heifer calves)',
               'Sell (R3 steers)', 'Sell (R2 steers)', 'Sell (steer calves)',
               'Sell (+2y bulls)', 'Sell (R2 bulls)', 'Sell (bull calves)',
               
               'Farm profit before tax (cattle)', 'Total income (gross revenue; cattle)', 'Total expenses (farm expenditure, cattle)',
               
               "Income: livestock sales", "Income: cows sales", "Income: R2 sales", "Income: R3 sales", "Income: bulls sales", "Income: calves sales", "Income: net livestock sales",
               
               "Expense: feed & grazing", "Expense: animal health", "Expense: labour",  "Expense: stock purchase", "Expense: other",
               "Profit", "Total income", "Livestock sales", "Cows sales", "R2 sales", "R3 sales", "Bulls sales", "Calves sales",
               "Feed", "Animal health", "Labour", "Stock purchase", "Total expense",
               "Loss per MA cow",
               '% Error (cattle income)', '% Error (cattle expense)', '% Error (cattle profit)')
    
    
    
  }else if (species == "sheep"){
    code <- c('n.ewe', 'n.hgt', 'n.lamb',
              'die.ewe', 'die.hgt', 'die.lamb', 'buy.hgt', 'buy.lamb', 'cull.ewe', 'cull.hgt', 'cull.lamb',
              'sell.ewe', 'sell.hgt', 'sell.lamb', 'wool', 
              'profit', 'inc.total', 'exp.total',
              'inc.stock', 'inc.stock.ewe', 'inc.stock.hgt', 'inc.stock.lamb', 'inc.stock.net', 'inc.wool',
              'exp.wool', 'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase', 'exp.other',
              'profit.pct', 'inc.total.pct', 'inc.stock.pct', 'inc.stock.ewe.pct', 'inc.stock.hgt.pct', 'inc.stock.lamb.pct', 'inc.wool.pct',
              'exp.wool.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct',
              "unit.loss",
              'error.inc', 'error.exp', 'error.profit')
    
    label <- c('Initial number (ewes)', 'Initial number (hoggets)', 'Number of lambs born',
               'Death (ewes)', 'Death (hoggets)', 'Death (lambs)',
               'Purchase (hoggets)', 'Purchase (lambs)',
               'Cull (ewes)', 'Cull (hoggets)', 'Cull (lambs)',
               'Sell (ewes)', 'Sell (hoggets)', 'Sell (lambs)',
               'Wool (kg)',
               'Farm profit before tax (sheep)', 'Total income (gross revenue; sheep+wool)', 'Total expense (farm expenditure; sheep+wool)',
               
               "Income: livestock sell/cull", "Income: ewes sell/cull", "Income: hoggets sell/cull", "Income: lambs sell/cull", "Income: net livestock sales", "Income: wool",
               "Expense: wool shearing", "Expense: feed & grazing", "Expense: animal health", "Expense: labour",  "Expense: stock purchase", "Expense: other",
               "Profit", "Total income", 'livestock sell/cull', "Ewes sell/cull", "Hoggets sell/cull", "Lambs sell/cull", "Wool",
               "Wool shearing", "Animal health", "Labour", "Stock purchase", "Total expense",
               "Loss per MA ewe",
               '% Error (sheep income)', '% Error (sheep expense)', '% Error (sheep profit)')
    
  }
  
  if(!full) {
    label <- gsub("^Income: |^Expense: ", "", label)
    substr(label, 1, 1) <- toupper(substr(label, 1, 1))
  }
  
  new <- factor(factor(original, levels = code, label = label))
  return(new)
}

plot_breakdown <- function(o, species = "dairy", class = NULL, detail = FALSE, baseline = FALSE, mytheme = NULL, scenario = "", interactive = FALSE, txtsize = 11){
  if(detail){
    
    if(species == "dairy"){
      items <- c('profit', 'inc.ms', 'inc.stock1', 'inc.stock2', 'inc.stock3', 
                 'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.purchase', 'exp.other')
      caption <- "2019-20 owner-operator average (source: DairyNZ)"
      
    }else if (species == "beef"){
      items <- c('profit', 'inc.stock.c', 'inc.stock.h', 'inc.stock.s', 'inc.stock.b', 'inc.stock.calves',
                 'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase', 'exp.other')
      caption <- "2018-19 mean (source: Beef+Lamb NZ economic service)"
      
      
    }else if (species == "sheep"){
      items <- c('profit', 'inc.stock.ewe', 'inc.stock.hgt', 'inc.stock.lamb', 'inc.wool',
                 'exp.wool', 'exp.ah', 'exp.lab', 'exp.purchase', 'exp.other')
      caption <- "2018-19 mean (source: Beef+Lamb NZ economic service)"
      
    }
    
  }else{
    
    if(species == "dairy"){
      items <- c('profit', 'inc.ms', 'inc.stock.net', 
                 'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.other')
      caption <- "2019-20 owner-operator average (source: DairyNZ)"
      
    }else if (species == "beef"){
      items <- c('profit', 'inc.stock.net',
                 'exp.feed', 'exp.ah', 'exp.lab')
      caption <- "2018-19 mean (source: Beef+Lamb NZ economic service)"
      
      
    }else if (species == "sheep"){
      items <- c('profit', 'inc.stock.net', 'inc.wool',
                 'exp.feed', 'exp.wool', 'exp.ah', 'exp.lab')
      caption <- "2018-19 mean (source: Beef+Lamb NZ economic service)"
      
    }
    
  }
  
  
  d <- ldply(o, function(x) {
    for(i in items){
      if(!i %in% names(x)) x[,i] <- NA
    }
    x[,c("class", items)]}
  )
  
  ## Breakdown of incomes/expenses by farm class
  m <- reshape2::melt(d[d$.id != "Partial Budget",], id = c(".id", "class"))
  levels(m$class) <- gsub("\\.|\\.\\.", " ", levels(m$class))
  if(!is.null(class)) {
    m <- m[m$class %in% class,]
    m$class <- factor(m$class, levels = class)
  }
  m1 <- m[grepl("inc", m$variable),]
  m1$variable <- terminology(m1$variable, species = species, full = TRUE)
  m1$value <- m1$value/1000
  m1 <-
    ddply(m1, .(.id, class), function(x){
      x <- x[order(x$variable, decreasing = TRUE),]
      x$cum <- cumsum(x$value);x
    })
  
  
  m2 <- m[grepl("exp", m$variable),]
  m2$variable <- terminology(m2$variable, species = species, full = TRUE)
  m2$value <- -m2$value/1000
  m2 <- ddply(m2, .(.id, class), function(x){
    x <- x[order(x$variable, decreasing = TRUE),]
    x$cum <- cumsum(x$value);x})
  
  mx <- rbind(m1, m2)
  mx$type <- factor(ifelse(mx$value < 0, 2, 1), labels = c("Income", "Expense"))
  mx$status <- factor(mx$.id, levels = c("Infected farm", "Non-infected farm"), labels = c("Disease", "No disease"))
  
  ## Add 0 lines
  m0 <- mx[mx$variable == 
             tail(grep("Income", levels(mx$variable), value = TRUE),1)
           ,]
  m0$value <- 0
  m0$cum <- 0
  m0$variable <- NA
  
  mx0 <- rbind(mx, m0)
  
  cols <- c(brewer_pal("seq", palette = 4, direction = 1)(length(levels(m2$variable))),
            brewer_pal("seq", palette = 11, direction = -1)(length(levels(m1$variable))))
  
  if(is.null(mytheme)) mytheme <- theme_bw(base_size = txtsize)
  
  title <- "Annual farm incomes/expenses"
  subtitle <- scenario
  
  w <- 0.6
  
  if(baseline){
    p <- 
      ggplot(mx0[mx0$status == "No disease" & !is.na(mx0$variable),], aes(x = class, fill = variable, y = value, alpha = status)) +
      geom_col(color = "grey20", width = w) +
      scale_fill_manual(name = "", breaks = c(levels(m2$variable), levels(m1$variable)), values = cols) +
      scale_alpha_manual(values = c(1, 1), guide = "none") +
      coord_flip() +
      xlab("") + ylab("Farm incomes and expenses (NZ$ thousand per year)") +
      mytheme +
      labs(title = "",
           subtitle = "",
           caption = "")
    
  }else{
    q <-
      ggplot(mx0, aes(x = status, fill = variable, y = value, alpha = status)) +
      geom_col(color = "grey20", width = w) +
      geom_line( aes(x = (1 - w) * as.numeric(status) + 1.5 * w,
                     y = cum, group = variable), color = "grey60") +
      scale_fill_manual(name = "", breaks = c(levels(m2$variable), levels(m1$variable)), values = cols) +
      scale_alpha_manual(values = c(1, 1), guide = "none") +
      coord_flip() +
      xlab("") + ylab("") +
      facet_wrap(~class) +
      mytheme +
      labs(title = title,
           subtitle = subtitle,
           caption = caption)
    
    
    if(interactive){
      p <- ggplotly(q, height = 600, width = 1200) %>%
        layout(
          margin = list(b=80, t = 100), ##bottom margin in pixels
          
          yaxis = list(
            title = list(text="Farm status")
          ),
          
          xaxis = list(
            title = list(text="Incomes and expenses (NZ$ k per farm per year)")
          ),
          
          title = list(text = paste0(title, '<br>', '<sup>', subtitle,'</sup>')),
          
          annotations = 
            list(x = 1, y = -0.2, #position of text adjust as needed 
                 text = caption, 
                 showarrow = F, xref='paper', yref='paper',
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10, color="grey40"))
        )
      
    } else   p <- q+xlab("Farm status") + ylab("Incomes and expenses (NZ$ k per farm per year)")
    
  }
  print(p)
  
}

plot_impact <- function(d, species = "dairy", class = NULL, detail = FALSE, percentage = TRUE, dis.name = "", mytheme = NULL, interactive = FALSE, txtsize = 11){
  
  m <- reshape2::melt(d, id = c("class"))
  levels(m$class) <- gsub("\\.|\\.\\.", " ", levels(m$class))
  if(!is.null(class)) m <- m[m$class %in% class,]
  
  if(detail){
    if(percentage){
      
      if(species == "dairy"){
        items <- c('profit.pct', 'inc.total.pct', 'inc.ms.pct', 'inc.stock1.pct', 'inc.stock2.pct', 'inc.stock3.pct',
                   'exp.feed.pct', 'exp.breed.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct')
        
      }else if (species == "beef"){
        items <- c('profit.pct', 'inc.total.pct', 'inc.stock.c.pct', 'inc.stock.h.pct', 'inc.stock.s.pct', 'inc.stock.b.pct', 'inc.stock.calves.pct',
                   'exp.feed.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct')
        
      }else if (species == "sheep"){
        items <- c('profit.pct', 'inc.total.pct', 'inc.stock.ewe.pct', 'inc.stock.hgt.pct', 'inc.stock.lamb.pct', 'inc.wool.pct',
                   'exp.feed.pct', 'exp.wool.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct')
        
      }
      
      ## %change
      m <- m[m$variable %in% items,]
      
      unit <- "(% per farm per year)"
      cap <- "Excluding replacement costs"
      
    }else{
      
      if(species == "dairy"){
        items <- c('profit', 'inc.ms', 'inc.stock1', 'inc.stock2', 'inc.stock3', 
                   'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.purchase')
        
      }else if (species == "beef"){
        items <- c('profit', 'inc.stock.c', 'inc.stock.h', 'inc.stock.s', 'inc.stock.b', 'inc.stock.calves',
                   'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase')
        
      }else if (species == "sheep"){
        items <- c('profit', 'inc.stock.ewe', 'inc.stock.hgt', 'inc.stock.lamb', 'inc.wool',
                   'exp.wool', 'exp.ah', 'exp.lab', 'exp.purchase')
        
      }
      ## absolute change
      m <- m[m$variable %in% items,]
      
      m$value <- m$value/1000
      unit <- "(NZ$ k per farm per year)"
      cap <- ""
      
    }
    
  }else{
    
    if(percentage){
      
      if(species == "dairy"){
        items <- c('profit.pct', 'inc.total.pct', 'inc.ms.pct', 'inc.stock.pct',
                   'exp.feed.pct', 'exp.breed.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct')
        
      }else if (species == "beef"){
        items <- c('profit.pct', 'inc.total.pct', 
                   'exp.feed.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct')
        
      }else if (species == "sheep"){
        items <- c('profit.pct', 'inc.total.pct', 'inc.stock.pct', 'inc.wool.pct',
                   'exp.wool.pct', 'exp.ah.pct', 'exp.lab.pct', 'exp.purchase.pct', 'exp.total.pct')
      }
      
      ## %change
      m <- m[m$variable %in% items,]
      
      unit <- "(% per farm per year)"
      
      
    }else{
      
      if(species == "dairy"){
        items <- c('profit', 'inc.ms', 'inc.stock', 
                   'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.purchase')
        
      }else if (species == "beef"){
        items <- c('profit', 'inc.stock',
                   'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase')
        
      }else if (species == "sheep"){
        items <- c('profit', 'inc.stock', 'inc.wool',
                   'exp.wool', 'exp.ah', 'exp.lab', 'exp.purchase')
      }
      ## absolute change
      m <- m[m$variable %in% items,]
      
      m$value <- m$value/1000
      unit <- "(NZ$ k per farm per year)"
      
    }
    
  }
  
  m$category <- ifelse(grepl("profit", m$variable), 1, ifelse(grepl("inc.", m$variable), 2, 3))
  m$category <- factor(m$category, levels = 1:3, labels = c("Profit", "Income", "Expense"))
  
  m$variable <- terminology(m$variable, species = species, full = FALSE)
  
  if(is.null(mytheme)) mytheme <- theme_bw(base_size = txtsize)
  
  title <- "Farm level disease impacts over one year"
  
  inf <- m[is.infinite(m$value),]
  m0 <- m[is.finite(m$value),]
  if(nrow(inf)>0) cap <- "*No estimates (denominator = 0)" else cap <- ""
  
  if(nrow(m0)>0){
    p <- ggplot(m0, aes(x = variable, weight = value, fill = category)) + geom_bar(color = "grey40") + 
      geom_text(data = inf, y = max(c(0,m0$value), na.rm = TRUE), label = "*") +
      scale_fill_brewer(palette = "RdYlBu") +
      facet_wrap(~class) +
      xlab("") + ylab(paste0("Difference from the baseline without disease ", unit)) +
      coord_flip() +
      scale_x_discrete(limits = rev(unique(m$variable))) +
      mytheme +
      labs(title = title, 
           subtitle = dis.name, 
           caption = cap)
    
    if(interactive){
      p <- 
        ggplotly(p, height = 700, width = 1000) %>%
        layout(
          title = list(text = paste0(title, '<br>', '<sup>', dis.name,'</sup>')),
          margin = list(b=90, t = 90), ##bottom/top margin in pixels
          annotations = 
            list(x = 1, y = -0.1, #position of text adjust as needed 
                 text = cap, 
                 showarrow = F, xref='paper', yref='paper',
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10, color="grey40"))
        )
      
    }
    
    print(p)
    
  }
}

summary_table <- function(o, class = NULL, species = "dairy", detail = FALSE){
  if(detail){
    
    if(species == "dairy"){
      items <- c('profit', 'inc.ms', 'inc.stock1', 'inc.stock2', 'inc.stock3', 
                 'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.purchase',
                 "unit.loss",
                 'error.inc', 'error.exp', 'error.profit')
      
    }else if (species == "beef"){
      items <- c('profit', 'inc.stock.c', 'inc.stock.r2', 'inc.stock.r3', 'inc.stock.b', 'inc.stock.calves',
                 'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase',
                 "unit.loss",
                 'error.inc', 'error.exp', 'error.profit')
      
      
    }else if (species == "sheep"){
      items <- c('profit', 'inc.stock.ewe', 'inc.stock.hgt', 'inc.stock.lamb', 'inc.wool',
                 'exp.feed', 'exp.wool', 'exp.ah', 'exp.lab', 'exp.purchase',
                 "unit.loss",
                 'error.inc', 'error.exp', 'error.profit')
    }
    
  }else{
    
    if(species == "dairy"){
      items <- c('profit', 'inc.ms', 'inc.stock', 
                 'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.purchase',
                 "unit.loss",
                 'error.inc', 'error.exp', 'error.profit')
      
    }else if (species == "beef"){
      items <- c('profit', 'inc.stock',
                 'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase',
                 "unit.loss",
                 'error.inc', 'error.exp', 'error.profit')
      
      
      
    }else if (species == "sheep"){
      items <- c('profit', 'inc.stock', 'inc.wool',
                 'exp.feed', 'exp.wool', 'exp.ah', 'exp.lab', 'exp.purchase',
                 "unit.loss",
                 'error.inc', 'error.exp', 'error.profit')
      
    }
    
  }
  
  
  d <- ldply(o, function(x) {
    for(i in items){
      if(!i %in% names(x)) x[,i] <- NA
    }
    x[,c("class", items)]}
  )
  
  ## Breakdown of incomes/expenses by farm class
  levels(d$class) <- gsub("\\.|\\.\\.", " ", levels(d$class))
  if(!any(class %in% levels(d$class))) class <- NULL
  if(!is.null(class)) d <- d[d$class %in% class,]
  
  x <- reshape2::dcast(d[d$.id != "Partial Budget",], class ~ .id, value.var = "profit")
  x$loss <- x$`Non-infected farm` - x$`Infected farm`
  x$loss.pct <- 100 * x$loss/x$`Non-infected farm`
  x <- reshape2::melt(x, id.var = "class")
  x$variable <- factor(x$variable, levels = c('Non-infected farm', 'Infected farm', 'loss', 'loss.pct'), 
                       labels = c("Farm profit before tax (without disease)", "Farm profit before tax (with disease)", "Total loss due to disease", "% Total loss due to disease"))
  tab0 <- data.frame(class = x$class, type = "Summary", 
                     variable = levels(x$variable)[x$variable],
                     amount = x$value)
  tab0$unit <- ifelse(grepl("%", tab0$variable), "% annual farm profit", "NZD per farm per year")
  
  
  m <- reshape2::melt(d[d$.id == "Partial Budget",], id = c(".id", "class"))
  
  m1 <- m[grepl("inc", m$variable) & !grepl("^error", m$variable),]
  m1$variable <- terminology(m1$variable, species = species, full = FALSE)
  
  m1$type <- ifelse(m1$value < 0, "Income foregone", "Additional income")
  m1$amount <- round(abs(m1$value))
  m1$unit <- "NZD per farm per year"
  m1 <- m1[order(m1$type, m1$variable),c("class", "type", "variable", "unit", "amount")]
  
  m2 <- m[grepl("exp", m$variable) & !grepl("^error", m$variable),]
  m2$variable <- terminology(m2$variable, species = species, full = FALSE)
  m2$type <- ifelse(m2$value < 0, "Expense saved", "Additional expense")
  m2$amount <- round(abs(m2$value))
  m2$unit <- "NZD per farm per year"
  m2 <- m2[order(m2$type, m2$variable),c("class", "type", "variable", "unit", "amount")]
  
  m0 <- m[grepl("^error", m$variable),]
  m0$variable <- terminology(m0$variable, species = species, full = FALSE)
  
  m0$type <- "Gross margin"
  m0$amount <- m0$value
  m0$unit <- "%"
  m0 <- m0[order(m0$type, m0$variable),c("class", "type", "variable", "unit", "amount")]
  
  m0x <- m[grepl("^unit", m$variable),]
  m0x$variable <- terminology(m0x$variable, species = species, full = FALSE)
  
  m0x$type <- "Summary"
  m0x$amount <- m0x$value
  m0x$unit <- "NZD per head per year"
  m0x <- m0x[order(m0x$type, m0x$variable),c("class", "type", "variable", "unit", "amount")]
  
  tab <- rbind(m0x, m1, m2, m0)
  # tab <- tab[abs(tab$amount) > 0.1,]
  tab$amount <- ifelse(abs(tab$amount) < 0.0001, 0, tab$amount)
  tab <- rbind(tab0, tab)
  # tab$amount <- format(tab$amount, scientific = FALSE, digits = 0, big.mark = ",")
  tab$amount <- ifelse(grepl("NZD", tab$unit), 
                       paste0("$", format(tab$amount, scientific = FALSE, digits = 1, big.mark = ",")),
                       paste0(round(tab$amount, digits = 2), "%"))
  tab <- tab[order(tab$class),]
  return(tab)
}

## ===================================================================================================
## Define server logic
shinyServer(function(input, output, session) {
  
  ## A function to update slider in one go
  updateslider <- function(data){
    data$num <- 1:nrow(data)
    lapply(data$num, function(i){
      
      if (!grepl("NZ\\$", data$unit[i])){
        
        ## Type: slider
        updateSliderInput(session,
                          data$no[i],
                          data$parameter[i],
                          min = 0, max = 100,
                          value = data$value[i],
                          step = 0.1)
      } else {
        
        ## Type: slider
        updateSliderInput(session, 
                          data$no[i],
                          data$parameter[i],
                          min = 0, max = 1000,
                          value = data$value[i],
                          step = 10)
      }
    }
    )
  }
  
  ## Update disease list once species is selected
  observe({
    x <- input$species
    
    if(x==3){#sheep disease
      dlist <- grep("sheep|Sheep", levels(para$variable), value = TRUE)
    } else if(x==2){#beef disease
      dlist <- levels(para$variable)[!grepl("sheep|Sheep|dairy", levels(para$variable))]
    } else{#dairy disease
      dlist <- levels(para$variable)[!grepl("sheep|Sheep|beef", levels(para$variable))]
    }
    
    updateSelectInput(session, "disease",
                      label = "Select a similar disease:",
                      choices = dlist,
                      selected = dlist[1])

  })
  
  ## Update farm class once species is selected
  observe({
    x <- input$species
    
    if(x==1){
      clist <- gsub("\\.|\\.\\.", " ", names(para2)[4:15])
    } else{
      clist <- gsub("\\.|\\.\\.", " ", names(para3)[4:20])
    }
    
    updateCheckboxGroupInput(session, "class",
                             choices = clist,
                             selected = clist[1], inline = TRUE)
    
  })
  
  ## Update sliders once disease is selected
  observe({
    
    ## Update parameter set
    y <- input$disease
    pa <- para[para$variable == y,]
    
    # Update slider input
    updateslider(pa)

  })
  
  ## ========================================================
  # Reactive expression to compose a data frame output
  estimate_impact <- reactive({
    
    ## Update parameter set
    y <- input$disease
    pa <- para[para$variable == y,]
    
    ## Read input values
    data <- sapply(pa$no, function(x) input[[x]])

    ## Update parameter value
    pa$value <- as.numeric(data[match(pa$no, names(data))])
    species <- factor(input$species, levels = 1:3, labels = c("dairy", "beef", "sheep"))

    if(species == "dairy"){
      
      ix <- input_dairy(pa, para2)
      out <- calc_dairy(ix)
      
    }else if(species == "beef"){
      
      ix <- input_beef(pa, para3)
      out <- calc_beef(ix)
      
    }else if(species == "sheep"){
      
      ix <- input_sheep(pa, para4)
      out <- calc_sheep(ix)
      
    }

    return(out)
  }) 
  
  which.disease <- reactive({
    y <- input$disease
    return(y)
  })
  
  which.species <- reactive({
    y <- factor(input$species, levels = 1:3, labels = c("dairy", "beef", "sheep"))
    return(y)
  })
  
  which.class <- reactive({
    y <- input$class
    return(y)
  })
  
  is.detail <- reactive({
    y <- input$detail
    return(y)
  })
  
  farm.definition <- reactive({
    sp <- which.species()
    cl <- which.class()
    
    
    pat <- gsub("\\(|\\)|\\&|\\-", " ", dfn$Farm.type)
    pat <- gsub("  ", " ", pat)
    
    nrow <- which(pat %in% cl)
    
    if(any(cl == "Farm type")){
      dfn0 <- NULL
    }else{
      
      if(sp == "dairy"){
        dfn0 <- dfn[nrow,c("Enterprise", "Farm.type", "Description", "peak.cow.no", "effective.area.ha")]

      }else if(sp == "beef"){
        dfn0 <- dfn[nrow,c("Enterprise", "Farm.type", "Description", "open.cattle.su", "effective.area.ha", "sheep.cattle.su.ratio")]

      }else if(sp == "sheep"){
        dfn0 <- dfn[nrow,c("Enterprise", "Farm.type", "Description", "open.sheep.su", "effective.area.ha", "sheep.cattle.su.ratio")]
      }
      
    }
      
    return(dfn0)
  })
  ## ========================================================
  ## Plotly plot
  # output$plot1 <- renderPlotly({
  #   ou <- estimate_impact()
  #   sp <- which.species()
  #   plot <- plot_breakdown(ou, sp, detail = FALSE, mytheme = NULL, which.disease(), interactive = TRUE)
  #   print(plot)
  # })
  # 
  # output$plot2 <- renderPlotly({
  #   ou <- estimate_impact()
  #   sp <- which.species()
  #   plot <- plot_impact(ou[[3]], detail = FALSE, sp, percentage = TRUE, which.disease(), mytheme = NULL, interactive = TRUE)
  #   print(plot)
  # })
  # 
  # output$plot3 <- renderPlotly({
  #   ou <- estimate_impact()
  #   sp <- which.species()
  #   plot <- plot_impact(ou[[3]], detail = FALSE, sp, percentage = FALSE, which.disease(), mytheme = NULL, interactive = TRUE)
  #   print(plot)
  # })
  ## ========================================================
  
  ## Non-interactive plot (quicker)
  output$plot1 <- renderPlot({
    ou <- estimate_impact()
    sp <- which.species()
    cl <- which.class()
    if(any(cl == "Farm type")) cl <- NULL
    plot <- plot_breakdown(ou, species = sp, class = cl, detail = is.detail(), baseline = FALSE, mytheme = NULL, which.disease(), interactive = FALSE, txtsize = ts)
    print(plot)
  })
  
  output$plot2 <- renderPlot({
    ou <- estimate_impact()
    sp <- which.species()
    cl <- which.class()
    if(any(cl == "Farm type")) cl <- NULL
    plot <- plot_impact(ou[[3]], class = cl, detail = is.detail(), species = sp, percentage = TRUE, which.disease(), mytheme = NULL, interactive = FALSE, txtsize = ts)
    print(plot)
  })
  
  output$plot3 <- renderPlot({
    ou <- estimate_impact()
    sp <- which.species()
    cl <- which.class()
    if(any(cl == "Farm type")) cl <- NULL
    plot <- plot_impact(ou[[3]], class = cl, detail = is.detail(), sp, percentage = FALSE, which.disease(), mytheme = NULL, interactive = FALSE, txtsize = ts)
    print(plot)
  })
  ## ========================================================
  output$summary <- renderReactable({
    ou <- estimate_impact()
    sp <- which.species()
    cl <- which.class()
    if(any(cl == "Farm type")) cl <- NULL
    tb <- summary_table(ou, class = cl, species = sp, detail = is.detail())
    row.names(tb) <- NULL


    sticky_style <- list(backgroundColor = "#f7f7f7")
    reactable(tb,
              defaultColDef = colDef(
                # header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value){
                  if(is.numeric(value)){
                    value <- ifelse( abs(value) < 0.001, 0, value)
                    v <- format(value, nsmall = 0, digits = 2, scientific = FALSE, big.mark = ",")
                    v
                  } else value
                  # gsub("NA", "", v)
                },
                minWidth = 80,
                # maxWidth = 300,
                align = "right",
                headerStyle = list(background = "#f7f7f8")
              ),
              columns = list( .id = colDef(align = "left",
                                               style = sticky_style,
                                               headerStyle = sticky_style),
                              class = colDef(align = "left"),
                              variable = colDef(minWidth = 120)),
              wrap = FALSE,
              resizable = TRUE,
              borderless = TRUE,
              highlight = TRUE,
              striped = TRUE,
              showSortable = TRUE,
              filterable = TRUE,
              # searchable = TRUE,
              # minRows = 10,
              pagination = FALSE,
              height = 720#,
              # width = 800
    )
    
  })
  
  output$calculation <- renderReactable({
    ou <- estimate_impact()
    sp <- which.species()
    cl <- which.class()
    if(any(cl == "Farm type")) cl <- NULL
    
    df <- reshape2::melt(ldply(ou), id = c(".id", "class"))
    df <- df[!is.na(df$value),]

    levels(df$class) <- gsub("\\.|\\.\\.", " ", levels(df$class))
    
    ## Subset
    df <- df[df$class %in% cl,]
    
    if(sp == "dairy"){
      itms <- c('n1', 'n2', 'n3',
                'die1', 'die2', 'die3', 'buy1', 'buy2', 'buy3', 'cull1', 'cull2', 'cull3',
                'sell1', 'sell2', 'sell3', 'ms', 
                'profit', 
                'inc.total', 'inc.ms', 'inc.stock', 'inc.stock1', 'inc.stock2', 'inc.stock3',
                'exp.total', 'exp.feed', 'exp.breed', 'exp.ah', 'exp.lab', 'exp.purchase'
      )
      
    } else if (sp == "beef"){
      itms <- c('n.c', 'n.h2', 'n.h1', 'n.h0', 'n.s2', 'n.s1', 'n.s0', "n.b", 'n.b1', 'n.b0',
                'die.c', 'die.h2', 'die.h1', 'die.h0', 'die.s2', 'die.s1', 'die.s0', "die.b", 'die.b1', 'die.b0',
                'buy.c', 'buy.h2', 'buy.h1', 'buy.h0', 'buy.s2', 'buy.s1', 'buy.s0', "buy.b", 'buy.b1', 'buy.b0',
                'cull.c', 'cull.h2', 'cull.h1', 'cull.h0', 'cull.s2', 'cull.s1', 'cull.s0', "cull.b", 'cull.b1', 'cull.b0',
                'sell.c', 'sell.h2', 'sell.h1', 'sell.h0', 'sell.s2', 'sell.s1', 'sell.s0', "sell.b", 'sell.b1', 'sell.b0', 
                'profit', 
                'inc.total', 'inc.stock.c', 'inc.stock.h', 'inc.stock.s', 'inc.stock.b', 'inc.stock.calves',
                'exp.total', 'exp.feed', 'exp.ah', 'exp.lab', 'exp.purchase'
      )
      
      
    } else if (sp == "sheep"){
      itms <- c('n.ewe', 'n.hgt', 'n.lamb',
                'die.ewe', 'die.hgt', 'die.lamb', 'buy.hgt', 'buy.lamb', 'cull.ewe', 'cull.hgt', 'cull.lamb',
                'sell.ewe', 'sell.hgt', 'sell.lamb', 'wool', 
                'profit', 
                'inc.total', 'inc.stock', 'inc.stock.ewe', 'inc.stock.hgt', 'inc.stock.lamb', 'inc.wool',
                'exp.total', 'exp.feed', 'exp.wool', 'exp.ah', 'exp.lab', 'exp.purchase'
      )
    }

    # df <- df[df$.id != "Non-infected farm",]
    df <- df[df$variable %in% itms,]
    
    df$variable <- terminology(df$variable, species = sp, full = TRUE)
    df$.id <- factor(df$.id, levels = c("Non-infected farm", "Infected farm", "Partial Budget"), labels = c("Baseline farm (A)", "Infected farm (B)", "Difference (B-A)"))

    df$value <- ifelse(df$value <10, round(df$value, 1), round(df$value, 0))
    df.wide <- reshape2::dcast(df, class + variable ~ .id, value.var = "value")
    
    df.wide$'Difference (B-A)' <- df.wide$'Infected farm (B)' - df.wide$'Baseline farm (A)'
    
    sticky_style <- list(backgroundColor = "#f7f7f7")
    reactable(df.wide,
              defaultColDef = colDef(
                # header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value){
                  if(is.numeric(value)){
                    value <- ifelse( abs(value) < 0.001, 0, value)
                    # v <- format(value, nsmall = 0, digits = 2, scientific = FALSE, big.mark = ",")
                    v <- format(value, nsmall = 0, scientific = FALSE, big.mark = ",")
                    v
                  } else value
                  # gsub("NA", "", v)
                },
                minWidth = 80,
                # maxWidth = 300,
                align = "right",
                headerStyle = list(background = "#f7f7f8")
              ),
              columns = list( .id = colDef(align = "left",
                                           style = sticky_style,
                                           headerStyle = sticky_style),
                              class = colDef(align = "left"),
                              variable = colDef(minWidth = 120)),
              wrap = FALSE,
              resizable = TRUE,
              borderless = TRUE,
              highlight = TRUE,
              striped = TRUE,
              showSortable = TRUE,
              filterable = TRUE,
              # searchable = TRUE,
              # minRows = 10,
              pagination = FALSE,
              height = 720#,
              # width = 800
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      ou <- estimate_impact()
      sp <- which.species()
      cl <- which.class()
      if(any(cl == "Farm type")) cl <- NULL
      tb <- summary_table(ou, class = cl, species = sp, detail = is.detail())
      row.names(tb) <- NULL
      
      write.csv(tb, con, row.names = FALSE)
    }
  )
  
  output$ftype <- renderReactable({
    
    dfn0 <- farm.definition()
    reactable(dfn0,
              defaultColDef = colDef(
                cell = function(value){
                  if(is.numeric(value)){
                    v <- format(value, nsmall = 0, digits = 2, scientific = FALSE, big.mark = ",")
                    v
                  } else value
                },
                minWidth = 40,
                # maxWidth = 300,
                align = "left",
                headerStyle = list(background = "#f7f7f8")
              ),
              columns = list(Description = colDef(minWidth = 160)),
              wrap = TRUE,
              resizable = TRUE,
              borderless = TRUE,
              highlight = TRUE,
              striped = TRUE,
              showSortable = TRUE,
              filterable = TRUE,
              # searchable = TRUE,
              # minRows = 10,
              pagination = FALSE,
              height = 720
    )
    
  })

  
  output$about <- 
    renderText({
    '<h3>NZ livestock disease impact estimator</h3>
    <b>Version: </b> 05<br>
    <b>Last updated: </b> 21 February 2023<br>
    <br>
    
    <h4>Developers</h4>
    This tool was developed by EpiCentre, Massey University with funding from Diagnostic and Surveillance Service directorate, Ministry for Primary Industries NZ.
    <br><br>
    
    <h4>Background</h4>
    This tool helps estimate the costs of livestock disease for typical New Zealand farms over a year, with given disease parameters.
    The interactive feature of the tool helps obtain the possible range of disease impacts for uncertain disease parameters.<br/>

    The tool can be useful for prompt evaluation of the likely impacts of an emerging disease under limited information, or crude comparison of the impacts of endemic diseases for priority setting.
    <br><br>

    The latest (2018-20) industry bench marking data are preloaded in the tool to reflect the current average livestock performance, farming system and market values.
    <br><br>
    
    Disease parameters are related to the prevalence, mortality, reproduction performance, milk yield, price of animals, culling rate, weight gain, and additional expenses for veterinary intervention.
    
    Some example disease parameters are included in the tool, which can be used as a starting point of analysis.
    The examples include Schmallenberg virus (SBV) in UK/France dairy (Hasler et al. 2015), beef (Raboisson et al. 2014) and sheep farms (Alarcon et al. 2014), bovine viral diarrhoea (BVD), enzootic bovine leukosis (EBL), Johnes disease (JD) and neosporosis in Canadian dairy farms (Chi et al. 2002), Akabane disease in Sudanese dairy farms (Elhassan et al. 2014), and BVD in NZ dairy and beef farms (Han et al. 2020).
    <br><br>
    
    <h4>Steps</h4>
    
    1) Specify the animal type and farm types of interest.<br/>
    
    2) Select a similar disease.<br/>
    
    3) Adjust disase impact parameters.<br/>
    </br>
    
    <h4>Farm data source</h4>
    <a href="https://www.dairynz.co.nz/publications/dairy-industry/dairynz-economic-survey-2019-20/">DairyNZ</a><br>
    <a href="https://beeflambnz.com/data-tools/benchmark-your-farm">Beef+Lamb</a><br>
    <br>
    <h4>Reference</h4>
    Alarcon, P. et al. (2014), "Application of integrated production and economic models to estimate the impact of Schmallenberg virus for various sheep production types in the UK and France", Vet Rec Open, 1(1), e000036. <a href="https://bvajournals.onlinelibrary.wiley.com/doi/10.1136/vetreco-2014-000036">doi:10.1136/vetreco-2014-000036</a>
    <br><br>
    Chi, J. et al. (2002), "Direct production losses and treatment costs from bovine viral diarrhoea virus, bovine leukosis virus, Mycobacterium avium subspecies paratuberculosis, and Neospora caninum", Preventive Veterinary Medicine, 55(2), 137-53. <a href="https://www.sciencedirect.com/science/article/abs/pii/S0167587702000946?via%3Dihub">doi:https://doi.org/10.1016/S0167-5877(02)00094-6</a>
    <br><br>
    Elhassan, A. M. et al. (2014), "A Serological Survey of Akabane Virus Infection in Cattle in Sudan", ISRN Veterinary Science, 2014, 123904. <a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4060564/">doi:10.1155/2014/123904</a>
    <br><br>
    Hasler, B. et al. (2015), "Integration of production and financial models to analyse the financial impact of livestock diseases: a case study of Schmallenberg virus disease on British and French dairy farms", Veterinary Record Open, 2(1), e000035. <a href="https://bvajournals.onlinelibrary.wiley.com/doi/10.1136/vetreco-2014-000035">doi:https://doi.org/10.1136/vetreco-2014-000035</a>
    <br><br>
    Raboisson, D. et al. (2014), "Application of integrated production and economic models to estimate the impact of Schmallenberg virus for various beef suckler production systems in France and the United Kingdom", Bmc Veterinary Research, 10. <a href="https://bmcvetres.biomedcentral.com/articles/10.1186/s12917-014-0254-z">doi:10.1186/s12917-014-0254-z</a>
    <br><br>
    Han, J. et al. (2020), "Modelling the economics of bovine viral diarrhoea virus control in pastoral dairy and beef cattle herds", Preventive Veterinary Medicine, 182:105092. <a href="https://doi.org/10.1016/j.prevetmed.2020.105092">doi:10.1016/j.prevetmed.2020.105092</a>
    <br><br>
    <br><br>
    
    '
    })
  
})