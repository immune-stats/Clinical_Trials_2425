library(randomizeR)

cr20 <- crPar(N = 20)
CR20  <- getAllSeq(cr20)
#plotSeq(CR20, plotAllSeq = TRUE)
rar20 <- rarPar(20)
RAR20  <- getAllSeq(rar20)
tbd20 <- tbdPar(bc = 20)
TBD20  <- getAllSeq(tbd20)

max_imb <- imbal("maxImb")
conv_str <- corGuess("CS")
div_str <- corGuess("DS")

asCR20 <- assess(CR20, max_imb, conv_str, div_str)
asRAR20 <- assess(RAR20, max_imb, conv_str, div_str)
asTBD20 <- assess(TBD20, max_imb, conv_str, div_str)

par(mfrow = c(1,3))
par(cex       = 1.1, 
    cex.axis  = 1.4, 
    cex.lab   = 1.6, 
    cex.main  = 2.0) 
boxplot(asCR20@D$maxImb, asRAR20@D$maxImb, asTBD20@D$maxImb, names = c("CR", "RAR", "TBD"), xlab = "Randomization Procedures", ylab = "Maximum Imbalance")
boxplot(asCR20@D$`propCG(CS)`, asRAR20@D$`propCG(CS)`, asTBD20@D$`propCG(CS)`,names = c("CR", "RAR", "TBD"),xlab = "Randomization Procedures",ylab = "Correct Guess (convergence strategy)")
title("Assessment for n = 20")
boxplot(asCR20@D$`propCG(DS)`, asRAR20@D$`propCG(DS)`, asTBD20@D$`propCG(DS)`,names = c("CR", "RAR", "TBD"),xlab = "Randomization Procedures",ylab = "Correct Guess (divergence strategy)")


summary(asCR20)
summary(asRAR20)
summary(asTBD20)






cr20 <- crPar(N = 20)
CR20  <- genSeq(cr20,10000, seed = 123)
rar20 <- rarPar(20)
RAR20  <- genSeq(rar20,10000, seed = 123)
tbd20 <- tbdPar(bc = 20)
TBD20  <- genSeq(tbd20,10000, seed = 123)

max_imb <- imbal("maxImb")
conv_str <- corGuess("CS")
div_str <- corGuess("DS")

asCR20 <- assess(CR20, max_imb, conv_str, div_str)
asRAR20 <- assess(RAR20, max_imb, conv_str, div_str)
asTBD20 <- assess(TBD20, max_imb, conv_str, div_str)

par(mfrow = c(1,3))
par(cex       = 1.1, 
    cex.axis  = 1.4, 
    cex.lab   = 1.6, 
    cex.main  = 2.0) 
boxplot(asCR20@D$maxImb, asRAR20@D$maxImb, asTBD20@D$maxImb, names = c("CR", "RAR", "TBD"), xlab = "Randomization Procedures", ylab = "Maximum Imbalance")
boxplot(asCR20@D$`propCG(CS)`, asRAR20@D$`propCG(CS)`, asTBD20@D$`propCG(CS)`,names = c("CR", "RAR", "TBD"),xlab = "Randomization Procedures",ylab = "Correct Guess (convergence strategy)")
title("Assessment for n = 20")
boxplot(asCR20@D$`propCG(DS)`, asRAR20@D$`propCG(DS)`, asTBD20@D$`propCG(DS)`,names = c("CR", "RAR", "TBD"),xlab = "Randomization Procedures",ylab = "Correct Guess (divergence strategy)")


summary(asCR20)
summary(asRAR20)
summary(asTBD20)





cr150 <- crPar(N = 150)
CR150  <- genSeq(cr150,10000, seed = 123)
rar150 <- rarPar(150)
RAR150  <- genSeq(rar150,10000, seed = 123)
tbd150 <- tbdPar(bc = 150)
TBD150  <- genSeq(tbd150,10000, seed = 123)

max_imb <- imbal("maxImb")
conv_str <- corGuess("CS")
div_str <- corGuess("DS")

asCR150 <- assess(CR150, max_imb, conv_str, div_str)
asRAR150 <- assess(RAR150, max_imb, conv_str, div_str)
asTBD150 <- assess(TBD150, max_imb, conv_str, div_str)

par(mfrow = c(1,3))
par(cex       = 1.1, 
    cex.axis  = 1.4, 
    cex.lab   = 1.6, 
    cex.main  = 2.0) 
boxplot(asCR150@D$maxImb, asRAR150@D$maxImb, asTBD150@D$maxImb, names = c("CR", "RAR", "TBD"), xlab = "Randomization Procedures", ylab = "Maximum Imbalance")
boxplot(asCR150@D$`propCG(CS)`, asRAR150@D$`propCG(CS)`, asTBD150@D$`propCG(CS)`,names = c("CR", "RAR", "TBD"),xlab = "Randomization Procedures",ylab = "Correct Guess (convergence strategy)")
title("Assessment for n = 150")
boxplot(asCR150@D$`propCG(DS)`, asRAR150@D$`propCG(DS)`, asTBD150@D$`propCG(DS)`,names = c("CR", "RAR", "TBD"),xlab = "Randomization Procedures",ylab = "Correct Guess (divergence strategy)")


summary(asCR150)
summary(asRAR150)
summary(asTBD150)


