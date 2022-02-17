
# Empty environment.
rm(list=ls())

# Load libraries.
library(dplyr)
library(tidyr)


# BH Analysis and p-value adjustments.----

#--Changes in the Coefficients (Model1 to Model3).
# 1) at 0.005.
rm(list=ls())
load(file = "Output\\DataD")
# Calculating BH adjusted p-value for p-values of same coefficient across countries
# and putting in 2-dim matrix BHDataD
CountryNames <- dimnames(DataD)[[1]]
CoefNamesD <- dimnames(DataD)[[2]]
padj <- NULL
for(i in 1:length(CoefNamesD)){
  padj <- c(padj, p.adjust(DataD[1:length(CountryNames),i,3], method="BH"))
} 
BHDataD <- matrix(padj, nrow=length(CountryNames), ncol=length(CoefNamesD), dimnames=list(CountryNames, CoefNamesD))
# Calculating p-value for families
BHCountriesPerCoefD <- apply(BHDataD, 2, min)
print(noquote("p-values for families"))
print(round(BHCountriesPerCoefD,6))
# BH testing of families
BHCoefD <- p.adjust(BHCountriesPerCoefD, method="BH")
print(noquote("BH testing of families"))
print(round(BHCoefD,6))
# RCoef
RCoef <- BHCoefD[BHCoefD <= 0.005]
AdjBHCountriesPerCoefD <- BHDataD*length(CoefNamesD)/max(length(RCoef),1)
# These can be compared to .005 but first should not pass 1
AdjBHCountriesPerCoefD <- pmin(AdjBHCountriesPerCoefD,1)
# Output only for those countries where some significant exist
print(noquote("Hierarchically adjusted significance for change in Coefficients"))
print(round(AdjBHCountriesPerCoefD[,BHCoefD <= 0.005],6))
# statistically non-significant changes were zeroed out
CoefD <- DataD[,,1]
CoefD[AdjBHCountriesPerCoefD > 0.005] <- 0
print(noquote("Coefficients (Note: The coefficient corresponding to statistically non-significant changes were zeroed out)"))
print(round(CoefD,3))
# bind it to the data array.
p_bh <- as.data.frame(round(AdjBHCountriesPerCoefD[,BHCoefD <= 0.005],6))
if(length(names(p_bh)) != length(CoefNamesD)){
  exc <- CoefNamesD[!CoefNamesD %in% names(p_bh)]
  p_bh[,exc] <- NA
  p_bh <- p_bh[, CoefNamesD]
}
BHsgnfcnt_D <- round(CoefD,3)
DataD_BH <- abind::abind(DataD, p_bh, BHsgnfcnt_D)
dimnames(DataD_BH)[[3]][4:5] <- c('p_BHadjusted_005', 'D_BHsignificant_005')
save("DataD_BH", file = "Output\\DataD_BH")
DataD_BH[,,3] #p-values.
DataD_BH[,,4] #p-BHadjusted.



# 2) at 0.05.
rm(list=ls())
load(file = "Output\\DataD_BH")
# Calculating BH adjusted p-value for p-values of same coefficient across countries
# and putting in 2-dim matrix BHDataD
CountryNames <- dimnames(DataD_BH)[[1]]
CoefNamesD <- dimnames(DataD_BH)[[2]]
padj <- NULL
for(i in 1:length(CoefNamesD)){
  padj <- c(padj, p.adjust(DataD_BH[1:length(CountryNames),i,3], method="BH"))
} 
BHDataD <- matrix(padj, nrow=length(CountryNames), ncol=length(CoefNamesD), dimnames=list(CountryNames, CoefNamesD))
# Calculating p-value for families
BHCountriesPerCoefD <- apply(BHDataD, 2, min)
print(noquote("p-values for families"))
print(round(BHCountriesPerCoefD,6))
# BH testing of families
BHCoefD <- p.adjust(BHCountriesPerCoefD, method="BH")
print(noquote("BH testing of families"))
print(round(BHCoefD,6))
# RCoef
RCoef <- BHCoefD[BHCoefD <= 0.05]
AdjBHCountriesPerCoefD <- BHDataD*length(CoefNamesD)/max(length(RCoef),1)
# These can be compared to .05 but first should not pass 1
AdjBHCountriesPerCoefD <- pmin(AdjBHCountriesPerCoefD,1)
# Output only for those countries where some significant exist
print(noquote("Hierarchically adjusted significance for change in Coefficients"))
print(round(AdjBHCountriesPerCoefD[,BHCoefD <= 0.05],6))
# statistically non-significant changes were zeroed out
CoefD <- DataD_BH[,,1]
CoefD[AdjBHCountriesPerCoefD > 0.05] <- 0
print(noquote("Coefficients (Note: The coefficient corresponding to statistically non-significant changes were zeroed out)"))
print(round(CoefD,3))
# bind it to the data array.
p_bh <- as.data.frame(round(AdjBHCountriesPerCoefD[,BHCoefD <= 0.05],6))
if(length(names(p_bh)) != length(CoefNamesD)){
  exc <- CoefNamesD[!CoefNamesD %in% names(p_bh)]
  p_bh[,exc] <- NA
  p_bh <- p_bh[, CoefNamesD]
}
BHsgnfcnt_D <- round(CoefD,3)
DataD_BH <- abind::abind(DataD_BH, p_bh, BHsgnfcnt_D)
dimnames(DataD_BH)[[3]][6:7] <- c('p_BHadjusted_05', 'D_BHsignificant_05')
save("DataD_BH", file = "Output\\DataD_BH")
DataD_BH[,,3] #p-values.
DataD_BH[,,6] #p-BHadjusted.



#reshape before export.
final_out <- DataD_BH %>% as.data.frame.table() %>% 
  pivot_wider(names_from = Var3, values_from = Freq) %>% 
  mutate(p_bonf_adj = p.adjust(pValue, method = "bonferroni")) #bonferroni adjusted pvalues.











