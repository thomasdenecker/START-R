################################################################################
# Convert Bam coverage file for STARTR
# Jean-Charles Cadoret
# 01/2019
#
################################################################################

################################################################################
# File parameters: to be modified by the user                   
################################################################################
Exp1_cy3 = read.table("Bureau/papier_startR_analyse/bamCoverage_early.csv", header = T,sep = ",", stringsAsFactors = F)
Exp1_cy5 = read.table("Bureau/papier_startR_analyse/bamCoverage_late.csv", header = T, sep = ",", stringsAsFactors = F)
Exp1_name = "test repli-seq"


################################################################################
# Main
################################################################################

#-------------------------------------------------------------------------------
# Extraction
#-------------------------------------------------------------------------------

Exp1_cy3_extract = cbind(paste0(Exp1_cy3[,"chr"],":",Exp1_cy3[,"start"], "-", Exp1_cy3[,"end"] ), Exp1_cy3[,"gProcessedSignal"])
Exp1_cy5_extract = cbind(paste0(Exp1_cy5[,"chr"],":",Exp1_cy5[,"start"], "-", Exp1_cy5[,"end"] ), Exp1_cy5[,"rProcessedSignal"])
  
#-------------------------------------------------------------------------------
# Column rename
#-------------------------------------------------------------------------------
colnames(Exp1_cy3_extract) = c("SystematicName", "gProcessedSignal")
colnames(Exp1_cy5_extract) = c("SystematicName", "rProcessedSignal")
  
#-------------------------------------------------------------------------------
# Remove random (RANDOM_GC50_TM, chrY_random,...)
#-------------------------------------------------------------------------------
  
Exp1_cy3_extract = Exp1_cy3_extract[!grepl("random", Exp1_cy3[,"chr"],ignore.case = T),]
Exp1_cy5_extract = Exp1_cy5_extract[!grepl("random", Exp1_cy5[,"chr"],ignore.case = T),]
  
#-------------------------------------------------------------------------------
# Merge
#-------------------------------------------------------------------------------
  
Exp1_merge = merge(Exp1_cy3_extract, Exp1_cy5_extract, by="SystematicName")
  
#-------------------------------------------------------------------------------
# Write
#-------------------------------------------------------------------------------
  
write.table(Exp1_merge, paste0(Exp1_name, "_START_R.txt"), sep = "\t", quote = F, row.names= F)
  
  
  
  