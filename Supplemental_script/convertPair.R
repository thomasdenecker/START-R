################################################################################
# Convert pair file
# Thomas DENECKER
# 01/2019
#
################################################################################

################################################################################
# File parameters: to be modified by the user                   
################################################################################

Exp1_cy3 = read.table("GSM450272_8924102_532.pair", header = T, skip = 1, sep = "\t", stringsAsFactors = F)
Exp1_cy5 = read.table("GSM450272_8924102_635.pair", header = T, skip = 1, sep = "\t", stringsAsFactors = F)
Exp1_name = "GSM450272_8924102"

Exp2_cy3 = read.table("GSM450272_8924402_532.pair", header = T, skip = 1, sep = "\t", stringsAsFactors = F)
Exp2_cy5 = read.table("GSM450272_8924402_635.pair", header = T, skip = 1, sep = "\t", stringsAsFactors = F)
Exp2_name = "GSM450272_8924402"


################################################################################
# Main
################################################################################

#-------------------------------------------------------------------------------
# Extraction
#-------------------------------------------------------------------------------

Exp1_cy3_extract = cbind(paste0(Exp1_cy3[,"SEQ_ID"],":",Exp1_cy3[,"POSITION"], "-", Exp1_cy3[,"POSITION"] ),
                         Exp1_cy3[,"PM"])

Exp1_cy5_extract = cbind(paste0(Exp1_cy5[,"SEQ_ID"],":",Exp1_cy5[,"POSITION"], "-", Exp1_cy5[,"POSITION"] ),
                         Exp1_cy5[,"PM"])


Exp2_cy3_extract = cbind(paste0(Exp2_cy3[,"SEQ_ID"],":",Exp2_cy3[,"POSITION"], "-", Exp2_cy3[,"POSITION"] ),
                         Exp2_cy3[,"PM"])

Exp2_cy5_extract = cbind(paste0(Exp2_cy5[,"SEQ_ID"],":",Exp2_cy5[,"POSITION"], "-", Exp2_cy5[,"POSITION"] ),
                         Exp2_cy5[,"PM"])

#-------------------------------------------------------------------------------
# Column rename
#-------------------------------------------------------------------------------

colnames(Exp1_cy3_extract) = c("SystematicName", "gProcessedSignal")
colnames(Exp1_cy5_extract) = c("SystematicName", "rProcessedSignal")
colnames(Exp2_cy3_extract) = c("SystematicName", "gProcessedSignal")
colnames(Exp2_cy5_extract) = c("SystematicName", "rProcessedSignal")

#-------------------------------------------------------------------------------
# Remove random (RANDOM_GC50_TM, chrY_random,...)
#-------------------------------------------------------------------------------

Exp1_cy3_extract = Exp1_cy3_extract[!grepl("random", Exp1_cy3[,"SEQ_ID"],ignore.case = T),]
Exp1_cy5_extract = Exp1_cy5_extract[!grepl("random", Exp1_cy5[,"SEQ_ID"],ignore.case = T),]
Exp2_cy3_extract = Exp2_cy3_extract[!grepl("random", Exp2_cy3[,"SEQ_ID"],ignore.case = T),]
Exp2_cy5_extract = Exp2_cy5_extract[!grepl("random", Exp2_cy5[,"SEQ_ID"],ignore.case = T),]

#-------------------------------------------------------------------------------
# Merge
#-------------------------------------------------------------------------------

Exp1_merge = merge(Exp1_cy3_extract, Exp1_cy5_extract, by="SystematicName")
Exp2_merge = merge(Exp2_cy3_extract, Exp2_cy5_extract, by="SystematicName")

#-------------------------------------------------------------------------------
# Write
#-------------------------------------------------------------------------------

write.table(Exp1_merge, paste0(Exp1_name, "_START_R.txt"), sep = "\t", quote = F, row.names= F)
write.table(Exp2_merge, paste0(Exp2_name, "_START_R.txt"), sep = "\t", quote = F, row.names= F)
