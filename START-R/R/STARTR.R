# START-R : a Simple tool to analyse Replication Timing with R
# 2018 - CNRS - Institut Jacques Monod
# Thomas DENECKER - PhD studient - Computational biology
# thomas.denecker@gmail.com



################################################################################
# Library
################################################################################

library(DNAcopy)
library(limma)
library(SNPchip)
library(pracma)
library(data.table)
library(clusterSim)
library(car)
require(data.table)
library("htmltools")


# write.table(c(packageVersion("DNAcopy"),
#   packageVersion("limma"),
#   packageVersion("SNPchip"),
#   packageVersion("pracma"),
#   packageVersion("data.table"),
#   packageVersion("clusterSim"),
#   packageVersion("car")
#   ), "testversion.txt")
# 
# write.table(R.Version()$version.string, "rversion.txt")

################################################################################
# Functions
################################################################################

centro_Mouse = function(chromo){
  tableau = matrix(0, 20, 2)
  colnames(tableau) = c("debut", "fin")
  rownames(tableau) = c("chr1", "chr2","chr3", "chr4", "chr5", "chr6",
                        "chr7", "chr8","chr9", "chr10", "chr11", "chr12",
                        "chr13", "chr14","chr15", "chr16", "chr17", "chr18",
                        "chr19", "chrX")
  return(tableau[which(rownames(tableau)== chromo),])
}

################################################################################
# Constants
################################################################################

increment = 1
moyenne = NULL
adresse = getwd()

# Differential or not
if (e5 == TRUE){
  tour = 2  
}else {
  tour = 1  
}

################################################################################
# Main
################################################################################

#///////////////////////////////////////////////////////////////////////////////
# Reading of centromers
#///////////////////////////////////////////////////////////////////////////////

centromer = read.csv2("Inputs/centromere.csv", header = T, sep = "\t")

#///////////////////////////////////////////////////////////////////////////////
# Creating a codebook
#///////////////////////////////////////////////////////////////////////////////

codebook = NULL
codebook = rbind(codebook, c("File 1 : ",nomtotal[1]))
codebook = rbind(codebook, c("File 2 : ",nomtotal[2]))
codebook = rbind(codebook, c("File 3 : ",nomtotal[3]))
codebook = rbind(codebook, c("File 4 : ",nomtotal[4]))
codebook = rbind(codebook, c("Sortie image : ",sortie_image))
codebook = rbind(codebook, c("Bed : ",bed))
codebook = rbind(codebook, c("Intra array : ",nor1))
codebook = rbind(codebook, c("Inter replica : ",nor2))
codebook = rbind(codebook, c("Inter experience : ",nor3))
codebook = rbind(codebook, c("Smooth : ",e1))
codebook = rbind(codebook, c("TTR : ",e2))
codebook = rbind(codebook, c("Segmentation : ",e3))
codebook = rbind(codebook, c("All : ",e4))
codebook = rbind(codebook, c("Differential analysis : ",e5))
codebook = rbind(codebook, c("Span : ",v1))
codebook = rbind(codebook, c("Number of SD : ",v2))
codebook = rbind(codebook, c("padjust : ",v3))
codebook = rbind(codebook, c("Smooth method : ",v4))
codebook = rbind(codebook, c("Size : ",v5))
codebook = rbind(codebook, c("Number of lines to skip : ",fs1))
codebook = rbind(codebook, c("Column name of green signal : ",fs2))
codebook = rbind(codebook, c("Column name of red signal : ",fs3))
codebook = rbind(codebook, c("Early fraction : ",fs4))
codebook = rbind(codebook, c("Late fraction : ",fs5))
codebook = rbind(codebook, c("P-value threshold : ",pv1))
codebook = rbind(codebook, c("Window size : ",pv2))
codebook = rbind(codebook, c("Adjusted Pvalue : ",pv3))
codebook = rbind(codebook, c("Overlap : ",pv4))
codebook = rbind(codebook, c("Type de différence : ",type_dif))
codebook = rbind(codebook, c("Seuil de différence (euclydienne): ",type_dif))
codebook = rbind(codebook, c("Organism : ",organisme))

#///////////////////////////////////////////////////////////////////////////////
# Creation of analysis folder
#///////////////////////////////////////////////////////////////////////////////

# Today's date to name folder
date = format(Sys.Date(), "%Y%m%d")
dateorigine = date

# Creation of a folder with the date and according to the analyses already 
# carried out during the day
direction = paste(directory,"/", sep ="")
setwd(direction)

i = 1
if (file.exists(date) == FALSE)
  date = paste(date,"_1",sep = "")

while (file.exists(date) == TRUE){
  date = paste(dateorigine,"_",i, sep ="")
  i = i +1
}

# Create folder
dir.create(date)
#directionf = paste(directory,"/",date, sep = "")
directionf = date

# Writing the codebook in the folder
write.table(codebook, paste0(directionf,"/codebook.txt"), quote = F, 
            col.names = F, row.names = F) 

setwd(directionf)

#///////////////////////////////////////////////////////////////////////////////
# Normalisation intra and inter replicats
#///////////////////////////////////////////////////////////////////////////////

increment = 1

for (z in 1:tour){
  
  cat("Script R works...")
  
  #=============================================================================
  # Analyse preparation
  #=============================================================================
  
  #-----------------------------------------------------------------------------
  # Get filename of replicat
  #-----------------------------------------------------------------------------
  
  nom = nomtotal[increment]
  file.copy(nom, "./")
  if(increment == 1){
    file.rename("0.txt", "E1_R1.txt")
    nom = "E1_R1.txt"
  } else{
    file.rename("0.txt", "E2_R1.txt")
    nom = "E2_R1.txt"
  }
  
  nom2 = nomtotal[increment+1]
  file.copy(nom2, "./")
  if(increment == 1){
    file.rename("0.txt", "E1_R2.txt")
    nom2 = "E1_R2.txt"
  } else{
    file.rename("0.txt", "E2_R2.txt")
    nom2 = "E2_R2.txt"
  }
  
  increment = 3

  
  nomT = removeExt(nom)
  nom2T = removeExt(nom2)
  num = substring(nom,1,4) 
  
  #-----------------------------------------------------------------------------
  # Analysis of Experience 1 or 2
  #-----------------------------------------------------------------------------
  
  if ( z == 1){
    prefixe = "Experience_1"
  } else if (z == 2){
    prefixe = "Experience_2"
  }
  
  #-----------------------------------------------------------------------------
  # Creation of subfolder
  #-----------------------------------------------------------------------------
  
  dir.create(prefixe, recursive = T)
  # direction = paste(directory,"/",date,"/",prefixe, sep = "")
  direction = prefixe
  setwd(dir = direction)
  
  dir.create("Autocorrelation")
  dir.create("Chromosomes")
  dir.create("Viewer")
  if (e2 ==TRUE){
    dir.create("Segmentation")
  }
  if (e1 == TRUE){
    dir.create("Smooth")
  }
  dir.create("Normalisation")
  dir.create("File_software")
  if (e4 == TRUE){
    dir.create("Fusion")
  }
  if (e3 == TRUE){
    dir.create("TTR")
  }
  setwd("..")

  #=============================================================================
  # Normalisation                        
  #=============================================================================
  
  #-----------------------------------------------------------------------------
  # Analysis of data type
  #-----------------------------------------------------------------------------
  
  filename = paste(nomT,".txt",sep="")
  filename2 = paste(nom2T,".txt",sep="")
  tab5rows <- read.table(filename,header = T, skip = fs1, sep = "\t")
  tab5rows_sauve = tab5rows
  tab5rows2 <- read.table(filename2,header = T, skip = fs1, sep = "\t")
  classes <- sapply(tab5rows, class)
  classes2 <- sapply(tab5rows2, class)
  
  #-----------------------------------------------------------------------------
  # Data extraction
  #-----------------------------------------------------------------------------
  
  mLymph1_Cy5_Cy3 <- read.table(filename,header=T,  comment.char = "", 
                                colClasses=classes, skip = fs1, sep = "\t")
  mLymph2_Cy5_Cy3 <- read.table(filename2,header=T,  comment.char = "", 
                                colClasses=classes, skip = fs1, sep = "\t")
  
  #-----------------------------------------------------------------------------
  # Intensity extraction
  #-----------------------------------------------------------------------------
  mLymph1 <- data.frame(S_Cy5 = mLymph1_Cy5_Cy3[,fs2] , 
                        S_Cy3 = mLymph1_Cy5_Cy3[,fs3])
  
  mLymph2 <- data.frame(S_Cy5 = mLymph2_Cy5_Cy3[,fs2] , 
                        S_Cy3 = mLymph2_Cy5_Cy3[,fs3])
  
  nom = sapply(strsplit(basename(nom),"\\."), 
               function(x) paste(x[1:(length(x)-1)], collapse="."))
  
  nom2 = sapply(strsplit(basename(nom2),"\\."), 
                function(x) paste(x[1:(length(x)-1)], collapse=".")) 
  
  #-----------------------------------------------------------------------------
  # Save in new format
  #-----------------------------------------------------------------------------

  filename = paste(prefixe,"/File_software/",nom,".rgl.txt",sep="")
  write.table(mLymph1,file=filename, row.names=F, quote=F, sep="\t", eol="\r\n")
  
  if(nom == nom2){
    filename2 = paste(prefixe,"/File_software/",nom2,"2.rgl.txt",sep="")
  }else {
    filename2 = paste(prefixe,"/File_software/",nom2,".rgl.txt",sep="")
  }
  write.table(mLymph2,file=filename2, row.names=F, quote=F, sep="\t", 
              eol="\r\n")
  
  #-----------------------------------------------------------------------------
  # Prepare input file for normalisation
  #-----------------------------------------------------------------------------

  tab_T = rbind(c("SlideNumber","Name","FileName","Cy3","Cy5"),
                c("1", "mLymph1", filename,	"late",	"early"),
                c("2",	"mLymph2",filename2, "late","early"))
  
  filenameT = paste(prefixe,"/File_software/replicat.csv", sep ="")
  
  write.table(tab_T,file=filenameT, row.names=F, quote=F, 
              sep="\t", eol="\r\n", col.names = F)
  
  ##### Eviter une lecture possible?
  t = read.csv2(filenameT, sep="\t", header = T)
  
  #-----------------------------------------------------------------------------
  # Read microarray data
  #-----------------------------------------------------------------------------
  
  if (fs4 == "Cy3" & fs5 == "Cy5" ){
    r = read.maimages(t, source="generic",columns=list(R="S_Cy5", G="S_Cy3"))
  }
  
  if (fs4 == "Cy5" & fs5 == "Cy3" ){
    r = read.maimages(t, source="generic",columns=list(R="S_Cy3", G="S_Cy5"))
  }
  
  #-----------------------------------------------------------------------------
  # Perform intra normalization
  #-----------------------------------------------------------------------------
  
  MA.l = normalizeWithinArrays(r, method=nor1) 
  
  #-----------------------------------------------------------------------------
  # Perform inter replica normalization
  #-----------------------------------------------------------------------------

  MA.q = normalizeBetweenArrays(MA.l, method=nor2)
  
  #-----------------------------------------------------------------------------
  # Graphical representation of normalisation
  #-----------------------------------------------------------------------------

  if(sortie_image == "Yes"){
    filename = paste( prefixe,"/Normalisation/",nom,"_normalisation.pdf", 
                      sep ="")
  
    pdf(filename)
    
    par(oma = c(0, 1, 4, 0), mfrow = c(3, 1), mex = 0.7)
    # Raw data
    plotDensities(r, main = "Raw data", legend = "topright")
    
    # After within-array normalization
    plotDensities(MA.l, main = paste("After within-array normalization (Method :", nor1,")"),  legend = "topright") 
    
    # After between-array normalization
    plotDensities(MA.q, main =paste( "After between-array normalization (Method :",nor2 ,")"),  legend = "topright")
    mtext("Distribution of spot intensities", side = 3, line = 1, 
          outer = TRUE, cex = 1.5)
    
    dev.off()
  }
  
  if(sortie_image == "Yes"){
    # Dependence of timing ratios on signeal intensity 
    filename = paste( prefixe,"/Normalisation/",nom,"_ratio_intensity.pdf", 
                      sep ="")
    
    pdf(filename)
    
    par(oma = c(0, 0, 3, 0), mfrow = c(2, 1), mex = 0.7)
    # Raw data, replicate 1
    plotMA(r, array=1, main = "Raw data") 
    # After within-array normalization
    plotMA(MA.l, array=1, main = "After within-array normalization")
    mtext("Dependence of timing ratios on signal intensities", side = 3, 
          line = 1, outer = TRUE, cex = 1.5)
    
    dev.off()
  }

  if(sortie_image == "Yes"){
    ## BOXPLOT
    filename = paste( prefixe,"/Normalisation/",nom,"_boxplot.pdf", sep ="")
    
    pdf(filename)
    
    par(oma = c(0, 0, 3, 0), mfrow = c(2, 1), mex = 0.7)
    boxplot(MA.l$M~col(MA.l$M), main = "Perform loess normalization", 
            border = "dodgerblue4", col = "cornflowerblue")
    boxplot(MA.q$M~col(MA.q$M), main = "Perform scale normalization", 
            border = "dodgerblue4", col = "cornflowerblue")
    mtext("Distribution replication timing values", side = 3, line = 1, 
          outer = TRUE, cex = 1.5)
    
    dev.off()
  }
  
  #-----------------------------------------------------------------------------
  # File of normalisation
  #-----------------------------------------------------------------------------
  filename = paste(prefixe,"/Normalisation/",nom,"_normalise.txt",sep="")
  write.table(MA.q$M, file = filename, quote=F, row.names=F, sep="\t")
  
  #=============================================================================
  # Position                           
  #=============================================================================
  
  #-----------------------------------------------------------------------------
  # Reading intermediate files
  #-----------------------------------------------------------------------------
  ##### Eviter une lecture possible?
  filename = paste(prefixe,"/Normalisation/",nom,"_normalise.txt",sep="")
  tab5rows = read.table(filename, header = T, nrows = 5)
  classes = sapply(tab5rows, class)
  RT = read.table(filename, header=T, comment.char = "", colClasses=classes)
  
  #-----------------------------------------------------------------------------
  # Reading original files
  #-----------------------------------------------------------------------------
  filename = paste(nomT,".txt",sep="")
  tab5rows = tab5rows_sauve 
  classes = sapply(tab5rows, class)
  a = read.table(filename, header=T, comment.char = "",colClasses=classes, 
                 skip = fs1, sep = "\t")
  
  #-----------------------------------------------------------------------------
  # Chromosome selection only
  #-----------------------------------------------------------------------------
  
  vect = NULL
  for ( i in 1:dim(a)[1]){
    if (substr(a$SystematicName[i], 1,3) != "chr"){
      vect = c(vect, i)
    }
    progress = (i/dim(a)[1]) * 100
    cat("Progression : ", progress,"%\n") 
  }
  if ( is.null(vect) == FALSE){
    RT = RT[-vect,]
    a = a[- vect, ]
  }
  
  x = strsplit(as.character(a$SystematicName), ":")
  xb = strsplit(as.character(a$SystematicName), "-")
  y = unlist(x)
  yb = unlist(xb)
  y1 = y[c(TRUE, FALSE)] 
  y2 = yb[c(FALSE,TRUE)] 
  
  y3 = y[c(FALSE, TRUE)]
  y3b = strsplit(as.character(y3), "-")
  y3b = unlist(y3b)
  y4 = y3b[c(TRUE, FALSE)] 
  y4 = as.numeric(y4)
  y2 = as.numeric(y2)

  RT = data.frame(CHR=y1, POSITION=y4, RT, stringsAsFactors=F)
  RT = RT[order(RT$CHR, RT$POSITION),]
  
  RT = na.omit(RT)
  RTS = as.data.frame(RT) 
  
  names(RT)[3:4] = c("mLymphR1", "mLymphR2")
  RT$mLymphAve = (RT[,3] + RT[,4])/2  
  filename = paste(prefixe,"/File_software/",nom,"_average.txt",sep="")
  write.table(RT,filename, row.names=F, quote=F, sep="\t")
  
  #=============================================================================
  # Autocorrelation                         
  #=============================================================================
  
  num = substring(nom,1,4) 
  if(sortie_image == "Yes"){
    ## enregistrer en image d'autocorrelation
    filename = paste(prefixe,"/Autocorrelation/autocorrelation_",num,".pdf", 
                     sep="")
    pdf(filename)
    par(mfrow=c(3,1))
    acf(RT[,3],lag=1000, main = "Replica 1")$acf[2]
    acf(RT[,4],lag=1000, main = "Replica 2")$acf[2] 
    acf(RT$mLymphAve, lag=1000, main = "Average two replica")$acf[2]
    dev.off() 
  }
}

#///////////////////////////////////////////////////////////////////////////////
# Normalisation between experiences
#///////////////////////////////////////////////////////////////////////////////

if (e5 == TRUE){
  #=============================================================================
  # Get data of Experience 1 
  #=============================================================================
  prefixe = "Experience_1"
  nom = "E1_R1.txt"
  nom = sapply(strsplit(basename(nom),"\\."), 
               function(x) paste(x[1:(length(x)-1)], collapse=".")) 
  data_a_nor1 = read.table(paste(prefixe,"/File_software/",nom,"_average.txt",
                                 sep=""), header = T)
  
  #=============================================================================
  # Get data of Experience 2 
  #=============================================================================
  prefixe = "Experience_2"
  nom = "E2_R1.txt"
  nom = sapply(strsplit(basename(nom),"\\."), 
               function(x) paste(x[1:(length(x)-1)], collapse=".")) 
  data_a_nor2 = read.table(paste(prefixe,"/File_software/",nom,"_average.txt",
                                 sep=""), header = T)
  
  #=============================================================================
  # Normalisation
  #=============================================================================
  RTT = as.data.frame(cbind(data_a_nor1$mLymphAve,data_a_nor2$mLymphAve))
  RTTn = data.Normalization(RTT, normalization="column", type=nor3)

  data_a_nor1$mLymphAve = RTTn$V1
  data_a_nor2$mLymphAve = RTTn$V2
  
  #=============================================================================
  # Write normalized data of Experience 1 
  #=============================================================================
  prefixe = "Experience_1"
  nom = "E1_R1.txt"
  
  nom = sapply(strsplit(basename(nom),"\\."), 
               function(x) paste(x[1:(length(x)-1)], collapse=".")) 
  filename = paste(prefixe,"/File_software/",nom,"_average.txt",sep="")
  write.table( data_a_nor1,filename, row.names=F, quote=F, sep="\t")
  
  #=============================================================================
  # Write normalized data of Experience 2 
  #=============================================================================
  prefixe = "Experience_2"
  nom = "E2_R1.txt"
  nom = sapply(strsplit(basename(nom),"\\."), 
               function(x) paste(x[1:(length(x)-1)], collapse=".")) 
  filename = paste(prefixe,"/File_software/",nom,"_average.txt",sep="")
  write.table( data_a_nor2,filename, row.names=F, quote=F, sep="\t")

}

#///////////////////////////////////////////////////////////////////////////////
# Smooth - TTR - CTR
#///////////////////////////////////////////////////////////////////////////////

increment = 1

for (z in 1 : tour){
  
  #=============================================================================
  # Analysis of Experience 1 or 2
  #=============================================================================
  
  if ( z == 1){
    prefixe = "Experience_1"
  } else if (z == 2){
    prefixe = "Experience_2"
  }
  
  #=============================================================================
  # Get replica filename
  #=============================================================================
  
  if(increment == 1){
    nom = "E1_R1.txt"
    nom2 = "E1_R2.txt"
  } else{
    nom = "E2_R1.txt"
    nom2 = "E2_R2.txt"
  }

  
  increment = 3
  
  
  nom = sapply(strsplit(basename(nom),"\\."), 
               function(x) paste(x[1:(length(x)-1)], collapse=".")) 
  nom2 = sapply(strsplit(basename(nom2),"\\."), 
                function(x) paste(x[1:(length(x)-1)], collapse=".")) 
  num = substring(nom,1,4) 
  RT = read.table(paste(prefixe,"/File_software/",nom,"_average.txt",sep=""), 
                  header = T)
  
  #=============================================================================
  # SMOOTH
  #=============================================================================
  
  if (e1 == TRUE){
    # Sort by chromosome then by
    RT = RT[order(RT$CHR, RT$POSITION),]
    RT = na.omit(RT) 
    RTS = as.data.frame(RT) 
    RTL = as.data.frame(RT$CHR)
    chrs = levels(RTL[,1])
    motelim = c("chr11_gl000202_random", "chr17_gl000204_random", 
                "chr17_gl000205_random","chr19_gl000209_random", 
                "chr1_gl000192_random", "chr4_gl000193_random",
                "chr7_gl000195_random", "chr9_gl000198_random", "chrM")
    
    # Remove random chromosomes
    chrs2 = chrs[!(chrs %in% motelim)] 
    
    str(chrs) 
    AllLoess = NULL 
    RT = na.omit(RT)
    model_loess = NULL
    
    for (chr in chrs2) {
      cat("Current chromosome Lissage: ", chr,"\n") 
      if (organisme == "Human"){
        centro = centromere(chr,"hg18")
      }else{
        centro = centro_Mouse(chr)
      }
        
      RTbinter = subset(RT, RT$CHR == chr)
      # Extraction chromosomes
      filename = paste(prefixe,"/Chromosomes/",chr,".txt",sep="")
      write.table(RTbinter, filename, row.names=F,quote=F, sep="\t")
      
      for (j in c(1,2)){
        RTb = subset(RT, RT$CHR == chr)
        if (j == 1){
          ligne = which(RTb$POSITION < centro[j])
        }else{
          ligne = which(RTb$POSITION > centro[j])
        }
        RTb = RTb[ligne, ]
        if( dim(RTb)[1] != 0){
          RTl = NULL 
          if (v4 != "Loess"){
            p_mov = v5
            methodo = v4
            inter = c(RTb$mLymphR1, rep (RTb$mLymphR1[length(RTb$mLymphR1)], 
                                         p_mov/2))
            RTla = movavg(inter, p_mov, methodo)
            RTla = RTla[-(1:p_mov/2)]
            
            inter = c(RTb$mLymphR2, rep (RTb$mLymphR2[length(RTb$mLymphR2)], 
                                         p_mov/2))
            RTlb =  movavg( inter, p_mov, methodo)
            RTlb = RTlb[-(1:p_mov/2)]
            
            inter = c(RTb$mLymphAve, rep (RTb$mLymphAve[length(RTb$mLymphAve)], 
                                          p_mov/2))
            RTlc =  movavg(inter, p_mov, methodo)
            RTlc = RTlc[-(1:p_mov/2)]
            RTl = data.frame(CHR=RTb$CHR,POSITION=RTb$POSITION, RTla,RTlb, RTlc)
            RTl[, (dim(RTl)[2]+1)] = j
            AllLoess = rbind(AllLoess, RTl)
            
          } else {
            lspan = v1/(max(RTb$POSITION)-min(RTb$POSITION)) 
            RTla = loess(RTb$ mLymphR1~ RTb$POSITION, span = lspan) 
            RTlb = loess(RTb$ mLymphR2~ RTb$POSITION, span = lspan)
            RTlc = loess(RTb$mLymphAve ~ RTb$POSITION, span = lspan)
            RTl = data.frame(CHR=RTb$CHR,POSITION=RTb$POSITION, 
                             RTla$fitted,RTlb$fitted, RTlc$fitted)
            RTl[, (dim(RTl)[2]+1)] = j
            AllLoess = rbind(AllLoess, RTl)
          }
        }
      }
      
      RTc = subset(RT, CHR == chr) 
      LSc = subset(AllLoess, CHR == chr) 
      LSc1 = LSc[which(LSc$V6==1), ]
      LSc2 = LSc[which(LSc$V6==2), ]
      
      #-------------------------------------------------------------------------
      # Write Smooth data
      #-------------------------------------------------------------------------
      filename = paste(prefixe,"/Chromosomes/Loess_",chr,".txt",sep="")
      write.table(LSc, filename, row.names=F,quote=F, sep="\t") 
      
      #-------------------------------------------------------------------------
      # Plot preparation
      #-------------------------------------------------------------------------
      size = dim(RTc)[2]+1
      
      RTc[RTc$mLymphR1 < 0,size] =  "gray87" #  "green"
      RTc[RTc$mLymphR1 > 0,size] =  "gray87" #  "red"
      
      RTc[RTc$mLymphR2 < 0,size +1] =  "gray87" #  "green"
      RTc[RTc$mLymphR2 > 0,size +1] =  "gray87" #  "red"
      
      RTc[RTc$mLymphAve < 0,size +2] = "gray87" #  "green"
      RTc[RTc$mLymphAve > 0,size +2] = "gray87" #  "red"
      
      names(LSc1)[3:5] = c("x300smo_mLymphR1", "x300smo_mLymphR2",
                           "x300smo_mLymphAve")
      
      names(LSc2)[3:5] = c("x300smo_mLymphR1", "x300smo_mLymphR2",
                           "x300smo_mLymphAve")
      #-------------------------------------------------------------------------
      # Smooth graphical output
      #-------------------------------------------------------------------------
        
      if(sortie_image == "Yes"){  
        filename = paste(prefixe,"/Smooth/Smooth_",chr,"_",num,".pdf",sep="")
        pdf(filename)
        
        par(mar=c(2.2,5.1,1,1), mfrow=c(3,1), pch=19, cex=0.5, cex.lab=1.8, 
            xaxs="i", oma = c(1, 0, 5, 0), mex = 0.7) 
        
        plot(RTc$mLymphR1~RTc$POSITION, ylab="Replica 1", xaxt="n", 
             col=RTc[,size], cex = 0.8) 
        lines(LSc1$x300smo_mLymphR1~LSc1$POSITION, col="blue3", lwd=3) 
        lines(LSc2$x300smo_mLymphR1~LSc2$POSITION, col="blue3", lwd=3)
        abline(h=0, col="black", lty = "dotted")
        
        plot(RTc$mLymphR2~RTc$POSITION, ylab="Replica 2", xaxt="n", 
             col=RTc[,size + 1], cex = 0.8) 
        lines(LSc1$x300smo_mLymphR2~LSc1$POSITION, col="blue3", lwd=3) 
        lines(LSc2$x300smo_mLymphR2~LSc2$POSITION, col="blue3", lwd=3) 
        abline(h=0, col="black", lty = "dotted")
        
        plot(RTc$mLymphAve~RTc$POSITION, xlab="Coordinate (bp)",ylab="Average", 
             col=RTc[,size + 2], cex = 0.8)
        lines(LSc1$x300smo_mLymphAve~LSc1$POSITION, col="blue3", lwd=3)
        lines(LSc2$x300smo_mLymphAve~LSc2$POSITION, col="blue3", lwd=3)
        abline(h=0, col="black", lty = "dotted")
        mtext(paste("Loess smoothing curves (",chr,")",sep = ""), side = 3, 
              line = 1, outer = TRUE, cex = 1.5)
        
        dev.off()
      }
    } 
    
    #---------------------------------------------------------------------------
    # Write all Smooth data
    #---------------------------------------------------------------------------
    x = as.data.frame(AllLoess)
    names(x)[3:5] = c("x300smo_mLymphR1", "x300smo_mLymphR2",
                      "x300smo_mLymphAve")
    filename = paste(prefixe,"/Smooth/",nom,"_Smooth.txt",sep="")
    write.table(x, filename, row.names=F,quote=F, sep="\t")
    
    # Data correlation
    cor(x[,c(3:5)])
  }
  
  #=============================================================================
  # TTR 
  #=============================================================================
  
  if (e4 == TRUE){
    Sauvegarde_RT = RT
    tab_coord_TTR = NULL
    tab_seg_glob = NULL
    sum = NULL

    chrs2 = chrs2[!chrs2 == "chrY"]
    for (chr in chrs2){
      cat("Current chromosomeTTR: ", chr,"\n")
      if (organisme == "Human"){
        centro = centromere(chr,"hg18")
      }else{
        centro = centro_Mouse(chr)
      }
      RTbAll = NULL
      PxAll = NULL
      RTseg = NULL
      colorAll = NULL
      diff_annot = 0
      compt_place = 1
      
      start = 1
      end = 0
      numTTR = 0
      vectnumTTR = NULL
      fut_seg = NULL
      fut_TTR = NULL
      
      for (j in 1:2){
        RTb = subset(RT, RT$CHR == chr)
        if (j == 1){
          RTb = RTb[which(RTb$POSITION < centro[j]),]
        }else{
          RTb = RTb[which(RTb$POSITION > centro[j]),]
        }
        if( dim(RTb)[1] != 0){ 
          lspan = v1/(max(RTb$POSITION)-min(RTb$POSITION))
          fit = loess(RTb$mLymphAve ~ RTb$POSITION, span = lspan) 
          px <- predict(fit, newdata=RTb$POSITION)
          #px1 <- diff(px )

          px1 <- px[-1] / px[-length(px)] ############################ / or -
          b = boxplot(px1, plot = F)
          dif = c(which( px1 < b$stats[1,1]), which(px1 > b$stats[5,1]))
          dif = sort(dif)
          
          # Test
          color = rep("white", length(px))
          color[dif] = "gold"
          
          succes = 3
          for (suc in 1:succes){
            recherche = rle(color)
            while (length(which(recherche$lengths <= suc))) {
              compt = 0
              for(r in 1:length(recherche$lengths)){
                compt = compt + recherche$lengths[r]
                if (recherche$lengths[r] <= suc){
                  if(recherche$values[r] == "gold")
                    color[(compt-suc+1):compt] = "white"
                  else
                    color[(compt-suc+1):compt] = "gold"
                }
              }
              recherche = rle(color)
            }
          }
          
          taille = (dim(RTb)[2]+1)
          RTb[which(color == "white"),taille] = 0 # pas TTR
          RTb[which(color == "gold"), taille] = 1 # TTR
          RTbAll = rbind(RTbAll,RTb)
          PxAll = c(PxAll, px)
          RTseg  = rbind(RTseg, RTb[which(color == "white"),])
          
          recherche = rle(color)
          for( r in 1:length(recherche$lengths)){
            end = end + recherche$lengths[r]
            if(recherche$values[r] == "white"){
              fut_seg = rbind(fut_seg, c(chr,  start, end))
            }
            if(recherche$values[r] == "gold"){
              numTTR = numTTR + 1
              vectnumTTR = c(vectnumTTR, rep(numTTR, recherche$lengths[r]))
              fut_TTR = rbind(fut_TTR, c(chr,  start, end))
            }
            start = start + recherche$lengths[r]
          }    
        } 
      }
      
      #=========================================================================
      # CTR 
      #=========================================================================
      
      if (e2 == TRUE && is.null(RTbAll) == F){
        print("In seg")
        fut_seg = as.data.frame(fut_seg)
        names(fut_seg) = c("CHR", "loc.start", "loc.end")
        fut_seg[,2] = strtoi(fut_seg[,2]) #str to int
        fut_seg[,3] =  strtoi(fut_seg[,3])
        
        Seg.mLymph = NULL
        for (i in 1: dim(fut_seg)[1]){
          mLymph = CNA(RTbAll$mLymphAve[fut_seg[i,2]:fut_seg[i,3]], 
                       RTbAll$CHR[fut_seg[i,2]:fut_seg[i,3]], 
                       RTbAll$POSITION[fut_seg[i,2]:fut_seg[i,3]], 
                       data.type="logratio", sampleid = "mLymph")
          s = segment(mLymph,  nperm=10000, alpha=1e-15,undo.splits="sdundo", 
                      undo.SD= v2, verbose=2)
          Seg.mLymph <- rbind(Seg.mLymph,cbind(s$output$loc.start, 
                                               s$output$seg.mean,
                                               s$output$loc.end,
                                               s$output$seg.mean))
        }
        colnames(Seg.mLymph) = c("loc.start","seg.mean","loc.end","seg.mean")
        Seg.mLymph = as.data.frame(Seg.mLymph)
        
        #-----------------------------------------------------------------------
        # CTR - Graphical outputs
        #-----------------------------------------------------------------------
        
        aff = which(RTbAll[,dim(RTbAll)[2]] == 1)
        col_dif = rep("firebrick3", length(PxAll[-aff]))
        col_dif[which(PxAll[-aff]< 0)] = "forestgreen"

        if(sortie_image == "Yes"){
          filename = paste(prefixe,"/Segmentation/seg_",chr,"_",num,".pdf",
                           sep="")
          pdf(filename)
          plot(RTbAll$mLymphAve~RTbAll$POSITION,
               main = paste("Segmentation in ",chr, sep = ""), 
               xlab="Coordinate (bp)", ylab="mLymph ave", cex = 0.5, pch = 16, 
               col = "gray80")
          segments(Seg.mLymph$loc.start, Seg.mLymph$seg.mean,Seg.mLymph$loc.end,
                   Seg.mLymph$seg.mean, col = "darkblue", lwd = 3)
          legend("topleft", "Early", horiz = F,
                 cex=1, bg = "white", bty = "n", text.col = "firebrick3", 
                 text.font = 2)
          legend("bottomleft", "Late", horiz = F,
                 cex=1, bg = "white", bty = "n", text.col = "forestgreen",
                 text.font = 2)
          abline(h=0, col="black", lty = "dotted")
          dev.off()
        }
        
      } 
      
      #-------------------------------------------------------------------------
      # TTR - Graphical outputs
      #-------------------------------------------------------------------------
      if (e4 == TRUE && is.null(RTbAll) == F){ 
        
        aff = which(RTbAll[,dim(RTbAll)[2]] == 1)
        col_dif = rep("firebrick3", length(PxAll[-aff]))
        col_dif[which(PxAll[-aff]< 0)] = "forestgreen"
        
        if(sortie_image == "Yes"){
          filename = paste(prefixe,"/TTR/TTR_",chr,"_",num,".pdf",sep="")
          pdf(filename)
          
          plot(RTbAll$mLymphAve~RTbAll$POSITION,
               main = paste("Timing Transition Region (TTR) in ",chr, sep = ""), 
               xlab="Coordinate (bp)", ylab="mLymph ave", cex = 0.5, pch = 16, 
               col = "gray80")
          points(RTbAll$POSITION[-aff],PxAll[-aff], col = col_dif, pch= 16)
          points(RTbAll$POSITION[aff],PxAll[aff], col = "gold", pch= 16)
          legend("topleft", "Early", horiz = F,
                 cex=1, bg = "white", bty = "n", text.col = "firebrick3", 
                 text.font = 2)
          legend("bottomleft", "Late", horiz = F,
                 cex=1, bg = "white", bty = "n", text.col = "forestgreen",
                 text.font = 2)
          legend("topright", c("Late","Early","TTR"),
                 cex=0.8, bg = "white", 
                 fill = c("forestgreen", "firebrick3" , "gold"), 
                 border = c("forestgreen", "firebrick3" , "gold"),
                 title = "Legends", title.adj = 0.5,  bty = "n")
          abline(h=0, col="black", lty = "dotted")
          
          dev.off()
        }
        
      } 

      #=========================================================================
      # TTR and CTR combinaison 
      #=========================================================================

      if (e3 == TRUE && is.null(RTbAll) == F){
        if(sortie_image == "Yes"){
          filename = paste(prefixe,"/Fusion/Fusion_",chr,"_",num,".pdf",sep="")
          pdf(filename)
          plot(RTbAll$mLymphAve~RTbAll$POSITION,main = 
                 paste("Timing Transition Region (TTR) & segmentation in ",
                       chr, sep = ""), 
               xlab="Coordinate (bp)", ylab="mLymph ave", cex = 0.5, pch = 16, 
               col = "gray80", xlim = c(0,10000000))
          points(RTbAll$POSITION[-aff],PxAll[-aff], col = col_dif, pch= 16)
          points(RTbAll$POSITION[aff],PxAll[aff], col = "gold", pch= 16)
          segments(Seg.mLymph$loc.start, Seg.mLymph$seg.mean,Seg.mLymph$loc.end,
                   Seg.mLymph$seg.mean, col = "darkblue", lwd = 3)
          
          legend("topleft", "Early", horiz = F,
                 cex=1, bg = "white", bty = "n", text.col = "firebrick3", 
                 text.font = 2)
          legend("bottomleft", "Late", horiz = F,
                 cex=1, bg = "white", bty = "n", text.col = "forestgreen",
                 text.font = 2)
          legend("topright", c("Late","Early","TTR"),
                 cex=0.8, bg = "white", 
                 fill = c("forestgreen", "firebrick3" , "gold"), 
                 border = c("forestgreen", "firebrick3" , "gold"),
                 title = "Legends", title.adj = 0.5,  bty = "n")
          
          abline(h=0, col="black", lty = "dotted")
          dev.off()
        }
        
      }

      if (e2 == TRUE){
        #-----------------------------------------------------------------------
        # Write segmentation informations
        #-----------------------------------------------------------------------
        
        chrom = Seg.mLymph
        filename=paste(prefixe,"/Segmentation/Segmentation_repartition_",
                       chr,"_",num,".pdf",sep="")
        sum1 = c(summary(chrom$seg.mean), nbr = dim(chrom)[1])
        sum = rbind(sum, chr = sum1)
        filename = paste(prefixe,"/Chromosomes/Segmentation_",chr,".txt",sep="")
        write.table (Seg.mLymph, filename , quote=F, row.names=F, sep="\t")
        
        #-----------------------------------------------------------------------
        # Graphical output of segmentation
        #-----------------------------------------------------------------------

        Lymph =  Seg.mLymph
        Lymph$size = Lymph$loc.end - Lymph$loc.start 
        LymphEarly = subset(Lymph, Lymph$seg.mean > 0) 
        LymphLate = subset(Lymph, Lymph$seg.mean < 0)
        
        if(sortie_image == "Yes"){
          filename=paste(prefixe,"/Segmentation/boxplot",chr,".pdf",sep="")
          pdf(filename)
          boxplot(LymphEarly$size, LymphLate$size, names = c("Early", "Late"),
                  main = "Distribution of early/late domain sizes",
                border = "dodgerblue4", col = "cornflowerblue", outline = F)
          dev.off()
        }
      }
      
      #-------------------------------------------------------------------------
      # Write position of TTR and CTR
      #-------------------------------------------------------------------------
      
      if (is.null(RTbAll) == F){
        #.......................................................................
        # TTR
        #.......................................................................
        
        filename = paste(prefixe,"/Chromosomes/TTR_",chr,".bed.bed",sep="")
        
        tableau_inter_TTR = cbind(RTbAll$POSITION[aff],PxAll[aff], vectnumTTR)
        Final_TTR = NULL
        for( deroul in 1:max(unique(tableau_inter_TTR[,3]))){
          subset_ttr = tableau_inter_TTR[tableau_inter_TTR[,3] == deroul,]
          Final_TTR = rbind(Final_TTR, c(subset_ttr[1,1], 
                                         subset_ttr[nrow(subset_ttr),1]))
        }
        
        write.table (Final_TTR, filename , quote=F, row.names=F,
                     col.names = F, sep="\t")
        filename = paste(prefixe,"/Chromosomes/TTR_all.bed",sep="")
        
        Final_TTR_chr = cbind(rep(chr, nrow(Final_TTR)), Final_TTR)
        write.table (Final_TTR_chr, filename , append=TRUE,quote=F, 
                     row.names=F,col.names = F, sep="\t")
        
        
        filename = paste(prefixe,"/Chromosomes/TTR_",chr,".bed",sep="")
        write.table (cbind(RTbAll$POSITION[aff],PxAll[aff], vectnumTTR), 
                     filename , quote=F, row.names=F,col.names = F, sep="\t")
        
        #.......................................................................
        # CTR or Not TTR
        #.......................................................................
        
        filename = paste(prefixe,"/Chromosomes/NTTR_",chr,".bed",sep="")
        write.table (cbind(RTbAll$POSITION[-aff],PxAll[-aff]), filename , 
                     quote=F, row.names=F, sep="\t")
        filename = paste(prefixe,"/Chromosomes/Segmentation_",chr,".bed",sep="")
        write.table (Seg.mLymph, filename , quote=F, row.names=F, sep="\t")
        
        ### modif
        if(!is.null(RTbAll$POSITION[aff])){
          tab_coord_TTR = rbind(tab_coord_TTR,RTbAll$POSITION[aff])
          tab_seg_glob = rbind(tab_seg_glob, 
                               cbind(CHR = rep(chr, nrow(Seg.mLymph)),Seg.mLymph)) 
        }
      }

      #=========================================================================
      # START-R viewer
      #=========================================================================
      
      #-------------------------------------------------------------------------
      # Read Data
      #-------------------------------------------------------------------------
      All_data = read.table(paste(prefixe,"/Chromosomes/", chr,".txt", 
                                  sep = ""), header = T)
      Loess_data = read.table(paste(prefixe,"/Chromosomes/Loess_", chr,".txt", 
                                    sep = ""), header = T)
      Seg_data = read.table(paste(prefixe,"/Chromosomes/Segmentation_", chr,".bed", 
                                  sep = ""), header = T)
      TTR_data = read.table(paste(prefixe,"/Chromosomes/TTR_", chr,".bed", 
                                  sep = ""), header = F)
      NTTR_data = read.table(paste(prefixe,"/Chromosomes/NTTR_",chr,".bed", 
                                   sep = ""), header = T)
      
      size = dim(All_data)[2]+1
      All_data[All_data$mLymphR1 < 0,size] = "gray87" # "green"
      All_data[All_data$mLymphR1 > 0,size] = "gray87" # "red"
      
      All_data[All_data$mLymphR2 < 0,size +1] = "gray87" # "green"
      All_data[All_data$mLymphR2 > 0,size +1] = "gray87" # "red"
      
      All_data[All_data$mLymphAve < 0,size +2] = "gray87" # "green"
      All_data[All_data$mLymphAve > 0,size +2] = "gray87" # "red"
      
      pos_seg = matrix("NaN",length(Seg_data$loc.start), 3)
      pos_seg[,1] = Seg_data$loc.start
      pos_seg[,2] = Seg_data$loc.end
      pos_seg = as.vector(t(pos_seg))
      
      moyen = matrix("NaN",length(Seg_data$seg.mean), 3)
      moyen[,1] = Seg_data$seg.mean
      moyen[,2] = Seg_data$seg.mean
      moyen = as.vector(t(moyen))
      
      couleur = as.character(TTR_data$V6[!duplicated(TTR_data$V6)])
      TTR = TTR_data[which(TTR_data$V6 == "gold"),]
      
      names(Loess_data)[3:5] = c("mLymphR1", "mLymphR2","mLymphAve")
      names(TTR_data)= c("POSITION","mLymphAve" )
      names(NTTR_data) = c("POSITION","mLymphAve" )
      
      NTTR_inf = NTTR_data[which(NTTR_data$mLymphAve < 0), ]
      NTTR_sup = NTTR_data[which(NTTR_data$mLymphAve > 0), ]
      
      Loess_data1 = Loess_data[which(Loess_data[,6] == 1 ), ]
      Loess_data2 = Loess_data[which(Loess_data[,6] == 2 ), ]
      
      Loessm = c( Loess_data1$mLymphAve,"NaN",  Loess_data2$mLymphAve)
      Loessp = c( Loess_data1$POSITION, "NaN",  Loess_data2$POSITION)
      
      Loess_data3 = Loess_data1[which(Loess_data[,6] == 1 ), ]
      Loess_data4 = Loess_data1[which(Loess_data[,6] == 2 ), ]
      Loessm2 = c( Loess_data3$mLymphAve, "NaN",  Loess_data4$mLymphAve)
      Loessp2 = c( Loess_data3$POSITION, "NaN",  Loess_data4$POSITION)
      
      
      # Général
      varg = " var trace1 = {
    x:["
      vary  = 
        " ] ,y: [" 
      vargo  = "],
    mode: 'markers',
    marker: {
    color : 'gray80'
    }, 
    opacity : 0.2,
    name : 'Data'
    };"
      
      # Loess
      var1 = " var trace2 = {
    x:["
      
      var1o = "],
    mode: 'lines',
    line: {
    color: 'blue',
    width : 2
    },
    name : 'Loess'
    };"
      
      # CRT late 
      var2 = " var trace3 = {
    x:["
      
      var2o = "],
    mode: 'markers',
    marker : {
    color: 'green'
    },
    name : 'CTR : Late'
    };"
      
      # CRT early 
      var3 = " var trace4 = {
    x:["
      
      var3o = "],
    mode: 'markers',
    marker : {
    color: 'red'
    },
    name : 'CTR : early'
    };
    "
      
      # TTR
      var4 = " var trace5 = {
    x:["
      
      var4o = "],
    mode: 'markers',
    marker : {
    color: 'gold'
    },
    name : 'TTR'
    };
    "
      
      # seg
      var5 = " var trace6 = {
    x:["
      
      var5o = "],
    mode: 'lines',
    line : {
    color: 'darkmagenta',
    width : 5
    },
    name : 'Segmentation'
    };
    "
      
      data = " var data = [trace1, trace2, trace3, trace4, trace5, trace6];"
      
      layout = paste(" var layout = {
                   title:'Timing replication study for ", chr,"',
                   xaxis: {
                   title: 'Position (pb)'}, 
                   yaxis: {
                   title: 'Intensity'}
                   };", sep = "")
      
      # Général
      filename = paste(prefixe,"/Viewer/",chr,".js",sep="")
      write( varg, filename)
      write.table( t(All_data$POSITION), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( vary, filename, append = T)
      write.table( t(All_data$mLymphAve), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( vargo, filename, append = T)
      
      # Loess
      write( var1, filename,  append = T)
      write.table( t(Loessp), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( vary, filename, append = T)
      write.table( t(Loessm), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( var1o, filename, append = T)
      
      # CRT late 
      write( var2, filename,  append = T)
      write.table( t(NTTR_inf$POSITION), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( vary, filename, append = T)
      write.table( t(NTTR_inf$mLymphAve), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( var2o, filename, append = T)
      
      # CRT early 
      write( var3, filename, append = T)
      write.table( t(NTTR_sup$POSITION), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( vary, filename, append = T)
      write.table( t(NTTR_sup$mLymphAve), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( var3o, filename, append = T)
      
      # TTR
      write( var4, filename, append = T)
      write.table( t(TTR_data$POSITION), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( vary, filename, append = T)
      write.table( t(TTR_data$mLymphAve), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( var4o, filename, append = T)
      
      # Seg
      write( var5, filename, append = T)
      write.table( t(pos_seg), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( vary, filename, append = T)
      write.table( t(moyen), filename, append = T, sep = ",", col.names = F, row.names = F)
      write( var5o, filename, append = T)
      
      write( data, filename, append = T)
      write( layout, filename, append = T)
      write("Plotly.plot('plot', data, layout); " , filename, append = T)  
      
      
      # General - Shiny viewer
      sink(paste0(prefixe,"/Viewer/",chr,".SRV"))
      
      # add important informations
      # norm intre, inter rep, inter exp, 
      write.table(cbind(nor1,nor2,nor3,type_dif, organisme, v4, chr),
                  row.names=FALSE,col.names=FALSE,sep="\t", quote = F)
      
      global_table_viewer = rbind(
        c("Position", "Intensity", "Name"),
        cbind(as.numeric(All_data$POSITION), as.numeric(All_data$mLymphAve), "Data"), 
        cbind(as.numeric(Loessp), as.numeric(Loessm), "Loess"),
        cbind(as.numeric(NTTR_inf$POSITION), as.numeric(NTTR_inf$mLymphAve), "CTR : late"),
        cbind(as.numeric(NTTR_sup$POSITION), as.numeric(NTTR_sup$mLymphAve), "CTR : early"),
        cbind(as.numeric(TTR_data$POSITION), as.numeric(TTR_data$mLymphAve), "TTR"),
        cbind(as.numeric(pos_seg), as.numeric(moyen), "Segmentation")
      )
      
      write.table(global_table_viewer, row.names=FALSE,col.names=FALSE,sep="\t", quote = F)
      
      sink()
      
    }  
  }
  
  ###################################
  # Zone d'écriture globale
  ###################################
  tab_coord_TTR = as.data.frame(tab_coord_TTR)
  filename = paste(prefixe,"/TTR/TTR_position_",num,".bed",sep="")
  write.table(tab_coord_TTR,filename, row.names=F, quote=F,col.names = F, sep="\t")
  
  tab_seg_glob = as.data.frame(tab_seg_glob)
  filename = paste(prefixe,"/Chromosomes/Segmentation_all.bed",sep="")
  write.table(tab_seg_glob,filename, row.names=F, quote=F,col.names = F, sep="\t")
  
  rownames(sum) = chrs2
  sum = as.data.frame(sum)
  filename = paste(prefixe,"/Segmentation/",nom,"_resume_segmentation.txt",sep="")
  write.table(sum, file = filename, quote=F, row.names=T, sep="\t")
  
  
  
  moyenne = cbind(RT$mLymphAve, moyenne)
  
} 

#///////////////////////////////////////////////////////////////////////////////
# Differential analysis
#///////////////////////////////////////////////////////////////////////////////


if (e5 == TRUE){
  dir.create("Differential")
  dir.create("Differential/Viewer")
  
  ALL_dif = NULL
  tab_pourcentage = NULL
  max_tot = 0
  sum_tot = 0
  
  tab_coord_dif = NULL
  
  for (chr in chrs2){
    cat(paste("dif :", chr))
    top = 0
    flag = 0
    etat = 0
    #===========================================================================
    # Read data 
    #===========================================================================
    
    All_data1 = read.table(paste("Experience_1/Chromosomes/", chr,".txt",
                                 sep = ""), header = T)
    Loess_data1 = read.table(paste("Experience_1/Chromosomes/Loess_", chr,".txt", 
                                   sep = ""), header = T)
    Seg_data1 = read.table(paste("Experience_1/Chromosomes/Segmentation_", 
                                 chr,".bed", sep = ""), header = T)
    TTR1 = read.table(paste("Experience_1/Chromosomes/TTR_", chr,".bed", 
                            sep = ""), header = F)
    
    All_data2 = read.table(paste("Experience_2/Chromosomes/", chr,".txt", 
                                 sep = ""), header = T)
    Loess_data2 = read.table(paste("Experience_2/Chromosomes/Loess_", 
                                   chr,".txt", sep = ""), header = T)
    Seg_data2 = read.table(paste("Experience_2/Chromosomes/Segmentation_", 
                                 chr,".bed", sep = ""), header = T)
    TTR2 = read.table(paste("Experience_2/Chromosomes/TTR_", chr,".bed", 
                            sep = ""), header = F)
    
    lspan = v1/(max(All_data1$POSITION)-min(All_data1$POSITION))
    
    RTb = cbind(All_data1$CHR, All_data1$POSITION, All_data1$mLymphAve, 
                All_data2$mLymphAve)
    RTb = as.data.frame(RTb)
    colnames(RTb)  = c("CHR","POSITION", "E1", "E2")
    
    #===========================================================================
    # Segment method
    #===========================================================================
    
    if(type_dif == "Segment method"){
      #-------------------------------------------------------------------------
      # Common TTR
      #-------------------------------------------------------------------------
      newTTR1 = NULL
      for (i in 1:TTR1[dim(TTR1)[1],3]){
        inter = TTR1[which(TTR1[,3]==i),]
        newTTR1 = rbind( newTTR1, c(loc.start= inter[1,1],
                                    loc.end = inter[dim(inter)[1],1], 
                                    num = i , mean = mean(inter[,2])))
      }
      
      newTTR2 = NULL
      for (i in 1:TTR2[dim(TTR2)[1],3]){
        inter = TTR2[which(TTR2[,3]==i),]
        newTTR2 = rbind( newTTR2, c(loc.start= inter[1,1],
                                    loc.end = inter[dim(inter)[1],1], 
                                    num = i , mean = mean(inter[,2])))
      }
      
      df1 = as.data.frame(newTTR1)
      df2 = as.data.frame(newTTR2)
      
      setDT(df1)  ## convert loc.end data.table without copy
      setDT(df2)
      
      setkey(df2, loc.start, loc.end)
      ans = foverlaps(df1, df2, type="any")
      ans = ans[, `:=`(loc.start = pmax(loc.start, i.loc.start), 
                       loc.end = pmin(loc.end, i.loc.end))]
      ans = ans[, `:=`(i.loc.start=NULL, i.loc.end=NULL)][loc.start <= loc.end]
      
      pascommunTTR2 = newTTR2[-ans$num,]
      pascommunTTR1 = newTTR1[-ans$i.num,]
      
      etude = rep("ADVANCED", dim(pascommunTTR2)[1])
      for (i in 1:dim(pascommunTTR2)[1]) {
        mo = mean(All_data1[which(All_data1$POSITION == pascommunTTR2[i,1]),5],
                  All_data1[which(All_data1$POSITION == pascommunTTR2[i,2]),5])
        if((pascommunTTR2[i,4]- mo) < 0 ){
          etude[i] = "DELAYED"
        }
      }
      pascommunTTR2[,3] = etude
      
      etude = rep("ADVANCED", dim(pascommunTTR1)[1])
      for (i in 1:dim(pascommunTTR1)[1]) {
        mo = mean(All_data2[which(All_data2$POSITION == pascommunTTR1[i,1]),5],
                  All_data2[which(All_data2$POSITION == pascommunTTR1[i,2]),5])
        if((pascommunTTR1[i,4]- mo) > 0){
          etude[i] = "DELAYED"
        }
      }
      pascommunTTR1[,3] = etude
      
      pentedif = NULL
      for(i in 1:dim(ans)[1]){
        if(length(TTR1[which(TTR1[,3]==ans$i.num[i]),2]) != 0 && length(TTR2[which(TTR2[,3]==ans$num[i]),2]) ){
          test1b = lm(TTR1[which(TTR1[,3]==ans$i.num[i]),2] ~ TTR1[which(TTR1[,3]==ans$i.num[i]),1] )
          test2b = lm(TTR2[which(TTR2[,3]==ans$num[i]),2] ~ TTR2[which(TTR2[,3]==ans$num[i]),1] )
          ano = anova(test1b,test2b)
          if(is.nan(ano$Pr[1]) == F && ano$Pr[1] > 0.05){
            pentedif = rbind(pentedif, ans[i,])
          }
        }
        
      }
      
      pentedif2 = NULL
      vec = unique(pentedif$i.num)
      for (v in vec){
        pentedif2 = rbind( pentedif2, newTTR1[which(newTTR1[,3] == v),1:2])
      }
      
      vec = unique(pentedif$num)
      for (v in vec){
        pentedif2 = rbind( pentedif2, newTTR2[which(newTTR2[,3] == v),1:2])
      }
      
      pentedif2 = as.data.frame(pentedif2)
      
      etude = rep("ADVANCED",dim(pentedif2)[1])
      for(i in 1:dim(pentedif2)[1]){
        if( length(All_data1[which(All_data1$POSITION == pentedif2[i,1]),5]) != 0){
          mo1 = mean(All_data1[which(All_data1$POSITION == pentedif2[i,1]),5],All_data1[which(All_data1$POSITION == pentedif2[i,2]),5])
          mo2 = mean(All_data2[which(All_data2$POSITION == pentedif2[i,1]),5],All_data2[which(All_data2$POSITION == pentedif2[i,2]),5])
          if(mo1-mo2 >0)
            etude[i]= "DELAYED"        
        }
      }
      
      pentedif2 = as.data.frame(cbind(pentedif2, etude))

      df1 = Seg_data1
      df2 = Seg_data2
      
      setDT(df1)  ## convert loc.end data.table without copy
      setDT(df2)
      
      setkey(df2, loc.start, loc.end)
      ans = foverlaps(df1, df2, type="any")
      ans = ans[, `:=`(loc.start = pmax(loc.start, i.loc.start), loc.end = pmin(loc.end, i.loc.end))]
      ans = ans[, `:=`(i.loc.start=NULL, i.loc.end=NULL)][loc.start <= loc.end]
      
      ecart = abs(ans$seg.mean - ans$i.seg.mean)
      ecart2 = ans$i.seg.mean -ans$seg.mean
      
      #-------------------------------------------------------------------------
      # Sample
      #-------------------------------------------------------------------------
      pos_ecart = NULL
      N = 10000
      
      for (k in 1:dim(ans)[1]){
        ks1 = All_data1$mLymphAve[which(All_data1$POSITION == ans$loc.start[k]):which(All_data1$POSITION == ans$loc.end[k])]
        ks2 = All_data2$mLymphAve[which(All_data2$POSITION == ans$loc.start[k]):which(All_data2$POSITION == ans$loc.end[k])]
        
        ks = c(ks1,ks2)
        ecart_true = abs(ans$seg.mean[k] - ans$i.seg.mean[k] )
        
        if (factorial(length(ks)) > N || factorial(length(ks)) == Inf ){
          ecart = rep(NA, N)
          for (s in 1:N) {
            kss = sample(ks)
            ecart[s] = abs(mean(kss[1:(length(kss)/2)]) - mean(kss[(length(kss)/2 +1 ):length(kss)] ))
          }
          pvalue = length(which(ecart > ecart_true))/ N
        }else{
          p = perms(ks)
          ecart = rep(NA,dim(p)[1])
          for (s in 1:dim(p)[1]) {
            ecart[s] = abs(mean(p[s, 1:(dim(p)[2]/2)]) - mean(p[s,(dim(p)[2]/2 +1 ):dim(p)[2]]))
          }
          pvalue = length(which(ecart > ecart_true))/ dim(p)[1]
        }  
        
        if (pvalue < 0.05){
          pos_ecart= c(pos_ecart, k)
        }
      }
      
      newseg = as.data.frame(cbind(ans$loc.start[pos_ecart], ans$loc.end[pos_ecart],  ecart2[pos_ecart]))
      
      if( dim(newseg)[1] != 0){
        etude = rep("ADVANCED", length(ecart2[pos_ecart]))
        etude[which( newseg[,3] > 0)] = "DELAYED"
        newseg[,3]  = etude
      }
      
      
      if( dim(pentedif2)[1] == 0) pentedif2 = matrix(NA,1,4)
      if( dim(newseg)[1] == 0) newseg = matrix(NA,1,4)
      if( dim(pascommunTTR1)[1] == 0) pascommunTTR1 = matrix(NA,1,4)
      if( dim(pascommunTTR2)[1] == 0) pascommunTTR2 = matrix(NA,1,4)
      
      advanced = rbind(cbind(newseg[which(newseg[,3] == "ADVANCED"),1], newseg[which(newseg[,3] == "ADVANCED"),2]),
                       cbind(pascommunTTR1[which(pascommunTTR1[,3] == "ADVANCED"),1],pascommunTTR1[which(pascommunTTR1[,3] == "ADVANCED"),2]),
                       cbind(pascommunTTR2[which(pascommunTTR2[,3]== "ADVANCED"),1],pascommunTTR2[which(pascommunTTR2[,3]== "ADVANCED"),2]),
                       cbind(pentedif2[which(pentedif2[,3] == "ADVANCED"),1], pentedif2[which(pentedif2[,3] == "ADVANCED"),2])
      )
      advanced = as.data.frame(advanced)
      advanced = na.omit(advanced)
      advanced[,1] = as.numeric(as.character(advanced[,1]))
      advanced[,2] = as.numeric(as.character(advanced[,2]))
      
      
      delayed = rbind(cbind(newseg[which(newseg[,3] != "ADVANCED"),1], newseg[which(newseg[,3] != "ADVANCED"),2]),
                      cbind(pascommunTTR1[which(pascommunTTR1[,3] != "ADVANCED"),1],pascommunTTR1[which(pascommunTTR1[,3] != "ADVANCED"),2]),
                      cbind(pascommunTTR2[which(pascommunTTR2[,3]!= "ADVANCED"),1],pascommunTTR2[which(pascommunTTR2[,3]!= "ADVANCED"),2]),
                      cbind(pentedif2[which(pentedif2[,3] != "ADVANCED"),1], pentedif2[which(pentedif2[,3] != "ADVANCED"),2])
      )
      
      delayed = as.data.frame(delayed)
      delayed = na.omit(delayed)
      delayed[,1] = as.numeric(as.character(delayed[,1]))
      delayed[,2] = as.numeric(as.character(delayed[,2]))
     
    #===========================================================================
    # Mean method
    #=========================================================================== 

    }else if (type_dif == "Mean method"){
      taille_fenetre = pv2
      overlap = pv4
      p_value = pv1
      log_p = 1 - log(p_value)
      
      tab_pval = matrix(0,dim(RTb)[1]/(taille_fenetre-overlap)-2,8)
      colnames(tab_pval) = c("POSITION","pval", "log", "col", "m1", "m2", 
                             "col2", "dif")
      posp = seq(1,dim(RTb)[1], taille_fenetre-overlap)
      
      for (i in 1:floor(dim(RTb)[1]/(taille_fenetre-overlap)-2) ){
        tab_pval[i,1] = as.numeric(as.character( RTb[posp[i], 2])) #+ floor(taille_fenetre/2)
        a = as.numeric(as.character(RTb[posp[i]:(posp[i]+taille_fenetre-1),3]))
        b = as.numeric(as.character(RTb[posp[i]:(posp[i]+taille_fenetre-1),4]))
        
        if(anyNA(a) == TRUE || anyNA(b) == TRUE  ){
          tab_pval[i,2] = 0
          tab_pval[i,3] = 0
          tab_pval[i,5] = 0
          tab_pval[i,6] = 0
        }else {
          tab_pval[i,2] = t.test(a,b)$p.value
          tab_pval[i,3] = 1 - log10(as.numeric(as.character(tab_pval[i,2])))
          tab_pval[i,5] = mean(a)
          tab_pval[i,6] = mean(b)
        }
      }
      tab_pval[, 2] =  p.adjust(as.numeric(as.character(tab_pval[,2])), method = pv3)
      
      for (i in 1:floor(dim(RTb)[1]/(taille_fenetre-overlap)-2) ){
        if (as.numeric(as.character(tab_pval[i,3])) < log_p){
          tab_pval[i,4] = "forestgreen"  
        }else if (as.numeric(as.character(tab_pval[i,3])) > log_p){
          tab_pval[i,4] = "red"
        }
        
        tab_pval[i,8]= as.numeric(as.character(tab_pval[i,5]))-as.numeric(as.character(tab_pval[i,6]))
        
        if (is.na(as.numeric(as.character(tab_pval[i,5])) ) || is.na(as.numeric(as.character(tab_pval[i,5])) )){
          tab_pval[i,7] = "white"
          tab_pval[i,8] = 0
        }else if (as.numeric(as.character(tab_pval[i,5])) > as.numeric(as.character(tab_pval[i,6])) && as.numeric(as.character(tab_pval[i,3])) > log_p){
          tab_pval[i,7] = "blue"  
        }else if (as.numeric(as.character(tab_pval[i,5])) < as.numeric(as.character(tab_pval[i,6])) && as.numeric(as.character(tab_pval[i,3])) > log_p ){
          tab_pval[i,7] = "chocolate"
        }
        
      }
      
      if (organisme == "Human"){
        centro = centromere(chr,"hg18")
      }else{
        centro = centro_Mouse(chr)
      }
      
      pos_centro = centro
      
      tab_centro = rbind(cbind(as.character(chr) ,pos_centro[1]),cbind(as.character(chr),pos_centro[2]))
      tab_centro = as.data.frame(tab_centro)
      tab_centro[,1] = as.character(tab_centro[,1])
      
      pos_sample = sample(2: (nrow(All_data1)-1), 1000)

      # Calcul de la taille moyenne entre 2 positions
      M_sample = NULL
      for(comp_sample in pos_sample){
         M_sample = c(M_sample, mean(c((All_data1$POSITION[comp_sample]- All_data1$POSITION[(comp_sample-1)]),(All_data1$POSITION[(comp_sample+1)] - All_data1$POSITION[(comp_sample)]))))
      }
      moyenne_entre_pos = mean(M_sample)

      centro_chrom = subset(tab_centro, tab_centro$V1 == chr)
      mat_centro = matrix(0,dim( centro_chrom)[1],8)
      colnames(mat_centro) = colnames(tab_pval)
      
      mat_centro[,1] = as.numeric(as.character(centro_chrom[,2]))
      mat_centro[,2] = 1
      mat_centro[,3] = 1 - log10(as.numeric(as.character(mat_centro[,2])))
      mat_centro[,4] = "forestgreen"
      mat_centro[,7] = "white"
      
      tab_pval = rbind(tab_pval,mat_centro)
      tab_pval = as.data.frame(tab_pval)
      tab_pval[,1] = as.numeric(as.character(tab_pval[,1]))
      tab_pval = tab_pval[order(tab_pval[,1]),]
      tab_pval = as.matrix(tab_pval)
      
      etat = 0  
      compt = 0
      test = NULL
      for (i in 2 : (dim(tab_pval)[1]-1)){
        
        if (tab_pval[i,4] != "forestgreen"){
          if (tab_pval[i,7] == "blue"){
            etat = etat + 2
            compt = compt +1
          }else if (tab_pval[i,7] == "chocolate"){
            etat = etat - 1 
            compt = compt +1
          } else if (tab_pval[i,7] == "white")
            etat = etat 
        }
        
        if ( tab_pval[i,4] != "forestgreen" & i == (dim(tab_pval)[1]-1)){
          max = tab_pval[i,"POSITION"]
          top = 2
        }
        
        if ( tab_pval[i-1,4] != "forestgreen" & i-1 == 1){
          min = tab_pval[i,"POSITION"]
          top = 1
        }
        if (tab_pval[i,4] == "forestgreen" & tab_pval[i+1,4] != "forestgreen" & tab_pval[i-1,4] != "forestgreen"){
          flag = 1 
          min2 = tab_pval[i,"POSITION"]
          max = tab_pval[i,"POSITION"]
          top = top + 1
          
        }
        
        if( tab_pval[i,4] == "forestgreen" & tab_pval[i-1,4] == "forestgreen" & tab_pval[i+1,4] != "forestgreen" & top != 1){
          min = tab_pval[i,"POSITION"]
          top = top +1
          
        }
        if( tab_pval[i,4] == "forestgreen" & tab_pval[i+1,4] == "forestgreen" & tab_pval[i-1,4] != "forestgreen" & top == 1){
          max = tab_pval[i,"POSITION"]
          top = top + 1
          
        }
        
        if(top == 2){
          if (flag == 0){
            if ( etat == 2 * compt){
              STATE = "DELAYED"
            } else if (etat == -1 * compt){
              STATE = "ADVANCED"
            }else{
              STATE = "ADVANCED & DELAYED"
            }
            tab_coord_dif = rbind(tab_coord_dif, c(CHR = chr,START = min,END = max, STATE = STATE))
            top = 0
            compt = 0
            etat = 0
            test = c(test, etat)
          }
          if (flag == 1){
            if ( etat == 2 * compt){
              STATE = "DELAYED"
            } else if (etat == -1 * compt){
              STATE = "ADVANCED"
            }else{
              STATE = "ADVANCED & DELAYED"
            }
            tab_coord_dif = rbind(tab_coord_dif, c(CHR = chr,START = min,END = max, STATE = STATE))
            min = min2
            top = 1
            flag = 0
            compt = 0
            etat = 0
            test = c(test, etat)
          }
        }
      }
      
      inter = tab_coord_dif[tab_coord_dif[,1]== chr,]
      if (is.vector(inter) == TRUE)
        inter = matrix (inter,1,3)
      debut1 = NULL
      debut2 = NULL
      delayed = NULL
      advanced = NULL
      inter = as.data.frame(inter)
      inter_ad = inter[inter$STATE == "ADVANCED",]
      if (dim(inter_ad)[1] != 0){
        debut1 = as.numeric(as.character(inter_ad[,2]))
        fin1 = as.numeric(as.character(inter_ad[,3]))
      }

      
      inter_de = inter[inter$STATE == "DELAYED",]
      if(dim(inter_de)[1] != 0){
        debut2 = as.numeric(as.character(inter_de[,2]))
        fin2 = as.numeric(as.character(inter_de[,3]))
      }

      if (!is.null(debut1[1])){
        if(debut1[1] == 0){
          debut1[1] = mean(c(All_data1$POSITION[1], All_data2$POSITION[1]))
        }
      }
      
      if (!is.null(debut2[1])){
        if(debut2[1] == 0){
          debut2[1] = mean(c(All_data1$POSITION[1], All_data2$POSITION[1]))
        }
      } 
      ############################################################################################
      ############################################################################################
      # ICI modification pour la localisation des différences dans la mean
      # Voir uniquement pour la Mouse car l'Human c'est bon
      ############################################################################################
      ############################################################################################
      
      
      if(!is.null(debut1)){
        if (organisme == "Human"){
          advanced = cbind(debut1 + Loess_data1$POSITION[1],fin1+ Loess_data1$POSITION[1] )
        } else if (organisme == "Mouse"){
          advanced = cbind((debut1 + Loess_data1$POSITION[1] - All_data1$POSITION[1] + overlap *  moyenne_entre_pos) ,(fin1+ Loess_data1$POSITION[1] - All_data1$POSITION[1] + overlap *  moyenne_entre_pos) ) 
          #advanced = cbind((debut1 + (fin1-debut1)/2),(fin1+ (fin1-debut1)/2))
        }
      }
      if(!is.null(debut2)){
        if (organisme == "Human"){
          delayed= cbind(debut2 + Loess_data1$POSITION[1], fin2+ Loess_data1$POSITION[1])
        } else if (organisme == "Mouse"){
          delayed= cbind((debut2 + Loess_data1$POSITION[1] - All_data1$POSITION[1] + overlap *  moyenne_entre_pos), (fin2+ Loess_data1$POSITION[1]- All_data1$POSITION[1] + overlap *  moyenne_entre_pos))
          #delayed= cbind((debut2 + (fin2-debut2)/2 ), (fin2 + (fin2-debut2)/2))
        }
      }
     
      if(dim(delayed)[1] == 0 && !is.null(delayed))
        delayed = rbind(delayed, c(NA,NA))
      if(dim(advanced)[1] == 0 && !is.null(advanced))
        advanced = rbind(delayed, c(NA,NA))
    }

    Loess_data1_1 = Loess_data1[which(Loess_data1[,6] == 1 ), ]
    Loess_data1_2 = Loess_data1[which(Loess_data1[,6] == 2 ), ]
    Loess1m = c( Loess_data1_1$RTlc.fitted, NA,  Loess_data1_2$RTlc.fitted)
    Loess1p = c( Loess_data1_1$POSITION, NA,  Loess_data1_2$POSITION)
    
    Loess_data2_1 = Loess_data2[which(Loess_data2[,6] == 1 ), ]
    Loess_data2_2 = Loess_data2[which(Loess_data2[,6] == 2 ), ]
    
    Loess2m = c( Loess_data2_1$RTlc.fitted, NA,  Loess_data2_2$RTlc.fitted)
    Loess2p = c( Loess_data2_1$POSITION, NA,  Loess_data2_2$POSITION)
    
    #===========================================================================
    # Euclidean method
    #===========================================================================
    
    if (type_dif == "Euclidean method"){
      px <- (Loess_data1$RTlc.fitted - Loess_data2$RTlc.fitted)**2
      b = boxplot(px, plot = F)
      
      color = rep('blue', length(px))
      color[which(px >= b$stats[5])] = "red"
      layout(matrix(c(1,2), 1, 2, byrow = TRUE), 
            widths=c(10,1))
      par(mar = c(5, 4, 4, 0) + 0.1)
      px <- (Loess_data1$RTlc.fitted - Loess_data2$RTlc.fitted)**2
      plot(px~Loess_data1$POSITION, col = color, pch = 20, axes = F, ylim= c(0,1),
           ylab = "Difference au carré", xlab = "Position en pb (chromosome 1)")
      axis(1)
      axis(2, c(seq(0,1.75,0.25)))
      abline(h= min(b$out), col = 'red', lwd = 2, lty = 2)
      abline(h = (0.67)**2)
      par(mar = c(5, 0, 4, 2) + 0.1)
      boxplot(px, axes = F, col = "blue", outcol = "red", pch = 20,ylim= c(0,1))
      b = boxplot(px, plot = F)
      dif = c(which( px <= b$stats[1,1]), which(px >= seuil) ) #b$stats[5,1]
      dif = sort(dif)
      
      sous = diff(dif)
      difference = NULL
      start = dif[1]
      for (boucle in 1:length(sous)){
        if(sous[boucle] != 1){
          end = dif[boucle]
          difference = rbind(difference, c(start,end))
          if(boucle != length(dif) )
            start =  dif[boucle + 1]
        }
        
        if(boucle == length(dif)){
          end = dif[boucle]
          difference = rbind(difference, c(start,end))
        }
        
      }
      
      ### Nouvelle recherche
      sous = diff(dif)
      difference = NULL
      start = dif[1]
      compteur = 1
      for (boucle in 1:length(sous)){
        
        if(sous[boucle] == 1){
          compteur = compteur +1
        }
        
        if(sous[boucle] != 1){
          if( compteur == 1 ){ # changer en fonction de la taille
            start = dif[boucle]
          } else{
            end = dif[boucle]
            difference = rbind(difference, c(start,end))
            if(boucle != length(dif) )
              start =  dif[boucle + 1]
          }
          compteur = 1
        }
        
        if(boucle == length(dif)){
          end = dif[boucle]
          difference = rbind(difference, c(start,end))
        }
        
      }  
     
      advanced = NULL
      delayed = NULL 
      if (is.null(difference) == F){
        for(t in 1:dim(difference)[1]){
          moy1 = (All_data1$mLymphAve[difference[t,1]]+All_data1$mLymphAve[difference[t,2]])/2
          moy2 = (All_data2$mLymphAve[difference[t,1]]+All_data2$mLymphAve[difference[t,2]])/2
          if (moy1 < moy2)
            advanced = rbind(advanced,c(All_data2$POSITION[difference[t,1]],All_data2$POSITION[difference[t,2]]))
          else 
            delayed = rbind(delayed,c(All_data2$POSITION[difference[t,1]],All_data2$POSITION[difference[t,2]]))
        }
        
        delayed_point = delayed
        advanced_point = advanced
      }
    }
    
    ## Image
    minus = round((min(min(All_data2$mLymphAve), min(All_data1$mLymphAve))-0.5))
    maxi = round(max(max(All_data2$mLymphAve), max(All_data1$mLymphAve)+ 0.5))
    
    if(sortie_image == "Yes"){
      filename = paste("Differential/Differential_",chr,"_",pv3,"_",num,".pdf",sep="")
      pdf(filename )
      
      plot(All_data1$POSITION, All_data1$mLymphAve, ylab = "Intensity", xlab="Coordinate (bp)",
           cex = 0.5, pch = 16, col = "gray80", main = paste("Differential (", chr,")"),  ylim = c(minus,maxi))
      points(All_data2$POSITION, All_data2$mLymphAve, cex = 0.5, pch = 16, col = "gray80", xlim = c(0,10000000))
      lines(Loess1m~Loess1p, col="blue3", lwd=3) 
      lines(Loess2m~Loess2p, col="red", lwd=3)
      
      if (is.null(advanced) != T){
        segments(advanced[,1], minus, advanced[,2], minus, col = "coral", lwd=3)
      }
        
      if (is.null(delayed) != T){
        segments(delayed[,1], minus, delayed[,2], minus, col = "dodgerblue", lwd=3)
      }

      legend("topright", legend = c("Exp. 1", "Exp. 2", "Advanced", "Delayed"), col = c("blue", "red", "coral", "dodgerblue"), lty = 1, inset = 0.01, box.lty = 0, title = "Legends")
      
      dev.off()
    }
    
    
    
    
    #########################################
    # Plotly
    #########################################
    
    # Général
    varg1 = " var trace1 = {
    x:["
    vary  = 
      " ] ,y: [" 
    vargo1  = "],
    mode: 'markers',
    marker: {
    color : 'gray80'
    }, 
    opacity : 0.2,
    name : 'Exp 1'
   };"
    
    varg2 = " var trace2 = {
    x:["
    
    vargo2  = "],
    mode: 'markers',
    marker: {
    color : 'gray79'
    }, 
    opacity : 0.2,
    name : 'Exp 2'
  };"
    
    
    # Loess
    var1 = " var trace3 = {
    x:["
    
    var1o = "],
    mode: 'lines',
    line: {
    color: 'blue',
    width : 2
    },
    name : 'Smooth Exp 1'
    };"
    
    var2 = " var trace4 = {
    x:["
    
    var2o = "],
    mode: 'lines',
    line: {
    color: 'red',
    width : 2
    },
    name : 'Smooth Exp 2'
    };"
    
    # seg advanced
    var3 = " var trace5 = {
    x:["
    
    var3o = "],
    mode: 'lines',
    line : {
    color: 'forestgreen',
    width : 5
    },
    name : 'Advanced'
    };
    "
    
    # seg advanced
    var4 = " var trace6 = {
    x:["
    
    var4o = "],
    mode: 'lines',
    line : {
    color: 'magenta',
    width : 5
    },
    name : 'Delayed'
  };
    "
    
    data = " var data = [ trace1, trace2, trace3, trace4, trace5, trace6];"
    
    layout = paste(" var layout = {
                   title:'Differential study for ", chr,"',
                   xaxis: {
                   title: 'Position (pb)'}, 
                   yaxis: {
                   title: 'Intensity'}
                   };", sep = "")
    
    # Général1
    filename = paste("Differential/Viewer/",chr,".js",sep="")
    write(varg1, filename)
    write.table( t(All_data1$POSITION), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vary, filename, append = T)
    write.table( t(All_data1$mLymphAve), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vargo1, filename, append = T)
    
    # Général2
    write( varg2, filename, append = T)
    write.table( t(All_data2$POSITION), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vary, filename, append = T)
    write.table( t(All_data2$mLymphAve), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vargo2, filename, append = T)
    
    # Loess1
    write( var1, filename,  append = T)
    write.table(recode(t(Loess1p), "NA ='NaN'") , filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vary, filename, append = T)
    write.table( recode(t(Loess1m), "NA ='NaN'"), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( var1o, filename, append = T)
    
    # Loess2
    write( var2, filename,  append = T)
    write.table(recode(t(Loess2p), "NA ='NaN'"), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vary, filename, append = T)
    write.table( recode(t(Loess2m), "NA ='NaN'"), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( var2o, filename, append = T)
    
    # Seg advanced
    pos_seg = matrix("NaN",length(advanced[,1]), 3)
    pos_seg[,1] = advanced[,1]
    pos_seg[,2] = advanced[,2]
    pos_seg = as.vector(t(pos_seg))
    
    moyen = matrix("NaN",length(advanced[,1]), 3)
    moyen[,1] = minus
    moyen[,2] = minus
    moyen = as.vector(t(moyen))
    
    write( var3, filename, append = T)
    write.table(  t(pos_seg), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vary, filename, append = T)
    write.table( t(moyen), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( var3o, filename, append = T)
    
    global_table_viewer = rbind(
      c("Position", "Intensity", "Name"),
      cbind(as.numeric(t(All_data1$POSITION)), as.numeric(t(All_data1$mLymphAve)), "Exp 1"), 
      cbind(as.numeric(t(All_data2$POSITION)), as.numeric(t(All_data2$mLymphAve)), "Exp 2"),
      cbind(as.numeric(recode(t(Loess1p), "NA ='NaN'")), as.numeric(recode(t(Loess1m), "NA ='NaN'")), "Smooth Exp 1"),
      cbind(as.numeric(recode(t(Loess2p), "NA ='NaN'")), as.numeric( recode(t(Loess2m), "NA ='NaN'")), "Smooth Exp 2")
    )
    
    if(length(pos_seg) != 0){
      global_table_viewer = rbind(global_table_viewer,
                                  cbind(as.numeric(t(pos_seg)), as.numeric(t(moyen)), "Advanced"))
    }
    
    # Seg Delayed
    
    pos_seg = matrix("NaN",length(delayed[,1]), 3)
    pos_seg[,1] = delayed[,1]
    pos_seg[,2] = delayed[,2]
    pos_seg = as.vector(t(pos_seg))
    
    moyen = matrix("NaN",length(delayed[,1]), 3)
    moyen[,1] = minus
    moyen[,2] = minus
    moyen = as.vector(t(moyen))
    
    write( var4, filename, append = T)
    write.table( t(pos_seg), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( vary, filename, append = T)
    write.table( t(moyen), filename, append = T, sep = ",", col.names = F, row.names = F)
    write( var4o, filename, append = T)
    
    write( data, filename, append = T)
    write( layout, filename, append = T)
    write("Plotly.plot('plot', data, layout); " , filename, append = T)  
    
    if(length(pos_seg) != 0){
      global_table_viewer = rbind(global_table_viewer,
                                  cbind(as.numeric(t(pos_seg)), as.numeric(t(moyen)), "Delayed"))
    }
    
   
    # General - Shiny viewer
    sink(paste0("Differential/Viewer/",chr,".SRV"))
    
    # add important informations
    # norm intre, inter rep, inter exp, 
    write.table(cbind(nor1,nor2,nor3,type_dif, organisme, v4, chr),
                row.names=FALSE,col.names=FALSE,sep="\t", quote = F)
    
    write.table(global_table_viewer, row.names=FALSE,col.names=FALSE,sep="\t", quote = F)
    
    sink()
    
    
    if(is.null(advanced) == F){
      advanced = as.data.frame(cbind(rep(chr, dim(advanced)[1]), advanced, rep("ADVANCED", dim(advanced)[1])))
      names(advanced) = c("CHR","START", "END", "STATUS")      
    }
    if(is.null(delayed) == F){
      delayed = as.data.frame(cbind(rep(chr, dim(delayed)[1]), delayed, rep("DELAYED", dim(delayed)[1])))
      names(delayed) = c("CHR","START", "END", "STATUS")
    }

    
    
    ALL_dif = rbind(ALL_dif, advanced)
    ALL_dif = rbind(ALL_dif, delayed)
    
    
    #########################################
    # A- Percent changes analysis
    #########################################
    max = max(as.numeric(as.character(RTb$POSITION)))
    dif = c(as.numeric(as.character(advanced[,3])) - as.numeric(as.character(advanced[,2])),  
            as.numeric(as.character(delayed[,3])) - as.numeric(as.character(delayed[,2])))
    sum = sum(dif)
    max_tot = max_tot + max
    sum_tot = sum_tot + sum 
    pourcentage = sum*100/max
    tab_pourcentage = rbind(tab_pourcentage, c(chr,pourcentage))
  }
  
  filename = paste("Differential/Differential_position_",num,".txt",sep="")
  write.table( ALL_dif,filename, row.names=F, quote=F, sep="\t")
  filename = paste("Differential/Differential_position_",num,".bed",sep="")
  write.table( ALL_dif,filename, row.names=F, quote=F, col.names = F, sep="\t")
  
  pourcentage_tot = sum_tot *100/max_tot
  tab_pourcentage = tab_pourcentage[order(tab_pourcentage[,2]),]
  tab_pourcentage = rbind(tab_pourcentage, c("Total",pourcentage_tot))
  colnames(tab_pourcentage) = c("chrom", "percentage")
  filename = paste("Differential/Differential_percentage_",num,".txt",sep="")
  write.table(tab_pourcentage,filename, row.names=F, quote=F, sep="\t")
  
}

setwd("../..")

system("docker stop srvd || true")

