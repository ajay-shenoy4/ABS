#### Plot Creation - AHDB function master



###
#Load Libraries
###
require("stringr")
library(zoo)
library(ggplot2)
library(ggplot2)
library(dplyr)
library(rlang)
library(patchwork)  
Breeds <- as.data.frame(read_excel("/data/dairy/EMEAProjects/Audit-Data-Loader/NatEvalResources/Breeds V1.xlsx"),stringsAsFactors=F)

dblHolstienGest <- 275.5

##Plot asthetics
GText <- "Genetic Trends Amongst Sire Usage and Dairy Animals for"
CText <- "Correlation of ###Perf### with ###Gen###"
MyBlue <- rgb(125, 155, 193, maxColorValue = 255)
MyDarkBlue <- rgb(0, 57, 118, maxColorValue = 255)
MyRed <- rgb(206, 17, 65, maxColorValue = 255)
MyDarkRed <- "DarkRed"
MyGray <- rgb(169, 168, 169, maxColorValue = 255)
MyBrown<-rgb(95,69,43,maxColorValue = 255)
MyBeige<-rgb(190,149,91,maxColorValue = 255)
MyCream<-rgb(224,202,163,maxColorValue = 255)
# 

##
#Function to create a folder if it does not exist
##
createFolder <- function(mainDir, subDir){
  if (! file.exists(paste(mainDir, subDir, sep = ""))){
    dir.create(file.path(mainDir, subDir))
  }
}

##
#function to produce PNG
##
ProducePNG <- function(strName){
  png(strName, res = 1000, width = 22.15 * 1.25, height = 10.56 * 1.5, units = "cm" )
}


##
#Run
##
# RunNeAnalysis <- function(Shortname, booProspect = T, booEngage = T, booAudit = F){
#   # Will stop if Shortname is not valid from AHDBCust, add customer within file stored -- Z:\Strategic Accounts Genetic Services\Genetic and Genomic Customer list
#   if(is.na(Shortname)){
#     stop("Invalid Customer Folder")
#   }
#   if(!Shortname %in% custs$Custfolder){
#     stop("Invalid Customer Folder")
#   }
#   if(Shortname == ""){
#     stop("Invalid Customer Folder")
#   }
# # Setting time for use for dating output folders
# AuditDTS <- Sys.time()
# # Create a folder if one does not exist
# if(! file.exists(paste0(strDirDestination, Shortname))){
#   createFolder(strDirDestination, Shortname)
# }
# # Create path variable to use to output analysis and creates folder
# StrCustDestination <- paste0(strDirDestination, Shortname, "\\")
# AuditFolder <- paste(Shortname, format(AuditDTS, "%Y_%m_%d"))
# 
# # If a current folder for the customer exist for today's date, it will ask you to delete the folder, hit Y to delete, N to cancel the script.
# # If you want to keep the original folder rename
# if(file.exists(paste0(StrCustDestination, AuditFolder))){
#   if(getValidatedResp(paste("Warning a folder '", AuditFolder, "' already exists for Customer '", Shortname,
#                             ".\nType 'Y' to DELETE the folder and contents or 'N', to escape:"))){
#     deleteFolder(StrCustDestination, AuditFolder)
#   } else{
#     stop("You Chose Not to delete the folder.")
#   }
# }
# # Creating the audit folder
# createFolder(StrCustDestination, AuditFolder)
# StrAuditDestination <- paste0(StrCustDestination, AuditFolder, "\\")
# Sourcedat <<- custs$MRO[custs$Custfolder == Shortname]
# cat(paste("/nData source is:", Sourcedat))
# 
# # Calls function to load in the customers data from there MRO info column
# 
# dfHerd <- getNEHerdsData(Shortname)
# 

#### Functions for plots
ProduceBenchmarkingPlots <- function(dfHerd, dfDataStruc, NHMH, StrAuditDestination){
  for(I in 1 : nrow(dfDataStruc)){
    if(dfDataStruc$Trait[I] %in% colnames(getMilkPerc())){
      ProducePNG(paste(StrAuditDestination, "Benchmark", dfDataStruc$Trait[I], ".png",sep=""))
      CloseupBox(dfHerd,dfDataStruc$Trait[I],dfDataStruc$Label[I],dfDataStruc$Nearest[I],NHMH)
      dev.off()
    }
  }
}

ProduceGTs <- function(dfHerd, dfDataStruc, StrAuditDestination){
  # Loop over each unique Category in the data structure (dfDataStruc)
  for(Category in unique(dfDataStruc$Category)){
    
    # Print the current category being processed to the console
    cat(paste("Running Category:", Category, "\n"))
    
    # Count how many traits belong to the current category
    Class <- nrow(dfDataStruc[dfDataStruc$Category == Category, ])
    
    # Start a PNG device to save the plot with a filename based on the category
    ProducePNG(paste(StrAuditDestination, Category, "GenImp.png", sep=""))
    
    # If there is only one trait in this category
    if(Class == 1){
      # Get the index of the trait in this category
      I <- which(dfDataStruc$Category == Category)
      # Plot the barplot for this single trait using gBarplot
      gBarplot(dfHerd, dfDataStruc$Trait[I], dfDataStruc$Label[I], dfDataStruc$Nearest[I], dfDataStruc$Increasing[I])
      
      # If there are exactly two traits in this category
    } else if (Class == 2){
      # Set the plotting area to have 1 row and 2 columns (side-by-side)
      par(mfrow = c(1, 2), oma = c(0, 0, 1, 0), mar = c(6, 4, 4, 3))  # oma sets outer margins
      
      Count <- 1
      # Loop over each trait in the current category
      for(I in which(dfDataStruc$Category == Category)){
        # Plot each trait side-by-side, show legend only for the first plot
        gBarplot(dfHerd, dfDataStruc$Trait[I], dfDataStruc$Label[I], dfDataStruc$Nearest[I], dfDataStruc$Increasing[I], T, Count == 1)
        Count <- Count + 1
      }
      # Add a main title above the two plots with category name and some global text GText
      mtext(paste(GText, Category, "Traits"), side = 3, outer = T, line = -0.5, font = 2, cex = 1.3)
      
      # If there are exactly three traits in this category
    } else if (Class == 3){
      # Define a custom layout for the plots: a 2x2 matrix with different widths
      layout(matrix(c(1, 2, 1, 3), nrow = 2, ncol = 2, byrow = TRUE), widths = c(7/12, 5/12))
      par(oma = c(0, 0, 1, 0), mar = c(6, 4, 4, 3))  # outer margins
      
      Count <- 1
      # Loop over each trait in this category
      for(I in which(dfDataStruc$Category == Category)){
        # Plot each trait with legend only on the first plot
        gBarplot(dfHerd, dfDataStruc$Trait[I], dfDataStruc$Label[I], dfDataStruc$Nearest[I], dfDataStruc$Increasing[I], T, Count == 1)
        Count <- Count + 1
      }
      # Add a main title for the group of traits
      mtext(paste(GText, Category, "Traits"), side = 3, outer = T, line = -0.5, font = 2, cex = 1.3)
      
      # If there are exactly four traits in this category
    } else if (Class == 4){
      # Set up a 2x2 plotting grid for the four traits
      par(mfrow = c(2, 2), oma = c(0, 0, 1, 0), mar = c(6, 4, 4, 3))  # outer margins
      
      Count <- 1
      # Loop through the traits and adjust margins individually for each subplot
      for(I in which(dfDataStruc$Category == Category)){
        # Set margins based on position (to avoid crowding)
        par(mar = c(ifelse(Count %in% c(1, 2), 3, 4), 
                    ifelse(Count %in% c(2, 4), 3, 4),
                    ifelse(Count %in% c(3, 4), 3, 4), 2))
        # Plot the trait with legend on the first plot
        gBarplot(dfHerd, dfDataStruc$Trait[I], dfDataStruc$Label[I], dfDataStruc$Nearest[I], dfDataStruc$Increasing[I], T, Count == 1)
        Count <- Count + 1
      }
      # Add main title for the set of four traits
      mtext(paste(GText, Category, "Traits"), side = 3, outer = T, line = -0.5, font = 2, cex = 1.3)
    }
    
    # Close the PNG device to save the file
    dev.off()
  }
}

# Function to create barplots for a specific trait
gBarplot <- function(dfHerd, Trait, Label, Sig, Inc = TRUE, Trunc = FALSE, Leg = TRUE){
  
  # If all sire breeding numbers are missing (NA), prepare SireData and CowData without pregnancy info
  if(all(is.na(dfHerd$SSireHBN))){
    # Sire data columns: SireTrait and lactation class factor
    SireData <- dfHerd[, c(paste("Sire", Trait, sep=""), "FactLactClass")]
    
    # Cow data columns: Trait and lactation class factor
    CowData <- dfHerd[, c(Trait, "FactLactClass")]
    
  } else {
    # Otherwise, include pregnancy trait data combined with sire and cow data
    
    # SireData includes regular sire trait plus pregnancy trait as a "Pregnancy" lactation class
    SireData <- rbind(dfHerd[, c(paste("Sire", Trait, sep=""), "FactLactClass")],
                      setNames(data.frame(dfHerd[, paste("Preg", Trait, sep="")], rep("Pregnancy", nrow(dfHerd))),
                               c(paste("Sire", Trait, sep=""), "FactLactClass")))
    
    # CowData includes regular cow trait plus pregnancy PA trait as "Pregnancy" lactation class
    CowData <- rbind(dfHerd[, c(Trait, "FactLactClass")],
                     setNames(data.frame(dfHerd[, paste("PregPA", Trait, sep="")], rep("Pregnancy", nrow(dfHerd))),
                              c(Trait, "FactLactClass")))
  }
  
  # Calculate mean, 25th and 75th percentile for SireData by lactation class
  SireAnalysis <- tapply(SireData[, paste("Sire", Trait, sep="")], SireData$FactLactClass, mean, na.rm=TRUE)
  SireLower <- tapply(SireData[, paste("Sire", Trait, sep="")], SireData$FactLactClass, quantile, probs=0.25, na.rm=TRUE)
  SireUpper <- tapply(SireData[, paste("Sire", Trait, sep="")], SireData$FactLactClass, quantile, probs=0.75, na.rm=TRUE)
  
  # Calculate mean, 25th and 75th percentile for CowData by lactation class
  CowAnalysis <- tapply(CowData[, Trait], CowData$FactLactClass, mean, na.rm=TRUE)
  CowLower <- tapply(CowData[, Trait], CowData$FactLactClass, quantile, probs=0.25, na.rm=TRUE)
  CowUpper <- tapply(CowData[, Trait], CowData$FactLactClass, quantile, probs=0.75, na.rm=TRUE)
  
  # Determine y-axis limits for plot based on min and max quantiles rounded to Sig increment
  MyYLim <- c(min(0, floor(min(CowLower, SireLower, na.rm=TRUE) / Sig) * Sig),
              ceiling(max(CowUpper, SireUpper, na.rm=TRUE) / Sig) * Sig)
  if(MyYLim[2] < 0){
    MyYLim[2] <- 0
  }
  
  # Create a barplot of sire means by lactation class with custom colors and main title
  Plot <- barplot(SireAnalysis, col = MyBlue, main = ifelse(Trunc, Label, paste(GText, Label)),
                  width = 0.75, space = 0.5, xaxt = "n", ylab = Label,
                  ylim = MyYLim)
  
  # Overlay red rectangles representing mean values for cows (dairy animals)
  rect(Plot - 0.5, 0, Plot + 0.1, as.numeric(CowAnalysis), col = MyRed)
  
  par(xpd = TRUE)  # Allow plotting outside plot region
  
  # Add x-axis ticks and labels for lactation classes
  axis(1, at = c((0:7) * 1.125) + 0.125, labels = FALSE, pos = 0)
  axis(1, at = c((0:6) * 1.125) + 0.125 + (1.125 / 2), labels = levels(SireData$FactLactClass), cex.axis = 0.8, lwd = NA)
  
  #par(xpd = FALSE) # Reset to default after axis drawing
  
  # Add a legend if requested
  if(Leg){
    legend(ifelse(Inc, "topleft", "topright"), c("Sires", "Dairy Animals", "IQ Range"),
           pch = c(22, 22, NA), pt.bg = c(MyBlue, MyRed, NA), lwd = c(NA, NA, 2))
  }
  
  # Draw lines representing upper and lower quartiles for Sires (blue)
  lines(matrix(c(apply(data.frame(Plot - 0.1, as.numeric(SireUpper), Plot + 0.1, as.numeric(SireUpper), NA, NA),
                       1, unlist)), ncol = 2, nrow = length(Plot) * 3 * 2, byrow = TRUE), lwd = 2, col = MyDarkBlue)
  lines(matrix(c(apply(data.frame(Plot - 0.1, as.numeric(SireLower), Plot + 0.1, as.numeric(SireLower), NA, NA),
                       1, unlist)), ncol = 2, nrow = length(Plot) * 3 * 2, byrow = TRUE), lwd = 2, col = MyDarkBlue)
  lines(matrix(c(apply(data.frame(Plot, as.numeric(SireLower), Plot, as.numeric(SireUpper), NA, NA),
                       1, unlist)), ncol = 2, nrow = length(Plot) * 3 * 2, byrow = TRUE), lwd = 1, lty = 2, col = MyDarkBlue)
  
  # Draw lines representing upper and lower quartiles for Dairy Animals (red)
  lines(matrix(c(apply(data.frame(Plot - 0.25, as.numeric(CowUpper), Plot - 0.15, as.numeric(CowUpper), NA, NA),
                       1, unlist)), ncol = 2, nrow = length(Plot) * 3 * 2, byrow = TRUE), lwd = 2, col = MyDarkRed)
  lines(matrix(c(apply(data.frame(Plot - 0.25, as.numeric(CowLower), Plot - 0.15, as.numeric(CowLower), NA, NA),
                       1, unlist)), ncol = 2, nrow = length(Plot) * 3 * 2, byrow = TRUE), lwd = 2, col = MyDarkRed)
  lines(matrix(c(apply(data.frame(Plot - 0.2, as.numeric(CowLower), Plot - 0.2, as.numeric(CowUpper), NA, NA),
                       1, unlist)), ncol = 2, nrow = length(Plot) * 3 * 2, byrow = TRUE), lwd = 1, lty = 2, col = MyDarkRed)
}

#ProduceGTs(dfHerd, dfDataStruc, "/data/dairy/USERS/ashenoy/")

# original
#gProduceCorrelationPlots <- function(dfHerd, dfDataStruc, StrAuditDestination){
#gProduceCorrelationPlots <- function(dfHerd, dfDataStruc, StrAuditDestination, ShowIDs = T){
 # ShowIDs <- ShowIDs
  #for(I in 1 : nrow(dfDataStruc)){
   # if(dfDataStruc$Phenotype[I] == ""){
    #  next
    #}
    #cat(dfDataStruc[I, "Trait"], "Correlation Plot\n")
    #for(X in 1 : length(unlist(strsplit(dfDataStruc$Phenotype[I], ",")))){
     # if(! all(is.na(dfHerd[, unlist(strsplit(dfDataStruc$Phenotype[I], ","))[X]]))){
        
      #  ProducePNG(paste(StrAuditDestination,dfDataStruc[I,"Trait"],"_cf_",unlist(strsplit(dfDataStruc$Phenotype[I],","))[X],"Corr.png",sep=""))
        # original
       # gCorrelationPlot(dfHerd,dfDataStruc[I,"Trait"],unlist(strsplit(dfDataStruc$Phenotype[I],","))[X],dfDataStruc[I,"Label"],PTA=F)
        
        #dev.off()
        
        #ProducePNG(paste(StrAuditDestination,"Sire",dfDataStruc[I,"Trait"],"_cf_",unlist(strsplit(dfDataStruc$Phenotype[I],","))[X],"Corr.png",sep=""))
        #layout(matrix(c(0,1,1,2,2,1,2,2,0),ncol=3,nrow=3,byrow=T),widths = c(2/3,1/3,1/3),heights = c(1/3,1/3,2/3))
        #par(oma=c(0,0,1,0),mar=c(5,4,2,0.5))#plt=c( 0.1,2/3, 0.1, 2/3)
        # original
        #gCorrelationPlot(dfHerd,paste("Sire",dfDataStruc[I,"Trait"],sep=""),unlist(strsplit(dfDataStruc$Phenotype[I],","))[X],paste("Sire",dfDataStruc[I,"Label"]),T,PTA=T)
        #gCorrelationPlot(dfHerd,dfDataStruc[I,"Trait"],unlist(strsplit(dfDataStruc$Phenotype[I],","))[X],dfDataStruc[I,"Label"],PTA=F)
        #dev.off()
    #  }
    #}
  #}
#}

gInbreedingPlot <- function(dfHerd){
  layout(matrix(1:2),heights=c(3/6,2/6))
  par(mar=c(4,4,4,1))
  InbData<-aggregate(InbreedingPercent~FactLactClass,dfHerd,mean,na.rm=T)
  dfDiff<-InbData[match(levels(dfHerd$FactLactClass)[2:6],InbData$FactLactClass),"InbreedingPercent"]-InbData[match(levels(dfHerd$FactLactClass)[1:5],InbData$FactLactClass),"InbreedingPercent"]
  dfDiff<-c(0,dfDiff)
  Pos<-barplot(dfDiff,names="",ylim=c(floor(min(dfDiff,na.rm=T)/1)*1,ceiling(max(dfDiff,na.rm=T)/1)*1),main="Average Change in Inbreeding",ylab="Change in Inbreeding %",col=MyDarkBlue)
  abline(h=0.25,col=MyRed,lwd=2,lty=2)
  abline(h=-0.25,col=MyBlue,lwd=2,lty=2)
  abline(h=0)
  axis(1,c(min(Pos)-1,Pos+0.6,max(Pos)+1.2),labels=NA)
  axis(1,c(Pos),labels=InbData[match(levels(dfHerd$FactLactClass)[1:6],InbData$FactLactClass),"FactLactClass"],lwd=NA)
  abline(v=c(min(Pos)-1.2,Pos+0.6,max(Pos)+1.2),lty=2,col=rgb(219,217,214,maxColorValue = 255))
  par(mar=c(5,4,0,1))
  barplot(InbData$InbreedingPercent,names=InbData$FactLactClass,col=rgb(100,101,105,maxColorValue = 255),xlab="Lactation Group",xlim=c(0.05,7.35),ylab="Inbreeding %")
  abline(h=0)
}
  
#gCorrelationPlot <- function(dfHerd,strGtTrait,strPhTrait,strGTLabel,Trunc=F,PTA=T, ShowIDs = F){
#gCorrelationPlot <- function(dfHerd,strGtTrait,strPhTrait,strGTLabel,Trunc=F,PTA=T){
#  if(strPhTrait=="DOPN"){
#    strPhLabel<-"DOPN"
#  }else if(strPhTrait=="KG305ME"){
#    strPhLabel<-"305 ME"
#  }else if(strPhTrait=="KGFat305ME"){
#    strPhLabel<-"305 ME Fat Kg"
#  }else if(strPhTrait=="KGProt305ME"){
#    strPhLabel<-"305 ME Protein Kg"
#  }else if(strPhTrait=="KGProt305ME"){
#    strPhLabel<-"305 ME Protein Kg"
#  }else if(strPhTrait=="TestFatPc"){
#    strPhLabel<-"Test Fat %"
#  }else if(strPhTrait=="TestProtPc"){
#    strPhLabel<-"Test Protein %"
#  }
#  if(!PTA){
#    dfHerd[,strGtTrait]<-dfHerd[,strGtTrait]*2
#  }
#  if(length(dfHerd[!is.na(dfHerd[,strPhTrait]),strPhTrait])<5){
 #   return(paste("Not enough phenotype Data for",strPhTrait))
  #}
  #Chris C....colours not appearing correctly, by removeing the NAs it corrected it
  #dfHerd <- dfHerd[!is.na(dfHerd[,strPhTrait]) & !is.na(dfHerd[,strGtTrait]) ,]
  
  #dfHerd$FactLactClass<-as.factor(dfHerd$FactLactClass)
  #dfHerd$FactLactClass <- factor(dfHerd$FactLactClass, levels = c("4+","3","2","1"))
  
  #if ("Yield305DayLact"%in% names(dfHerd)){
   # dfHerd$FactLactClass<-dfHerd$Yield305DayLact
  #  dfHerd$FactLactClass[dfHerd$FactLactClass > 3] <- "4+"
  #  dfHerd$FactLactClass<- factor(dfHerd$FactLactClass, levels = rev(c( "4+","3","2","1")), ordered = T)
#  } else if ("LactNo_305"%in% names(dfHerd) ){
 #   dfHerd$FactLactClass<-dfHerd$Yield305DayLact
#    dfHerd$FactLactClass[dfHerd$FactLactClass > 3] <- "4+"
 #   dfHerd$FactLactClass<- factor(dfHerd$FactLactClass, levels = rev(c( "4+","3","2","1")), ordered = T)
  #}
  
  
#  Cols <- colorRampPalette(c(rgb(206,17,65,maxColorValue = 255),rgb(169,168,169,maxColorValue =255),rgb(29,79,145,maxColorValue =255)),bias =2)(max(as.numeric(dfHerd$FactLactClass),na.rm=T))
#  strMain <- paste(ifelse(Trunc,paste(strGTLabel,"cf.",strPhLabel),gsub("###Gen###",strGTLabel,gsub("###Perf###",strPhLabel,CText))),ifelse(PTA,"PTA","(g)EBV"))
  
 # plot(dfHerd[, strGtTrait], dfHerd[, strPhTrait], bg = Cols[as.numeric(dfHerd$FactLactClass)], col = Cols[as.numeric(dfHerd$FactLactClass)],
  #     pch = ifelse(dfHerd$GenomicIndicator %in% "G",16,1), main = strMain,type = "n", xlab = paste(strGTLabel, ifelse(PTA, "PTA", "(g)EBV")), ylab = strPhLabel)
  
  #rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
#  points(dfHerd[, strGtTrait], dfHerd[, strPhTrait],bg=Cols[as.numeric(dfHerd$FactLactClass)],col=Cols[as.numeric(dfHerd$FactLactClass)],pch=ifelse(dfHerd$GenomicIndicator%in%"G",16,1))
  
 # lmModel<-lm(as.formula(paste(strPhTrait,"~",strGtTrait)),dfHerd)
  #abline(lmModel,lwd=2,lty=2,col=MyDarkBlue)
  #Argh<-summary(lmModel)$adj.r.squared
  #if(Argh<0.001){
  #  Argh<-"<0.001"
  #}else{
  #  Argh<-round(Argh,3)
  #}
  #M<-summary(lmModel)$coefficients[2,1]
  #C<-summary(lmModel)$coefficients[1,1]
  #if(abs(M)<0.001){
  #  M<-ifelse(M<0,"<0.-001","<0.001")
  #}else{
  #  M<-round(M,3)
  #}
  #if(abs(C)<0.001){
  #  C<-ifelse(C<0,"<0.-001","<0.001")
  #}else{
  #  C<-round(C,ifelse(C>1000,0,3))
  #}
  #if(!Trunc){
  #  text(par("usr")[2],
  #       ifelse(strPhTrait=="DOPN",
  #              par("usr")[4]-strheight(bquote(.(strPhLabel)==.(strGTLabel)%*%.(M)+.(C))),
  #              par("usr")[3]+strheight(bquote(.(strPhLabel)==.(strGTLabel)%*%.(M)+.(C)))+1.2*strheight(bquote(bold({r^2}==.(Argh))))),
  #       bquote(.(strPhLabel)==.(strGTLabel)%*%.(M)+.(C)),pos=2)
  #  text(par("usr")[2],
  #       ifelse(strPhTrait=="DOPN",
  #              par("usr")[4]-1.2*strheight(bquote(bold({r^2}==.(Argh))))-strheight(bquote(.(strPhLabel)==.(strGTLabel)%*%.(M)+.(C))),
  #              par("usr")[3]+strheight(bquote(bold({r^2}==.(Argh))))),
  #       bquote({r^2}==.(Argh)),pos=2)
  #  legend(
  #    "topleft",
  #    c(paste("Lact",1:3),"Lact 4+" ,"PA","GT"), 
  #    pch=c(rep(15,max(dfHerd$FactLactClass,na.rm=T)),1,16),
  #    col=c(Cols,"black","black"))
  #}else{
  #  text(par("usr")[2],
  #       ifelse(strPhTrait=="DOPN",
  #              par("usr")[4]-strheight(bquote(.(strPhLabel)==.(strGTLabel)%*%.(M)+.(C))),
  #              par("usr")[3]+strheight(bquote(.(strPhLabel)==.(strGTLabel)%*%.(M)+.(C)))+1.25*strheight(bquote(bold({r^2}==.(Argh))))),
  #       bquote(''%*%.(M)+.(C)),pos=2)
  #  text(par("usr")[2],
  #       ifelse(strPhTrait=="DOPN",
  #              par("usr")[4]-1.25*strheight(bquote(bold({r^2}==.(Argh))))-strheight(bquote(.(strPhLabel)==.(strGTLabel)%*%.(M)+.(C))),
  #              par("usr")[3]+strheight(bquote(bold({r^2}==.(Argh))))),
  #       bquote({r^2}==.(Argh)),pos=2)
  #}
  
  
  # if(ShowIDs %in% T){
  #   # Creating unique tag for animals - removing herd prefix as all the same normaly
  #   Prefix <- unique(substring(dfHerd$Eartag, 0 ,6))
  #   
  #   # Could also use line number
  #   if(length(Prefix) %in% 1){
  #     dfHerd$tags <- substring(dfHerd$Eartag, 7)
  #     
  #     # pos of 4 is to the right of the dots cex 0.5 is half the normal font size
  #     text(dfHerd[, strGtTrait], dfHerd[, strPhTrait], labels = dfHerd$tags, pos = 4, cex = 0.5)
  #   } else {
  #     # Showing all eartag
  #     text(dfHerd[, strGtTrait], dfHerd[, strPhTrait], labels = dfHerd$Eartag, pos = 4, cex = 0.5)
  #     
  #   }
  #   
  # 
  # }
#}

TGenPlot<-function(dfHerd,intBeefPercentage=60){
  if(is.na(intBeefPercentage)){
    intBeefPercentage<-60
  }
  
  dfHerdTGenPlot <- dfHerd
  dfHerdTGenPlot <- dfHerdTGenPlot[!is.na(dfHerdTGenPlot$TGenPLI),]
  dfHerdTGenPlot <- dfHerdTGenPlot[!is.na(dfHerdTGenPlot$PLI),]
  
  decLabCex<-1.5
  plot(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],dfHerdTGenPlot$PLI[dfHerdTGenPlot$GenomicIndicator=="G"],pch=21,bg=MyBlue,xlab="3 Generation Sire Stack Parent Average",ylab="Genomic Test",main=paste("Sire Stack Parent Average PLI vs. Genomic Tested PLI\nShowing Selection Threshold at ",intBeefPercentage,"% Beef Usage",sep=""))
  abline(v=quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
  abline(h=quantile(dfHerdTGenPlot$PLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
  text(quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&
                                         dfHerdTGenPlot$TGenPLI>=quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                         dfHerdTGenPlot$PLI<quantile(dfHerdTGenPlot$PLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenPlot$TGenPLI))&(!is.na(dfHerdTGenPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=4,col=MyRed,font=2)
  
  text(quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&
                                         dfHerdTGenPlot$TGenPLI<quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                         dfHerdTGenPlot$PLI<quantile(dfHerdTGenPlot$PLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenPlot$TGenPLI))&(!is.na(dfHerdTGenPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=2,col=MyDarkBlue,font=2)
  
  text(quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&
                                         dfHerdTGenPlot$TGenPLI<quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                         dfHerdTGenPlot$PLI>=quantile(dfHerdTGenPlot$PLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenPlot$TGenPLI))&(!is.na(dfHerdTGenPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=2,col=MyRed,font=2)
  
  text(quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&
                                         dfHerdTGenPlot$TGenPLI>=quantile(dfHerdTGenPlot$TGenPLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                         dfHerdTGenPlot$PLI>=quantile(dfHerdTGenPlot$PLI[dfHerdTGenPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenPlot[dfHerdTGenPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenPlot$TGenPLI))&(!is.na(dfHerdTGenPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=4,col=MyDarkBlue,font=2)
}


TGenYoungstockPlot<-function(dfHerd,intBeefPercentage=60){
  if(is.na(intBeefPercentage)){
    intBeefPercentage<-60
  }
  
  dfHerdTGenYoungstockPlot <- dfHerd
  dfHerdTGenYoungstockPlot <- dfHerdTGenYoungstockPlot[!is.na(dfHerdTGenYoungstockPlot$TGenPLI),]
  dfHerdTGenYoungstockPlot <- dfHerdTGenYoungstockPlot[!is.na(dfHerdTGenYoungstockPlot$PLI),]
  dfHerdTGenYoungstockPlot <- dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$FactLactClass%in%c("Calf","Heifer"),]
  
  if(nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator %in%"G",]) == 0){
    
    cat("\nNo GT animals in Calf or Heifer")
    return(NULL)
  }
  
  decLabCex<-1.5
  plot(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],dfHerdTGenYoungstockPlot$PLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],pch=21,bg=MyBlue,xlab="3 Generation Sire Stack Parent Average",ylab="Genomic Test",main=paste("Sire Stack Parent Average PLI vs. Genomic Tested PLI\nShowing Selection Threshold at ",intBeefPercentage,"% Beef Usage For Youngstock",sep=""))
  abline(v=quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
  abline(h=quantile(dfHerdTGenYoungstockPlot$PLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
  text(quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&
                                                   dfHerdTGenYoungstockPlot$TGenPLI>=quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                   dfHerdTGenYoungstockPlot$PLI<quantile(dfHerdTGenYoungstockPlot$PLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenYoungstockPlot$TGenPLI))&(!is.na(dfHerdTGenYoungstockPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=4,col=MyRed,font=2)
  
  text(quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&
                                                   dfHerdTGenYoungstockPlot$TGenPLI<quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                   dfHerdTGenYoungstockPlot$PLI<quantile(dfHerdTGenYoungstockPlot$PLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenYoungstockPlot$TGenPLI))&(!is.na(dfHerdTGenYoungstockPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=2,col=MyDarkBlue,font=2)
  
  text(quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&
                                                   dfHerdTGenYoungstockPlot$TGenPLI<quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                   dfHerdTGenYoungstockPlot$PLI>=quantile(dfHerdTGenYoungstockPlot$PLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenYoungstockPlot$TGenPLI))&(!is.na(dfHerdTGenYoungstockPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=2,col=MyRed,font=2)
  
  text(quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&
                                                   dfHerdTGenYoungstockPlot$TGenPLI>=quantile(dfHerdTGenYoungstockPlot$TGenPLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                   dfHerdTGenYoungstockPlot$PLI>=quantile(dfHerdTGenYoungstockPlot$PLI[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenYoungstockPlot[dfHerdTGenYoungstockPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenYoungstockPlot$TGenPLI))&(!is.na(dfHerdTGenYoungstockPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=4,col=MyDarkBlue,font=2)
}

TGenMilkingHerdPlot<-function(dfHerd,intBeefPercentage=60){
  if(is.na(intBeefPercentage)){
    intBeefPercentage<-60
  }
  
  dfHerdTGenMilkingHerdPlot <- dfHerd
  dfHerdTGenMilkingHerdPlot <- dfHerdTGenMilkingHerdPlot[!is.na(dfHerdTGenMilkingHerdPlot$TGenPLI),]
  dfHerdTGenMilkingHerdPlot <- dfHerdTGenMilkingHerdPlot[!is.na(dfHerdTGenMilkingHerdPlot$PLI),]
  dfHerdTGenMilkingHerdPlot <- dfHerdTGenMilkingHerdPlot[!dfHerdTGenMilkingHerdPlot$FactLactClass%in%c("Calf","Heifer"),]
  
  decLabCex<-1.5
  plot(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],dfHerdTGenMilkingHerdPlot$PLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],pch=21,bg=MyBlue,xlab="3 Generation Sire Stack Parent Average",ylab="Genomic Test",main=paste("Sire Stack Parent Average PLI vs. Genomic Tested PLI\nShowing Selection Threshold at ",intBeefPercentage,"% Beef Usage For Milking Herd",sep=""))
  abline(v=quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
  abline(h=quantile(dfHerdTGenMilkingHerdPlot$PLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
  text(quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&
                                                    dfHerdTGenMilkingHerdPlot$TGenPLI>=quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                    dfHerdTGenMilkingHerdPlot$PLI<quantile(dfHerdTGenMilkingHerdPlot$PLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenMilkingHerdPlot$TGenPLI))&(!is.na(dfHerdTGenMilkingHerdPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=4,col=MyRed,font=2)
  
  text(quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&
                                                    dfHerdTGenMilkingHerdPlot$TGenPLI<quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                    dfHerdTGenMilkingHerdPlot$PLI<quantile(dfHerdTGenMilkingHerdPlot$PLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenMilkingHerdPlot$TGenPLI))&(!is.na(dfHerdTGenMilkingHerdPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=2,col=MyDarkBlue,font=2)
  
  text(quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&
                                                    dfHerdTGenMilkingHerdPlot$TGenPLI<quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                    dfHerdTGenMilkingHerdPlot$PLI>=quantile(dfHerdTGenMilkingHerdPlot$PLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenMilkingHerdPlot$TGenPLI))&(!is.na(dfHerdTGenMilkingHerdPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=2,col=MyRed,font=2)
  
  text(quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),
       par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
       paste(round(nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&
                                                    dfHerdTGenMilkingHerdPlot$TGenPLI>=quantile(dfHerdTGenMilkingHerdPlot$TGenPLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T)&
                                                    dfHerdTGenMilkingHerdPlot$PLI>=quantile(dfHerdTGenMilkingHerdPlot$PLI[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"],intBeefPercentage/100,na.rm=T),])/
                     nrow(dfHerdTGenMilkingHerdPlot[dfHerdTGenMilkingHerdPlot$GenomicIndicator=="G"&(!is.na(dfHerdTGenMilkingHerdPlot$TGenPLI))&(!is.na(dfHerdTGenMilkingHerdPlot$PLI)),])*100,
                   0),"%",sep="")
       ,cex=decLabCex,pos=4,col=MyDarkBlue,font=2)
}


PlotPliSmooth<-function(dfHerd){
  dfHerd<-dfHerd[order(dfHerd$FactLactClass,decreasing=T),]
  Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(29,79,145,maxColorValue =255),rgb(169,168,169,maxColorValue =255)))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  MyHistogram<-ggplot(dfHerd,aes(x=PLI)) +
    geom_density(data=subset(dfHerd,FactLactClass == "4+"),alpha=0.7,position="identity",aes(fill="4+")) +
    geom_density(data=subset(dfHerd,FactLactClass == "4+"),position="identity",show.legend=F,fill=NA,aes(fill=NA,colour="4+")) + 
    geom_density(data=subset(dfHerd,FactLactClass == "3"),alpha=0.7,position="identity",aes(fill="3")) +
    geom_density(data=subset(dfHerd,FactLactClass == "3"),position="identity",show.legend=F,fill=NA,aes(fill=NA,colour="3")) + 
    geom_density(data=subset(dfHerd,FactLactClass == "2"),alpha=0.7,position="identity",aes(fill="2")) +
    geom_density(data=subset(dfHerd,FactLactClass == "2"),position="identity",show.legend=F,fill=NA,aes(fill=NA,colour="2")) + 
    geom_density(data=subset(dfHerd,FactLactClass == "1"),alpha=0.7,position="identity",aes(fill="1")) +
    geom_density(data=subset(dfHerd,FactLactClass == "1"),position="identity",show.legend=F,fill=NA,aes(fill=NA,colour="1")) + 
    geom_density(data=subset(dfHerd,FactLactClass == "Heifer"),alpha=0.7,position="identity",aes(fill="Heifer")) +
    geom_density(data=subset(dfHerd,FactLactClass == "Heifer"),position="identity",show.legend=F,fill=NA,aes(fill=NA,colour="Heifer")) + 
    geom_density(data=subset(dfHerd,FactLactClass == "Calf"),alpha=0.7,position="identity",aes(fill="Calf")) +
    geom_density(data=subset(dfHerd,FactLactClass == "Calf"),position="identity",show.legend=F,fill=NA,aes(fill=NA,colour="Calf")) + 
    aes(y=stat(count)/sum(stat(count))) + theme_bw()+ggtitle(paste("Histogram Showing Distribution on PLI by Lactation Group"))+
    labs(x="PLI",y="Proportion of the Herd / Batch")+scale_fill_manual(name = 'Demographic', 
                                                                       values =Cols,breaks=names(Cols),labels=names(Cols))+scale_color_manual(values =Cols)+ guides(fill = guide_legend(override.aes = list(alpha = 0.5)))+
    theme(text = element_text(size=12),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(),legend.position=c(0.08,0.7),plot.title = element_text(hjust=0.5,size=13,face="bold",margin=margin(0,0,3,0,unit="pt"),vjust=3), axis.title  = element_text(size=12,face="bold"),axis.title.x = element_text(vjust=-3),axis.title.y = element_text(vjust=3),plot.margin=margin(0.75,0.25,1,1,unit="cm"))+  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  return(MyHistogram)
}

gDemographic <- function(dfHerd) {
  Cols <- colorRampPalette(c(rgb(206,17,65,maxColorValue=255),
                             rgb(29,79,145,maxColorValue=255),
                             rgb(169,168,169,maxColorValue=255)))(6)
  
  # Raw counts
  count_G <- tapply(dfHerd$GenomicIndicator[dfHerd$GenomicIndicator == "G"], 
                    dfHerd$FactLactClass[dfHerd$GenomicIndicator == "G"], length)
  count_U <- tapply(dfHerd$GenomicIndicator[dfHerd$GenomicIndicator != "G"], 
                    dfHerd$FactLactClass[dfHerd$GenomicIndicator != "G"], length)
  
  # Replace NAs with 0
  count_G[is.na(count_G)] <- 0
  count_U[is.na(count_U)] <- 0
  
  # Align both tables to the same names
  all_classes <- union(names(count_G), names(count_U))
  count_G <- count_G[all_classes]; count_G[is.na(count_G)] <- 0
  count_U <- count_U[all_classes]; count_U[is.na(count_U)] <- 0
  
  # Remove 'Pregnancy'
  count_G <- count_G[names(count_G) != "Pregnancy"]
  count_U <- count_U[names(count_U) != "Pregnancy"]
  
  counts_matrix <- rbind(count_G, count_U)
  percent_matrix <- round(counts_matrix / rep(colSums(counts_matrix), each=2) * 100, 0)
  rownames(percent_matrix) <- c("Genomic Tested", "Untested")
  
  # Total counts for top barplot
  total_counts <- colSums(counts_matrix)
  
  layout(matrix(c(1,2)), heights=c(1/3,2/3))
  
  # Top barplot: Herd Demographic
  par(mar=c(0,4,4,1))
  top_bar_midpoints <- barplot(total_counts,
                               col=rev(Cols),
                               main="Herd Demographic",
                               xlim=c(0.4, 8.6),
                               xaxt="n")
  
  # Add raw total counts on top
  text(x = top_bar_midpoints,
       y = total_counts - 12,
       labels = total_counts,
       col = "white", cex = 1.2, font = 2)
  
  # Bottom barplot: % Genomic Tested vs Untested
  par(mar=c(5,4,3,1))
  bar_midpoints <- barplot(percent_matrix,
                           col=c(MyRed, MyDarkBlue),
                           legend.text=rownames(percent_matrix),
                           xlim=c(0.4,8.6),
                           main="Percentage of Genomic Tested Animals by Demographic",
                           xlab="Demographic", ylab="Percentage")
  
  # Add raw counts with position logic
  for (j in 1:ncol(percent_matrix)) {
    # Genomic Tested (bottom of bar)
    if (percent_matrix[1, j] >= 10) {
      text(x=bar_midpoints[j], 
           y=5,  # near the bottom
           labels=paste0(percent_matrix[1, j], "%"),
           col="white", cex=1.2, font = 2)
    }
    # Untested (top of stack)
    if (percent_matrix[2, j] >= 10) {
      total_height <- sum(percent_matrix[, j])
      text(x=bar_midpoints[j], 
           y=total_height - 5,  # near the top
           labels=paste0(percent_matrix[2, j], "%"),
           col="white", cex=1.2, font = 2)
    }
  }
}

gPLIRainbow<-function(dfHerd,intBeefPercentage=""){
  #Rainbow plot for combined PA and GT data each colored for lactation group
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$PLI,col=rainbow(max(as.numeric(dfHerd$CurrentLact),na.rm=T))[as.numeric(dfHerd$CurrentLact)+1],xlab="Date Of Birth",ylab="PLI",main="PLI by Birthdate",pch=ifelse(dfHerd$GenomicIndicator!="G",1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1))
  #abline(v=as.numeric(as.Date("2016-09-01")),lwd=2,lty=2)
  #abline(v=as.numeric(quantile(dfHerd$CI,as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col="darkred",lty=2)
  if(!is.na(as.numeric(intBeefPercentage))){
    abline(h=as.numeric(quantile(dfHerd$PLI[!dfHerd$FactLactClass%in%c("Pregnancy","Calf")],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  }
  axis(1,at=
         seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
             seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
  legend(
    "topleft",
    c(paste("Lact",1:max(as.numeric(dfHerd$CurrentLact),na.rm=T)-1),"PA","GT"), 
    pch=c(rep(15,max(as.numeric(dfHerd$CurrentLact),na.rm=T)),1,16),
    col=c(rainbow(max(as.numeric(dfHerd$CurrentLact),na.rm=T)),"black","black"))
}

gPLIRainbowEC <- function(dfHerd,intBeefPercentage=""){
  #Rainbow plot for combined PA and GT data each colored for lactation group
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$Enviro.Cow,col=rainbow(max(as.numeric(dfHerd$CurrentLact),na.rm=T))[as.numeric(dfHerd$CurrentLact)+1],xlab="Date Of Birth",ylab="Enviro Cow",main="Enviro Cow by Birthdate",pch=ifelse(dfHerd$GenomicIndicator!="G",1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1))
  #abline(v=as.numeric(as.Date("2016-09-01")),lwd=2,lty=2)
  #abline(v=as.numeric(quantile(dfHerd$CI,as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col="darkred",lty=2)
  if(!is.na(as.numeric(intBeefPercentage))){
    abline(h=as.numeric(quantile(dfHerd$Feed.Advantage[!dfHerd$FactLactClass%in%c("Pregnancy","Calf")],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  }
  axis(1,at=
         seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
             seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
  legend(
    "topleft",
    c(paste("Lact",1:max(as.numeric(dfHerd$CurrentLact),na.rm=T)-1),"PA","GT"), 
    pch=c(rep(15,max(as.numeric(dfHerd$CurrentLact),na.rm=T)),1,16),
    col=c(rainbow(max(as.numeric(dfHerd$CurrentLact),na.rm=T)),"black","black"))
}

gPLIRainbowFA <- function(dfHerd,intBeefPercentage=""){
  #Rainbow plot for combined PA and GT data each colored for lactation group
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$Feed.Advantage,col=rainbow(max(as.numeric(dfHerd$CurrentLact),na.rm=T))[as.numeric(dfHerd$CurrentLact)+1],xlab="Date Of Birth",ylab="Feed Advantage",main="Feed Advantage by Birthdate",pch=ifelse(dfHerd$GenomicIndicator!="G",1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1))
  #abline(v=as.numeric(as.Date("2016-09-01")),lwd=2,lty=2)
  #abline(v=as.numeric(quantile(dfHerd$CI,as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col="darkred",lty=2)
  if(!is.na(as.numeric(intBeefPercentage))){
    abline(h=as.numeric(quantile(dfHerd$Feed.Advantage[!dfHerd$FactLactClass%in%c("Pregnancy","Calf")],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  }
  axis(1,at=
         seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
             seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
  legend(
    "topleft",
    c(paste("Lact",1:max(as.numeric(dfHerd$CurrentLact),na.rm=T)-1),"PA","GT"), 
    pch=c(rep(15,max(as.numeric(dfHerd$CurrentLact),na.rm=T)),1,16),
    col=c(rainbow(max(as.numeric(dfHerd$CurrentLact),na.rm=T)),"black","black"))
}


gPLIClass<-function(dfHerd,intBeefPercentage=""){
  Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(29,79,145,maxColorValue =255),rgb(169,168,169,maxColorValue =255)))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$PLI,col=Cols[as.character(dfHerd$FactLactClass)],xlab="Date Of Birth",ylab="PLI",main="PLI by Birthdate",pch=ifelse(dfHerd$GenomicIndicator!="G",1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1)
  )
  if(!is.na(as.numeric(intBeefPercentage))){
    abline(h=as.numeric(quantile(dfHerd$PLI[!dfHerd$FactLactClass%in%c("Pregnancy","Calf")],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  }
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]",names(Cols)),paste("Lact",names(Cols)),names(Cols)),"PA","GT"), 
    pch=c(rep(15,6),1,16),
    col=c(Cols,"Black","Black"))
  
  
  axis(1, at = seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
                   seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
}

#V1 showing the whole herd
# Adding percentile levels on right axis ** showing percent increase and only older than 12 month old
gPLIClassPerc <- function(dfHerd, intBeefPercentage = ""){
  
  # Setting colours
  Cols <- colorRampPalette(c(rgb(206, 17, 65, maxColorValue = 255), rgb(29, 79, 145, maxColorValue = 255),
                             rgb(169, 168, 169, maxColorValue = 255)))(6)
  Cols <- setNames(Cols, c("Calf", "Heifer", "1", "2", "3", "4+"))
  
  dfHerdPer <- dfHerd[as.Date(dfHerd$DatDOB, "%d/%m/%Y") < (Sys.Date() - 365),]
  
  ## Getting percentiles for the herd
  quants <- quantile(dfHerdPer$PLI, probs = c(0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = T)
  listquants <- c(quants[[1]], quants[[2]], quants[[3]], quants[[4]], quants[[5]], quants[[6]], quants[[7]], quants[[8]], quants[[9]])
  
  ## add extra space to right margin of plot within frame
  par(mar = c(5, 4, 4, 6) + 0.1)
  
  # Plotting normal PLI over year plot
  plot(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), dfHerd$PLI, col = Cols[as.character(dfHerd$FactLactClass)],
       xlab = "Date Of Birth", ylab = "PLI", main = "PLI by Birthdate", pch = ifelse(dfHerd$GenomicIndicator != "G", 1, 16),
       xlim = c(as.Date(paste0(format(min(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), "%Y-%m"), "-01")),
                seq(as.Date(paste0(format(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), "%Y-%m"), "-01")), length.out = 1, by = "months")[1] - 1))
  
  # Setting the 9 tick markers to replace the values of teh percentiles
  PercetLabs <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
  
  # #axis(1, at=c(0:71), NA, cex.axis=.7, font=1, tck=.01)
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[1]], cex = 0.7, "10%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[2]], cex = 0.7, "20%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[3]], cex = 0.7, "30%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[4]], cex = 0.7, "40%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[5]], cex = 0.7, "50%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[6]], cex = 0.7, "60%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[7]], cex = 0.7, "70%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[8]], cex = 0.7, "80%")
  # text(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), quants[[9]], cex = 0.7, "90%")
  
  # abline(h = as.numeric(quantile(dfHerd$PLI[! dfHerd$FactLactClass %in% c("Pregnancy", "Calf")],
  #                                
  #                                 as.numeric(intBeefPercentage) / 100, na.rm = T)), lwd = 3, col = MyDarkRed, lty = 2)
  # 
  # line(h = quants[[1]], v = max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), lwd = 1)
  
  axis(side = 4, at = c(round(listquants, 0)), las = 1, tick = 0.05, labels = PercetLabs,cex.axis=.7)
  mtext("Percentiles Markers", side = 4, col = "black", line = 3)
  
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]", names(Cols)), paste("Lact", names(Cols)), names(Cols)), "PA", "GT"),
    pch = c(rep(15, 6), 1, 16),
    col = c(Cols, "Black", "Black"))
  
  axis(1, at = seq(as.Date(paste0(format(min(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), "%Y-%m"), "-01")),
                   seq(as.Date(paste0(format(max(as.Date(dfHerd$DatDOB, "%d/%m/%Y"), na.rm = T), "%Y-%m"), "-01")), length.out = 2, by = "months")[2], by = "months"),
       labels = F, tcl = -0.25)
}


gPLIClassMatings<-function(dfHerd,intBeefPercentage=""){
  
  dfHerd$SSireBreedCode<-trimws(dfHerd$SSireBreedCode)
  #Mixed Semen (27) is technically beef and not dairy as Excel file stated
  dfHerd$SSireBreedCodeDairy[dfHerd$SSireBreedCode%in%c(1,2,3,4,5,6,7,12,13,15,17,20,22,23,24,28,31,41,42,43,
                                                        46,47,48,52,54,59,60,61,62,63,64,65,66,67,68,69,70,71,72,
                                                        73,74,75,76,77,78,79)]<-"Dairy"
  
  dfHerd$SSireBreedCodeDairy[dfHerd$SSireBreedCode%in%c(8,9,10,11,14,16,18,19,21,25,26,27,29,30,32,33,34,35,36,37,
                                                        38,39,41,44,45,50,51,53,55,56,57)]<-"Beef"
  
  dfHerd$SSireBreedCodeDairy[!dfHerd$SSireBreedCodeDairy%in%"Dairy"&!dfHerd$SSireBreedCodeDairy%in%"Beef"]<-"Open"
  
  #Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(29,79,145,maxColorValue =255),rgb(169,168,169,maxColorValue =255)))(6)
  #Cols<-c("snow","#404281","#A9A8A9")
  Cols<-c(MyRed,"#404281","#A9A8A9")
  Cols<-setNames(Cols,c("Open","Dairy","Beef"))
  
  dfHerdPer <- dfHerd[as.Date(dfHerd$DatDOB, "%d/%m/%Y") < (Sys.Date() - 365),]
  
  ## Getting percentiles for the herd
  quants <- quantile(dfHerdPer$PLI, probs = c(0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = T)
  listquants <- c(quants[[1]], quants[[2]], quants[[3]], quants[[4]], quants[[5]], quants[[6]], quants[[7]], quants[[8]], quants[[9]])
  
  ## add extra space to right margin of plot within frame
  par(mar = c(5, 4, 4, 6) + 0.1)
  
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$PLI,col=Cols[as.character(dfHerd$SSireBreedCodeDairy)],
       xlab="Date Of Birth",ylab="PLI",main="PLI by Birthdate",
       pch=ifelse(!dfHerd$SSireBreedCodeDairy%in%c("Dairy","Beef") ,1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1)
  )
  # Setting the 9 tick markers to replace the values of teh percentiles
  PercetLabs <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
  
  axis(side = 4, at = c(round(listquants, 0)), las = 1, tick = 0.05, labels = PercetLabs,cex.axis=.7)
  mtext("Percentiles Markers", side = 4, col = "black", line = 3)
  
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]",names(Cols)),paste("Lact",names(Cols)),names(Cols))), 
    pch=c(1,16,16),
    col=c(Cols))
  
  axis(1, at = seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
                   seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
}


gPLIClassMatingsFull<-function(dfHerd,intBeefPercentage=""){
  
  dfHerd$SSireBreedCode<-trimws(dfHerd$SSireBreedCode)
  #Mixed Semen (27) is technically beef and not dairy as Excel file stated
  dfHerd$SSireBreedCodeDairy[dfHerd$SSireBreedCode%in%c(1,2,3,4,5,6,7,12,13,15,17,20,22,23,24,28,31,41,42,43,
                                                        46,47,48,52,54,59,60,61,62,63,64,65,66,67,68,69,70,71,72,
                                                        73,74,75,76,77,78,79)]<-"Dairy"
  
  dfHerd$SSireBreedCodeDairy[dfHerd$SSireBreedCode%in%c(8,9,10,11,14,16,18,19,21,25,26,27,29,30,32,33,34,35,36,37,
                                                        38,39,41,44,45,50,51,53,55,56,57)]<-"Beef"
  
  dfHerd$SSireBreedCodeDairy[!dfHerd$SSireBreedCodeDairy%in%"Dairy"&!dfHerd$SSireBreedCodeDairy%in%"Beef"]<-"Open"
  dfHerd$SSireBreedCodeDairy[as.Date(dfHerd$DatDOB, "%d/%m/%Y") > (Sys.Date() - 456)]<-"Youngstock"
  
  Cols<-c(MyRed,"#404281","#A9A8A9",MyBrown)
  Cols<-setNames(Cols,c("Open","Dairy","Beef","Youngstock"))
  
  dfHerdPer <- dfHerd[as.Date(dfHerd$DatDOB, "%d/%m/%Y") < (Sys.Date() - 365),]
  
  ## Getting percentiles for the herd
  quants <- quantile(dfHerdPer$PLI, probs = c(0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = T)
  listquants <- c(quants[[1]], quants[[2]], quants[[3]], quants[[4]], quants[[5]], quants[[6]], quants[[7]], quants[[8]], quants[[9]])
  
  ## add extra space to right margin of plot within frame
  par(mar = c(5, 4, 4, 6) + 0.1)
  
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$PLI,col=Cols[as.character(dfHerd$SSireBreedCodeDairy)],xlab="Date Of Birth",
       ylab="PLI",main="PLI by Birthdate",pch=ifelse(!dfHerd$SSireBreedCodeDairy%in%c("Dairy","Beef") ,1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1)
  )
  # Setting the 9 tick markers to replace the values of teh percentiles
  PercetLabs <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
  
  axis(side = 4, at = c(round(listquants, 0)), las = 1, tick = 0.05, labels = PercetLabs,cex.axis=.7)
  mtext("Percentiles Markers", side = 4, col = "black", line = 3)
  
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]",names(Cols)),paste("Lact",names(Cols)),names(Cols))), 
    pch=c(1,16,16,1),
    col=c(Cols))
  
  axis(1, at = seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
                   seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
}


gPLIClassFA <- function(dfHerd,intBeefPercentage=""){
  Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(29,79,145,maxColorValue =255),rgb(169,168,169,maxColorValue =255)))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$Feed.Advantage,col=Cols[as.character(dfHerd$FactLactClass)],xlab="Date Of Birth",ylab="Feed Advantage",main="Feed Advantage by Birthdate",pch=ifelse(dfHerd$GenomicIndicator!="G",1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1)
  )
  # if(!is.na(as.numeric(intBeefPercentage))){
  #   abline(h=as.numeric(quantile(dfHerd$Feed.Advantage[!dfHerd$FactLactClass%in%c("Pregnancy","Calf")],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  # }
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]",names(Cols)),paste("Lact",names(Cols)),names(Cols)),"PA","GT"), 
    pch=c(rep(15,6),1,16),
    col=c(Cols,"Black","Black"))
  
  
  axis(1,at=
         seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
             seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
}


gPLIClassEC <- function(dfHerd,intBeefPercentage=""){
  Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(29,79,145,maxColorValue =255),rgb(169,168,169,maxColorValue =255)))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  plot(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),dfHerd$Enviro.Cow,col=Cols[as.character(dfHerd$FactLactClass)],xlab="Date Of Birth",ylab="Enviro Cow",main="Enviro Cow by Birthdate",pch=ifelse(dfHerd$GenomicIndicator!="G",1,16),
       xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1)
  )
  # if(!is.na(as.numeric(intBeefPercentage))){
  #   abline(h=as.numeric(quantile(dfHerd$Enviro.Cow[!dfHerd$FactLactClass%in%c("Pregnancy","Calf")],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  # }
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]",names(Cols)),paste("Lact",names(Cols)),names(Cols)),"PA","GT"), 
    pch=c(rep(15,6),1,16),
    col=c(Cols,"Black","Black"))
  
  
  axis(1,at=
         seq(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
             seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
}


gPLIClassECvFA <- function(dfHerd,intBeefPercentage=""){
  Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(29,79,145,maxColorValue =255),rgb(169,168,169,maxColorValue =255)))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  plot(dfHerd$Enviro.Cow,dfHerd$Feed.Advantage,col=Cols[as.character(dfHerd$FactLactClass)],xlab="Feed Advantage",ylab="Enviro Cow",main="Enviro Cow by Feed Advantage",pch=ifelse(dfHerd$GenomicIndicator!="G",1,16),
       # xlim=c(as.Date(paste(format(min(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
       #        seq(as.Date(paste(format(max(as.Date(dfHerd$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1)
  )
  # if(!is.na(as.numeric(intBeefPercentage))){
  #   abline(h=as.numeric(quantile(dfHerd$Enviro.Cow[!dfHerd$FactLactClass%in%c("Pregnancy","Calf")],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  # }
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]",names(Cols)),paste("Lact",names(Cols)),names(Cols)),"PA","GT"), 
    pch=c(rep(15,6),1,16),
    col=c(Cols,"Black","Black"))
}


CloseupBar <- function(dfHerd,gTrait){
  MilkPerc<-getMilkPerc()
  YoungPerc<-getYoungPerc()
  PlotResults<-rbind(c('Milking'=mean(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],na.rm=T),'Youngstock'=mean(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],na.rm=T),'Pregnancy'=mean(dfHerd[,paste("PregPA",gTrait,sep="")],na.rm=T)),c(mean(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],na.rm=T),mean(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],na.rm=T),mean(dfHerd[,paste("Preg",gTrait,sep="")],na.rm=T)))
  rownames(PlotResults)<-c("Herd","Sires")
  
  bP<-barplot(PlotResults,beside=T,ylim=c(min(0,PlotResults,MilkPerc[MilkPerc$Percentile==95,gTrait],YoungPerc[YoungPerc$Percentile==95,gTrait],quantile(NHMH[,gTrait],0.05,na.rm=T),na.rm=T),max(PlotResults,MilkPerc[MilkPerc$Percentile==5,gTrait],YoungPerc[YoungPerc$Percentile==5,gTrait],quantile(NHMH[,gTrait],0.95,na.rm=T),na.rm=T)),col=c(MyGray,rgb(100,101,105,maxColorValue = 255),MyRed,MyDarkRed,MyBlue,MyDarkBlue))
  MyBoxPlot(bP[1,1],MilkPerc[MilkPerc$Percentile==50,gTrait],MilkPerc[MilkPerc$Percentile==25,gTrait],MilkPerc[MilkPerc$Percentile==75,gTrait],MilkPerc[MilkPerc$Percentile==5,gTrait],MilkPerc[MilkPerc$Percentile==95,gTrait],MyGray,rgb(100,101,105,maxColorValue = 255))
  MyBoxPlot(bP[2,1],quantile(NHMH[,gTrait],0.5,na.rm=T),quantile(NHMH[,gTrait],0.75,na.rm=T),quantile(NHMH[,gTrait],0.25,na.rm=T),quantile(NHMH[,gTrait],0.95,na.rm=T),quantile(NHMH[,gTrait],0.05,na.rm=T),MyBlue,MyDarkBlue)
  
  MyBoxPlot(bP[1,2],YoungPerc[YoungPerc$Percentile==50,gTrait],YoungPerc[YoungPerc$Percentile==25,gTrait],YoungPerc[YoungPerc$Percentile==75,gTrait],YoungPerc[YoungPerc$Percentile==5,gTrait],YoungPerc[YoungPerc$Percentile==95,gTrait],MyRed,MyDarkRed)
  MyBoxPlot(bP[2,2],quantile(NHMH[,gTrait],0.5,na.rm=T),quantile(NHMH[,gTrait],0.75,na.rm=T),quantile(NHMH[,gTrait],0.25,na.rm=T),quantile(NHMH[,gTrait],0.95,na.rm=T),quantile(NHMH[,gTrait],0.05,na.rm=T),MyBlue,MyDarkBlue)
  
  MyBoxPlot(bP[1,2],YoungPerc[YoungPerc$Percentile==50,gTrait],YoungPerc[YoungPerc$Percentile==25,gTrait],YoungPerc[YoungPerc$Percentile==75,gTrait],YoungPerc[YoungPerc$Percentile==5,gTrait],YoungPerc[YoungPerc$Percentile==95,gTrait],MyRed,MyDarkRed)
  MyBoxPlot(bP[2,2],quantile(NHMH[,gTrait],0.5,na.rm=T),quantile(NHMH[,gTrait],0.75,na.rm=T),quantile(NHMH[,gTrait],0.25,na.rm=T),quantile(NHMH[,gTrait],0.95,na.rm=T),quantile(NHMH[,gTrait],0.05,na.rm=T),MyBlue,MyDarkBlue)
  
  MyBoxPlot(bP[1,3],YoungPerc[YoungPerc$Percentile==50,gTrait],YoungPerc[YoungPerc$Percentile==25,gTrait],YoungPerc[YoungPerc$Percentile==75,gTrait],YoungPerc[YoungPerc$Percentile==5,gTrait],YoungPerc[YoungPerc$Percentile==95,gTrait],MyRed,MyDarkRed)
  MyBoxPlot(bP[2,3],quantile(NHMH[,gTrait],0.5,na.rm=T),quantile(NHMH[,gTrait],0.75,na.rm=T),quantile(NHMH[,gTrait],0.25,na.rm=T),quantile(NHMH[,gTrait],0.95,na.rm=T),quantile(NHMH[,gTrait],0.05,na.rm=T),MyBlue,MyDarkBlue)
  
}

CloseupBox <- function(dfHerd,gTrait,Label,Sig,NHMH){
  MilkPerc<-getMilkPerc()
  YoungPerc<-getYoungPerc()
  PlotResults<-c(quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],c(0.95,0.05),na.rm=T),
                 quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],c(0.95,0.05),na.rm=T),
                 quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],c(0.95,0.05),na.rm=T),
                 quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],c(0.95,0.05),na.rm=T),
                 quantile(dfHerd[,paste("Preg",gTrait,sep="")],c(0.95,0.05),na.rm=T),
                 quantile(dfHerd[,paste("PregPA",gTrait,sep="")],c(0.95,0.05),na.rm=T),
                 MilkPerc[MilkPerc$Percentile%in%c(5,95),gTrait],
                 YoungPerc[YoungPerc$Percentile%in%c(5,95),gTrait],
                 quantile(NHMH[,gTrait],c(0.05,0.95),na.rm=T))
  #PlotResults<-rbind(c('Milking'=mean(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],na.rm=T),'Youngstock'=mean(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],na.rm=T),'Pregnancy'=mean(dfHerd[,paste("PregPA",gTrait,sep="")],na.rm=T)),c(mean(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],na.rm=T),mean(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],na.rm=T),mean(dfHerd[,paste("Preg",gTrait,sep="")],na.rm=T)))
  #rownames(PlotResults)<-c("Herd","Sires")
  
  par(mfrow=c(1,2),oma=c(0,0,1,0))
  plot(1,ylim=c(floor(min(PlotResults,na.rm=T)/Sig)*Sig,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1)),xlim=c(-0.2,2.45),type="n",xaxt="n",ylab=Label,xlab="",main="AHDB Holstein Averages (Herd PTAs)")
  
  MyBoxPlot(0,quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],0.5,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],0.75,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],0.45,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],0.95,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),gTrait],0.05,na.rm=T),MyGray,rgb(100,101,105,maxColorValue = 255))
  MyBoxPlot(0.5,MilkPerc[MilkPerc$Percentile==50,gTrait],MilkPerc[MilkPerc$Percentile==25,gTrait],MilkPerc[MilkPerc$Percentile==75,gTrait],MilkPerc[MilkPerc$Percentile==5,gTrait],MilkPerc[MilkPerc$Percentile==95,gTrait],rgb(169,168,169,maxColorValue = 255,alpha=255*1/3),rgb(100,101,105,maxColorValue = 255))
  text(0,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"Milking\nHerd",pos=1)
  text(0.5,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"AHDB\nAverage",pos=1)
  
  MyBoxPlot(1.25,quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],0.5,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],0.75,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],0.25,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],0.95,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),gTrait],0.05,na.rm=T),MyRed,MyDarkRed)
  MyBoxPlot(1.25+0.5,quantile(dfHerd[,paste("PregPA",gTrait,sep="")],0.5,na.rm=T),quantile(dfHerd[,paste("PregPA",gTrait,sep="")],0.75,na.rm=T),quantile(dfHerd[,paste("PregPA",gTrait,sep="")],0.25,na.rm=T),quantile(dfHerd[,paste("PregPA",gTrait,sep="")],0.95,na.rm=T),quantile(dfHerd[,paste("PregPA",gTrait,sep="")],0.05,na.rm=T),MyBlue,MyDarkBlue)
  MyBoxPlot(1.25+0.5+0.5,YoungPerc[YoungPerc$Percentile==50,gTrait],YoungPerc[YoungPerc$Percentile==25,gTrait],YoungPerc[YoungPerc$Percentile==75,gTrait],YoungPerc[YoungPerc$Percentile==5,gTrait],YoungPerc[YoungPerc$Percentile==95,gTrait],rgb(206,17,65,maxColorValue =255,alpha=255*2/3),MyDarkRed)
  text(1.25,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"Current\nYS",pos=1)
  text(1.25+0.5,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"Current\nPregs",pos=1)
  text(1.25+0.5+0.5,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"AHDB\nAverage\nYS",pos=1)
  abline(v=mean(c(0.75,1)),lwd=3,lend=1,lty=2)
  
  axis(1,c(0.25,1.75),labels=c("Milking","Youngstock"),lwd=NA)
  
  plot(1,ylim=c(floor(min(PlotResults,na.rm=T)/Sig)*Sig,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1)),xlim=c(-0.2,2.45),type="n",xaxt="n",ylab=Label,xlab="",main="NBDC Active List (Sire PTAs)",)
  MyBoxPlot(0,quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],0.5,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],0.75,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],0.45,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],0.95,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("4+","3","2","1"),paste("Sire",gTrait,sep="")],0.05,na.rm=T),MyBlue,MyDarkBlue)
  MyBoxPlot(0.5,quantile(NHMH[,gTrait],0.5,na.rm=T),quantile(NHMH[,gTrait],0.75,na.rm=T),quantile(NHMH[,gTrait],0.25,na.rm=T),quantile(NHMH[,gTrait],0.95,na.rm=T),quantile(NHMH[,gTrait],0.05,na.rm=T),rgb(125,155,193,maxColorValue =255,alpha=255*2/3),MyDarkBlue)
  text(0,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"Herd\nSires",pos=1)
  text(0.5,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"UK Active\nList",pos=1)
  
  MyBoxPlot(1.25,quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],0.5,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],0.75,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],0.45,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],0.95,na.rm=T),quantile(dfHerd[dfHerd$FactLactClass%in%c("Calf","Heifer"),paste("Sire",gTrait,sep="")],0.05,na.rm=T),MyBlue,MyDarkBlue)
  MyBoxPlot(1.25+0.5,quantile(dfHerd[,paste("Preg",gTrait,sep="")],0.5,na.rm=T),quantile(dfHerd[,paste("Preg",gTrait,sep="")],0.75,na.rm=T),quantile(dfHerd[,paste("Preg",gTrait,sep="")],0.25,na.rm=T),quantile(dfHerd[,paste("Preg",gTrait,sep="")],0.95,na.rm=T),quantile(dfHerd[,paste("Preg",gTrait,sep="")],0.05,na.rm=T),MyBlue,MyDarkBlue)
  MyBoxPlot(1.25+0.5+0.5,quantile(NHMH[,gTrait],0.5,na.rm=T),quantile(NHMH[,gTrait],0.75,na.rm=T),quantile(NHMH[,gTrait],0.25,na.rm=T),quantile(NHMH[,gTrait],0.95,na.rm=T),quantile(NHMH[,gTrait],0.05,na.rm=T),rgb(125,155,193,maxColorValue =255,alpha=255*2/3),MyDarkBlue)
  
  text(1.25,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"YS\nSires",pos=1)
  text(1.25+0.5,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"Pregs\nSires",pos=1)
  text(1.25+0.5+0.5,(ceiling(max(PlotResults,na.rm=T)/Sig)*Sig)+((ceiling(max(PlotResults,na.rm=T)/Sig)*Sig-floor(min(PlotResults,na.rm=T)/Sig)*Sig)*0.1),"UK Active\nList",pos=1)
  
  abline(v=mean(c(0.75,1)),lwd=3,lend=1,lty=2)
  
  axis(1,c(0.25,1.75),labels=c("Milking","Youngstock"),lwd=NA)
  
  mtext(paste("Benchmarking:",Label),3,outer=T,line=-0.5,cex=1.5,font=2)
  mtext("Whiskers Show 95th Percentile, AHDB Data taken from https://breedingdairy.ahdbdigital.org.uk/",1,outer=T,line=-1.5)
}

MyBoxPlot <- function(x,Med,Uq,Lq,Ur,Lr,Bxcol,LnCol,Bw=0.2,Ww=0.1){
  lines(c(x,x),c(Lr,Lq))
  lines(c(x,x),c(Ur,Uq))
  lines(c(x-Ww,x+Ww),c(Lr,Lr))
  lines(c(x-Ww,x+Ww),c(Ur,Ur))
  rect(x-Bw,Lq,x+Bw,Uq,col=Bxcol)
  lines(c(x-Bw,x+Bw),c(Med,Med),col=LnCol,lend=1,lwd=3)
}

gGeneticLag <- function(dfHerd,LastProofs,gTrait="PLI",gLabel="£ PLI",ExtLabel="PLIN"){
  
  funcData <- dfHerd[!dfHerd$PLI %in% "NaN",]
  funcData$YOB <- as.numeric(format(as.Date(funcData$DatDOB,"%d/%m/%Y"),"%Y"))
  ## Old way static on year 2013 --- crashing ou on some herds which do not have any animals alive in 2013
  #boxplot(as.formula(paste(ExtLabel,"*2~Birth_YearN")),LastProofs[which(LastProofs$Birth_YearN>2013&LastProofs$Birth_YearN<as.numeric(format(Sys.Date(),"%Y"))-1),],main=paste("Genetic Lag for",gLabel),outline=F,col=rgb(206,17,65,maxColorValue = 255),xlab="Birth Year",ylab="PLI",ylim=c(min(dfHerd$PLI*2, na.rm = T),2000))
  #MySeq<-2013:(as.numeric(format(Sys.Date(),"%Y"))-2)
  
  MyYears <- unique(funcData$YOB[as.numeric(funcData$YOB) >= (as.numeric(format(Sys.Date(),"%Y"))-7)])
  
  ##subset LastProofs to animals within the last 7 years
  LastProofs <- LastProofs[as.numeric(LastProofs$Birth_YearN) %in% MyYears,]
  
  # New way go back 10 years from system date
  #boxplot(as.formula(paste(ExtLabel,"*2~Birth_YearN")),LastProofs[which(LastProofs$Birth_YearN >= (as.numeric(format(Sys.Date(),"%Y"))-10) &LastProofs$Birth_YearN<as.numeric(format(Sys.Date(),"%Y"))-1),],main=paste("Genetic Lag for",gLabel),outline=F,col=rgb(206,17,65,maxColorValue = 255),xlab="Birth Year",ylab="PLI",ylim=c(min(dfHerd$PLI*2, na.rm = T),2000))
  
  #new way with just using MyYears incase a year of data is missing
  boxplot(as.formula(paste(ExtLabel,"*2~Birth_YearN")),
          LastProofs,main=paste("Genetic Lag for",gLabel),
          outline=F,
          col=MyBlue,
          xlab="Birth Year",
          ylab="PLI",
          ylim=c(min(dfHerd$PLI*2, na.rm = T),2000))
  
  MySeq <- unique(LastProofs$Birth_YearN)
  
  # Subsetting funcData to just have the years which are present in the sire's birth years
  funcData <- funcData[as.numeric(funcData$YOB) %in% MySeq,]
  
  boxplot(as.formula(paste(gTrait,"*2~as.character(YOB)"))  ,
          funcData,
          outline=F,
          #at= seq(which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-7),which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-3)),
          #at = c(length(MySeq):1),
          add=T,
          xaxt="n",yaxt="n",col=MyRed)
  
  # use this line below for dropping the first year for cust data boxplot
  #boxplot(as.formula(paste(gTrait,"*2~as.character(YOB)"))  ,funcData[which(funcData$YOB>as.numeric(format(Sys.Date(),"%Y"))-7&funcData$YOB<as.numeric(format(Sys.Date(),"%Y"))-1),],outline=F,at=seq(which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-6),which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-3)),add=T,xaxt="n",yaxt="n",col=MyBlue)
  legend("topleft",c("Industry Genetic Progress",paste("Herd Genetic Progress")),pt.bg = c(MyBlue,MyRed),col=c("black",rgb(0,57,118,maxColorValue = 255)),pch=22,lwd=NA,pt.cex=2)
}

extractGeneticLagData <- function(dfHerd, LastProofs, gTrait = "PLI", ExtLabel = "PLIN") {
  funcData <- dfHerd[!dfHerd[[gTrait]] %in% "NaN", ]
  funcData$YOB <- as.numeric(format(as.Date(funcData$DatDOB, "%d/%m/%Y"), "%Y"))
  
  MyYears <- unique(funcData$YOB[funcData$YOB >= (as.numeric(format(Sys.Date(), "%Y")) - 7)])
  
  LastProofs <- LastProofs[LastProofs$Birth_YearN %in% MyYears, ]
  
  MySeq <- unique(LastProofs$Birth_YearN)
  funcData <- funcData[funcData$YOB %in% MySeq, ]
  
  # Return both data sets used in the plot
  return(list(
    HerdData = funcData,
    IndustryData = LastProofs
  ))
}

## old original plot --- can delete if needed 
# gGeneticLag<-function(dfHerd,LastProofs,gTrait="PLI",gLabel="£ PLI",ExtLabel="PLIN"){
#   funcData<-dfHerd
#   funcData$YOB<-as.numeric(format(as.Date(funcData$DatDOB,"%d/%m/%Y"),"%Y"))
#   boxplot(as.formula(paste(ExtLabel,"*2~Birth_YearN")),LastProofs[which(LastProofs$Birth_YearN>2013&LastProofs$Birth_YearN<as.numeric(format(Sys.Date(),"%Y"))-1),],main=paste("Genetic Lag for",gLabel),outline=F,col=rgb(206,17,65,maxColorValue = 255),xlab="Birth Year",ylab="PLI",ylim=c(min(dfHerd$PLI*2, na.rm = T),2000))
#   MySeq<-2013:(as.numeric(format(Sys.Date(),"%Y"))-2)
#   boxplot(as.formula(paste(gTrait,"*2~as.character(YOB)"))  ,funcData[which(funcData$YOB>as.numeric(format(Sys.Date(),"%Y"))-7&funcData$YOB<as.numeric(format(Sys.Date(),"%Y"))-1),],outline=F,at=seq(which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-7),which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-3)),add=T,xaxt="n",yaxt="n",col=MyBlue)
#   # use this line below for dropping the first year for cust data boxplot
#   #boxplot(as.formula(paste(gTrait,"*2~as.character(YOB)"))  ,funcData[which(funcData$YOB>as.numeric(format(Sys.Date(),"%Y"))-7&funcData$YOB<as.numeric(format(Sys.Date(),"%Y"))-1),],outline=F,at=seq(which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-6),which(MySeq==as.numeric(format(Sys.Date(),"%Y"))-3)),add=T,xaxt="n",yaxt="n",col=MyBlue)
#   legend("topleft",c("Industry Genetic Progress",paste("Herd Genetic Progress")),pt.bg = c(rgb(206,17,65,maxColorValue = 255),rgb(125,155,193,maxColorValue = 255)),col=c("black",rgb(0,57,118,maxColorValue = 255)),pch=22,lwd=NA,pt.cex=2)
# }

#gCorrPlotGE <- function(dfHerd, TraitX, TraitXLabel, TraitY, TraitYLabel, ToB = T, LegendPlot = T){
  
  
  
 # Cols <- colorRampPalette(c(rgb(206, 17, 65, maxColorValue = 255),
  #                           rgb(169, 168, 169, maxColorValue = 255),
  #                           rgb(29, 79, 145, maxColorValue = 255)), bias = 2)  (max(as.numeric(dfHerd$Yield305DayLact), na.rm = T))
  
#  strMain <- paste("Correlation of", TraitYLabel, "with", TraitXLabel)
  
  # Plotting traits against each other
 # plot(dfHerd[, TraitX]*2, dfHerd[, TraitY], bg = Cols[as.numeric(dfHerd$Yield305DayLact)],
  #     col = Cols[dfHerd$Yield305DayLact], pch = ifelse(dfHerd$GenomicIndicator=="G", 16, 1),
   #    main = strMain, type = "n",xlab = TraitXLabel, ylab = TraitYLabel)
  
  # Setting dot colours to lact number and filled is GT
#  points(dfHerd[,TraitX]*2,dfHerd[,TraitY],
#         bg = Cols[as.numeric(dfHerd$Yield305DayLact)],
#         col = Cols[as.numeric(dfHerd$Yield305DayLact)],
#         pch = ifelse(dfHerd$GenomicIndicator == "G", 16, 1))
  
  # Getting line of best fit and print on plot
 # lmModel <- lm(as.formula(paste(TraitY, "~", TraitX)), dfHerd)
#  abline(lmModel, lwd = 2, lty = 2,col = MyDarkBlue)
  
  # Getting values for Line Coef to be included on plot with value cleaning ( rounding to 3 dp)
#  Argh <- summary(lmModel)$adj.r.squared
 # if(Argh < 0.001){
  #  Argh <- "<0.001"
  #}else{
  #  Argh <- round(Argh, 3)
  #}
  #M <- summary(lmModel)$coefficients[2, 1]
  #C <- summary(lmModel)$coefficients[1, 1]
  #if(abs(M) < 0.001){
  #  M <- ifelse(M < 0, "<0.-001", "<0.001")
  #}else{
   # M <- round(M, 3)
  #}
  #if(abs(C) < 0.001){
  #  C <- ifelse(C < 0, "<0.-001", "<0.001")
  #}else{
   # C <- round(C, ifelse(C > 1000, 0, 3))
  #}
  # Hopefully be able to add more traits to the **ifelse(TraitXLabel == "FertilityIndex PTA"** if texts needs moving
  
  # includes line coef of interceot and grad with r ^2 value
  # True for Top
#  text(par("usr")[2],
 #      ifelse(ToB == T,
  #            par("usr")[4] - strheight(bquote(.(TraitYLabel)==.(TraitX)%*%.(M)+.(C))), # Prints at top if false
              
   #           par("usr")[3] + strheight(bquote(.(TraitYLabel)==.(TraitX)%*%.(M)+.(C))) + 1.2 * strheight(bquote(bold({r^2}==.(Argh))))),
    #   bquote(.(TraitYLabel)==.(TraitYLabel)%*%.(M)+.(C)), pos = 2)
  
  #text(par("usr")[2],
   #    ifelse(ToB == T,
    #          par("usr")[4] - strheight(bquote(bold({r^2}==.(Argh)))) - strheight(bquote(.(TraitYLabel)==.(TraitX)%*%.(M)+.(C))),
     #         par("usr")[3] + strheight(bquote(bold({r^2}==.(Argh))))),
      # bquote({r^2}==.(Argh)), pos = 2)
  
  # Plots legend on top left of the plot is set to True
  #if(LegendPlot == T){
    
   # legend("topleft",              
    #       legend = c(paste("Lact", 1 : max(dfHerd$Yield305DayLact, na.rm = T)), "PA", "GT"),
     #      pch = c(rep(15, max(dfHerd$Yield305DayLact, na.rm = T)),1,16), 
      #     col = c(Cols, "black", "black"))
  #}
#}


gAHDBPLI <- function(dfHerd, intBeefPercentage=60){
  if(is.na(intBeefPercentage)){
    intBeefPercentage<-60
  }
  plotData<-dfHerd[which(dfHerd$ExGI!="G"&dfHerd$GenomicIndicator=="G"),]
  plotData<-plotData[!is.na(plotData$ExPLI),]
  plotData<-plotData[plotData$PLI!=(-999),]
  if(nrow(plotData)>0){
    decLabCex<-1.5
    plot(plotData$ExPLI,plotData$PLI,pch=21,bg=MyBlue,xlab="Parent Average",ylab="Genomic Test",main=paste("AHDB Parent Average PLI vs. Genomic Tested PLI\nShowing Selection Threshold at ",intBeefPercentage,"% Beef Usage",sep=""))
    abline(v=quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
    abline(h=quantile(plotData$PLI,intBeefPercentage/100,na.rm=T),col=MyRed,lwd=2,lty=2)
    text(quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T),
         par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
         paste(round(nrow(plotData[plotData$GenomicIndicator=="G"&
                                     plotData$ExPLI>=quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T)&
                                     plotData$PLI<quantile(plotData$PLI,intBeefPercentage/100,na.rm=T),])/
                       nrow(plotData)*100,
                     0),"%",sep="")
         ,cex=decLabCex,pos=4,col=MyRed,font=2)
    
    text(quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T),
         par("usr")[3]+strheight("980%",cex=decLabCex,font=2),
         paste(round(nrow(plotData[plotData$GenomicIndicator=="G"&
                                     plotData$ExPLI<quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T)&
                                     plotData$PLI<quantile(plotData$PLI,intBeefPercentage/100,na.rm=T),])/
                       nrow(plotData)*100,
                     0),"%",sep="")
         ,cex=decLabCex,pos=2,col=MyDarkBlue,font=2)
    
    text(quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T),
         par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
         paste(round(nrow(plotData[plotData$GenomicIndicator=="G"&
                                     plotData$ExPLI<quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T)&
                                     plotData$PLI>=quantile(plotData$PLI,intBeefPercentage/100,na.rm=T),])/
                       nrow(plotData)*100,
                     0),"%",sep="")
         ,cex=decLabCex,pos=2,col=MyRed,font=2)
    
    text(quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T),
         par("usr")[4]-strheight("980%",cex=decLabCex,font=2),
         paste(round(nrow(plotData[plotData$GenomicIndicator=="G"&
                                     plotData$ExPLI>=quantile(plotData$ExPLI,intBeefPercentage/100,na.rm=T)&
                                     plotData$PLI>=quantile(plotData$PLI,intBeefPercentage/100,na.rm=T),])/
                       nrow(plotData)*100,
                     0),"%",sep="")
         ,cex=decLabCex,pos=4,col=MyDarkBlue,font=2)
  }else{
    plot(1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
    text(1,1,"No Data")
  }
  
  
  
}


#SourceDir<-"C:\\Users\\Corrett\\OneDrive - Genus PLC\\Documents\\DATA\\CUSTOMERPROOFS\\"
#Customer<-"FOOT"
#CustomerLabel="David Foot Ltd."
#Res<-1000
#Milking<-read.csv(paste(SourceDir,Customer,"\\",list.files(paste(SourceDir,Customer,"\\",sep=""))[grepl("Milking",list.files(paste(SourceDir,Customer,"\\",sep="")))],sep=""),stringsAsFactors=F)
##ilking$Birth.Date<-""
#Youngstock<-read.csv(paste(SourceDir,Customer,"\\",list.files(paste(SourceDir,Customer,"\\",sep=""))[grepl("Youngstock",list.files(paste(SourceDir,Customer,"\\",sep="")))],sep=""),stringsAsFactors=F)
#Youngstock$Curr.Lact<-0
#Youngstock$PI<-"PI"
#dfHerd<-rbind.data.frame(Milking,Youngstock[,colnames(Milking)],stringsAsFactors=F)
#dfHerd$DOB<-as.Date(dfHerd$Birth.Date,"%d/%m/%Y")
#colnames(dfHerd)<-gsub("[.]","",gsub("?.PLI","PLI",colnames(dfHerd)))


#png(paste(SourceDir,"\\",CustomerName,"\\","GeneticVariation",format(Sys.time(),"%Y_%m_%d_%H_%M"),".png",sep=""),width=15,height=11,units = "cm",res=Res,pointsize=9)

MyHistogram <- function(dfHerd,strDisplay){
  if(strDisplay%in%c("",NULL)|is.na(strDisplay)){
    strDisplay<-""
  }else{
    strDisplay<-paste("For",strDisplay)
  }
  hist(dfHerd$PLI[dfHerd$PLI>-500]*2,main=paste("Distribution of PLI",strDisplay),xlab="£PLI (EBV Scale)",col=MyDarkBlue)
  abline(v=mean(dfHerd$PLI[dfHerd$PLI>-500]*2),lwd=4,col=MyRed,lty=2)
  arrows(mean(dfHerd$PLI[dfHerd$PLI>-500]*2)-sd(dfHerd$PLI[dfHerd$PLI>-500]*2),par("usr")[3]+(2*((par("usr")[4]-par("usr")[3])/3)),mean(dfHerd$PLI[dfHerd$PLI>-500]*2)+sd(dfHerd$PLI[dfHerd$PLI>-500]*2),par("usr")[3]+(2*((par("usr")[4]-par("usr")[3])/3)),lwd=4,col=MyRed,code=3,angle=90)
  text(mean(dfHerd$PLI[dfHerd$PLI>-500]*2)+sd(dfHerd$PLI[dfHerd$PLI>-500]*2),par("usr")[3]+(2*((par("usr")[4]-par("usr")[3])/3)),round(sd(dfHerd$PLI[dfHerd$PLI>-500]*2),2),pos=4,cex=1.5,font=2)
  text(mean(dfHerd$PLI[dfHerd$PLI>-500]*2),0,round(mean(dfHerd$PLI[dfHerd$PLI>-500]*2),2),pos=3,cex=1.5,font=2,col="white")
  #dev.off()
}

#png(paste(SourceDir,"\\",CustomerName,"\\","GeneticLag",format(Sys.time(),"%Y_%m_%d_%H_%M"),".png",sep=""),width=15,height=11,units = "cm",res=Res,pointsize=9)
#boxplot(PLIN*2~Birth_YearN,LastProofs[which(LastProofs$Birth_YearN>2010&LastProofs$Birth_YearN<as.numeric(format(Sys.Date(),"%Y"))-1),],main=paste("Genetic Lag for",CustomerLabel),outline=F,col=rgb(206,17,65,maxColorValue = 255),xlab="Birth Year",ylab="?PLI",ylim=c(min(dfHerd$PLI[dfHerd$PLI>-500]*2),2000))
#boxplot(PLI*2~LactNo,dfHerd[dfHerd$PLI>-500,],outline=F,border=rgb(0,57,118,maxColorValue = 255),add=T,at=seq(length(seq(2011,as.numeric(format(Sys.Date(),"%Y"))-1,1))-6,length(seq(2011,as.numeric(format(Sys.Date(),"%Y"))-2,1)),1),col=rgb(125,155,193,maxColorValue = 255),xaxt="n")
#legend("topleft",c("Industry Genetic Progress",paste(CustomerLabel,"Genetic Progress")),pt.bg = c(rgb(206,17,65,maxColorValue = 255),rgb(125,155,193,maxColorValue = 255)),col=c("black",rgb(0,57,118,maxColorValue = 255)),pch=22,lwd=NA,pt.cex=2)
#dev.off()


ROISexed<-function(dfHerd,strDisplay,Prop=0.6){
  GeneticVariation<-sd(dfHerd$PLI[dfHerd$PLI>-990]*2, na.rm = T)
  if(strDisplay%in%c("",NULL)|is.na(strDisplay)){
    Country<-"this customer"
  }else{
    Country<-strDisplay  
  }
  
  Reference<-"AHDB Data"
  Trait<-"£ PLI"
  Mean<-mean(dfHerd$PLI[dfHerd$PLI>-990]*2,na.rm=T)
  
  Magnitude<-10
  By<-100
  #Selection intensity
  
  #Proportion of the herd selected (sexcel)
  SCSIProp35<-0.35
  SCSIProp40<-0.4
  SCSIProp45<-0.45
  
  SCSI35<-dnorm(qnorm(1-SCSIProp35))/SCSIProp35
  SCSI40<-dnorm(qnorm(1-SCSIProp40))/SCSIProp40
  SCSI45<-dnorm(qnorm(1-SCSIProp45))/SCSIProp45
  
  
  #Calculation of selected mean
  Mn1<-Mean+(GeneticVariation*sqrt(0.33)*SCSI45)
  Mn2<-Mean+(GeneticVariation*sqrt(0.33)*SCSI40)
  Mn3<-Mean+(GeneticVariation*sqrt(0.33)*SCSI35)
  Mn4<-Mean+(GeneticVariation*sqrt(0.7)*SCSI45)
  Mn5<-Mean+(GeneticVariation*sqrt(0.7)*SCSI40)
  Mn6<-Mean+(GeneticVariation*sqrt(0.7)*SCSI35)
  
  Min<-(floor(Mean/100)*100)
  Max<-(ceiling(max(Mean,Mn1,Mn2,Mn3,Mn4,Mn5,Mn6)/100)*100)+50
  barplot(c(Mean,Mn1,Mn2,Mn3,Mn4,Mn5,Mn6),
          #You need to change the y lim (limits to the y axis to make it look pretty)
          ylim=c(Min,Max),xlim=c(0+0.2,8-0.2),ylab=paste(Trait,"BV"),
          main="Genetic Improvement* in One Generation Using Various Technology",
          col=c("darkred","royalblue1","royalblue2","royalblue3","royalblue4","royalblue","blue","blue1"),
          xpd=F,xaxt="n",yaxt="n",width=0.8,space=0.4,bg = "grey93",font.lab=2)
  axis(1,c(0.15,1.25,4.65,8),labels=F,tck=-0.05 )
  axis(1,c(2.95,6.3),labels=c("PA","GT"),tck=0,line=1.5,lwd=0,font=2,cex.axis=1.25)
  axis(1,c(1.85,2.95,4.05,5.15,6.25,7.35),labels=c("55% Beef","60% Beef","65% Beef","55% Beef","60% Beef","65% Beef"),tck=0,line=0,lwd=0,font=2)
  axis(1,c(0.7),labels=c("Original"),tck=0,line=0,lwd=0,font=2)
  axis(1,c(2.35,3.55,5.75,6.85),labels=F,tck=-0.02)
  abline(v=1.25,lty=2)
  #You need to change the axis sequence to make it look pretty
  axis(2,at=seq(from=Min,to=Max,by=50),pos=0.15,font.axis=2)
  YText<-(par("usr")[4]-(par("usr")[4]-par("usr")[3])*0.15)
  YTextTwo<-(par("usr")[4]-(par("usr")[4]-par("usr")[3])*0.025)
  text(1.85,YText,paste("+",round(Mn1-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(2.95,YText,paste("+",round(Mn2-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(4.05,YText,paste("+",round(Mn3-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(5.15,YText,paste("+",round(Mn4-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(6.25,YText,paste("+",round(Mn5-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(7.35,YText,paste("+",round(Mn6-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(0.7,YTextTwo,"Initial Herd",font=2)
  text(4.75,YTextTwo,"Replacements After One Generation of Selection",font=2)
  mtext(
    paste("*Genetic variation for ",Country," taken from ",Reference,". Differences in mean are halved (PTA) to reflect dam contribution.\nSelection intensity of bottom 55%, 60% and 65% beef are modelled with selection taking place on both PA and GT values.",sep=""),
    side=1,line=-1,outer=T,cex=0.75 
  )
  
}

ROI<-function(dfHerd,strDisplay,Prop=0.6){
  GeneticVariation<-sd(dfHerd$PLI[dfHerd$PLI>-990]*2, na.rm = T)
  if(strDisplay%in%c("",NULL)|is.na(strDisplay)){
    Country<-"this customer"
  }else{
    Country<-strDisplay  
  }
  
  Reference<-"AHDB Data"
  Trait<-"£ PLI"
  Mean<-mean(dfHerd$PLI[dfHerd$PLI>-990]*2,na.rm=T)
  
  Magnitude<-10
  By<-100
  #Selection intensity
  SI<-dnorm(qnorm(1-Prop))/Prop
  
  #Proportion of the herd selected (sexcel)
  SCSIProp<-((Prop*0.5)*(1/0.85))
  SCSI<-dnorm(qnorm(1-SCSIProp))/SCSIProp
  SI<-dnorm(qnorm(1-Prop))/Prop
  
  #Calculation of selected mean
  Mn1<-Mean+(GeneticVariation*sqrt(0.33)*SI)
  Mn2<-Mean+(GeneticVariation*sqrt(0.33)*SCSI)
  Mn3<-Mean+(GeneticVariation*sqrt(0.7)*SI)
  Mn4<-Mean+(GeneticVariation*sqrt(0.7)*SCSI)
  Min<-(floor(Mean/100)*100)
  Max<-(ceiling(max(Mean,Mn1,Mn2,Mn3,Mn4)/100)*100)+50
  barplot(c(Mean,Mn1,Mn2,Mn3,Mn4),
          #You need to change the y lim (limits to the y axis to make it look pretty)
          ylim=c(Min,Max),xlim=c(0+0.2,8-0.2),ylab=paste("£ PLI BV"),main="Genetic Improvement* in One Generation Using Various Technology",col=c("darkred","royalblue1","royalblue2","royalblue3","royalblue4"),xpd=F,xaxt="n",yaxt="n",width=1,space=0.5,bg = "grey93",font.lab=2)
  axis(1,c(0.25,1.75,4.75,7.75),labels=F,tck=-0.05 )
  axis(1,c(3.25,6.25),labels=c("Parent Average","Genomic Tested"),tck=0,line=1.5,lwd=0,font=2,cex.axis=1.25)
  axis(1,c(2.5,4,5.5,7),labels=c("Conventional","Sexcel","Conventional","Sexcel"),tck=0,line=0,lwd=0,font=2)
  axis(1,c(1),labels=c("Original"),tck=0,line=0,lwd=0,font=2)
  axis(1,c(3.25,6.25),labels=F,tck=-0.01)
  abline(v=1.75,lty=2)
  #You need to change the axis sequence to make it look pretty
  axis(2,at=seq(from=Min,to=Max,by=50),pos=0.25,font.axis=2)
  YText<-(par("usr")[4]-(par("usr")[4]-par("usr")[3])*0.15)
  YTextTwo<-(par("usr")[4]-(par("usr")[4]-par("usr")[3])*0.025)
  text(2.5,YText,paste("+",round(Mn1-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.3)
  text(4,YText,paste("+",round(Mn2-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.3)
  text(5.5,YText,paste("+",round(Mn3-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.3)
  text(7,YText,paste("+",round(Mn4-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.3)
  text(1,YTextTwo,"Initial Herd",font=2)
  text(4.75,YTextTwo,"Replacements After One Generation of Selection",font=2)
  mtext(
    paste("*Genetic variation for ",Country," taken from ",Reference," Differences in mean are halved (PTA) to reflect dam contribution.\nConventional selection intensity of top ",Prop*100,"%. For Sexcel the top ",round(SCSIProp,2)*100,"% used for the same number of Heifers. All cows served to conception.",sep=""),
    side=1,line=-1,outer=T,cex=0.75 
  )
}


gPLIBoxplot <- function(dfHerd, intBeefPercentage = "", output_path = NULL) {
  library(ggplot2)
  library(dplyr)
  
  # --- Color palette from your definitions ---
  MyDarkBlue <- rgb(0, 57, 118, maxColorValue = 255)
  MyRed <- rgb(206, 17, 65, maxColorValue = 255)
  MyGray <- rgb(169, 168, 169, maxColorValue = 255)
  MyBrown <- rgb(95, 69, 43, maxColorValue = 255)
  MyBeige <- rgb(190, 149, 91, maxColorValue = 255)
  MyCream <- rgb(224, 202, 163, maxColorValue = 255)
  
  # --- Lactation class order ---
  lact_levels <- c("4+", "3", "2", "1", "Heifer", "Calf")
  
  # --- Custom colors by lactation class ---
  lact_color_map <- c(
    "4+" = MyRed,
    "3" = MyBrown,
    "2" = MyGray,
    "1" = MyDarkBlue,
    "Heifer" = MyBeige,
    "Calf" = MyCream
  )
  
  # --- Filter and format data ---
  dfHerd <- dfHerd %>%
    filter(FactLactClass %in% lact_levels) %>%
    mutate(
      FactLactClass = factor(FactLactClass, levels = lact_levels),
      GenomicStatus = case_when(
        GenomicIndicator == "G" ~ "Genomic",
        TRUE ~ "Untested"
      )
    )
  
  # --- Optional beef cutoff ---
  beef_cutoff <- NA
  if (!is.na(as.numeric(intBeefPercentage))) {
    beef_cutoff <- quantile(dfHerd$PLI, probs = as.numeric(intBeefPercentage) / 100, na.rm = TRUE)
  }
  
  # --- Plot ---
  p <- ggplot(dfHerd, aes(x = FactLactClass, y = PLI)) +
    geom_boxplot(
      aes(color = FactLactClass), fill = NA, outlier.shape = NA, size = 1.2
    ) +
    geom_jitter(
      aes(color = FactLactClass, shape = GenomicStatus),
      width = 0.2, size = 2, alpha = 0.75
    ) +
    scale_color_manual(
      values = lact_color_map,
      name = "Lactation Class"
    ) +
    scale_shape_manual(
      values = c("Genomic" = 16, "Untested" = 1),
      name = "Genomic Status"
    ) +
    labs(
      title = "Genetic Trends in PLI by Lactation Group",
      x = "Lactation Group",
      y = "PLI"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        hjust = 0.5, size = 16, face = "bold", color = MyDarkBlue
      ),
      axis.title.x = element_text(size = 14, face = "bold", color = MyBrown),
      axis.title.y = element_text(size = 14, face = "bold", color = MyBrown),
      axis.text = element_text(size = 12, color = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "right"
    )
  
  # --- Optional: Add beef cutoff line ---
  # if (!is.na(beef_cutoff)) {
  #   p <- p + geom_hline(yintercept = beef_cutoff, linetype = "dashed", color = "red", size = 1)
  # }
  
  # --- Save or print ---
  if (!is.null(output_path)) {
    ggsave(filename = paste0(output_path, "PLI_Boxplot.png"), plot = p, width = 10, height = 6, dpi = 300)
  } else {
    print(p)
  }
}

lact_groups_organizer <- function(dfHerd, output_dir = ".") {
  traits <- c("MilkKg", "FatKg", "ProtKg", "FatPc", "ProtPc", "FertilityIndex", "Lifespan", "GestationLength", "dCE", "mCE", 
              "CalfSurvival", "SCC", "Mastitis", "LamenessAdvantage", "DigitalDermatitis", "TBAdvantage", "HealthyCow", "LegsFeet", 
              "RearLegRearView", "FootAngle", "STAT", "CW", "RumpAngle", "RumpWidth", "BodyDepth", "Angularity", "PLI", "ACI", "SCI", 
              "PLIRel", "TypeMerit", "UdderSupport", "UdderDepth", "ForeUdderAttachment", "RearUdderHeight", "Mammary", "Locomotion", 
              "Temperament", "MSP", "FrontTeatPlaceRearView", "TLG", "RTP", "TeatPlacementSideView", "Feed.Advantage", "Maintenance", "Enviro.Cow")
  
  # Keep only existing traits
  traits <- traits[traits %in% colnames(dfHerd)]
  
  # Define rounding groups
  whole_number_traits <- c("MilkKg", "FatKg", "ProtKg", "Lifespan", "PLI", "SCI", "ACI", "SCC", "PLIRel", "HealthyCow", "Feed.Advantage", "Maintenance", "GestationLength")
  one_decimal_traits <- c("FertilityIndex", "mCE", "dCE", "LamenessAdvantage", "TBAdvantage", "CalfSurvival", "DigitalDermatitis", "Enviro.Cow", "TypeMerit")
  
  # Mean of each trait by LactClass
  trait_means <- aggregate(dfHerd[, traits], 
                           by = list(LactClass = dfHerd$FactLactClass), 
                           FUN = function(x) mean(x, na.rm = TRUE))
  
  # Order LactClass explicitly
  ordered_classes <- c("4+", "3", "2", "1", "Heifer", "Calf")
  trait_means <- trait_means[match(ordered_classes, trait_means$LactClass), ]
  
  # Apply custom rounding
  for (trait in traits) {
    if (trait %in% whole_number_traits) {
      # Round to whole numbers
      trait_means[, trait] <- round(trait_means[, trait], 0)
    } else if (trait %in% one_decimal_traits) {
      # Round to one decimal place
      trait_means[, trait] <- round(trait_means[, trait], 1)
    } else {
      # Default: round to 2 decimal places
      trait_means[, trait] <- round(trait_means[, trait], 2)
    }
  }
  
  # Transpose so LactClass becomes columns, traits become rows
  rownames(trait_means) <- trait_means$LactClass
  trait_means_t <- as.data.frame(t(trait_means[,-1]))  # remove LactClass column before transposing
  colnames(trait_means_t) <- rownames(trait_means)
  
  # Add Trait column
  trait_means_t$Trait <- rownames(trait_means_t)
  
  # Reorder columns to put Trait first
  trait_means_t <- trait_means_t[, c("Trait", setdiff(names(trait_means_t), "Trait"))]
  
  # Add difference columns with appropriate rounding
  for (trait in rownames(trait_means_t)) {
    if (trait %in% whole_number_traits) {
      # Whole number traits - round differences to whole numbers
      trait_means_t[trait, "Diff_3-4+"] <- round(trait_means_t[trait, "3"] - trait_means_t[trait, "4+"], 0)
      trait_means_t[trait, "Diff_2-3"] <- round(trait_means_t[trait, "2"] - trait_means_t[trait, "3"], 0)
      trait_means_t[trait, "Diff_1-2"] <- round(trait_means_t[trait, "1"] - trait_means_t[trait, "2"], 0)
      trait_means_t[trait, "Diff_Heifer-1"] <- round(trait_means_t[trait, "Heifer"] - trait_means_t[trait, "1"], 0)
      trait_means_t[trait, "Diff_Calf-Heifer"] <- round(trait_means_t[trait, "Calf"] - trait_means_t[trait, "Heifer"], 0)
    } else if (trait %in% one_decimal_traits) {
      # One decimal place traits - round differences to one decimal place
      trait_means_t[trait, "Diff_3-4+"] <- round(trait_means_t[trait, "3"] - trait_means_t[trait, "4+"], 1)
      trait_means_t[trait, "Diff_2-3"] <- round(trait_means_t[trait, "2"] - trait_means_t[trait, "3"], 1)
      trait_means_t[trait, "Diff_1-2"] <- round(trait_means_t[trait, "1"] - trait_means_t[trait, "2"], 1)
      trait_means_t[trait, "Diff_Heifer-1"] <- round(trait_means_t[trait, "Heifer"] - trait_means_t[trait, "1"], 1)
      trait_means_t[trait, "Diff_Calf-Heifer"] <- round(trait_means_t[trait, "Calf"] - trait_means_t[trait, "Heifer"], 1)
    } else {
      # Default: round differences to 2 decimal places
      trait_means_t[trait, "Diff_3-4+"] <- round(trait_means_t[trait, "3"] - trait_means_t[trait, "4+"], 2)
      trait_means_t[trait, "Diff_2-3"] <- round(trait_means_t[trait, "2"] - trait_means_t[trait, "3"], 2)
      trait_means_t[trait, "Diff_1-2"] <- round(trait_means_t[trait, "1"] - trait_means_t[trait, "2"], 2)
      trait_means_t[trait, "Diff_Heifer-1"] <- round(trait_means_t[trait, "Heifer"] - trait_means_t[trait, "1"], 2)
      trait_means_t[trait, "Diff_Calf-Heifer"] <- round(trait_means_t[trait, "Calf"] - trait_means_t[trait, "Heifer"], 2)
    }
  }
  
  trait_means_t <- trait_means_t[, c("Trait", "4+", "Diff_3-4+", "3", "Diff_2-3", "2", 
                                     "Diff_1-2", "1", "Diff_Heifer-1", "Heifer", "Diff_Calf-Heifer", "Calf")]
  
  colnames(trait_means_t) <- c("Trait", "Lact Group 4", "Lact Groups 3 to 4 Change", "Lact Group 3",
                               "Lact Groups 2 to 3 Change", "Lact Group 2", "Lact Groups 1 to 2 Change", 
                               "Lact Group 1", "Lact Group Heifer to 1 Change", "Heifers", 
                               "Calf to Heifer Change", "Calf")
  
  # Save numeric version using write.table
  write.table(trait_means_t, 
              file = file.path(output_dir, "LactClass_TraitMeans_Numeric.csv"), 
              sep = ",", 
              row.names = FALSE, 
              quote = FALSE,
              na = "")
  
  # Create character-formatted version
  trait_means_char <- trait_means_t
  numeric_cols <- colnames(trait_means_char)[colnames(trait_means_char) != "Trait"]
  
  for (i in 1:nrow(trait_means_char)) {
    trait <- trait_means_char$Trait[i]
    
    for (col in numeric_cols) {
      value <- trait_means_char[i, col]
      
      if (!is.na(value) && is.numeric(value)) {
        if (trait %in% whole_number_traits) {
          trait_means_char[i, col] <- sprintf("%.0f", value)
        } else if (trait %in% one_decimal_traits) {
          trait_means_char[i, col] <- sprintf("%.1f", value)
        } else {
          trait_means_char[i, col] <- sprintf("%.2f", value)
        }
      }
    }
  }
  
  # Save character-formatted version
  write.csv(trait_means_char, 
            file = file.path(output_dir, "LactClass_TraitMeans_Formatted.csv"), 
            row.names = FALSE)
  
  # Return both versions as a list
  return(list(
    numeric = trait_means_t,
    formatted = trait_means_char
  ))
}


calculate_parent_average <- function(dam_eartag, sire_eartag, output_dir) {
  dam_eartag <- trimws(dam_eartag)
  
  if (!(dam_eartag %in% dfHerd$Eartag)) {
    cat("Eartag", dam_eartag, "is NOT present in the herd.\n")
    return(NULL)
  }
  
  dam_row <- dfHerd[dfHerd$Eartag == dam_eartag, ] 
  
  # Trait list
  traits <- c("Name", "LactClass", "MilkKg", "FatKg", "ProtKg", "FatPc", "ProtPc", "FertilityIndex", "Lifespan", "GestationLength", 
              "dCE", "mCE", "CalfSurvival", "SCC", "Mastitis", "LamenessAdvantage", "DigitalDermatitis", "TBAdvantage", "HealthyCow", 
              "LegsFeet", "RearLegRearView", "FootAngle", "STAT", "CW", "RumpAngle", "RumpWidth", "BodyDepth", "Angularity", "PLI", 
              "ACI", "SCI", "PLIRel", "TypeMerit", "UdderSupport", "UdderDepth", "ForeUdderAttachment", "RearUdderHeight", "Mammary", 
              "Locomotion", "Temperament", "MSP", "FrontTeatPlaceRearView", "TLG", "RTP", "TeatPlacementSideView", "Feed.Advantage", 
              "Maintenance", "Enviro.Cow")
  
  # Extract dam data
  trait_values <- dam_row[, traits, drop = FALSE]
  trait_values$Eartag <- dam_eartag
  if ("GenomicIndicator" %in% colnames(dfHerd)) trait_values$GenomicIndicator <- dam_row$GenomicIndicator
  if ("ParentAverageIndicator" %in% colnames(dfHerd)) {
    pai <- dam_row$ParentAverageIndicator
    if (!is.na(pai) && pai != "") trait_values$ParentAverageIndicator <- pai
  }
  col_order <- c("Eartag")
  if ("GenomicIndicator" %in% colnames(trait_values)) col_order <- c(col_order, "GenomicIndicator")
  if ("ParentAverageIndicator" %in% colnames(trait_values)) col_order <- c(col_order, "ParentAverageIndicator")
  col_order <- c(col_order, setdiff(colnames(trait_values), col_order))
  trait_values <- trait_values[, col_order]
  
  assign(paste0("Dam_", dam_eartag), trait_values, envir = .GlobalEnv)
  print(trait_values)
  cat("Saved dam row to environment as:", paste0("Dam_", dam_eartag), "\n")
  
  sire_eartag <- trimws(sire_eartag)
  hbn_numeric <- sub("^0+", "", sire_eartag)
  if (!"HBN" %in% colnames(dfSires)) {
    cat("Error: dfSires does not have a column named 'HBN'\n")
    return(NULL)
  }
  dfSires$HBN_stripped <- sub("^0+", "", as.character(dfSires$HBN))
  match_row <- dfSires[dfSires$HBN_stripped == hbn_numeric, ]
  if (nrow(match_row) == 0) {
    cat("No sire found with HBN:", hbn_numeric, "\n")
    return(NULL)
  }
  match_row$HBN_stripped <- NULL
  assign(paste0("Sire_", hbn_numeric), match_row, envir = .GlobalEnv)
  print(match_row)
  cat("Saved sire row to environment as:", paste0("Sire_", hbn_numeric), "\n")
  
  dam_numeric <- trait_values[, sapply(trait_values, is.numeric), drop = FALSE]
  sire_numeric <- match_row[, sapply(match_row, is.numeric), drop = FALSE]
  shared_traits <- intersect(colnames(dam_numeric), colnames(sire_numeric))
  if (length(shared_traits) == 0) {
    cat("No common numeric traits found to calculate parent average.\n")
    return(NULL)
  }
  
  # === Rounding rules from lact_groups_organizer ===
  whole_number_traits <- c("MilkKg", "FatKg", "ProtKg", "Lifespan", "PLI", "SCI", "ACI", "SCC", 
                           "PLIRel", "HealthyCow", "Feed.Advantage", "Maintenance", "GestationLength")
  one_decimal_traits <- c("FertilityIndex", "mCE", "dCE", "LamenessAdvantage", "TBAdvantage", 
                          "CalfSurvival", "DigitalDermatitis", "Enviro.Cow", "TypeMerit")
  
  # === Parent averages and rounding ===
  pa_values <- mapply(function(trait) {
    d <- as.numeric(dam_numeric[, trait])
    s <- as.numeric(sire_numeric[, trait])
    pa <- ifelse(is.na(d) | is.na(s), NA, (d + s) / 2)
    if (trait %in% whole_number_traits) {
      return(round(pa, 0))
    } else if (trait %in% one_decimal_traits) {
      return(round(pa, 1))
    } else {
      return(round(pa, 2))
    }
  }, shared_traits, SIMPLIFY = TRUE)
  
  dam_rounded <- sapply(shared_traits, function(trait) {
    val <- as.numeric(dam_numeric[, trait])
    if (trait %in% whole_number_traits) {
      round(val, 0)
    } else if (trait %in% one_decimal_traits) {
      round(val, 1)
    } else {
      round(val, 2)
    }
  })
  
  sire_rounded <- sapply(shared_traits, function(trait) {
    val <- as.numeric(sire_numeric[, trait])
    if (trait %in% whole_number_traits) {
      round(val, 0)
    } else if (trait %in% one_decimal_traits) {
      round(val, 1)
    } else {
      round(val, 2)
    }
  })
  
  # === Metadata ===
  dam_name <- if ("Name" %in% colnames(dam_row)) dam_row$Name else NA
  lact_class <- if ("LactClass" %in% colnames(dam_row)) dam_row$LactClass else NA
  dam_eval <- if (!is.na(dam_row$GenomicIndicator)) "Genomic" else if (!is.na(dam_row$ParentAverageIndicator)) "Parent Average" else "Unknown"
  sire_name <- if ("RegName" %in% colnames(match_row)) match_row$RegName else NA
  
  metadata <- data.frame(
    Trait = c("Dam_Eartag", "Dam_Name", "Lactation_Class", "Dam_Evaluation", "Sire_HBN", "Sire_Name"),
    Dam_Value = c(dam_eartag, dam_name, lact_class, dam_eval, NA, NA),
    Sire_Value = c(NA, NA, NA, NA, hbn_numeric, sire_name),
    Parent_Average = rep(NA, 6),
    Parent_Average_Percentile = rep(NA, 6),
    stringsAsFactors = FALSE
  )
  
  parent_avg_df <- data.frame(
    Trait = shared_traits,
    Dam_Value = dam_rounded,
    Sire_Value = sire_rounded,
    Parent_Average = pa_values,
    stringsAsFactors = FALSE
  )
  
  # Percentiles
  percentiles <- sapply(shared_traits, function(trait) {
    pa_val <- as.numeric(pa_values[trait])
    if (is.na(pa_val) || !(trait %in% colnames(dfHerd))) return(NA)
    herd_vals <- dfHerd[[trait]]
    herd_vals <- herd_vals[!is.na(herd_vals)]
    round(mean(herd_vals <= pa_val) * 100, 1)
  })
  parent_avg_df$Parent_Average_Percentile <- percentiles
  
  parent_avg_df <- rbind(metadata, parent_avg_df)
  rownames(parent_avg_df) <- parent_avg_df$Trait
  parent_avg_df$Trait <- NULL
  
  var_name <- paste0("ParentAverage_", dam_eartag, "_", hbn_numeric)
  assign(var_name, parent_avg_df, envir = .GlobalEnv)
  print(parent_avg_df)
  
  file_name <- file.path(output_dir, paste0(var_name, ".csv"))
  write.csv(parent_avg_df, file = file_name, row.names = TRUE)
  cat("Saved Parent Average table as CSV:", file_name, "\n")
}
#UK581723102636
#000011720553

gPLIClassPlot <- function(dfHerd, trait_col, trait_label = NULL, intBeefPercentage = "", show_legend = TRUE) {
  if (is.null(trait_label)) trait_label <- trait_col
  
  Cols <- colorRampPalette(c(rgb(206,17,65,maxColorValue=255), rgb(29,79,145,maxColorValue=255), rgb(169,168,169,maxColorValue=255)))(6)
  Cols <- setNames(Cols, c("Calf", "Heifer", "1", "2", "3", "4+"))
  
  DOB <- as.Date(dfHerd$DatDOB, "%d/%m/%Y")
  YVals <- dfHerd[[trait_col]]
  
  # Remove NAs
  valid_idx <- !is.na(DOB) & !is.na(YVals)
  DOB <- DOB[valid_idx]
  YVals <- YVals[valid_idx]
  
  # Compute percentiles
  percentiles <- quantile(YVals, probs = seq(0.1, 1.0, by = 0.1), na.rm = TRUE)
  percentile_labels <- paste0(seq(10, 100, by = 10), "%")
  
  # Plot
  plot(DOB, YVals,
       col = Cols[as.character(dfHerd$FactLactClass[valid_idx])],
       xlab = "Date Of Birth",
       ylab = "",  
       main = "",
       pch = ifelse(dfHerd$GenomicIndicator[valid_idx] != "G", 1, 16),
       yaxt = "n", xaxt = "n",
       xlim = c(
         as.Date(paste(format(min(DOB, na.rm = TRUE), "%Y-%m"), "-01", sep = "")),
         seq(as.Date(paste(format(max(DOB, na.rm = TRUE), "%Y-%m"), "-01", sep = "")), length.out = 2, by = "months")[2] - 1
       )
  )
  
  # Left Y-axis (actual trait values)
  axis(2, las = 2)
  
  # Right Y-axis (percentile labels)
  axis(4, at = percentiles, labels = percentile_labels, las = 2)
  
  # Optional: add horizontal grid lines at each percentile
  #abline(h = percentiles, col = "gray85", lty = 3)
  
  #plot titles for each trait
  title(main = paste(trait_label, "by Birthdate"), line = 1)
  
  # X-axis ticks
  axis(1,
       at = seq(
         as.Date(paste(format(min(DOB, na.rm = TRUE), "%Y-%m"), "-01", sep = "")),
         seq(as.Date(paste(format(max(DOB, na.rm = TRUE), "%Y-%m"), "-01", sep = "")), length.out = 2, by = "months")[2],
         by = "months"
       ),
       labels = FALSE, tcl = -0.25)
  
  # Legend
  if (show_legend) {
    legend("topleft",
           c(ifelse(grepl("^[0-9]", names(Cols)), paste("Lact", names(Cols)), names(Cols)), "PA", "GT"),
           pch = c(rep(15, 6), 1, 16),
           col = c(Cols, "Black", "Black"))
  }
}

ProduceBirthdateTrendPlots <- function(dfHerd, dfDataStruc, StrAuditDestination) {
  for (Category in unique(dfDataStruc$Category)) {
    cat(paste("Running Category:", Category, "\n"))
    
    TraitsInCat <- dfDataStruc[dfDataStruc$Category == Category, ]
    Class <- nrow(TraitsInCat)
    
    png(file = paste0(StrAuditDestination, "/", Category, "_BirthdateTrend.png"), width = 4200, height = 2100, res = 400)
    
    if (Class == 1) {
      par(mfrow = c(1, 1), oma = c(0, 0, 0.5, 0), mar = c(2.25, 3, 3.5, 3), mgp = c(1, 0.5, 0))
    } else if (Class == 2) {
      par(mfrow = c(1, 2), oma = c(0, 0, 0.5, 0), mar = c(2.25, 3, 3.5, 3), mgp = c(1, 0.5, 0))
    } else if (Class == 3) {
      layout(matrix(c(1, 2, 1, 3), nrow = 2, byrow = TRUE), widths = c(6/12, 6/12))
      par(oma = c(0, 0, 0.5, 0), mar = c(2.25, 3, 3.5, 3), mgp = c(1, 0.5, 0))
    } else if (Class == 4) {
      par(mfrow = c(2, 2), oma = c(0, 0, 0.5, 0), mar = c(2.25, 3, 3.5, 3), mgp = c(1, 0.5, 0))
    } else {
      layout(matrix(1:Class, nrow = ceiling(Class / 2), byrow = TRUE))
      par(oma = c(0, 0, 0.5, 0), mar = c(2.25, 3, 3.5, 3), mgp = c(1, 0.5, 0))
    }
    
    for (i in 1:Class) {
      Trait <- TraitsInCat$Trait[i]
      Label <- TraitsInCat$Label[i]
      
      # Show legend only on the first plot
      show_legend <- (i == 1)
      
      gPLIClassPlot(dfHerd, trait_col = Trait, trait_label = Label, show_legend = show_legend)
    }
    
    mtext(paste("Birthdate Trend -", Category, "Traits"), side = 3, outer = TRUE, line = -1, font = 2, cex = 1.3)
    
    dev.off()
  }
}
