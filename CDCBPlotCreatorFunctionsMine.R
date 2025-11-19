options(scipen=999)
library(dplyr)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#proddev<-"//gbsta-data2/public$/Product_Development_RnD/"
#Scriptspath<-"\\\\gbsta-data2\\public$\\Product_Development_RnD\\Scripts\\National Evaluation KAPP CDCB\\"
#OutputDir<-"//gbsta-data2/public$/Product_Development_RnD/National Evaluation CDCB"

#source(paste0(Scriptspath,"National Eval CDCB Parent AverageV3.R"))
#source(paste0(Scriptspath,"National Eval CDCB Produce Plots.R"))
#SharedCustFilePath <- "\\\\gbsta-data2\\public$\\Strategic Accounts Genetic Services\\Genetic and Genomic Customer list\\"
#SharedCustFile <- "CDCBCust.csv"
#custs <<- read.csv(paste0(SharedCustFilePath,SharedCustFile),stringsAsFactors = F)
#names(custs)[1] <- "Custfolder"
columnspec <- read.csv("/data/dairy/EMEAProjects/Audit-Data-Loader/Scripts - CDCB/ColumnSpec.csv",stringsAsFactors = F, check.names = F)


library(ggplot2)

library(zoo)

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



IndexClass<-function(CustData,Index,intBeefPercentage=40){
  
  IndexLabel<-columnspec[columnspec$Input%in%Index,2]
  
  Cols<-colorRampPalette(c("#CE1141","#003976","#7D9BC1","#BD955A","#555559","#A9A8A9"))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  plot(as.Date(CustData$DatDOB,"%d/%m/%Y"),CustData[,Index],col=Cols[as.character(CustData$FactLactClass)],xlab="Date Of Birth",ylab=paste(IndexLabel),main=paste(IndexLabel,"by Birthdate"),pch=ifelse(CustData$GenomicIndicator!="G",1,16),
       xlim=c(as.Date(paste(format(min(as.Date(CustData$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
              seq(as.Date(paste(format(max(as.Date(CustData$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2]-1)
  )
  if(!is.na(as.numeric(intBeefPercentage))){
    abline(h=as.numeric(quantile(CustData[!CustData$FactLactClass%in%c("Pregnancy","Calf"),Index],as.numeric(intBeefPercentage)/100,na.rm=T)),lwd=3,col=MyDarkRed,lty=2)
  }
  legend(
    "topleft",
    c(ifelse(grepl("^[0-9]",names(Cols)),paste("Lact",names(Cols)),names(Cols)),"PA","GT"), 
    pch=c(rep(15,6),1,16),
    col=c(Cols,"Black","Black"))
  
  
  axis(1,at=
         seq(as.Date(paste(format(min(as.Date(CustData$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),
             seq(as.Date(paste(format(max(as.Date(CustData$DatDOB,"%d/%m/%Y"),na.rm=T),"%Y-%m"),"-01",sep="")),length.out=2,by="months")[2],by="months"),
       labels=F,tcl=-0.25)
}


Indexes<-function(CustData,FolderName,AuditFolder,intBeefPercentage=40,Breed){
  
  cat(paste("Producing Index Class \n"))
  
  CustData<-CustData
  if (Breed%in%"JE"){
    png(paste(AuditFolder, "JPIClass",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    IndexClass(CustData,"JPI",intBeefPercentage=intBeefPercentage)
    dev.off()
  }else{
    png(paste(AuditFolder, "TPIClass",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    IndexClass(CustData,"TPI",intBeefPercentage=intBeefPercentage)
    dev.off()
  }
  png(paste(AuditFolder, "NMClass",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  IndexClass(CustData,"NM",intBeefPercentage=intBeefPercentage)
  dev.off()
  
  
  png(paste(AuditFolder, "FMClass",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  IndexClass(CustData,"FM",intBeefPercentage=intBeefPercentage)
  dev.off()
  
  png(paste(AuditFolder, "CMClass",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  IndexClass(CustData,"CM",intBeefPercentage=intBeefPercentage)
  dev.off()
  
  png(paste(AuditFolder, "GMClass",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  IndexClass(CustData,"GM",intBeefPercentage=intBeefPercentage)
  dev.off()
}

GenerateTraitSummaryAndPlots <- function(CustData, BullDat, PlotLocation, traits = c("TPI", "NM", "FM", "CM", "GM", "JPI")) {
  library(gridExtra)
  library(grid)
  library(magick)
  library(data.table)

  # === Step 1: Append JPI to Sire/Mgs/Mggs ===
  for (prefix in c("Sire", "Mgs", "Mggs")) {
    id_col <- paste0(prefix, "HBN")
    trait_col <- paste0(prefix, "JPI")
    
    if ("JPI" %in% names(BullDat)) {
      CustData <- merge(CustData, BullDat[, c("HBN", "JPI")], by.x = id_col, by.y = "HBN", all.x = TRUE)
      names(CustData)[names(CustData) == "JPI"] <- trait_col
    } else {
      message("Skipping ", trait_col, " - JPI not found in BullDat.")
      CustData[[trait_col]] <- NA
    }
  }
  
  # === Step 2: Add cow JPI if needed ===
  if ("JPI" %in% traits && !"JPI" %in% colnames(CustData)) {
    if ("JPI" %in% colnames(BullDat)) {
      CustData <- CDCBAppendTrait("JPI", CustData, BullDat)
    } else {
      message("Skipping JPI for cows – not available in BullDat or CustData")
      traits <- setdiff(traits, "JPI")
    }
  }
  
  # === Step 3: Filter traits available in CustData ===
  traits <- traits[traits %in% colnames(CustData)]
  
  # === Step 4: Generate Summary Tables ===
  lactation_groups <- c("4+", "3", "2", "1", "Heifer", "Calf")
  CustData$FactLactClass <- as.character(CustData$FactLactClass)
  summary_list <- list()
  
  for (trait in traits) {
    temp_data <- CustData[!is.na(CustData[[trait]]) & CustData$FactLactClass %in% lactation_groups, ]
    if (nrow(temp_data) == 0) {
      message("No data for trait ", trait)
      next
    }
    
    means <- sapply(lactation_groups, function(group) {
      round(mean(temp_data[temp_data$FactLactClass == group, trait], na.rm = TRUE))
    })
    differences <- round(c(NA, diff(means)))
    
    df_summary <- data.frame(
      `Lactation Group` = c(paste(trait, "Average"), paste(trait, "Change")),
      rbind(means, differences),
      check.names = FALSE,
      row.names = NULL
    )
    summary_list[[trait]] <- df_summary
  }
  
  # === Step 5: Save Summary Tables ===
  for (trait in names(summary_list)) {
    df <- summary_list[[trait]]
    table_grob <- tableGrob(df, rows = NULL)
    
    png(filename = file.path(PlotLocation, paste0(trait, "_Summary_Table.png")), width = 1175, height = 300, res = 260)
    grid.newpage()
    grid.draw(table_grob)
    dev.off()
  }
  
  # === Step 6: Calculate Yearly Change ===
  yearly_change_all_traits <- sapply(names(summary_list), function(trait) {
    df <- summary_list[[trait]]
    if (all(c("3", "Calf") %in% colnames(df))) {
      avg_lact3 <- df[1, "3"]
      avg_calf <- df[1, "Calf"]
      return(round((avg_calf - avg_lact3) / 4, 2))
    } else {
      return(NA)
    }
  })
  
  yearly_change_df <- data.frame(
    Trait = names(yearly_change_all_traits),
    Yearly_Change = yearly_change_all_traits,
    row.names = NULL
  )
  
  # === Step 7: Save Yearly Change Tables ===
  yearly_tables <- lapply(1:nrow(yearly_change_df), function(i) {
    trait <- yearly_change_df$Trait[i]
    value <- round(yearly_change_df$Yearly_Change[i])
    dt <- data.table(Value = value)
    setnames(dt, "Value", paste(trait, "Yearly Genetic Change"))
    return(dt)
  })
  names(yearly_tables) <- yearly_change_df$Trait
  
  for (trait in names(yearly_tables)) {
    dt <- yearly_tables[[trait]]
    table_grob <- tableGrob(dt, rows = NULL)
    
    png(filename = file.path(PlotLocation, paste0(trait, "_Yearly_Change_Table.png")),
        width = 575, height = 300, res = 240)
    grid.newpage()
    grid.draw(table_grob)
    dev.off()
  }
  
  # === Step 8: Combine Summary and Yearly Tables ===
  combine_table_and_stat <- function(table_path, stat_path, output_path) {
    total_width <- 1800
    stat_width <- 600
    table_width <- 1200
    
    # Read both images
    table_img <- image_read(table_path)
    stat_img <- image_read(stat_path)
    
    # Resize to desired dimensions
    table_img <- image_resize(table_img, geometry = paste0(table_width))
    stat_img <- image_resize(stat_img, geometry = paste0(stat_width))
    
    # Combine side-by-side
    combined_img <- image_append(c(table_img, stat_img), stack = FALSE)
    
    # Save result
    image_write(combined_img, path = output_path, format = "png")
  }
  
  for (trait in names(summary_list)) {
    summary_path <- file.path(PlotLocation, paste0(trait, "_Summary_Table.png"))
    yearly_path  <- file.path(PlotLocation, paste0(trait, "_Yearly_Change_Table.png"))
    output_path  <- file.path(PlotLocation, paste0(trait, "_Summary_and_Yearly_Change.png"))
    
    if (file.exists(summary_path) && file.exists(yearly_path)) {
      combine_table_and_stat(summary_path, yearly_path, output_path)
    } else {
      message("Missing image(s) for trait ", trait)
    }
  }
  
  # === Step 9: Combine Class Plots with Summary + Yearly Change ===
  combine_plot_and_table <- function(plot_path, table_path, output_path) {
    plot_img <- image_read(plot_path)
    table_img <- image_read(table_path)
    table_img <- image_resize(table_img, paste0(image_info(plot_img)$width))
    combined_img <- image_append(c(plot_img, table_img), stack = TRUE)
    image_write(combined_img, path = output_path, format = "png")
  }
  
  for (trait in names(summary_list)) {
    plot_files <- list.files(PlotLocation, pattern = paste0("^", trait, "Class.*\\.png$"), full.names = TRUE)
    if (length(plot_files) == 0) {
      message("No class plot found for trait ", trait)
      next
    }
    
    # Use most recently modified file
    plot_file <- plot_files[which.max(file.info(plot_files)$mtime)]
    table_file <- file.path(PlotLocation, paste0(trait, "_Summary_and_Yearly_Change.png"))
    output_file <- file.path(PlotLocation, paste0("Combined_", trait, "_Class_Summary.png"))
    
    if (file.exists(table_file)) {
      combine_plot_and_table(plot_file, table_file, output_file)
    } else {
      message("Missing table image for trait ", trait)
    }
  }
}

Indexes2<-function(CustData,FolderName,AuditFolder,intBeefPercentage=40,NationalIndex){
  
  cat(paste("Producing National Index Class \n"))
  
  for (i in 1:length(NationalIndex)){
    
    # CustData<-CustData
    png(paste(AuditFolder, NationalIndex[i],"Class",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    IndexClass(CustData,NationalIndex[i],intBeefPercentage=intBeefPercentage)
    dev.off()
    
  }
  
}





gBarplot<-function(CustData,Trait,Sig=1,Inc=T,Trunc=F,Leg=T,Min=0,XPD=T){
  
  traitlabel<-columnspec[columnspec$Input%in%Trait,"Output"]
  
  SireData<-CustData[,c(paste("Sire",Trait,sep=""),"FactLactClass")]
  
  CowData<-CustData[,c(Trait,"FactLactClass")]
  
  
  
  SireAnalysis<-tapply(SireData[,paste("Sire",Trait,sep="")],SireData$FactLactClass,mean,na.rm=T)
  SireLower<-tapply(SireData[,paste("Sire",Trait,sep="")],SireData$FactLactClass,quantile,probs=0.25,na.rm=T)
  SireUpper<-tapply(SireData[,paste("Sire",Trait,sep="")],SireData$FactLactClass,quantile,probs=0.75,na.rm=T)
  
  CowAnalysis<-tapply(CowData[,Trait],CowData$FactLactClass,mean,na.rm=T)
  CowLower<-tapply(CowData[,Trait],CowData$FactLactClass,quantile,probs=0.25,na.rm=T)
  CowUpper<-tapply(CowData[,Trait],CowData$FactLactClass,quantile,probs=0.75,na.rm=T)
  MyYLim<-c(min(Min,floor(min(CowLower,SireLower,na.rm=T)/Sig)*Sig),ceiling(max(CowUpper,SireUpper,na.rm=T)/Sig)*Sig)
  if(MyYLim[2]<0){
    MyYLim[2]<-0
  }
  Plot<-barplot(as.numeric(SireAnalysis),col=MyBlue,main=ifelse(Trunc,traitlabel,paste(traitlabel)),
                width=0.75,space=0.5,xaxt="n",ylab=traitlabel,
                ylim=MyYLim,xpd = XPD)
  
  rect(Plot-0.5,Min,Plot+0.1,as.numeric(CowAnalysis),col=MyRed)
  axis(1,at=c((-1:6)*1.125)+0.125,labels=F,pos=0)
  axis(1,at=c((0:5)*1.125)+0.125+(1.125/2),labels=levels(SireData$FactLactClass),lwd=NA)
  if(Leg){
    legend(ifelse(Inc,"topleft","topright"),c("Sires","Dairy Animals","IQ Range"),pch=c(22,22,NA),pt.bg=c(MyBlue,MyRed,NA),lwd=c(NA,NA,2))
  }
  
  lines(matrix(c(apply(data.frame(Plot-0.1,as.numeric(SireUpper),Plot+0.1,as.numeric(SireUpper),NA,NA),1,unlist)),ncol=2,nrow=length(Plot)*3*2,byrow=T),lwd=2,col=MyDarkBlue)
  lines(matrix(c(apply(data.frame(Plot-0.1,as.numeric(SireLower),Plot+0.1,as.numeric(SireLower),NA,NA),1,unlist)),ncol=2,nrow=length(Plot)*3*2,byrow=T),lwd=2,col=MyDarkBlue)
  lines(matrix(c(apply(data.frame(Plot,as.numeric(SireLower),Plot,as.numeric(SireUpper),NA,NA),1,unlist)),ncol=2,nrow=length(Plot)*3*2,byrow=T),lwd=1,lty=2,col=MyDarkBlue)
  
  lines(matrix(c(apply(data.frame(Plot-0.25,as.numeric(CowUpper),Plot-0.15,as.numeric(CowUpper),NA,NA),1,unlist)),ncol=2,nrow=length(Plot)*3*2,byrow=T),lwd=2,col=MyDarkRed)
  lines(matrix(c(apply(data.frame(Plot-0.25,as.numeric(CowLower),Plot-0.15,as.numeric(CowLower),NA,NA),1,unlist)),ncol=2,nrow=length(Plot)*3*2,byrow=T),lwd=2,col=MyDarkRed)
  lines(matrix(c(apply(data.frame(Plot-0.2,as.numeric(CowLower),Plot-0.2,as.numeric(CowUpper),NA,NA),1,unlist)),ncol=2,nrow=length(Plot)*3*2,byrow=T),lwd=1,lty=2,col=MyDarkRed)
  
  #  arrows(Plot,SireLower,Plot,SireUpper,code=3,angle=90,lwd=2,col=MyDarkBlue)
  #  arrows(Plot-0.2,CowLower,Plot-.2,CowUpper,code=3,angle=90,lwd=2,col=MyDarkRed,length=0.125)
}



FertilityBar<-function(CustData,FolderName,AuditFolder){
  
  
  png(paste(AuditFolder, "FertilityBar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=30,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,2,1,3),nrow=2,byrow=T),widths=c(1,1))
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"DPR",Inc=F)
  par(mar=c(3, 4, 7, 1))
  gBarplot(CustData,"HCR",Leg = F)
  gBarplot(CustData,"CCR",Leg = F)
  mtext("Genetic Improvement in Fertility PTAs",3,line=-2,cex=1.2,font=2,outer=T)
  dev.off()
  
}


LongetivityBar<-function(CustData,FolderName,AuditFolder){
  
  
  png(paste(AuditFolder,"Longevity & Survivability BAR",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=30,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,2,1,2),nrow=2,byrow=T),widths=c(1,1))
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"PL")
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"LIV",Leg = F)
  
  mtext("Genetic Improvement in Longevity & Survivability PTAs",3,line=-2,cex=1.2,font=2,outer=T)
  dev.off()
  
}


FeedSavedBar<-function(CustData,FolderName,AuditFolder){
  
  png(paste(AuditFolder,"Feed Saved BAR",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=30,height=10.56*1.5,units = "cm" )
  
  gBarplot(CustData,"FS")
  
  dev.off()
  
}



ProductionBar<-function(CustData,FolderName,AuditFolder){
  
  png(paste(AuditFolder, "ProductionBar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,2,1,3),nrow=2,byrow=T),widths=c(1,1))
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"MILK")#
  par(mar=c(3, 4, 7, 1))
  gBarplot(CustData,"PRO",Leg = F)
  par(mar=c(5, 4, 3, 1))
  gBarplot(CustData,"FAT",Leg = F)
  mtext("Genetic Improvement in Production PTAs",3,line=-1.5,cex=1.2,font=2,outer=T)
  dev.off()
  
}


IndexBar<-function(CustData,FolderName,AuditFolder,Breed){
  
  png(paste(AuditFolder, "NMIndexBar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  gBarplot(CustData,"NM")
  dev.off()
  
  png(paste(AuditFolder, "CMIndexBar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  gBarplot(CustData,"CM",Leg = F)
  dev.off()
  
  png(paste(AuditFolder, "FMIndexBar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  gBarplot(CustData,"FM",Leg = F)
  dev.off()
  
  if(Breed%in%"JE"){
    png(paste(AuditFolder, "JPIIndexBar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    gBarplot(CustData,"JPI",Leg = F)
    dev.off() 
  }else{
    png(paste(AuditFolder, "TPIIndexBar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    gBarplot(CustData,"TPI",Leg = F)
    dev.off()
  }
  
  
  
}



BodyCompBar<-function(CustData,FolderName,AuditFolder){
  cat(paste("Producing Body Comp Bar \n"))
  
  png(paste(AuditFolder, "BodyCompositebar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,3,4,2,5,6),nrow=2,byrow=T),widths=c(1.5,1.25,1.25))
  
  par(mar=c(2, 4, 7, 1))
  gBarplot(CustData,"BWC",Inc=F)
  par(mar=c(3, 4, 2, 1))
  gBarplot(CustData,"STA",Leg = F)
  par(mar=c(2, 4, 7, 1))
  gBarplot(CustData,"STR",Leg = F)
  par(mar=c(2, 4, 7, 1))
  gBarplot(CustData,"TRW",Leg = F)
  par(mar=c(3, 4, 2, 1))
  gBarplot(CustData,"DFM",Leg = F)
  par(mar=c(3, 4, 2, 1))
  gBarplot(CustData,"BDE",Leg = F)
  
  mtext("Genetic Improvement in Body Composition PTAs",3,line=-1.5,cex=1.2,font=2,outer=T)
  dev.off()
  
}

FLCBar<-function(CustData,FolderName,AuditFolder){
  
  cat(paste("Producing FLC Bar \n"))
  
  png(paste(AuditFolder, "FLCbar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,2,3,1,4,4),nrow=2,byrow=T),widths=c(2,1,1))
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"FLC")
  par(mar=c(3, 4, 7, 1))
  gBarplot(CustData,"FTA",Leg = F)
  par(mar=c(3, 4, 7, 1))
  gBarplot(CustData,"RLR",Leg = F)
  par(mar=c(3, 4, 3, 1))
  gBarplot(CustData,"FLS",Leg = F)
  mtext("Genetic Improvement in FLC PTAs",3,line=-1.5,cex=1.2,font=2,outer=T)
  dev.off()
  
}


UdderCompBar<-function(CustData,FolderName,AuditFolder,Breed){
  
  
  cat(paste("Producing Udder Comp Bar \n"))
  
  png(paste(AuditFolder, "UdderCompositebar",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,3,4,2,5,6),nrow=2,byrow=T),widths=c(2,1,1))
  
  if (Breed %in%"JE"){
    par(mar=c(2, 4, 7, 1))
    gBarplot(CustData,"JUI",Inc=T)
  }else{
    par(mar=c(2, 4, 7, 1))
    gBarplot(CustData,"UDC",Inc=T)
  }
  par(mar=c(3, 4, 2, 1))
  gBarplot(CustData,"UDP",Leg = F)
  par(mar=c(2, 4, 7, 1))
  gBarplot(CustData,"UCL",Leg = F)
  par(mar=c(2, 4, 7, 1))
  gBarplot(CustData,"RUW",Leg = F)
  par(mar=c(3, 4, 2, 1))
  gBarplot(CustData,"RTP",Leg = F)
  par(mar=c(3, 4, 2, 1))
  gBarplot(CustData,"TLG",Leg = F)
  
  mtext("Genetic Improvement in Udder PTAs",3,line=-1.5,cex=1.2,font=2,outer=T)
  dev.off()
  
}



UdderHealthBar<-function(CustData,FolderName,AuditFolder){
  
  cat(paste("Producing Udder Health Bar \n"))
  
  png(paste(AuditFolder, "Udder Health BAR",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=30,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,2,1,2),nrow=2,byrow=T),widths=c(1,1))
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"MAS")
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"SCS",Leg = F,Min=2,XPD=F)
  
  mtext("Genetic Improvement in Udder Health PTAs",3,line=-2,cex=1.2,font=2,outer=T)
  dev.off()
  
}



transHealthBar<-function(CustData,FolderName,AuditFolder){
  
  
  cat(paste("Producing Transition Health Bar \n"))
  
  
  png(paste(AuditFolder, "Trans Health BAR",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=30,height=10.56*1.5,units = "cm" )
  
  layout(matrix(c(1,2,1,2),nrow=2,byrow=T),widths=c(1,1))
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"KET")
  par(mar=c(5, 4, 7, 1))
  gBarplot(CustData,"MET",Leg = F)
  
  mtext("Genetic Improvement in Transistion Health PTAs",3,line=-2,cex=1.2,font=2,outer=T)
  dev.off()
  
}

PlotNMSmooth<-function(CustData){
  CustData<-CustData[order(CustData$FactLactClass,decreasing=T),]
  Cols<-colorRampPalette(c("#CE1141","#003976","#7D9BC1","#BD955A","#555559","#A9A8A9"))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  MyHistogram <- CustData %>%
    ggplot( aes(x=NM, fill=FactLactClass,color=FactLactClass)) +
    geom_density(alpha=0.7, position = 'identity')  +
    scale_fill_manual(name = 'Demographic', 
                      values =Cols,breaks=names(Cols),labels=names(Cols))+
    scale_color_manual(name = 'Demographic', 
                       values =Cols,breaks=names(Cols),labels=names(Cols))+
    aes(y=stat(count)/sum(stat(count))) + theme_bw()+ggtitle(paste("Histogram Showing Distribution on Net Merit ($) by Lactation Group"))+
    labs(x="Net Merit ($)",y="Proportion of Lact Class")+
    theme(text = element_text(size=12),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(),legend.position=c(0.1,0.7),plot.title = element_text(hjust=0.5,size=13,face="bold",margin=margin(0,0,3,0,unit="pt"),vjust=3), axis.title  = element_text(size=12,face="bold"),axis.title.x = element_text(vjust=-3),axis.title.y = element_text(vjust=3),plot.margin=margin(0.75,0.25,1,1,unit="cm"))+  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  return(MyHistogram)
}

PlotCMSmooth<-function(CustData){
  CustData<-CustData[order(CustData$FactLactClass,decreasing=T),]
  Cols<-colorRampPalette(c("#CE1141","#003976","#7D9BC1","#BD955A","#555559","#A9A8A9"))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  MyHistogram <- CustData %>%
    ggplot( aes(x=CM, fill=FactLactClass,color=FactLactClass)) +
    geom_density(alpha=0.7, position = 'identity')  +
    scale_fill_manual(name = 'Demographic', 
                      values =Cols,breaks=names(Cols),labels=names(Cols))+
    scale_color_manual(name = 'Demographic', 
                       values =Cols,breaks=names(Cols),labels=names(Cols))+
    aes(y=stat(count)/sum(stat(count))) + theme_bw()+ggtitle(paste("Histogram Showing Distribution on Cheese Merit ($) by Lactation Group"))+
    labs(x="Cheese Merit ($)",y="Proportion of Lact Class")+
    theme(text = element_text(size=12),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(),legend.position=c(0.1,0.7),plot.title = element_text(hjust=0.5,size=13,face="bold",margin=margin(0,0,3,0,unit="pt"),vjust=3), axis.title  = element_text(size=12,face="bold"),axis.title.x = element_text(vjust=-3),axis.title.y = element_text(vjust=3),plot.margin=margin(0.75,0.25,1,1,unit="cm"))+  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  return(MyHistogram)
}

PlotGMSmooth<-function(CustData){
  CustData<-CustData[order(CustData$FactLactClass,decreasing=T),]
  Cols<-colorRampPalette(c("#CE1141","#003976","#7D9BC1","#BD955A","#555559","#A9A8A9"))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  MyHistogram <- CustData %>%
    ggplot( aes(x=GM, fill=FactLactClass,color=FactLactClass)) +
    geom_density(alpha=0.7, position = 'identity')  +
    scale_fill_manual(name = 'Demographic', 
                      values =Cols,breaks=names(Cols),labels=names(Cols))+
    scale_color_manual(name = 'Demographic', 
                       values =Cols,breaks=names(Cols),labels=names(Cols))+
    aes(y=stat(count)/sum(stat(count))) + theme_bw()+ggtitle(paste("Histogram Showing Distribution on Grazing Merit ($) by Lactation Group"))+
    labs(x="Cheese Merit ($)",y="Proportion of Lact Class")+
    theme(text = element_text(size=12),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(),legend.position=c(0.1,0.7),plot.title = element_text(hjust=0.5,size=13,face="bold",margin=margin(0,0,3,0,unit="pt"),vjust=3), axis.title  = element_text(size=12,face="bold"),axis.title.x = element_text(vjust=-3),axis.title.y = element_text(vjust=3),plot.margin=margin(0.75,0.25,1,1,unit="cm"))+  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  return(MyHistogram)
}


PlotTPISmooth<-function(CustData){
  CustData<-CustData[order(CustData$FactLactClass,decreasing=T),]
  Cols<-colorRampPalette(c("#CE1141","#003976","#7D9BC1","#BD955A","#555559","#A9A8A9"))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  MyHistogram <- CustData %>%
    ggplot( aes(x=TPI, fill=FactLactClass,color=FactLactClass)) +
    geom_density(alpha=0.7, position = 'identity')  +
    scale_fill_manual(name = 'Demographic', 
                      values =Cols,breaks=names(Cols),labels=names(Cols))+
    scale_color_manual(name = 'Demographic', 
                       values =Cols,breaks=names(Cols),labels=names(Cols))+
    aes(y=stat(count)/sum(stat(count))) + theme_bw()+ggtitle(paste("Histogram Showing Distribution on TPI by Lactation Group"))+
    labs(x="TPI",y="Proportion of Lact Class")+
    theme(text = element_text(size=12),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(),legend.position=c(0.1,0.7),plot.title = element_text(hjust=0.5,size=13,face="bold",margin=margin(0,0,3,0,unit="pt"),vjust=3), axis.title  = element_text(size=12,face="bold"),axis.title.x = element_text(vjust=-3),axis.title.y = element_text(vjust=3),plot.margin=margin(0.75,0.25,1,1,unit="cm"))+  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  return(MyHistogram)
}

PlotJPISmooth<-function(CustData){
  CustData<-CustData[order(CustData$FactLactClass,decreasing=T),]
  Cols<-colorRampPalette(c("#CE1141","#003976","#7D9BC1","#BD955A","#555559","#A9A8A9"))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  MyHistogram <- CustData %>%
    ggplot( aes(x=JPI, fill=FactLactClass,color=FactLactClass)) +
    geom_density(alpha=0.7, position = 'identity')  +
    scale_fill_manual(name = 'Demographic', 
                      values =Cols,breaks=names(Cols),labels=names(Cols))+
    scale_color_manual(name = 'Demographic', 
                       values =Cols,breaks=names(Cols),labels=names(Cols))+
    aes(y=stat(count)/sum(stat(count))) + theme_bw()+ggtitle(paste("Histogram Showing Distribution on JPI by Lactation Group"))+
    labs(x="JPI",y="Proportion of Lact Class")+
    theme(text = element_text(size=12),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(),legend.position=c(0.1,0.7),plot.title = element_text(hjust=0.5,size=13,face="bold",margin=margin(0,0,3,0,unit="pt"),vjust=3), axis.title  = element_text(size=12,face="bold"),axis.title.x = element_text(vjust=-3),axis.title.y = element_text(vjust=3),plot.margin=margin(0.75,0.25,1,1,unit="cm"))+  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  return(MyHistogram)
}

PlotFMSmooth<-function(CustData){
  CustData<-CustData[order(CustData$FactLactClass,decreasing=T),]
  Cols<-colorRampPalette(c("#CE1141","#003976","#7D9BC1","#BD955A","#555559","#A9A8A9"))(6)
  Cols<-setNames(Cols,c("Calf","Heifer","1","2","3","4+"))
  MyHistogram <- CustData %>%
    ggplot( aes(x=FM, fill=FactLactClass,color=FactLactClass)) +
    geom_density(alpha=0.7, position = 'identity')  +
    scale_fill_manual(name = 'Demographic', 
                      values =Cols,breaks=names(Cols),labels=names(Cols))+
    scale_color_manual(name = 'Demographic', 
                       values =Cols,breaks=names(Cols),labels=names(Cols))+
    aes(y=stat(count)/sum(stat(count))) + theme_bw()+ggtitle(paste("Histogram Showing Distribution on Fluid Merit ($) by Lactation Group"))+
    labs(x="Fluid Merit ($)",y="Proportion of Lact Class")+
    theme(text = element_text(size=12),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(),legend.position=c(0.1,0.7),plot.title = element_text(hjust=0.5,size=13,face="bold",margin=margin(0,0,3,0,unit="pt"),vjust=3), axis.title  = element_text(size=12,face="bold"),axis.title.x = element_text(vjust=-3),axis.title.y = element_text(vjust=3),plot.margin=margin(0.75,0.25,1,1,unit="cm"))+  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  return(MyHistogram)
}

Histograms<-function(CustData,FolderName,AuditFolder,Breed){
  
  cat(paste("Producing Index Hists \n"))
  
  CustData<-CustData
  
  if (Breed%in%"JE"){
    png(paste(AuditFolder, "JPIHist",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    plot(PlotJPISmooth(CustData))
    dev.off()
  }else{
    png(paste(AuditFolder, "TPIHist",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    plot(PlotTPISmooth(CustData))
    dev.off()
  }
  png(paste(AuditFolder, "NMHist",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  plot(PlotNMSmooth(CustData))
  dev.off()
  
  
  png(paste(AuditFolder, "FMHist",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  plot(PlotFMSmooth(CustData))
  dev.off()
  
  png(paste(AuditFolder, "CMHist",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  plot(PlotCMSmooth(CustData))
  dev.off()
  
  png(paste(AuditFolder, "GMHist",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  plot(PlotGMSmooth(CustData))
  dev.off()
}


# 
# MyHistogram<-function(CustData,strDisplay){
#   if(strDisplay%in%c("",NULL)|is.na(strDisplay)){
#     strDisplay<-""
#   }else{
#     strDisplay<-paste("For",strDisplay)
#   }
#   hist(CustData$PLI[CustData$PLI>-500]*2,main=paste("Distribution of PLI",strDisplay),xlab="?PLI (EBV Scale)",col=MyDarkBlue)
#   abline(v=mean(CustData$PLI[CustData$PLI>-500]*2),lwd=4,col=MyRed,lty=2)
#   arrows(mean(CustData$PLI[CustData$PLI>-500]*2)-sd(CustData$PLI[CustData$PLI>-500]*2),par("usr")[3]+(2*((par("usr")[4]-par("usr")[3])/3)),mean(CustData$PLI[CustData$PLI>-500]*2)+sd(CustData$PLI[CustData$PLI>-500]*2),par("usr")[3]+(2*((par("usr")[4]-par("usr")[3])/3)),lwd=4,col=MyRed,code=3,angle=90)
#   text(mean(CustData$PLI[CustData$PLI>-500]*2)+sd(CustData$PLI[CustData$PLI>-500]*2),par("usr")[3]+(2*((par("usr")[4]-par("usr")[3])/3)),round(sd(CustData$PLI[CustData$PLI>-500]*2),2),pos=4,cex=1.5,font=2)
#   text(mean(CustData$PLI[CustData$PLI>-500]*2),0,round(mean(CustData$PLI[CustData$PLI>-500]*2),2),pos=3,cex=1.5,font=2,col="white")
#   #dev.off()
# }

#png(paste(SourceDir,"\\",CustomerName,"\\","GeneticLag",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),width=15,height=11,units = "cm",res=Res,pointsize=9)
#boxplot(PLIN*2~Birth_YearN,LastProofs[which(LastProofs$Birth_YearN>2010&LastProofs$Birth_YearN<as.numeric(format(Sys.Date(),"%Y"))-1),],main=paste("Genetic Lag for",CustomerLabel),outline=F,col=rgb(206,17,65,maxColorValue = 255),xlab="Birth Year",ylab="?PLI",ylim=c(min(CustData$PLI[CustData$PLI>-500]*2),2000))
#boxplot(PLI*2~LactNo,CustData[CustData$PLI>-500,],outline=F,border=rgb(0,57,118,maxColorValue = 255),add=T,at=seq(length(seq(2011,as.numeric(format(Sys.Date(),"%Y"))-1,1))-6,length(seq(2011,as.numeric(format(Sys.Date(),"%Y"))-2,1)),1),col=rgb(125,155,193,maxColorValue = 255),xaxt="n")
#legend("topleft",c("Industry Genetic Progress",paste(CustomerLabel,"Genetic Progress")),pt.bg = c(rgb(206,17,65,maxColorValue = 255),rgb(125,155,193,maxColorValue = 255)),col=c("black",rgb(0,57,118,maxColorValue = 255)),pch=22,lwd=NA,pt.cex=2)
#dev.off()



ROI<-function(CustData,Prop=0.6,trait,name){
  
  if(Prop %in% 0){
    
    #cat("\n Setting beef prop to 5%")
    Prop <- 0.05
  }
  traitlabel<-columnspec[columnspec$Input%in%trait,"Output"]
  
  
  GeneticVariation<-sd(CustData[CustData[,trait]>-990,trait]*2, na.rm = T)
  
  
  
  Reference<-"AHDB Data"
  Trait<-trait
  Mean<-mean(CustData[CustData[,trait]>-990,trait]*2,na.rm=T)
  
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
  Mn3<-Mean+(GeneticVariation*sqrt(0.65)*SI)
  Mn4<-Mean+(GeneticVariation*sqrt(0.65)*SCSI)
  Min<-(floor(Mean/100)*100)
  if(Min==floor(Mean)){
    
    Min<-Min-100
  }
  
  Max<-(ceiling(max(Mean,Mn1,Mn2,Mn3,Mn4)/100)*100)+50
  barplot(c(Mean,Mn1,Mn2,Mn3,Mn4),
          #You need to change the y lim (limits to the y axis to make it look pretty)
          ylim=c(Min,Max),xlim=c(0+0.2,8-0.2),ylab=paste(traitlabel,"BV"),main=paste("Genetic Improvement* for ",name, " \n in One Generation Using Various Technology",sep=""),col=c("darkred","royalblue1","royalblue2","royalblue3","royalblue4"),xpd=F,xaxt="n",yaxt="n",width=1,space=0.5,bg = "grey93",font.lab=2)
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
    paste("*Genetic variation for is approximated from a three generation sire stack.\nConventional selection intensity of top ",round(Prop,2)*100,"%. For Sexcel the top ",round(SCSIProp,2)*100,"% used for the same number of Heifers. All cows served to conception.",sep=""),
    side=1,line=-1,outer=T,cex=0.75 
  )
  
}


ROISexedCDCB<-function(CustData,Prop=0.6,trait,name){
  
  #traitlabel<-columnspec[columnspec$Input%in%trait,"Output"]
  
  
  GeneticVariation<-sd(CustData[CustData[,trait]>-990,trait]*2, na.rm = T)
  
  
  
  Reference<-"AHDB Data"
  Trait<-trait
  Mean<-mean(CustData[CustData[,trait]>-990,trait]*2,na.rm=T)
  
  Magnitude<-10
  By<-100
  
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
  Max<-(ceiling(max(Mean,Mn1,Mn2,Mn3,Mn4,Mn5,Mn6)/100)*100)+120
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
    paste("*Genetic variation for is approximated from a three generation sire stack. Differences in mean are halved (PTA) to reflect dam contribution.\nSelection intensity of bottom 55%, 60% and 65% beef are modelled with selection taking place on both PA and GT values.",sep=""),
    side=1,line=-1,outer=T,cex=0.75
  )
}


ROINoBeefCDCB<-function(CustData,Prop=0.6,trait,name){
  
  #traitlabel<-columnspec[columnspec$Input%in%trait,"Output"]
  
  
  GeneticVariation<-sd(CustData[CustData[,trait]>-990,trait]*2, na.rm = T)
  
  Trait<-trait
  Mean<-mean(CustData[CustData[,trait]>-990,trait]*2,na.rm=T)
  
  Magnitude<-10
  By<-100
  
  ###Need to calculate the SI for varying degrees of sexed and conventional
  
  #####20% SEXED
  #Sexed %
  SexcelProp20<-0.2
  #Conv %
  ConvProp20<-1-SexcelProp20
  #No. of heifers born as a %)
  Sexcelcows20<-SexcelProp20*0.95
  Convcows20<-ConvProp20*0.5
  #Calculate the total no. of heifers born (again as a %)
  Totalcows20<-Sexcelcows20+Convcows20
  #Get proportion of heifers born through sexed and conventional
  SexcelPropcows20<-Sexcelcows20/Totalcows20
  ConvPropcows20<-Convcows20/Totalcows20
  
  x20<-(SexcelPropcows20*SexcelProp20)+(ConvPropcows20*1)
  SI20<-dnorm(qnorm(1-x20))/x20
  
  ####40% SEXED
  SexcelProp40<-0.4
  ConvProp40<-1-SexcelProp40
  #selection intensity
  Sexcelcows40<-SexcelProp40*0.95
  Convcows40<-ConvProp40*0.5
  Totalcows40<-Sexcelcows40+Convcows40
  
  SexcelPropcows40<-Sexcelcows40/Totalcows40
  ConvPropcows40<-Convcows40/Totalcows40
  
  x40<-(SexcelPropcows40*SexcelProp40)+(ConvPropcows40*1)
  SI40<-dnorm(qnorm(1-x40))/x40
  
  SexcelProp60<-0.6
  ConvProp60<-1-SexcelProp60
  #selection intensity
  Sexcelcows60<-SexcelProp60*0.95
  Convcows60<-ConvProp60*0.5
  Totalcows60<-Sexcelcows60+Convcows60
  
  SexcelPropcows60<-Sexcelcows60/Totalcows60
  ConvPropcows60<-Convcows60/Totalcows60
  
  x60<-(SexcelPropcows60*SexcelProp60)+(ConvPropcows60*1)
  SI60=dnorm(qnorm(1-x60))/x60
  
  
  #Calculation of selected mean
  Mn1<-Mean+(GeneticVariation*sqrt(0.33)*SI20)
  Mn2<-Mean+(GeneticVariation*sqrt(0.33)*SI40)
  Mn3<-Mean+(GeneticVariation*sqrt(0.33)*SI60)
  Mn4<-Mean+(GeneticVariation*sqrt(0.7)*SI20)
  Mn5<-Mean+(GeneticVariation*sqrt(0.7)*SI40)
  Mn6<-Mean+(GeneticVariation*sqrt(0.7)*SI60)
  
  Min<-(floor(Mean/100)*100)
  Max<-(ceiling(max(Mean,Mn1,Mn2,Mn3,Mn4,Mn5,Mn6)/100)*100)+120
  barplot(c(Mean,Mn1,Mn2,Mn3,Mn4,Mn5,Mn6),
          #You need to change the y lim (limits to the y axis to make it look pretty)
          ylim=c(Min,Max),xlim=c(0+0.2,8-0.2),ylab=paste(Trait,"BV"),
          main="Genetic Improvement* in One Generation Using Various Technology",
          col=c("darkred","royalblue1","royalblue2","royalblue3","royalblue4","royalblue","blue","blue1"),
          xpd=F,xaxt="n",yaxt="n",width=0.8,space=0.4,bg = "grey93",font.lab=2)
  axis(1,c(0.15,1.25,4.65,8),labels=F,tck=-0.05 )
  axis(1,c(2.95,6.3),labels=c("PA","GT"),tck=0,line=1.5,lwd=0,font=2,cex.axis=1.25)
  axis(1,c(1.85,2.95,4.05,5.15,6.25,7.35),labels=c("20% Sexcel","40% Sexcel","60% Sexcel","20% Sexcel","40% Sexcel","60% Sexcel"),tck=0,line=0,lwd=0,font=2)
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
    paste("*Genetic variation for is approximated from a three generation sire stack. Differences in mean are halved (PTA) to reflect dam contribution.\nSelection intensity of top 20%, 40% and 60% Sexcel are modelled with selection taking place on both PA and GT values.",sep=""),
    side=1,line=-1,outer=T,cex=0.75
  )
}

ReplacementRate<-0.3
ROINoBeefWithSaleCDCB<-function(CustData,Prop=0.6,trait,name){
  
  #traitlabel<-columnspec[columnspec$Input%in%trait,"Output"]
  
  
  GeneticVariation<-sd(CustData[CustData[,trait]>-990,trait]*2, na.rm = T)
  
  Trait<-trait
  Mean<-mean(CustData[CustData[,trait]>-990,trait]*2,na.rm=T)
  
  Magnitude<-10
  By<-100
  
  ###Need to calculate the SI for varying degrees of sexed and conventional
  
  #####20% SEXED
  #Sexed %
  SexcelProp20<-0.2
  #Conv %
  ConvProp20<-1-SexcelProp20
  #No. of heifers born as a %)
  Sexcelcows20<-SexcelProp20*0.95
  Convcows20<-ConvProp20*0.5
  #Calculate the total no. of heifers born (again as a %)
  Totalcows20<-Sexcelcows20+Convcows20
  #Get proportion of heifers born through sexed and conventional
  SexcelPropcows20<-Sexcelcows20/Totalcows20
  ConvPropcows20<-Convcows20/Totalcows20
  
  x20<-(SexcelPropcows20*SexcelProp20)+(ConvPropcows20*1)
  SI20<-dnorm(qnorm(1-x20))/x20
  
  ####40% SEXED
  SexcelProp40<-0.4
  ConvProp40<-1-SexcelProp40
  #selection intensity
  Sexcelcows40<-SexcelProp40*0.95
  Convcows40<-ConvProp40*0.5
  Totalcows40<-Sexcelcows40+Convcows40
  
  SexcelPropcows40<-Sexcelcows40/Totalcows40
  ConvPropcows40<-Convcows40/Totalcows40
  
  x40<-(SexcelPropcows40*SexcelProp40)+(ConvPropcows40*1)
  SI40<-dnorm(qnorm(1-x40))/x40
  
  SexcelProp60<-0.6
  ConvProp60<-1-SexcelProp60
  #selection intensity
  Sexcelcows60<-SexcelProp60*0.95
  Convcows60<-ConvProp60*0.5
  Totalcows60<-Sexcelcows60+Convcows60
  
  SexcelPropcows60<-Sexcelcows60/Totalcows60
  ConvPropcows60<-Convcows60/Totalcows60
  
  x60<-(SexcelPropcows60*SexcelProp60)+(ConvPropcows60*1)
  SI60=dnorm(qnorm(1-x60))/x60
  
  #Calculation of selected mean
  Mn1<-Mean+(GeneticVariation*sqrt(0.33)*SI20)
  Mn2<-Mean+(GeneticVariation*sqrt(0.33)*SI40)
  Mn3<-Mean+(GeneticVariation*sqrt(0.33)*SI60)
  Mn4<-Mean+(GeneticVariation*sqrt(0.7)*SI20)
  Mn5<-Mean+(GeneticVariation*sqrt(0.7)*SI40)
  Mn6<-Mean+(GeneticVariation*sqrt(0.7)*SI60)
  
  ##############################
  #Now need to include the selection intensity from selling the calves 
  
  #Calculate the proportion of animals which will be kept to meet replacement rate
  SIOffspringKept20<-ReplacementRate/Totalcows20
  #Calculate the SI of keeping the animals
  SIOffspring20<-dnorm(qnorm(1-SIOffspringKept20))/SIOffspringKept20
  
  SIOffspringKept40<-ReplacementRate/Totalcows40
  SIOffspring40<-dnorm(qnorm(1-SIOffspringKept40))/SIOffspringKept40
  
  SIOffspringKept60<-ReplacementRate/Totalcows60
  SIOffspring60<-dnorm(qnorm(1-SIOffspringKept60))/SIOffspringKept60
  
  GeneticVariationOffspring<-GeneticVariation*0.33
  #Assume the genetic variation in the animals born is approx. 1/3rd of the herd variation
  Mn1Final<-(GeneticVariationOffspring*sqrt(0.33)*SIOffspring20)
  Mn2Final<-(GeneticVariationOffspring*sqrt(0.33)*SIOffspring40)
  Mn3Final<-(GeneticVariationOffspring*sqrt(0.33)*SIOffspring60)
  Mn4Final<-(GeneticVariationOffspring*sqrt(0.7)*SIOffspring20)
  Mn5Final<-(GeneticVariationOffspring*sqrt(0.7)*SIOffspring40)
  Mn6Final<-(GeneticVariationOffspring*sqrt(0.7)*SIOffspring60)
  
  BarplotData<-matrix(c(Mean,Mn1,Mn2,Mn3,Mn4,Mn5,Mn6,
                        0,Mn1Final,Mn2Final,Mn3Final,Mn4Final,Mn5Final,Mn6Final),nrow=2,byrow = T)
  
  colnames(BarplotData)<-c("Mean","Mn1","Mn2","Mn3","Mn4","Mn5","Mn6")
  rownames(BarplotData)<-c("Breeding Intensity","Sale Intensity")
  
  Min<-(floor(Mean/100)*100)
  Max<-(ceiling(max(Mean,Mn1+Mn1Final,Mn2+Mn2Final,Mn3+Mn3Final,
                    Mn4+Mn4Final,Mn5+Mn5Final,Mn6+Mn6Final)/100)*100)+120
  
  barplot(BarplotData,
          #You need to change the y lim (limits to the y axis to make it look pretty)
          ylim=c(Min,Max),xlim=c(0+0.2,8-0.2),ylab=paste(Trait,"BV"),
          main="Genetic Improvement* in One Generation Using Various Technology",
          col=c("darkred","royalblue1","royalblue2","royalblue3","royalblue4","royalblue","blue","blue1"),
          xpd=F,xaxt="n",yaxt="n",width=0.8,space=0.4,bg = "grey93",font.lab=2)
  axis(1,c(0.15,1.25,4.65,8),labels=F,tck=-0.05 )
  axis(1,c(2.95,6.3),labels=c("PA","GT"),tck=0,line=1.5,lwd=0,font=2,cex.axis=1.25)
  axis(1,c(1.85,2.95,4.05,5.15,6.25,7.35),labels=c("20% Sexcel","40% Sexcel","60% Sexcel","20% Sexcel","40% Sexcel","60% Sexcel"),tck=0,line=0,lwd=0,font=2)
  axis(1,c(0.7),labels=c("Original"),tck=0,line=0,lwd=0,font=2)
  axis(1,c(2.35,3.55,5.75,6.85),labels=F,tck=-0.02)
  abline(v=1.25,lty=2)
  #You need to change the axis sequence to make it look pretty
  axis(2,at=seq(from=Min,to=Max,by=50),pos=0.15,font.axis=2)
  YText<-(par("usr")[4]-(par("usr")[4]-par("usr")[3])*0.15)
  YTextTwo<-(par("usr")[4]-(par("usr")[4]-par("usr")[3])*0.025)
  text(1.85,YText,paste("+",round(Mn1+Mn1Final-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(2.95,YText,paste("+",round(Mn2+Mn2Final-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(4.05,YText,paste("+",round(Mn3+Mn3Final-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(5.15,YText,paste("+",round(Mn4+Mn4Final-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(6.25,YText,paste("+",round(Mn5+Mn5Final-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(7.35,YText,paste("+",round(Mn6+Mn6Final-Mean,2)/2,"\n(PTA)",sep=""),font=2,cex=1.2)
  text(0.7,YTextTwo,"Initial Herd",font=2)
  text(4.75,YTextTwo,"Replacements After One Generation of Selection",font=2)
  mtext(
    paste("*Differences in mean are halved (PTA) to reflect dam contribution.Selection intensity of top 20%, 40% and 60% Sexcel are modelled with selection taking place\n on both PA and GT values. The benefits of selection from selling excess heifers is also included",sep=""),
    side=1,line=-1,outer=T,cex=0.75
  )
}


ROIPngs<-function(CustData,FolderName,AuditFolder,name,prop=0.6,Breed){
  
  if (Breed%in%"JE"){
    png(paste(AuditFolder, "JPIROI",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    ROI(CustData,trait = "JPI",name=name,Prop=prop)
    dev.off()
  }else{
    png(paste(AuditFolder, "TPIROI",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    ROI(CustData,trait = "TPI",name=name,Prop=prop)
    dev.off()
  }
  png(paste(AuditFolder, "NMROI",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  ROI(CustData,trait = "NM",name=name,Prop=prop)
  dev.off()
  
  png(paste(AuditFolder, "CMROI",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  ROI(CustData,trait = "CM",name=name,Prop=prop)
  dev.off()
  
  png(paste(AuditFolder, "FMROI",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  
  ROI(CustData,trait = "FM",name=name,Prop=prop)
  dev.off()
  
  png(paste(AuditFolder, "GMROI",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  ROI(CustData,trait = "GM",name=name,Prop=prop)
  dev.off()
  
}




phenotypecorrtriangle<-function(traitEBV,Phenotype){
  
  if(Phenotype=="DOPN"){
    strPhLabel<-"DOPN"
  }else if(Phenotype=="KG305ME"){
    strPhLabel<-"305 ME"
  }else if(Phenotype=="KGFat305ME"){
    strPhLabel<-"305 ME Fat Kg"
  }else if(Phenotype=="KGProt305ME"){
    strPhLabel<-"305 ME Protein Kg"
  }else if(Phenotype=="KGProt305ME"){
    strPhLabel<-"305 ME Protein Kg"
  }else if(Phenotype=="TestFatPc"){
    strPhLabel<-"Test Fat %"
  }else if(Phenotype=="TestProtPc"){
    strPhLabel<-"Test Protein %"
  }
  
  
  traitlabel<-columnspec[columnspec$Input%in%traitEBV,"Output"]
  
  
  
  CustData[,traitEBV]<- CustData[,traitEBV]*2
  
  
  Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(169,168,169,maxColorValue =255),rgb(29,79,145,maxColorValue =255)),bias =2)(length(unique((CustData$LactClass[!CustData$LactClass%in%c("Calf","Heifer")]))))
  
  plot(CustData[,traitEBV],CustData[,Phenotype],col=ifelse(CustData$GenomicIndicator=="G",Cols,adjustcolor(c=Cols,0.5)),bg=ifelse(CustData$GenomicIndicator=="G",Cols,adjustcolor(c=Cols,0.1)),pch=ifelse(CustData$GenomicIndicator=="G",17,16),cex=ifelse(CustData$GenomicIndicator=="G",1.1,1),ylab=strPhLabel,xlab=paste(traitlabel,"(g)EBV"),main=paste("Genetic Correlation to" ,traitlabel ,"for",strPhLabel))
  summ<-summary(lm(CustData[,Phenotype]~CustData[,traitEBV]))
  if(summ$coefficients[2,4]<0.1){
    abline(lm(CustData[,Phenotype]~CustData[,traitEBV]),col="#003976",lty=2,lwd=3)
  }
  
  #mtext(sqrt(summ$r.squared))
  summ$coefficients[1,1]
  summ$coefficients[2,1]
  #mtext("test upper left",side=3,outer=T,adj=0,line=-1)
  text(par("usr")[1], par("usr")[4]-strheight("SOME\nBIGTEXt"),paste(" r squared =",signif(summ$r.squared,3),"\n",strPhLabel,"=",traitEBV,"PTA x",signif(summ$coefficients[2,1],3),"+",signif(summ$coefficients[1,1],3)), pos = 4)
  legend(
    "topright",
    c(paste("Lact",1:3),"4+","PA","GT"), 
    pch=c(rep(15,4),16,17),
    col=c(adjustcolor(c=Cols,0.4),adjustcolor("black",0.4),"black"))
  
  
}






phenotypecorr<-function(traitEBV,Phenotype,CustData){
  
  if(Phenotype=="DOPN"){
    strPhLabel<-"DOPN"
  }else if(Phenotype=="KG305ME"){
    strPhLabel<-"305 ME"
  }else if(Phenotype=="KGFat305ME"){
    strPhLabel<-"305 ME Fat Kg"
  }else if(Phenotype=="KGProt305ME"){
    strPhLabel<-"305 ME Protein Kg"
  }else if(Phenotype=="KGProt305ME"){
    strPhLabel<-"305 ME Protein Kg"
  }else if(Phenotype=="TestFatPc"){
    strPhLabel<-"Test Fat %"
  }else if(Phenotype=="TestProtPc"){
    strPhLabel<-"Test Protein %"
  }
  
  traitlabel<-columnspec[columnspec$Input%in%traitEBV,"Output"]
  CustData[,traitEBV]<- CustData[,traitEBV]*2
  
  
  Cols<-colorRampPalette(c(rgb(206,17,65,maxColorValue =255),rgb(169,168,169,maxColorValue =255),rgb(29,79,145,maxColorValue =255)),bias =2)(length(unique((CustData$LactClass[!CustData$LactClass%in%c("Calf","Heifer")]))))
  CustData$Lactcolor<-ifelse(CustData$CurrentLact%in%0,NA,ifelse(CustData$CurrentLact>4,4,CustData$CurrentLact))
  plot(CustData[,traitEBV],CustData[,Phenotype],col=Cols[as.numeric(CustData$Lactcolor)],bg=ifelse(CustData$GenomicIndicator=="G",Cols,adjustcolor(c=Cols,0.1)),pch=ifelse(CustData$GenomicIndicator=="G",16,1),cex=ifelse(CustData$GenomicIndicator=="G",1,1),ylab=strPhLabel,xlab=paste(traitlabel,"(g)EBV"),main=paste("Genetic Correlation to" ,traitlabel ,"for",strPhLabel))
  summ<-summary(lm(CustData[,Phenotype]~CustData[,traitEBV]))
  if(summ$coefficients[2,4]<0.1){
    abline(lm(CustData[,Phenotype]~CustData[,traitEBV]),col="#003976",lty=2,lwd=3)
  }
  
  #mtext(sqrt(summ$r.squared))
  summ$coefficients[1,1]
  summ$coefficients[2,1]
  rsq<-signif(summ$r.squared,3)
  M<-signif(summ$coefficients[2,1],3)
  C<-signif(summ$coefficients[1,1],3)
  #mtext("test upper left",side=3,outer=T,adj=0,line=-1)
  #text(par("usr")[1], par("usr")[4]-strheight("SOME\nBIGTEXt"),paste(" r squared =",signif(summ$r.squared,3),"\n",strPhLabel,"=",traitEBV,"PTA x",signif(summ$coefficients[2,1],3),"+",signif(summ$coefficients[1,1],3)), pos = 4)
  text(par("usr")[1], par("usr")[4]-(1.5*strheight("SOME\nBIGTEXt")),
       bquote({R^2}==.(rsq)),pos=4)
  text(par("usr")[1], par("usr")[4]-(0.5*strheight("SOME\nBIGTEXt")),
       bquote(.(strPhLabel)==.(traitlabel)%*%.(M)+.(C))
       ,pos=4)
  
  #paste(strPhLabel,"=",traitEBV,"PTA x",,"+",)
  legend(
    "topright",
    c(paste("Lact",1:3),"4+","PA","GT"), 
    pch=c(rep(15,4),1,16),
    col=c(adjustcolor(c=Cols,1),adjustcolor("black",1),"black"))
  
  
}




producePhenotypes<-function(CustData,FolderName,AuditFolder,Breed){
  
  if (Breed%in%"JE"){
    EBVS<-c('NM',"NM",'CM',"CM",'FM',"FM","GM","GM","JPI","JPI",'MILK','FAT','FATPCT','PRO','PROPCT',"DPR","CCR")
    phenotype<-c(rep(c("KG305ME","DOPN"),5),"KG305ME","KGFat305ME","TestFatPc","KGProt305ME","TestProtPc","DOPN","DOPN")
  }else{
    EBVS<-c('NM',"NM",'CM',"CM",'FM',"FM","GM","GM","TPI","TPI",'MILK','FAT','FATPCT','PRO','PROPCT',"DPR","CCR")
    phenotype<-c(rep(c("KG305ME","DOPN"),5),"KG305ME","KGFat305ME","TestFatPc","KGProt305ME","TestProtPc","DOPN","DOPN")
  }
  plots<-data.frame(cbind(EBVS,phenotype))
  
  
  for(i in 1:nrow(plots)){
    
    traits<-plots[i,]
    trait1<-traits$EBVS
    pheno1<-traits$phenotype
    cat(paste("Producing",trait1,"Vs",pheno1,"\n"))
    
    if(nrow(CustData[!is.na(CustData[,pheno1]),])<10){
      cat(paste("Not enough data for phenotpye",trait1,"Vs",pheno1,": Skipped\n"))
      next
      
    }
    
    png(paste(AuditFolder, trait1,"Vs",pheno1,"Corr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    phenotypecorr(trait1,pheno1,CustData)
    dev.off()
    
    
  }
  
  
}

producePhenotypes2<-function(CustData,FolderName,AuditFolder,EBVS,phenotype){
  
  
  #EBVS<-c('NM',"NM",'CM',"CM",'FM',"FM","TPI","TPI",'MILK','FAT','FATPCT','PRO','PROPCT',"DPR","CCR")
  #phenotype<-c(rep(c("KG305ME","DOPN"),4),"KG305ME","KGFat305ME","TestFatPc","KGProt305ME","TestProtPc","DOPN","DOPN")
  
  plots<-data.frame(cbind(EBVS,phenotype))
  
  
  for(i in 1:nrow(plots)){
    
    traits<-plots[i,]
    trait1<-traits$EBVS
    pheno1<-traits$phenotype
    cat(paste("Producing",trait1,"Vs",pheno1,"\n"))
    
    if(nrow(CustData[!is.na(CustData[,pheno1]),])<10){
      cat(paste("Not enough data for phenotpye",trait1,"Vs",pheno1,": Skipped\n"))
      next
    }
    
    if(nrow(CustData[!is.na(CustData[,trait1])&!CustData$CurrentLact%in%0 ,])<10){
      cat(paste("Not enough data for genetic",trait1,"Vs",pheno1,": Skipped\n"))
      next
    }
    
    png(paste(AuditFolder, trait1,"Vs",pheno1,"Corr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    phenotypecorr(trait1,pheno1,CustData)
    dev.off()
    
  }
  
}



SirePhenotypes<-function(CustData,FolderName,AuditFolder){
  
  
  EBVS<-c('NM',"NM",'CM',"CM",'FM',"FM","TPI","TPI",'MILK','FAT','FATPCT','PRO','PROPCT',"FI")
  phenotype<-c(rep(c("KG305ME","DOPN"),4),"KG305ME","KGFat305ME","TestFatPc","KGProt305ME","TestProtPc","DOPN")
  
  plots<-data.frame(cbind(EBVS,phenotype))
  
  
  for(i in 1:nrow(plots)){
    
    traits<-plots[i,]
    trait1<-traits$EBVS
    pheno1<-traits$phenotype
    siretrait<-paste0("Sire",trait1)
    cat(paste("Producing",trait1,"Vs",pheno1,"\n"))
    
    if(nrow(CustData[!is.na(CustData[,pheno1]),])<10){
      cat(paste("Not enough data for phenotpye",trait1,"Vs",pheno1,": Skipped\n"))
      next
      
    }
    
    png(paste(AuditFolder, "Sire",trait1,"Vs",pheno1,"Corr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    
    # layout(matrix(c(0,1,1,2,2,1,2,2,0),ncol=3,nrow=3,byrow=T),widths = c(2/3,1/3,1/3),heights = c(1/3,1/3,2/3))
    #par(oma=c(0,0,1,0),mar=c(5,4,2,0.5))#plt=c( 0.1,2/3, 0.1, 2/3)
    
    phenotypecorr(siretrait,pheno1)
    # phenotypecorr(trait1,pheno1)
    # rect(par("usr")[1], par("usr")[3],
    #      par("usr")[2], par("usr")[4],
    #      col = "white") # Color
    # 
    # # Add a new plot
    # par(new = TRUE)
    # phenotypecorr(trait1,pheno1)
    dev.off()
    
    
  }
  
  
}




produceYRquarter<-function(CustData,FolderName,AuditFolder,Breed){
  
  png(paste(AuditFolder, "NMyrqtr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(NM~as.yearqtr(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2013,],col=MyRed,main="Genetic Improvement in NM by Year Quarter (2013+)",xlab="Year Quarter")
  dev.off()
  
  png(paste(AuditFolder, "CMyrqtr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(CM~as.yearqtr(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2013,],col=MyRed,main="Genetic Improvement in CM by Year Quarter (2013+)",xlab="Year Quarter")
  dev.off()
  
  png(paste(AuditFolder, "FMyrqtr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(FM~as.yearqtr(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2013,],col=MyRed,main="Genetic Improvement in FM by Year Quarter (2013+)",xlab="Year Quarter")
  dev.off()
  
  png(paste(AuditFolder, "GMyrqtr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(GM~as.yearqtr(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2013,],col=MyRed,main="Genetic Improvement in FM by Year Quarter (2013+)",xlab="Year Quarter")
  dev.off()
  
  if (Breed%in%"JE"){
    png(paste(AuditFolder, "JPIyrqtr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    boxplot(JPI~as.yearqtr(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2013,],col=MyRed,main="Genetic Improvement in JPI by Year Quarter (2013+)",xlab="Year Quarter")
    dev.off()
  }else{
    png(paste(AuditFolder, "TPIyrqtr",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    boxplot(TPI~as.yearqtr(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2013,],col=MyRed,main="Genetic Improvement in TPI by Year Quarter (2013+)",xlab="Year Quarter")
    dev.off()
  }
  
  
  
}


produceYRMon <- function(CustData,FolderName,AuditFolder,Breed){
  
  png(paste(AuditFolder, "NMyrmonth",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(NM~as.yearmon(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2015,],col=MyRed,main="Genetic Improvement in NM by Year Month (2016+)",xlab="Year and Month")  
  dev.off()
  
  png(paste(AuditFolder, "FMyrmonth",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(FM~as.yearmon(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2015,],col=MyRed,main="Genetic Improvement in FM by Year Month (2016+)",xlab="Year and Month") 
  dev.off()
  
  png(paste(AuditFolder, "CMyrmonth",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(CM~as.yearmon(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2015,],col=MyRed,main="Genetic Improvement in CM by Year Month (2016+)",xlab="Year and Month")
  dev.off()
  
  png(paste(AuditFolder, "GMyrmonth",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
  boxplot(GM~as.yearmon(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2015,],col=MyRed,main="Genetic Improvement in CM by Year Month (2016+)",xlab="Year and Month")
  dev.off()
  
  if (Breed%in%"JE"){
    png(paste(AuditFolder, "JPIyrmonth",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    boxplot(JPI~as.yearmon(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2015,],col=MyRed,main="Genetic Improvement in JPI by Year Month (2016+)",xlab="Year and Month")
    dev.off() 
  }else{
    png(paste(AuditFolder, "TPIyrmonth",format(Sys.time(), "%d-%m-%Y-%H-%M",tz = "GMT"),".png",sep=""),res=1000,width=22.15*1,height=10.56*1.5,units = "cm" )
    boxplot(TPI~as.yearmon(as.Date(DatDOB,"%d/%m/%Y"),"%Y"),CustData[as.numeric(format(as.Date(CustData$DatDOB,"%d/%m/%Y"),"%Y"))>2015,],col=MyRed,main="Genetic Improvement in TPI by Year Month (2016+)",xlab="Year and Month")
    dev.off()
  }
  
  
}
# head(CustData)
# 
# 
# aggnm<-aggregate(NM~FactLactClass,data=CustData,FUN=mean)
# 
# ggplot(CustData, aes(x = FactLactClass, y = NM+100)) + 
#   stat_boxplot(geom = "errorbar",
#       width = 0.15,color=adjustcolor("slateblue",alpha=0.1))+
#   geom_boxplot(fill="slateblue", alpha=0.2,color=adjustcolor("slateblue",alpha=0.1),outlier.shape = NA)+
#   geom_beeswarm(data=CustData,aes(x = FactLactClass, y = NM),cex = 1,shape=ifelse(CustData$GenomicIndicator%in%"G",21,1),color="red",fill="blue",alpha=0.5,position = "dodge")+theme_classic()
# 
# 
# 
# 
# CustData$pch
