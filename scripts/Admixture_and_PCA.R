plot.admixture<-function(directory){
  
  require(reshape2)
  require(grid)
  require(ggplot2)
  require(plyr)
  require(RColorBrewer)
  library(tidyverse)
  
  
  Qmatrix <- dir(directory,pattern="*.Q")
  fam<-dir(directory, pattern="*.fam")
  temp.name<-read.csv(paste(directory, fam, sep=""), sep=" ", header=FALSE)
  
  imax<-function(x,n){
    tmp<-as.data.frame(cbind(x, 1:length(x)))
    colnames(tmp)<-c("val","index")
    stmp<-tmp[order(tmp$val, decreasing=TRUE),]
    return(stmp$index[n])
  }
  vmax<-function(x,n){
    tmpd<-sort(x, decreasing=TRUE)
    return(tmpd[n])
  }
  
  parse.Q.files<-function(x){
    dat<-read.csv(file=paste(directory,x, sep=""), sep=" ",header=FALSE)
    ldat<-length(dat)
    
    outter <- NULL	
    
    for(i in 1:ldat){	
      outter<-c(outter,list(apply(dat, 1, FUN=imax, n=i)))
      outter<-c(outter,list(apply(dat, 1, FUN=vmax, n=i)))
    }
    
    dat<-cbind(temp.name$V2,dat)
    colnames(dat)<-c( "names", 1:ldat)
    
    dat<-dat[do.call(order, outter),]
    dat$ov<-1:length(dat$names)
    
    dat2<-melt(dat, id.vars=c("names", "ov"))	
    
    
    dat2$Krun<-ldat
    colnames(dat2)<-c("Name","ov","Admixture.Group","Value", "Krun")
    
    
    return(dat2)
  }	
  
  
  
  plotgg<-function(datframe){
    
    my.max.k<-max(as.numeric(as.character(datframe$Admixture.Group)))
    
    
    datframe<-datframe[order(datframe$ov),]
    datframe$Name<-factor(datframe$Name, levels = unique(datframe$Name), ordered=TRUE)
    
    my.col<-colors()[c(26,547,498,69,33,51,536,100,76,200,300,400,450)]
    
    the.plot<-ggplot(datframe, aes(x=Name, y=Value, fill=Admixture.Group))+geom_bar(stat="identity")
    the.plot<-the.plot+scale_fill_manual(values = my.col[1:my.max.k], name="Admixture group")
    the.plot<-the.plot+theme_classic(18)
    the.plot<-the.plot+theme(axis.text.x = element_text(angle = 90, hjust = 1))
    the.plot<-the.plot+labs(x="Individual", y="Fraction Ancestry")
    the.plot
    
  }
  
  fdat<-ldply(Qmatrix, parse.Q.files, .inform=TRUE)
  
  my.plots<-dlply(fdat, .(Krun), plotgg)
  
  return(my.plots)
  
}

plot.all.together<-function(file.name, plotCols, list.of.plots){
  
  numPlots = length(list.of.plots)
  plotRows = ceiling(numPlots/plotCols)
  # Fiddle with the to adjust your plot dimentions 
  pdf(file=paste(file.name, "pdf", sep="."),bg="transparent", width=18*plotCols, height=8*plotRows)
  
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols))) 
  vplayout <- function(x, y) 
    viewport(layout.pos.row = x, layout.pos.col = y) 
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(my.plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
  dev.off() 
  
}

#crab analysis
results<- plot.admixture("/path/to/directory/")

k3 <- results$`3`
k4 <- results$`4`
k5 <- results$`5`

k3
k4
k5

#Add metadata

meta <- read.csv("Metadata_analysis2.csv", header= TRUE)

#Attach labels 

eigenVectors$Spec <- meta$Spec

#PCA
eigenValues <-read.table("crabsplinkPCA.eigenval", sep = " ", col.names = F)
eigenVectors <-read.delim("crabsplinkPCA.eigenvec", sep = "", header = F)

#Proportion of variation captured by each vector
eigen_percent <- round((eigenValues / (sum(eigenValues))*100), 2)

#PCA Plot
ggplot(data = eigenVectors) +
  geom_point(mapping = aes(x = V3, y = V4, col = Spec), size = 3, show.legend = TRUE ) +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") + theme_classic() +
  labs(title = "",
       x = paste0("Principal component 1 (",eigen_percent[1,1]," %)"),
       y = paste0("Principal component 2 (",eigen_percent[2,1]," %)"))


#end of analysis

