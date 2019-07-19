#'Plot graphs to do Exploratory Data Analysis
#'
#'Provide Dataframe along with list of column number in the form of vector and also provide directory if you want to save plots in specific directory
#'(x,vect=c(1,2,3,..), dir="../EDAplots")
#'@param x= dataframe
#'@param vect= list of column number to be pass(if not provided, whole dataset will be taken into consideration)
#'@param dir= "provide valid path to save plots" (if not provided, plots will be saved in working directory)
#'@return plots will be directly saved in the given directory
#'@export


##Function to be called
eda<-function(g,vect=NULL,dir=NULL)
{


  scatterPlot<-function(data,dir=NULL)
  {
    if (!is.null(vect))
    {
      temp=data[,1]
      name=c()
      for (i in vect)
      {
        temp=data.frame(temp,data[,i])
        name[i]=names(data)[i]
      }
      temp=temp[,-1]
      name<-name[!is.na(name)]
      colnames(temp)<-name
      data=temp
    }
    nums <- unlist(lapply(data, is.numeric))
    temp=data[ , nums]


    #par(mfrow=c(3,3))
    for (i in 1:ncol(temp)-1)
    {

      if (length(unique(temp[,i]))>15)
      {
        for (j in (i+1):ncol(temp))
        {
          if(length(unique(temp[,j]))>15)
          {
            png(paste(dir,"Scatter Plot for ",names(temp)[i]," vs ",names(temp)[j],".png",sep=""))
            scatter.smooth(temp[,i],temp[,j],xlab = names(temp)[i],ylab = names(temp)[j])
            dev.off()
          }
        }
      }
    }
  }

  gplot<-function(fram,coln,dir=NULL)
  {
    a<-length(unique(fram))
    if (is.factor(fram))

    {
      if (a<15)
      {
        png(paste(dir,"Barplot and Pie chart of ",coln,".png",sep=""))
        par(mfrow=c(1,2))
        barplot(table(fram),main=paste("Barplot of", coln),col="maroon")
        b<-data.frame(table(fram))
        pie(table(fram),labels = b[,2], main = paste("Piechart of", coln),col = rainbow(length(unique(fram))))
        legend("topright", legend=as.character(b[,1]), fill = rainbow(length(unique(fram))))
        dev.off()
      }
    }
    else
    {
      if (a<15)
      {
        png(paste(dir,"Barplot and Pie chart of ",coln,".png",set=""))
        par(mfrow=c(1,2))
        barplot(table(fram),main=paste("Barplot of", coln),col="green")
        b<-data.frame(table(fram))
        pie(table(fram),labels = b[,2], main = paste("Piechart of", coln),col = rainbow(length(unique(fram))))
        legend("topright", legend=as.character(b[,1]), fill = rainbow(length(unique(fram))))
        dev.off()
      }
      else
      {
        png(paste(dir,"Boxplot and Histogram of ",coln,".png",set=""))
        par(mfrow=c(1,2))
        boxplot(fram, main=paste("Boxplot of", coln),col="maroon", border="grey5", horizontal=F)
        hist(fram, main=paste("Histogram of", coln), xlab=names(fram), ylab="Number of houses" ,col="Blue")
        dev.off()
      }
    }
    #}
  }
  withdir<-function(g,vect=NULL,dir)
  {
    if (!is.null(vect) && class(vect)=="numeric")
    {
      for (i in vect)
      {
        gplot(g[,i],col=names(g)[i],dir)
      }
    }
    else
    {
      for (i in 1:ncol(g))
      {
        gplot(g[,i], col=names(g)[i],dir)
      }
    }

  }
  if (!is.null(dir))
  {
    dir=paste(dir,"/",sep="")
    withdir(g,vect,dir)
    scatterPlot(g,dir)
  }
  else
  {
    scatterPlot(g,dir)
    if (!is.null(vect) && class(vect)=="numeric")
    {
      for (i in vect)
      {
        gplot(g[,i],col=names(g)[i])
      }
    }
    else
    {
      for (i in 1:ncol(g))
      {
        gplot(g[,i],col=names(g)[i])
      }
    }
  }
}


