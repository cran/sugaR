
#
# Author and copyright holder 2009:
#
#    Steffen Moeller <steffen_moeller@gmx.de>
#
#
# License:
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This package is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


library(grDevices)
library(graphics)
library(stats)

##################### HELPER FUNCTION - please skip this section #######################

# helper function to convert a HH:MM time string repesentation
# to HH+MM/60 double. It is used by the other helping function 'h'
# and the specification of activities.
u<-function(X) {
    v<-as.numeric(unlist(strsplit(X,":")))
    if (any(is.na(v))) stop(paste("Could not convert entry '",X,"'.\n",sep=""))
    if (v[2]>59) stop(paste("Too many seconds in '",X,"'.\n",sep=""))
    if (v[1]>23) stop(paste("Too many hours in '",X,"'.\n",sep=""))
    r<-c(v[1]+v[2]/60)
}

# helper function to convert A:B C into c(as.numeric(A)+as.numeric(B)/100,as.integer(C))
#    you can change it if you know what you are doing to help easing the entry of data
#    that is performed in the next section

h<-function(X,
	ncol=2,
	debug=FALSE) {
  if (is.array(X)) {
    # presuming the entries to be converted to be passed directly
    entries<-X
  }
  else {
    # presuming entries to be passed as comma-separated string,
    # possibly broken up into multiple lines
    entries <- unlist(strsplit(X,",[ \n]*"))
  }
  if (debug) {
    cat("Entries: ")
    print(entries)
  }
  entries.bival<-lapply(entries,function(X){
    # removing blanks at beginning, as in " 12:45 0.5"
    X.chomped<-paste(unlist(strsplit(X,"^[ \n]+")),collapse="")
    # continuing on syntax-corrected entries
    unlist(strsplit(X.chomped,"[ \n]+"))
  })
  
  if (FALSE & debug) {
    cat("Entries bival: ")
    print(entries.bival)
  }
  entries.converted<-lapply(entries.bival,function(X){
    if (length(X)<ncol) {
      if (debug) cat(paste("Extending '",paste(X,collapse=" "),"' with zeros.\n",sep=""))
      X<-c(X,rep(0,ncol-length(X)))
    }
    if (ncol<length(X)) {
      stop(paste("There are too many fields at '",paste(X,collapse=" "),". Expected are just ",ncol,"\n",sep=""))
    }
    r<-u(X[1])
    return(c(r,as.numeric(X[2:length(X)])))
  })
  
  if (FALSE & debug) {
    cat("Entries converted:\n")
    print(entries.converted)
  }
  
  if (FALSE & debug) {
    cat("Entries converted unlisted:\n")
    print(unlist(entries.converted))
  }
  
  ret<-matrix(unlist(entries.converted),ncol=ncol,byrow=TRUE)
  
  if (debug) {
    cat("Entries converted unlisted as matrix:\n")
    print(ret)
  }
  
  return(ret)
}


if (FALSE) {
  data(diabetesDiary)
  cat("Glucose concentrations:\n")
  print(myGlucose)
  cat("Influx of glucose and insulin:\n")
  print(myIntake)
  cat("Basal rate:\n")
  print(myBasal)
  cat("Factors:\n")
  print(myFactors)
}

############# Display parameters - should barely need any changes ##############

legend.cex=0.5  # ratio of fontsize in legend


## localisation # feel free to improve / add another language

labels.for.diagrams<-list(
  "kids" = list(
    english = list(
      "glucose" = "SUGAR",
      "hour"    = "TIME",
      "basal"   = "BASAL RATE"
    ),
    german = list(
      "glucose" = "ZUCKER",
      "hour"    = "UHR",
      "basal"   = "BASALRATE"
    ) 
  ),
  "adults" = list(
    english = list(
      "glucose" = "glucose concentration",
      "hour"    = "hour",
      "basal"   = "basal rate"
    ),
    german = list(
      "glucose" = "Blutzucker",
      "hour"    = "Tageszeit",
      "basal"   = "Basalrate"
    )
  )
)


## a series of somewhat funny images - not used

images <- list(
	"http://images.bcdb.com/gallery/d/2458-2/little_roq.jpg",
	"http://images.bcdb.com/gallery/d/2467-2/mighty_mouse.jpg",
	"http://images.bcdb.com/gallery/d/2500-2/seaside.jpg",
	"http://images.bcdb.com/gallery/d/2516-2/yankee.jpg",
	"http://images.bcdb.com/gallery/d/2512-2/woodman.jpg",
	"http://images.bcdb.com/gallery/d/1797-2/dd_lion_around.jpg",
	"http://images.bcdb.com/gallery/d/1825-2/for_birds.jpg",
	"http://images.bcdb.com/gallery/d/1923-2/susie.jpg",
	"http://images.bcdb.com/gallery/d/109-2/Alice_in_Wonderland_2.jpg",
	"http://images.bcdb.com/gallery/d/134-2/Beegle_Beagle.jpg"
)


################ ONLY FUNCTIONAL CHANGES BEYOND THIS POINT #####################

# checking if the duration for basal values sums up to 24 hours

diabetes.checkbasal<-function(data.basal, debug=FALSE) {
  if (debug) {
    cat("****** checkbasal *** data.basal ** start **\n")
    print(data.basal)
    cat("****** checkbasal *** data.basal ** end **\n")
  }
  
  if (is.null(data.basal)) {
    invisible(TRUE);
  }
  else {
    ret<-TRUE
    for(i in 1:length(data.basal)) {
      v<-sum(data.basal[[i]][,1]) 
      if (24 != v) {
        cat(paste("Values for basal profile '",names(data.basal)[i],"' sum up to ",v," != 24.\n",sep=""))
        ret=FALSE;
      }
    }
    invisible(ret)
  }
}

# plots representation of numerical sugar and insulin values
diabetes.property<-function(symbol,time,sugar,sugar.reference,insulin,insulin.reference,cols,index,y=0,represent.carbohydrates.by.area=FALSE) {
  #if (FALSE & debug) cat("diabetes.property: time:",round(time,2)," sugar:",sugar," insulin:(",paste(insulin,collapse=","),") y:",y,"\n")
  #if (TRUE) cat("diabetes.property: time: (",paste(round(time,2),collapse=","),") sugar: (",paste(sugar,collapse=","),") insulin:(",paste(insulin,collapse=","),") y: (",paste(y,collapse=","),")\n")
  if (is.na(y)||0==y) {
       y=(index-1)*15
  }
  if ("circle"==symbol) {
      # insulin levels not shown
      if (sugar>0)
	if (represent.carbohydrates.by.area) {
	  sugar<-sqrt(sugar)
	  sugar.reference=sqrt(sugar.reference)
	}
	symbols(x=time,y=y,
	  circles=1,
	  fg="gray30", bg=cols[index],
	  inches=0.2*sugar/sugar.reference,
	  add=TRUE)
  }
  else if ("thermometer"==symbol) {
      # insulin levels shown as thermometer value
      if (sugar>0) {
        factor.to.be.applied<-0.6
	symbols(x=time, y=y, thermometers=cbind(.2, 1,
			#insulin[1]/insulin.reference),
			0.5*insulin[1]/sugar/factor.to.be.applied),
		inches=.8*sugar/sugar.reference,
		fg="gray30", bg=cols[index], add=TRUE)
      }
      else {
	# first idea to circumvent problem of insulin-correction without food uptake
	symbols(x=time, y=y, thermometers=cbind(.2, 1, sum(insulin)/insulin.reference),
		inches=.8*sugar/sugar.reference,
		fg="gray30", bg=cols[index], add=TRUE)
      }
  }
  else {
    stop(paste("Unimplemented symbol: '",symbol,"'\n",sep=""))
  }
}

# function to co-plot glucose and intake data
plot.glucose<-function(	data.glucose,
			data.intake=NULL,
			data.activities=NULL,
			data.factors=NULL,
			col=rainbow,legend=TRUE,
			threshold.low=80, threshold.high=140,
			symbol="thermometer",
			intake.at.bottom=FALSE,
			represent.carbohydrates.by.area=TRUE,
			labels.language="english",labels.type="kids",debug=FALSE
) {

  cols=col(length(data.glucose))

  glucose.max<-max(c(threshold.high,sapply(data.glucose,function(X){return(max(X[,2]))})))
  glucose.min<-min(c(threshold.low,sapply(data.glucose,function(X){return(min(X[,2]))})))

  #insulin.single.max<-max(sapply(data.intake,function(X){return(max(X[,4]))}))
  insulin.single.max<-1.1

  intake.min<-intake.max<-NA
  if (!is.null(data.intake)) {
    intake.max<-max(sapply(data.intake,function(X){return(max(X[,2]))}))
    intake.min<-min(sapply(data.intake,function(X){return(min(X[,2]))}))
    if(debug) {
      cat(paste("Glucose max: ",glucose.max,"\n",sep=""))
      cat(paste("Intake  max: ",intake.max,"\n",sep=""))
    }
  }
  
  # internal function to determine glucose level for a given time and day
  glucose.level.at.on <- function(time,day) {
    y=approx(data.glucose[[day]][,c(1,2)],xout=time)[["y"]]
    return(y)
  }
  
  b<-NULL
  if (intake.at.bottom) {
    b<-matrix(c(0,0,24,glucose.max),2,byrow=TRUE)
  }
  else {
    b<-matrix(c(0,glucose.min,24,glucose.max),2,byrow=TRUE)
  }
  plot(b, type="l", lty=0, xlab=labels.for.diagrams[[labels.type]][[labels.language]][["hour"]],
                           ylab=labels.for.diagrams[[labels.type]][[labels.language]][["glucose"]],
	  #mai=c(0.8,0.8,0,0), # mai borders in inches c(bottom, left, top, right) 
	  lab=c(24,3,2)
  )
  
  # Presenting the food/insulin intake data
  if (!is.null(data.intake)) {

    # prepare also for intake->insulin factor
    f.names<-names(data.factors)
    if (debug) print(f.names)

    for(i in 1:length(data.glucose)) {
      dayId<-names(data.glucose)[i]
      intakes<-data.intake[[dayId]]

      f.sel<-NULL
      if (!is.null(f.names)) {
        f.sel.no<-max(which(f.names<=dayId))	# find latest factor assignment
        if (debug) cat(paste("Factor names: ",paste(f.names,collapse=",")," sel:",f.sel.no," name: ",f.names[[f.sel.no]],"\n"),sep="")
        f.sel<-data.factors[[f.sel.no]]
      }

      if (is.null(intakes)) {
        cat(paste("No intake data found for day '",dayId,"'.\n",sep=""))
      }
      else {
        # drawing these first, so the lines are not interrupted
        apply(intakes,1,function(X){
          if (FALSE & debug) { cat("Intake: "); print(X) }
	  y=glucose.level.at.on(X[1],i)
	  #y=approx(data.glucose[[i]][,c(1,2)],xout=X[1])[["y"]]
	  if (FALSE & debug) cat(paste("Height: ",y,"\n",sep=""))

	  insulin.reference<-insulin.single.max
	  if(!is.null(f.sel)) {
		f.times.sel<-max(which(f.sel[,1]<=X[1]))
		if (debug) cat(paste("  factor time: ",f.sel[f.times.sel,1], " glucose time:", X[1]," factor: ",f.sel[f.times.sel,2],"\n"),sep="")
		insulin.reference<-f.sel[f.times.sel,2]*X[2]
	  }
	
	  diabetes.property(symbol=symbol,
		    time=X[1],
		    sugar=X[2],sugar.reference=intake.max,
		    insulin=X[c(3,4)], insulin.reference=insulin.single.max,
		    cols=cols,index=i,
		    represent.carbohydrates.by.area=represent.carbohydrates.by.area,
		    y=y)
        })
      }
    }
  }
  
  # Presentation of activity data
  if (!is.null(data.activities))
   for(i in 1:length(data.glucose)) {
    dayId<-names(data.glucose)[i]
    activities<-data.activities[[dayId]]
    if(is.null(activities)) {
      if (debug) {
        cat(paste("No activity data found for day '",dayId,"'.\n",sep=""))
      }
    }
    else {
      lapply(activities,function(A){
	l<-c(A[["f"]],glucose.level.at.on(A[["f"]],i))
	times<-data.glucose[[i]][,1]
	times.to.be.added<-(times>A[["f"]])&(times<A[["t"]])
	l<-rbind(l,data.glucose[[i]][times.to.be.added,])
	l<-rbind(l,c(A[["t"]],glucose.level.at.on(A[["t"]],i)))
	if (FALSE & debug) {cat("Points to connect for activity:\n"); print(l)}
	lines(l,lwd=(1*A[["a"]]),col=cols[i])
      })
    }
  }
    
  # Drawing lines connecting glucose values
  for(i in 1:length(data.glucose)) {  
    if (1 < i) {
      # a connection to the previous value should be drawn
      v<-data.glucose[[i-1]][nrow(data.glucose[[i-1]]),]
      lines(rbind(c(v[1]-24,v[2]),
                  data.glucose[[i]][1,]),
	    col=cols[i-1], lty="dashed")
    }
    lines(data.glucose[[i]],col=cols[i])
    if (i<length(data.glucose)) {
      # a connection to the next value should be drawn
      v<-data.glucose[[i+1]][1,]
      lines(rbind(data.glucose[[i]][nrow(data.glucose[[i]]),],
                  c(v[1]+24,v[2])),
	    col=cols[i+1], lty="dashed")
    }
  }

  # Indication of sufficiently good values
  abline(threshold.low, 0,col="gray60")
  abline(threshold.high,0,col="gray60")
  if(legend){
    legend(24,glucose.max,legend=names(data.glucose),col=cols,fill=cols, bty="n",xjust=1,yjust=1,cex=legend.cex)
  }
}

plot.basalrate<-function(data.basal,col=rainbow,legend=TRUE,labels.language="english",labels.type="kids",debug=FALSE
) {
  
  d<-data.basal
  
  l<-length(d)
  cols<-col(l)
  basal.max<-max(sapply(d,function(X){return(max(X[,2]))}))
  b<-matrix(c(0,0,24,basal.max),2,byrow=TRUE)
  plot(b, type="l", lty=0, xlab=labels.for.diagrams[[labels.type]][[labels.language]][["hour"]],
                           ylab=labels.for.diagrams[[labels.type]][[labels.language]][["basal"]],
   	  lab=c(24,3,2)
  )
  
  # plotting box for every line
  
  for(i in 1:l) {
    
    prev.t<-0
    
    if (all(d[[i]][1,1]==d[[i]][,1])) {
      w<-d[[i]][1,1]
      if (debug)
        cat(paste("Found regular intervals (",w,") for basal rate '",names(d)[i],"'.\n",sep=""))
      
      # hourly reset basal rate, shown as 'histogram'
      for(r in 1:nrow(d[[i]])) {
	delta<-d[[i]][r,1]
	shift.x<-(i-1)*w/l
	rect(prev.t+shift.x,0,prev.t+shift.x+w/l,d[[i]][r,2],col=cols[i],border=cols[i])
	prev.t<-prev.t+delta
      }
    }
    else {
      cat(paste("Found irregular intervals for basal rate '",names(d)[i],"'.\n",sep=""))
       # values are shifted by part of an hour to remain somewhat legible
      shift.x <- (i-1)*0.25/l
      shift.y <- 0
      #density<-(length(d)-i+1)*50/(length(d)+1)
      density=0
      for(r in 1:nrow(d[[i]])) {
	 w<-d[[i]][r,1]
	next.t<-prev.t+w
	rect(prev.t+shift.x,0+shift.y,next.t+shift.x,d[[i]][r,2]+shift.y,density=density,col=cols[i],border=cols[i])
	prev.t<-next.t
      }
    }
  }
  if (legend) {
    legend(24,basal.max,legend=names(d),col=cols,fill=cols,bty="n",xjust=1,yjust=1,cex=legend.cex)
  }
}


# checks if names of list elements are ordered
check.ordered.names<-function(l) {
	if (is.null(l)) {
		return(TRUE)
	}
	else {
		return(all(order(names(l))==1:length(l)))
        }
}

# key routine which the user will invoke directly
sugar.over.time <- function(data.glucose,
			  data.basal=NULL,data.intake=NULL,data.activities=NULL,data.factors=NULL,
			  symbol="thermometer", col=rainbow,
                          represent.carbohydrates.by.area=TRUE,
                          threshold.low=80, threshold.high=140,
			  labels.language="english",labels.type="kids",debug=FALSE
) {
			  
  #
  # check of input data
  #
  if(!check.ordered.names(data.glucose)) {
    # for the transition between nights this is wrong, not sorting manually since
    # this data is too critical to allow mistakes
    stop("Glucose measurements are not sorted chronologically in list. Please correct.")
  }

  if(!check.ordered.names(data.intake)) {
    # values are searched for - no semantic problem, but might be an indicator of a typo
    warning("Intake data are not sorted chronologically in list. Please check.")
  }

  if(!check.ordered.names(data.activities)) {
    # values are searched for - no semantic problem, but might be an indicator of a typo
    warning("Intake data are not sorted chronologically in list. Please check.")
  }

  if(!check.ordered.names(data.factors)) {
    # the data could be sorted automatically, but this data is too critical to
    # allow mistakes
    stop("Factors are not sorted chronologically in list. Please correct.")
  }

  if (!diabetes.checkbasal(data.basal)) {
    stop("Values for basal rate are invalid.\n")
  }

  #
  # prepare for plotting time-dependent data over basal rate
  #
      
  op<-par(pty="m",        # maximal plotting region
      ask=FALSE,
      mai=c(0.5,0.8,0,0), # mai borders in inches c(bottom, left, top, right) 
      fig=c(4, 9, 1, 4)/10
  )
 
  if (is.null(data.basal)) {
    nf <- layout(matrix(c(1),1,1,byrow=TRUE))
  }
  else {
    nf <- layout(matrix(c(1,2),2,1,byrow=TRUE), heights=c(2.7,1))
  }

  #
  # do the plots
  #

  plot.glucose(data.glucose=data.glucose,
               data.intake=data.intake,
	       data.activities=data.activities,
	       data.factors=data.factors,
	       represent.carbohydrates.by.area=represent.carbohydrates.by.area,
               threshold.low=threshold.low, threshold.high=threshold.high,
	       labels.language=labels.language,labels.type=labels.type,
               symbol=symbol,debug=debug,
	       col=col)
  
  if (!is.null(data.basal)) {
    plot.basalrate(data.basal,col,debug=debug,labels.language=labels.language,labels.type=labels.type)
  }
  
  # restore arrangement of plots to original value
  par(op)
}

if (FALSE) 
    sugar.over.time(data.glucose=myGlucose, data.basal=myBasal, data.intake=myIntake, data.activities=myActivities, data.factors=myFactors, symbol="thermometer", labels.language="german",labels.type="kids",debug=FALSE)			 
