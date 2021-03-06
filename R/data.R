
# This file helps to prepare the data.RData. Once executed it
# will return a list of data resembling the information gathered
# for a single individual. 

prepareRData <- function (s=FALSE) {

  ##################### INPUT DATA - AMEND/EXTEND FOR UPDATES ############################

  # glucose data measured 
  myGlucose<-list(
    "20090426"=h("8:47 139,11:12 75,16:18 112,19:46 71,21:48 75",ncol=2),
    "20090427"=h("6:48 264,10:15 260,12:14 185,12:51 176,15:04 88,18:28 137,20:07 249",ncol=2),
    "20090428"=h("2:09 243,7:10 96,11:11 77,14:56 231,17:52 222,19:53 68,20:30 117",ncol=2),
    "20090429"=h("6:47 268,8:06 286,8:48 223,11:37 186,15:00 336,18:00 127,20:45 111,22:46 119",ncol=2),
    "20090430"=h("3:36 291,7:47 270,9:31 203,12:06 91,14:51 200,17:54 73,20:03 150,22:06 230",ncol=2),
    "20090501"=h("8:00 212,12:12 291,13:49 236,15:42 291,17:42 112,19:15 74,20:35 202,22:39 139",ncol=2),
    "20090502"=h("8:00 169,12:00 242",ncol=2)
    # the function 'h' will transform the data to a format analogous to
    #"20090501"=matrix(c(8,180,9,190,10,200,11,190,12,180),2,byrow=TRUE)
  )

  # carbohydrate uptake, insulin dosage and corrections
  myIntake<-list(
    "20090426"=h("8:47 1.5 1.3,10:00 0.45,11:00 0.45, 11:12 2 0.8,12:17 2.7 2.1,12:24 1.9 1.5,12:25 0.4 0.3,12:27 1.5 1.2,13:09 0.3 0.2,13:30 0.45,16:18 2.5 2.0,17:09 2.5 2.0,17:29 2 1.5,18:22 0.66666,18:28 0.3333333,19:46 0.5,19:56 0.7 0.4,20:00 1.2 0.7,20:10 2 1.2,20:11 0.5 0.3,21:48 0.7",ncol=4),
    "20090427"=h("6:48 1.5 1.3 0.7,10:15 1 0.6 0.4,10:19 0.27 0.2,12:51 1.65 1.3,13:22 0.3 0.2,13:37 2 1.6,13:49 1 0.8,15:04 1 0 0,18:28 2 1.2,18:50 1 0.6,19:38 1.5 0.9,22:07 0 0 0.6",ncol=4),
    "20090428"=h("2:01 0 0 0.6,7:25 2 1.8,8:50 0.6 0.5,11:54 1.5 1.2,12:10 2 1.6,12:25 1 0.8,12:38 2.1 1.7,14:56 1.5 1.2 0.5,17:52 0.8 0.5 0.5,18:15 4 2.4,18:30 1 0.6,19:53 1.8 0.6",ncol=4),
    "20090429"=h("6:47 1.5 1.3 0.7,8:48 3 2.4 0.5,11:37 0.8 0.6 0.3,11:55 0.3 0.2,12:02 0.45 0.4 0,12:17 1.4 1.1 0,15:00 2.5 2.0 1.1,18:10 1.7 1.0,18:25 1.9 1.1,18:20 1.4 0.9,18:40 0.8 0.5,18:45 0.8 0.5,20:45 1 0.6 0",ncol=4),
    "20090430"=h("7:47 1.5 1.3 0.7,9:31 0 0 0.4,12:06 1.7 1.4 0,12:58 0.4 0.3 0,14:51 1.9 1.5 0.4,17:54 1.4 0.4 0,18:00 1.2 0.7,18:10 2 1.2,18:30 2.7 1.7,18:40 0.5 0.3 0,18:50 0.5 0.3 0",ncol=4),
    "20090501"=h("8:00 1.5 1.4 0.4,9:30 2 1.6 0,12:12 2.2 1.7 0.9,13:49 1.5 1.2 0.6,14:03 0.3 0.3 0,15:42 1.3 1.0 0.5,17:42 3.4 2.0 0,17:55 1.3 0.8 0,18:08 0.4 0.2 0,19:15 0.3 0 0,20:35 1.4 0.9 0.4",ncol=4),
    "20090502"=h("8:00 1.5 1.3 0.2,9:30 2.3 1.8,9:55 0.4 0.3,12:00 2.8 2.5 0.6,12:15 1.6 1.3",ncol=4)
  )

  # basal insulin - as set for insulin pumps
  #  for every profile provide a separate entry.  It should contain a (n x 2)-dimensional
  #  matrix of (duration, rate) pairs.
  myBasal<-list(
    "A"=cbind(1,c(.25,.2,.2,.25,.4,.4,.5,.5,.45,.35,.15,.1,.1,.15,.15,.3,.45,.45,.45,.5,.55,.6,.45,.3)),
    "standard"=cbind(1,.1*c(2.5,2,2,2.5,4,4,4,4.5,4.5,3.5,1.5,1,1,1.5,1.5,3,4.5,4.5,4.5,5,6,6.5,5,3.5)),
    "B"=cbind(1,c(.3,.25,.25,.3,.4,.4,.4,.45,.45,.35,.15,.1,.1,.15,.15,.3,.45,.45,.45,.5,.5,.65,.5,.4)),
    #               0  1   2  3   4   5  6  7  8  9  10  11 12 13  14  15 16  17  18  19 20 21 22 23
    "A'"=cbind(1,c(.3,.25,.25,.3,.4,.45,.5,.5,.5,.4,.15,.1,.1,.15,.15,.3,.45,.45,.45,.5,.55,.6,.5,.35))
  )

  # description of physical activity over the day, represented by the thickness of connecting
  # lines. The default is 1 (weak). The max is 5.
  myActivities<-list(
    "20090501"=list(
        list(f=u("15:00"),t=u("16:15"),a=3,c="medium-crazy"),
        list(f=u("18:00"),t=u("19:15"),a=2,c="walking the dog")
    )
  )

  myEvents<-list(
    "20090426"=list(
	list(t=u("11:30"),e="catheter",  plot=TRUE),
	list(t=u("12:30"),e="don't show",plot=FALSE)
     ),
    "20090428"=list(
	list(t=u("12:00"),e="catheter",plot=TRUE)
     ),
    "20090429"=list(
	list(t=u("12:30"),e="catheter",plot=TRUE)
     ),
    "20090430"=list(
	list(t=u("13:00"),e="catheter",plot=TRUE)
     ),
    "20090501"=list(
	list(t=u("13:30"),e="catheter",plot=TRUE)
     )
  )

  # only dates with changes to the regular factors need to be noted
  myFactors<-list(
    "20090101"=matrix(
      c(u("0:00"),0.6,
        u("6:00"),0.8,
        u("11:00"),0.8,
        u("16:00"),0.7,
        u("18:00"),0.6),ncol=2,byrow=TRUE),
    "20100101"=h("0:00 0.7",ncol=2),
    "20110101"=h("0:00 0.8,6:30 0.9,10:30 0.8",ncol=2)
  )

  if (s) {
    save(myGlucose,myIntake,myBasal,myActivities,myEvents,myFactors,file="diabetesDiary.RData")
  }

  invisible(list(myGlucose=myGlucose,
  		myIntake=myIntake,myBasal=myBasal,
		myActivities=myActivities,myEvents=myEvents,myFactors=myFactors))

}
