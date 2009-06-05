\name{sugar.over.time}
\Rdversion{1.1}
\alias{sugar.over.time}
\title{Plot with curves and glyphs to help diabetics}
\description{
This function prepares a single plot that presents all information gathered from up to
a week on a single page on blood glucose levels (lines connecting consecutive measurements)
carbohydrate uptake (spots with area or radius indicating amount), insulin dosage (bar in thermometers),
physical activity (width of lines) and the basal rate (optional for insulin pump carriers).
}
\usage{
sugar.over.time(data.glucose,data.basal=NULL,data.intake=NULL,
		data.activities=NULL,data.factors=NULL,data.events=NULL,
		symbol="thermometer",col=rainbow,
                represent.carbohydrates.by.area=TRUE,
                threshold.low=80, threshold.high=140,
		labels.language="english",labels.type="kids",
		debug=FALSE
	)
}
\arguments{
\item{data.glucose}{list of days with (nx2) matrix of (time, glucose level) pairs}
\item{data.basal}{list of days with (nx2) matrix of (duration, units insulin per hour) pairs}
\item{data.intake}{list of days with (nx4) matrix of (time,carbohydrates, units insulin for food, units insulin to correct hyperglucemiar) quadruples}
\item{data.activities}{list of days with list of list with f=start of activity, t=end of activity, a=degree of activity (a number from 1 (weak) to 5 (strong)), c=free text as comment}
\item{data.events}{list of days with list of list with t=time of event, e=the event itself, plot=binary indicating if the event shall be shown in the plot}
\item{data.factors}{list of days at which factors were changed. Every list entry is a (nx2) matrix of (time,factor) pairs.}
\item{symbol}{The symbol to be used for displaying glucose data. The default are thermometers, which allow the co-presentation of insulin dosage. An alternative are circles. Either represents the amount of carbohydrate intake by the size of the symbol.}
\item{col}{function emitting a series of colors to help distinguishing days}
\item{represent.carbohydrates.by.area}{Indicates if the area (default) or the radius should represent the carbohydrates taken in.}
\item{threshold.low}{Glucose concentration no longer considered too low}
\item{threshold.high}{Upper-most tolerable Glucose concentration}
\item{debug}{Informs about internal states.}
\item{labels.language}{Currently supported are 'english' and 'german'.}
\item{labels.type}{Currently supported are 'kids' and 'adults'.}.
}
\details{
There is yet no standardised interface to support the data entry. For
the time speaking, the plan is to provide interfaces to existing tools
external to R that read out glucose meters or insulin pumps for this
matter. However, at least for the activity data \code{sugar.over.time}
seems to be unique. Until a more practical solution is available,
the user is kindly asked to enter the data manually as demonstrated
in the example.

To bring the plot to paper, it is suggested to execute \code{
postscript("filename.ps",format="A4",horizontal=FALSE)
sugar.over.time(...)
dev.off()
} The printing should be performed via \code{\link{postscript}}, not via the now more common \code{pdf}, since \code{postscript} will use the full page when invoked. When not printing the basal rate (or when you have many different profiles) one may prefer adding \code{horizontal=TRUE} as an argument to the postscript function.
}
\seealso{
\code{\link{postscript}},\code{\link{u}},\code{\link{h}},\code{\link{myGlucose}}.
}
\examples{

## retrieving readily prepared input data
data(diabetesDiary)

## invoking the plot
sugar.over.time(data.glucose=myGlucose,
	      data.basal=myBasal,
	      data.intake=myIntake,
	      data.activities=myActivities,
              data.factors=myFactors)
}
\author{Steffen Moeller \email{steffen_moeller@gmx.de}}

