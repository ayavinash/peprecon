# Load required libraries
library(Hmisc)
library(lattice)
library(gridBase)

## Function for scatterplot
scatterplot <- function(mydata,regions,markerslist,pepmz.good,elutime.good,pepmz.bad,elutime.bad,pepmz.greyRegion,elutime.greyRegion,pepmz.unidentified,elutime.unidentified)
{
    elutime.min=0
    elutime.max=max(mydata$elution_start)
    pepmz.min=min(mydata$pep_mz)
    pepmz.max=max(mydata$pep_mz)
    pepmz.range <- c(pepmz.min,pepmz.max)
        elutime.range <- c(elutime.min,elutime.max)
    elutime.ticks <- c(elutime.range[1], signif(elutime.range[2],0), round(elutime.range[2]/10))
        pepmz.range.ticks <- signif(pepmz.range, 2)
        if (pepmz.range.ticks[1] == pepmz.range.ticks[2]) {
            pepmz.ticks <- c(pepmz.range.ticks[1], pepmz.range.ticks[2], 1)
        } else {
            pepmz.ticks <- c(pepmz.range.ticks[1], pepmz.range.ticks[2], round((pepmz.range.ticks[2]-pepmz.range.ticks[1])/50))  }

    eltime.label = "Elution time (min)"
        mz.label = ""
plot(0, 0, xlim=pepmz.range,ylim=elutime.range,xlab="", ylab="", yaxt='n', xaxt='n', type='n')
par(new=T)
                ## Min/Max lines
                elutime.range.min <- elutime.range[1]
                elutime.range.max <- elutime.range[2]
                abline(h=c(elutime.range.min, elutime.range.max), col="lightblue4")
                text(pepmz.min * 1.05, elutime.range.min, paste("0% (min) =", round(elutime.range.min)), col="lightblue4", adj=c(0,0))
                text(pepmz.min * .98, elutime.range.max, paste("100% (max) =", round(elutime.range.max)), col="lightblue4", adj=c(0,0))
                pepmz.range.min <- pepmz.range[1]
                pepmz.range.max <- pepmz.range[2]
                abline(v=c(pepmz.range.min, pepmz.range.max), col="lightblue4")
                text(pepmz.range.min, elutime.min,  paste("0% (min) =", round(pepmz.range.min)), col="lightblue4", adj=c(0,0), srt=90)
                text(pepmz.range.max, elutime.min, paste("100% (max) =", round(pepmz.range.max)), col="lightblue4", adj=c(0,0), srt=90)
    ## Quantile lines
                quantiles <- c(0.25, 0.5, 0.75)
                elutime.quantile <- quantile(c(elutime.good, elutime.bad), quantiles)
                abline(h=elutime.quantile, col="lightblue4")
                text(pepmz.max * .98, elutime.quantile, paste(quantiles * 100, "% = ", round(elutime.quantile), sep=""), col="lightblue4", adj=c(0, 0))
                quantiles <- c(0.25,0.5,0.75)
        pepmz.quantile <- quantile(c(pepmz.good,pepmz.bad), quantiles)
                abline(v=pepmz.quantile, col="lightblue4")
                text(pepmz.quantile, elutime.min, paste(quantiles * 100, "% = ", round(pepmz.quantile), sep=""), col="lightblue4", srt=90, adj=c(0,0))
                par(new=T)
### plotting markers
if (regions>1) {	
        abline(v=markerslist, col="black")
        markerno<-seq(length(markerslist))
        text(markerslist,elutime.max*0.85, paste("Marker",markerno,"=",markerslist),col="black",adj=c(0,-0.5),srt=90,cex=2)
par(new=T) }
par(new=T)
if (length(pepmz.good) != 0) {
plot(pepmz.good, elutime.good, xlim=pepmz.range,ylim=elutime.range,col="darkred",xlab="",ylab="",yaxt='n',xaxp=pepmz.ticks, pch=17,cex=0.75)
par(new=T) }
if (length(pepmz.bad) != 0) {
plot(pepmz.bad, elutime.bad, xlim=pepmz.range,ylim=elutime.range,col="darkorange",xlab="",ylab="",yaxt='n',xaxp=pepmz.ticks,pch=2,cex=0.5)
par(new=T) }
if (length(pepmz.greyRegion) > 0) {
plot(pepmz.greyRegion, elutime.greyRegion, xlim=pepmz.range,ylim=elutime.range,col="darkgrey",xlab="",ylab="",yaxt='n',xaxp=pepmz.ticks, pch=16,cex=0.7)
par(new=T) }
if (length(pepmz.unidentified) > 0) {
plot(pepmz.unidentified, elutime.unidentified, xlim=pepmz.range,ylim=elutime.range,col="lightblue",xlab="",ylab="",yaxt='n',xaxp=pepmz.ticks,pch=1,cex=0.5)
par(new=T) }
    axis(side=4, yaxp=elutime.ticks)
        mtext(side=4, "Elution time (min)", line=3, srt=180, cex=0.9)
        mtext(side=1, "Mass-to-charge ratio (m/z)", line=3, cex=0.9)
        par(new=F)

## Legend in Scatterplot
if (grey_percent>0 & grey_percent!=-1) {
legend.text<- c()
        legend.text[[1]] <- c(paste("Good score (", length(pepmz.good), ")", sep=""))
        legend.text[[2]] <- c(paste("Poor score (", length(pepmz.bad) , ")", sep=""))
    legend.text[[3]] <- c(paste("Grey region score (", length(pepmz.greyRegion) , ")", sep=""))
    legend.text[[4]] <- c(paste("Unidentified ms/ms (", length(pepmz.unidentified) , ")", sep=""))
    legend(pepmz.max*1.01, elutime.max*0.97, legend.text, col=c("darkred", "darkorange", "darkgrey", "lightblue"), pch=c(17,2,16,1), xjust=1, cex=2)}
else {
legend.text<- c()
        legend.text[[1]] <- c(paste("Good score (", length(pepmz.good), ")", sep=""))
        legend.text[[2]] <- c(paste("Poor score (", length(pepmz.bad) , ")", sep=""))
    legend.text[[3]] <- c(paste("Unidentified ms/ms (", length(pepmz.unidentified) , ")", sep=""))
    legend(pepmz.max*1.01, elutime.max*0.97, legend.text, col=c("darkred", "darkorange", "lightblue"), pch=c(17,2,1), xjust=1, cex=2) } }

## score histogram function
ms_bargraph <- function(x, score, x.range, score.range, score.cutoff, colour, horiz) {
        if (length(x) == 0) {
        warning("Data-list contains no values")
        return()      }
    x.unique <- unique(x)
    x.unique.count <- length(x.unique)
    if (x.unique.count < 10) {
        x.bclass <- factor(cut2(x, g=x.unique.count))
        x.breaks <- quantile(x, 0:x.unique.count/x.unique.count)    # Fixed bin size (counts in bin) instead of fixed bin widths
    } else {
        x.bclass <- factor(cut2(x, g=10))
        x.breaks <- quantile(x, 0:10/10) } # Fixed bin size (counts in bin) instead of fixed bin widths
    score.means <- by(score, x.bclass, function(x) mean(x))
    histheight.raw <- as.numeric(score.means)
    histheight <- c(0.00001, histheight.raw)  # To shift the position
    offset <- min(x)
    histbreaks <- c(offset, as.numeric(diff(x.breaks)))
    if (horiz) {
        barplot(histheight, width=histbreaks, yaxt='n', xaxt='n', space=0, col=colour, xlim=score.range, ylim=x.range, xpd=FALSE, horiz=TRUE, ylab="")
        axis.ticks <- seq(0, signif(min(score.range),1), -20)
        axis.labels <- -1 * axis.ticks
        axis(side=1, at=axis.ticks, labels=axis.labels)
        mtext("Average mascot score", side=1, line=3, cex=0.9)
        abline(v=0)  # Ensure axis is drawn for entire length
        abline(v=score.cutoff, col=c("black","darkgrey"))
        text(score.cutoff[1]*1.03, min(x.range)*0.0, paste("    Mascot Score Cutoff=", score.cutoff[1] * -1), srt=90, adj=c(0,0), cex=1, col="black")
if (grey_percent >0 & grey_percent!=-1) {
        text(score.cutoff[2]*1.03, min(x.range)*0.0, paste("    Grey threshold Margin=", score.cutoff[2] * -1), srt=90, adj=c(0,0), cex=1, col="darkgrey")  }      
    } else {
        bp <- barplot(histheight, width=histbreaks, xaxt='n', yaxt='n', space=0, col=colour, ylim=score.range, xlim=x.range, xpd=FALSE, xlab="Avg. score")
        axis(side=4)
        mtext("Average mascot score", side=4, line=3, cex=0.9)
        abline(h=0)  # Ensure axis is drawn for entire length
        abline(h=score.cutoff, col=c("darkgrey","black"))
if (grey_percent >0 & grey_percent!=-1) {
        text(min(x.range), score.cutoff[1]*1.02, paste("    Grey threshold Margin =", score.cutoff[1]), adj=c(-0.4,0), cex=1, col="darkgrey") }
        text(min(x.range), score.cutoff[2]*1.02, paste("                       Mascot Score Cutoff =", score.cutoff[2]), adj=c(0.4,0), cex=1, col="black")  }
}

###Main call function
do_composite_plot <- function(filename,pngfile,exp,regions,grey_percent,mz.binsize,el.binsize) {
bitmap(pngfile, type="png16m", h=56, w=56, pointsize=40)
    par(cex=2)
mydata <- read.csv(filename)
        mydata <- mydata[order(mydata$pep_mz),]  #order w.r.t m/z value
title=names(mydata[21])
mascot_db=names(mydata[22])
mascot_taxonomy=names(mydata[23])
mascot_db_fasta=names(mydata[24])
    calibration_error_da <- mean(mydata$pep_mass_diff)
        calibration_error_ppm <- mean(10^6 * mydata$pep_mass_diff/mydata$pep_mz)

        ###procressing input parameters....
    grey_percent=as.numeric(grey_percent)
    exp=as.numeric(exp)
    regions=as.numeric(regions)
    mz.binsize=as.numeric(mz.binsize)
    el.binsize=as.numeric(el.binsize)
    identitythres<-	c(log10((mydata$pep_qmatch)/(exp*20))/log10(10)*10)	
    expectancy<- c(exp((identitythres-mydata$mascot_score)/10)*exp)
    mydata<-cbind(mydata,identitythres,expectancy)
    AMS=round(mean(mydata$identitythres[mydata$identitythres >0]))
    if (grey_percent >0) {
    grey_threshold=AMS-((as.numeric(grey_percent)/100)*AMS) }
    else {grey_threshold=AMS}
    score.cutoff <- c(grey_threshold,AMS)

    ## Calculating GPF m/z regions
if (regions >1) {	
    markerslist=c()
    totalpoints=length(mydata$pep_mz)
    pointsinregions=totalpoints/regions
    i=1
    while (i<regions) {
    marker=mydata$pep_mz[pointsinregions*i -1]
    markerslist[[i]]=signif(marker,4)
    i=i+1
    }}

    expectancy.unidentified <-mydata$expectancy[mydata$mascot_score < 0]
    pepmz.unidentified <-	mydata$pep_mz[mydata$mascot_score < 0]
    elutime.unidentified <-	mydata$elution_start[mydata$mascot_score < 0]
    score.unidentified <-	mydata$mascot_score[mydata$mascot_score < 0]*5

    expectancy.good	<-mydata$expectancy[mydata$expectancy<exp & mydata$mascot_score > 0]
    pepmz.good <-	mydata$pep_mz[mydata$expectancy<exp & mydata$mascot_score > 0]
    elutime.good <-	mydata$elution_start[mydata$expectancy<exp & mydata$mascot_score > 0]
    score.good <-	mydata$mascot_score[mydata$expectancy<exp & mydata$mascot_score > 0]

    expectancy.bad <-	mydata$expectancy[expectancy>=exp & mydata$mascot_score > 0]
    pepmz.bad <-	mydata$pep_mz[expectancy>=exp & mydata$mascot_score > 0]
    elutime.bad <-	mydata$elution_start[expectancy>=exp & mydata$mascot_score > 0]
    score.bad <-	mydata$mascot_score[expectancy>=exp & mydata$mascot_score > 0]

if (grey_threshold<AMS) {
    expectancy.bad <-	mydata$expectancy[expectancy>=exp & mydata$mascot_score >0 & mydata$mascot_score < grey_threshold]
    pepmz.bad <-	mydata$pep_mz[expectancy>=exp & mydata$mascot_score > 0 & mydata$mascot_score < grey_threshold]
    elutime.bad <-	mydata$elution_start[expectancy>=exp & mydata$mascot_score > 0 & mydata$mascot_score < grey_threshold]
    score.bad <-	mydata$mascot_score[expectancy>=exp & mydata$mascot_score > 0 & mydata$mascot_score < grey_threshold]

    expectancy.greyRegion <-  mydata$expectancy[expectancy>=exp & mydata$mascot_score >= grey_threshold & mydata$mascot_score < AMS]
    pepmz.greyRegion <-	mydata$pep_mz[expectancy>=exp & mydata$mascot_score >= grey_threshold & mydata$mascot_score < AMS]
    elutime.greyRegion <-	mydata$elution_start[expectancy>=exp & mydata$mascot_score >= grey_threshold & mydata$mascot_score < AMS]
    score.greyRegion <-	mydata$mascot_score[expectancy>=exp & mydata$mascot_score >= grey_threshold & mydata$mascot_score < AMS] }
else {
expectancy.greyRegion=c()
pepmz.greyRegion=c()
elutime.greyRegion=c()
score.greyRegion=c() }
    pepmz.min=min(mydata$pep_mz)
    pepmz.max=max(mydata$pep_mz)
    pepmz.range <- c(pepmz.min,pepmz.max)
    elutime.min =0
    elutime.max=max(mydata$elution_start)	
    elutime.range <- c(elutime.min,elutime.max)
    score.good.max=max(score.good)
    score.good.min=0
    score.good.range=c(score.good.min,score.good.max)
    score.range = c(-6, mean(score.good.range)*1.2)
zones=matrix(c(7,7,4,7,6,2,5,3,1),ncol=3,byrow=TRUE)
layout(zones,widths=c(4/20,2/20,16/20),heights=c(4/20,2/20,16/20))

####plotting the scatter plot function
    par(mar=c(5,1,1,5))
scatterplot(mydata,regions,markerslist,pepmz.good,elutime.good,pepmz.bad,elutime.bad,pepmz.greyRegion,elutime.greyRegion,pepmz.unidentified,elutime.unidentified)
    par(new=F)
peakcounts1 <- mydata$peaksfrom1[mydata$pep_charge == '1']
peakcounts2 <- mydata$peaksfrom1[mydata$pep_charge == '2']
peakcounts3 <- mydata$peaksfrom1[mydata$pep_charge == '3']
peakcounts4 <- mydata$peaksfrom1[mydata$pep_charge == '4']

### Peak count plots ## plot area 2...
        par(mar=c(0,1,1,5))
if (mz.binsize >0) {
        mz.bincount <- seq(min(mydata$pep_mz),max(mydata$pep_mz)+mz.binsize, mz.binsize)
        mz.1p.hist <- hist(mydata$pep_mz[mydata$pep_charge == '1'], plot=FALSE, breaks=mz.bincount)
        mz.2p.hist <- hist(mydata$pep_mz[mydata$pep_charge == '2'], plot=FALSE, breaks=mz.bincount)
        mz.3p.hist <- hist(mydata$pep_mz[mydata$pep_charge == '3'], plot=FALSE, breaks=mz.bincount)
        mz.4p.hist <- hist(mydata$pep_mz[mydata$pep_charge == '4'], plot=FALSE, breaks=mz.bincount)
        mz_max_peak_count <- max(mz.2p.hist$counts, mz.3p.hist$counts, mz.4p.hist$counts)
        count.range <- c(0, mz_max_peak_count)
    plot(mz.1p.hist$mids, mz.1p.hist$counts, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkorange", ylim=count.range)
    par(new=T)
plot(mz.2p.hist$mids, mz.2p.hist$counts, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkred", ylim=count.range)
    par(new=T)
plot(mz.3p.hist$mids, mz.3p.hist$counts, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkblue", ylim=count.range)
par(new=T)
plot(mz.4p.hist$mids, mz.4p.hist$counts, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkgreen", ylim=count.range)
}

else {
    peakcounts1 <- mydata$peaksfrom1[mydata$pep_charge == '1']
    mzwithcharge1 <-mydata$pep_mz[mydata$pep_charge == '1']
peakcounts2 <- mydata$peaksfrom1[mydata$pep_charge == '2']
    mzwithcharge2 <-mydata$pep_mz[mydata$pep_charge == '2']
peakcounts3 <- mydata$peaksfrom1[mydata$pep_charge == '3']
    mzwithcharge3 <-mydata$pep_mz[mydata$pep_charge == '3']
peakcounts4 <- mydata$peaksfrom1[mydata$pep_charge == '4']
    mzwithcharge4 <-mydata$pep_mz[mydata$pep_charge == '4']
    mz_max_peak_count=max(peakcounts2,peakcounts3,peakcounts4)
count.range <- c(0, mz_max_peak_count)
plot(mzwithcharge1, peakcounts1, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkorange", ylim=count.range)
par(new=T)
plot(mzwithcharge2, peakcounts2, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkred", ylim=count.range)
    par(new=T)
plot(mzwithcharge3, peakcounts3, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkblue", ylim=count.range)
par(new=T)
plot(mzwithcharge4, peakcounts4, xaxt="n", yaxt="n", xlab="", main="",type='l', xlim=pepmz.range, ylab="Peak count", 	col="darkgreen", ylim=count.range)}
axis(side=4)
        mtext(side=4, line=3, "Peak count per charge", cex=0.8)
    text_base <- pepmz.min
        text_offset <- pepmz.max
if (mz.binsize > 0) {
text(text_base + text_offset*0, 0.95 * mz_max_peak_count,  paste("Survey scan peak count in ",mz.binsize,"Da window"), col="darkgrey", adj=c(0,1)) }
else {
text(text_base + text_offset*0, 0.95 * mz_max_peak_count,  paste("Survey scan peaks"), col="darkgrey", adj=c(0,1)) }
        text(text_base + text_offset*0.22, 0.95 * mz_max_peak_count,"1+", col="darkorange", adj=c(0,1))
        text(text_base + text_offset*0.24, 0.95 * mz_max_peak_count, "2+", col="darkred", adj=c(0,1))
        text(text_base + text_offset*0.26, 0.95 * mz_max_peak_count, "3+", col="darkblue", adj=c(0,1))
        text(text_base + text_offset*0.28, 0.95 * mz_max_peak_count, "4+", col="darkgreen", adj=c(0,1))
        text(text_base + text_offset*0.30, 0.95 * mz_max_peak_count, "charge", col="darkgrey", adj=c(0,1))
par(new=F)

###Peak count plot ## plot area 3
        par(mar=c(5,1,1,1))
mydata <- mydata[order(mydata$elution_start),]
if (el.binsize>0) {
    el.bincount=seq(0,max(mydata$elution_start)+el.binsize,el.binsize)

        el.1p.hist <- hist(mydata$elution_start[mydata$pep_charge == '1'], plot=FALSE, breaks=el.bincount)
        el.2p.hist <- hist(mydata$elution_start[mydata$pep_charge == '2'], plot=FALSE, breaks=el.bincount)
        el.3p.hist <- hist(mydata$elution_start[mydata$pep_charge == '3'], plot=FALSE, breaks=el.bincount)
    el.4p.hist <- hist(mydata$elution_start[mydata$pep_charge == '4'], plot=FALSE, breaks=el.bincount)
        el_max_peak_count <- max(el.2p.hist$counts, el.3p.hist$counts, el.4p.hist$counts)
        count.range <- c(0, el_max_peak_count)
plot(el.1p.hist$counts,el.1p.hist$mids, xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkorange", xlim=rev(count.range))
    par(new=T)
plot(el.2p.hist$counts,el.2p.hist$mids, xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkred", xlim=rev(count.range))
    par(new=T)
plot(el.3p.hist$counts,el.3p.hist$mids,xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkblue", xlim=rev(count.range))
    par(new=T)
plot(el.4p.hist$counts,el.4p.hist$mids,xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkgreen", xlim=rev(count.range))}
else {

    elpeakcounts1 <- mydata$peaksfrom1[mydata$pep_charge == '1']
    elwithcharge1 <-mydata$elution_start[mydata$pep_charge == '1']
elpeakcounts2 <- mydata$peaksfrom1[mydata$pep_charge == '2']
    elwithcharge2 <-mydata$elution_start[mydata$pep_charge == '2']
elpeakcounts3 <- mydata$peaksfrom1[mydata$pep_charge == '3']
    elwithcharge3 <-mydata$elution_start[mydata$pep_charge == '3']
elpeakcounts4 <- mydata$elution_start[mydata$pep_charge == '4']
    elwithcharge4 <-mydata$pep_mz[mydata$pep_charge == '4']
    el_max_peak_count=max(elpeakcounts2,elpeakcounts3,elpeakcounts4)
    count.range <- c(0, el_max_peak_count)
    plot(elpeakcounts1,elwithcharge1, xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkorange", xlim=rev(count.range))
    par(new=T)
plot(elpeakcounts2,elwithcharge2, xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkred", xlim=rev(count.range))
    par(new=T)
plot(elpeakcounts3,elwithcharge3,xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkblue", xlim=rev(count.range))
    par(new=T)
plot(elpeakcounts4,elwithcharge4,xlab="", main="",type='l', ylim=elutime.range, ylab="",col="darkgreen", xlim=rev(count.range)) }
axis(side=2)
        mtext(side=1, line=3, "Peak count per charge ", cex=0.8)
    text_base <- (0.95 * el_max_peak_count)-1
        text_offset <-elutime.max
if (el.binsize >0) {
text(text_base,text_offset*0, paste("Survey scan peak count in ",el.binsize," min window"), col="darkgrey", adj=c(0,1),srt=90,cex=1.5) }
else {
text(text_base,text_offset*0, paste("Survey scan peaks"), col="darkgrey", adj=c(0,1),srt=90,cex=1.5) }       
        text(text_base,text_offset*0.70, "1+", col="darkorange", adj=c(0,1),srt=90)
        text(text_base,text_offset*0.75, "2+", col="darkred", adj=c(0,1),srt=90)
        text(text_base,text_offset*0.80, "3+", col="darkblue", adj=c(0,1),srt=90)
        text(text_base,text_offset*0.85, "4+", col="darkgreen", adj=c(0,1),srt=90)
        text(text_base,text_offset*0.90, "charge", col="darkgrey", adj=c(0,1),srt=90)
par(new=F)

#### Score Histograms ## plot area 4
        par(mar=c(0,1,4,5))
        if (length(score.good) > 0) {
            ms_bargraph(pepmz.good, score.good, pepmz.range, score.range, score.cutoff, "red",  FALSE)
            par(new=T)        }
if (grey_percent>0) {
        if (length(score.greyRegion) > 0) {
            ms_bargraph(pepmz.greyRegion, score.greyRegion, pepmz.range, score.range, score.cutoff, "grey50", FALSE)
            par(new=T)        } }
        if (length(score.bad) > 0) {
            ms_bargraph(pepmz.bad, score.bad, pepmz.range, score.range, score.cutoff, "darkorange",  FALSE)
            par(new=T)        }
        if (length(score.unidentified) > 0) {
            ms_bargraph(pepmz.unidentified, score.unidentified, pepmz.range, score.range, score.cutoff, rgb(85/255,156/255,185/255), FALSE)
            par(new=T)        }
        par(new=F)
        title(main=title)
        text(pepmz.min * 1.35, mean(score.good) + 0.75 * sd(score.good), "Classified average mascot score in centiles", col="darkgrey", adj=c(0.5,0.5))

    par(new=F)
        text((mean(score.good) + 1.5 * sd(score.good)) * -1, elutime.max * .12 * -1, "Classified average mascot score in centiles", col="darkgrey", adj=c(0.5,1), srt=90)

    ### Score Histograms ### plot area 5
        par(mar=c(5,0,1,1))
        if (length(score.good) > 0) {
            ms_bargraph(elutime.good * -1, score.good * -1, elutime.range * -1, rev(score.range * -1), rev(score.cutoff * -1), "red", TRUE)
            par(new=T)        }
if (grey_percent>0) {
        if (length(score.greyRegion) > 0) {
            ms_bargraph(elutime.greyRegion * -1, score.greyRegion * -1, elutime.range * -1, rev(score.range * -1),  rev(score.cutoff * -1), "grey50", TRUE)
            par(new=T)        } }
        if (length(score.bad) > 0) {
            ms_bargraph(elutime.bad * -1, score.bad * -1, elutime.range * -1, rev(score.range * -1), rev(score.cutoff * -1), "darkorange", TRUE)
            par(new=T)        }
        if (length(score.unidentified) > 0) {
            ms_bargraph(elutime.unidentified * -1, score.unidentified * -1, elutime.range * -1, rev(score.range * -1),  rev(score.cutoff * -1), rgb(85/255,156/255,185/255), TRUE)
            par(new=F)        }
###total Experiment overview plot ### plot area 6
par(mar=c(0,2,0,0))
plot(0, 0, xlim=c(0,100),ylim=c(0,105),xlab="", ylab="",xaxt='n',yaxt='n',type='n')
unidentifiedline=signif((length(pepmz.unidentified)/length(mydata$pep_mz))*100,2)
badline=signif((length(pepmz.bad)/length(mydata$pep_mz))*100,2)
goodline=signif((length(pepmz.good)/length(mydata$pep_mz))*100,2)
if (grey_percent>0) {
greyline=signif((length(pepmz.greyRegion)/length(mydata$pep_mz))*100,2)
rect(0,0,50,unidentifiedline,col="darkblue")
rect(0,unidentifiedline,50,unidentifiedline+badline,col="darkorange")
rect(0,unidentifiedline+badline,50,unidentifiedline+greyline+badline,col="darkgrey")
rect(0,unidentifiedline+greyline+badline,50,unidentifiedline+greyline+badline+goodline,col="red") }
else {
rect(0,0,50,unidentifiedline,col="darkblue")
rect(0,unidentifiedline,50,unidentifiedline+badline,col="darkorange")
rect(0,unidentifiedline+badline,50,unidentifiedline+badline+goodline,col="red") }
text(75,70, paste("Good:", goodline, "%"), col="red")
text(75,60, paste("Poor:", badline, "%"), col="darkorange")
if (grey_percent>0) {
text(75,50, paste("Grey:", greyline, "%"), col="darkgrey")
text(75,40, paste("No identiy:", unidentifiedline, "%"), col="darkblue") }
else {
text(75,50, paste("No identity:", unidentifiedline, "%"), col="darkblue") }
mtext(side=2, "Total queries (100%)",cex=0.7)
par(new=F)
        ### Textual Statistics section ### Plot area 7
        par(mar=c(2,0,0,0))
        plot(c(1,1), axes=FALSE, xlim=c(0,100), ylim=c(0,20), type="n", xlab="")
    text(1,18, paste("Total queries:........", length(mydata$pep_mz)), adj=0,cex=2)
if (grey_percent>0) {        
    text(1,17, paste("Peptide identifications:.....", length(pepmz.good) + length(pepmz.bad)+length(pepmz.greyRegion)), adj=0,cex=2) }
else {
text(1,17, paste("Peptide identifications:.....", length(pepmz.good) + length(pepmz.bad)), adj=0,cex=2) }
    text(1,16, paste("good score (p<",exp,"):......", length(pepmz.good)), adj=0,cex=1.5)
    text(1,15, paste("poor score (p>=",exp,"):......", length(pepmz.bad)), adj=0,cex=1.5)
if (grey_percent >0) {
    text(1,14, paste("grey score (p>",exp,"&",grey_threshold,"<=score<",AMS,"):.", length(pepmz.greyRegion)), adj=0,cex=1.5) }
    text(1,13, paste("good/poor ratio:..........", round((length(pepmz.good)) / length(pepmz.bad),2)), adj=0,cex=1.5)
    text(1,12, paste("Mascot Score Cutoff(MSC):........", round(AMS,2)), adj=0,cex=2)
if (grey_percent >0) {
    text(1,11, paste("% cutoff margin below MSC :....", grey_percent, "%"), adj=0,cex=2) }
if (regions >1){
    text(1,10, paste("List of Markers:.", toString(markerslist)), adj=0,cex=1.5) }
    text(1,9, "Database:", adj=0,cex=1.5)
    text(1,8, paste("Taxonomy:..", mascot_taxonomy), adj=0,cex=1.5)
    text(1,7, paste("Name:......", mascot_db), adj=0,cex=1.5)
    text(1,6, paste("Release:...", mascot_db_fasta), adj=0,cex=1.5)
    text(1,5, paste("Calibration error:.", round(calibration_error_da, 4), "Da"), adj=0,cex=1.5)
    text(1,4, paste("Calibration error:.", round(calibration_error_ppm, 3), "ppm"), adj=0,cex=1.5)
    text(1,3,paste("Charge states:.,  1+    2+    3+    4+"),adj=0,cex=1.5)
totalcounts=length(peakcounts1)+length(peakcounts2)+length(peakcounts3)+length(peakcounts4)
text(1,2,paste("Charge counts:.(",round(length(peakcounts1)/totalcounts*100,0)," ",round(length(peakcounts2)/totalcounts*100,0)," ",round(length(peakcounts3)/totalcounts*100,0)," ",round(length(peakcounts4)/totalcounts*100,0),")%"),adj=0,cex=1.5)
    dev.off()
}
