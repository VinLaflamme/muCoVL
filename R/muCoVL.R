

effSizeVL <- function (xx, alpha=.05) {
  NN<-length(xx)
  Mean<-mean(xx)
  StD<-sd(xx)
  StE<-StD/sqrt(NN)
  CIbar<-StE*qt(p=alpha/2,df = NN-1,lower.tail=F)
  CIlow<-Mean-CIbar
  CIhigh<-Mean+CIbar
  return(list(NN=NN,Mean=Mean,StD=StD,StE=StE,CIlow=CIlow,CIhigh=CIhigh))
}


#' Calculate basic statistics for a data.table
#' 
#' This function calculates the descriptive statistics of a data table.
#' 
#' @param dataT data.table
#' @param facS Name of the factors on which to calculate the descriptive statistics. Can be a character vector or a single comma separated character value.
#' @param otherF Name of the other factors to aggregate.
#' @param DV Name of the dependant variable.
#' @param subF Name of the subject factor.
#' @param alpha Confidence level.
#' @param Eng whether the output is in English (true) or French (false)
#' @param Std whether to report the standard deviation (true) or the standard error (false)
#' @param Unit measurement unit of the DV
#' @param digits number of rounding digits.
#' @return a data.table with the following columns
#' \itemize{
#' \item Mean: Sample mean.
#' \item StD: Sample standard deviation.
#' \item StE: Sample standard error.
#' \item NN: Sample size.
#' \item CIlow: Lower bound of the confidence interval.
#' \item CIhigh: Higher bound of the confidence interval.
#' }
#' @examples
#' data("donn_ag",package="muCoVL")
#' out<- effSizeVLDT(dataT=donn_ag,facS="Trial_Block,Group,dur",otherF="dist",DV="relE",subF="sub")
#' @export

effSizeVLDT<-function(dataT,facS,otherF=NULL,DV,subF,alpha=.05,Eng=T,Std=T, Unit="",digits=2){
  if(!(is.data.table(dataT))) stop("Invalid dataT parameter")
  if(!(is.character(facS))) stop("Invalid facS parameter")
  if(!(is.null(otherF))){
    if(!(is.character(otherF))) stop("Invalid groupF parameter")
  }
  if(!(is.character(DV))) stop("Invalid DV parameter")
  if(!(is.character(subF))) stop("Invalid subF parameter")
  if(length(facS)>1){
    facSs<-paste0(facS,collapse=",")
    facSl<-facS
  }else{
    facSs<-facS
    facSl<-gsub(pattern = " ",replacement = "",x = facS)
    facSl<-unlist(strsplit(x = facSl,split = ","))
  }
  if(!(is.null(otherF))){
    if(length(otherF)>1){
      otherFs<-paste0(otherF,collapse=",")
      otherFl<-otherF
    }else{
      otherFs<-otherF
      otherFl<-gsub(pattern = " ",replacement = "",x = otherF)
      otherFl<-unlist(strsplit(x = otherFl,split = ","))
    }
  }else{
    otherFs<-NULL
    otherFl<-NULL
  }
  if(!(length(unique(c(otherFl,facSl,DV,subF)))==length(c(otherFl,facSl,DV,subF)))) stop("DV,facS and otherF not mutually exclusive")
  if(!all(c(otherFl,facSl,DV,subF) %in% names(dataT))) stop("DV,facS and otherF are not all column names")
  if(is.null(otherF)){
    paired<-F
  }else{
    paired<-rep(T,length.out = length(otherFl))
    oo<-1
    for(ii in otherFl){
      strstr1<-paste0("length(unique(dataT[,list(",subF,",",ii,")])[[1]])")
      strstr2<-paste0("length(unique(dataT[,list(",subF,")])[[1]])")
      if(eval(parse(text=strstr1))==eval(parse(text=strstr2))) paired[oo]<-F
      oo<-oo+1
    }
  }
  
  if(any(paired)){
    strstr<-parse(text=paste0("list(",DV,"=mean(",DV,"))"))
    out<-dataT[,eval(strstr),by=c(facSl,otherFl[!paired],subF)]
  }else{
    out<-dataT
  }
  strstr<-parse(text=paste0("effSizeVL(",DV,",alpha=alpha)"))
  out2<-out[,eval(strstr),by=c(facSl)]
  if(Eng){
    if(Std){
      out2[,txxtHTML:=paste0("<i>M<sub>",DV,"</sub></i> = ", roundVL2(Mean,digits)," ",Unit,", <i>SD</i> = ",roundVL2(StD, digits)," ",Unit)]
    }else{
      out2[,txxtHTML:=paste0("<i>M<sub>",DV,"</sub></i> = ", roundVL2(Mean,digits)," ",Unit,", <i>SE</i> = ",roundVL2(StE, digits)," ",Unit)]
    }
  }else{
    if(Std){
      out2[,txxtHTML:=paste0("<i>M<sub>",DV,"</sub></i> = ", roundVL2(Mean,digits)," ",Unit,", <i>ÉT</i> = ",roundVL2(StD, digits)," ",Unit)]
    }else{
      out2[,txxtHTML:=paste0("<i>M<sub>",DV,"</sub></i> = ", roundVL2(Mean,digits)," ",Unit,", <i>ES</i> = ",roundVL2(StE, digits)," ",Unit)]
    }
  }
  
  return(out2)
}


#' Prints a graph of descriptive statistics
#' 
#' Prints a graph of the descriptive statistics of the data in a data.table
#' 
#' @param dataT Data.table containing the data.
#' @param xx Name of the factor to put on the x axis of the graph. Must be singular.
#' @param groupF Name of the factor to use as the grouping factor of the graph. Must be singular
#' @param facetF Name of the factor(s) to use as faceting. There can be more than one. Either a single comma separated character or a character vector.
#' @inheritParams effSizeVLDT
#' @param dWidth Dodge width.
#' @param eWidth Error bar width.
#' @param pSize Point size.
#' @param pFill Fill colour.
#' @return A graph.
#' @examples
#' data("donn_ag",package="muCoVL")
#' dataT<-donn_ag
#' xx<-"dur"
#' groupF<-"Group"
#' facetF<-"Trial_Block"
#' otherF<-"dist"
#' DV<-"relE"
#' subF<-"sub"
#' result<-graDescVL(dataT = dataT,xx = xx,groupF = groupF,facetF = facetF,otherF = otherF,DV = DV,subF = subF,eWidth = 1000,dWidth = 1000)
#' @export


graDescVL<-function(dataT,xx,groupF=NULL,facetF=NULL,otherF=NULL,DV,subF,alpha=.05,dWidth=.5,eWidth=.5,pSize=3,pFill="white"){
  if(!(is.data.table(dataT))) stop("Invalid dataT parameter")
  if(!(is.character(xx))) stop("Invalid xx parameter")
  if(!(is.null(otherF))){
    if(!(is.character(otherF))) stop("Invalid otherF parameter")
  }
  if(!(is.null(facetF))){
    if(!(is.character(facetF))) stop("Invalid facetF parameter")
  }
  if(!(is.null(groupF))){
    if(!(is.character(groupF))) stop("Invalid groupF parameter")
  }
  if(!(is.character(DV))) stop("Invalid DV parameter")
  if(!(is.character(subF))) stop("Invalid DV parameter")
  if(length(xx)>1) stop("Invalid xx parameter")
  if(length(DV)>1) stop("Invalid DV parameter")
  if(length(subF)>1) stop("Invalid subF parameter")
  if(length(groupF)>1) stop("Invalid groupF parameter")
  if(is.null(facetF)){
    facetFs<-NULL
    facetFl<-NULL
  }else{
    if(length(facetF)>1){
      facetFs<-paste0(facetF,collapse=T)
      facetFl<-facetFS
    }else{
      facetFs<-facetF
      facetFl<-gsub(pattern = " ",replacement = "",x = facetF)
      facetFl<-unlist(strsplit(x = facetFl,split = ","))
    }
  }
  if(is.null(otherF)){
    otherFs<-NULL
    otherFl<-NULL
  }else{
    if(length(otherF)>1){
      otherFs<-paste0(otherF,collapse=T)
      otherFl<-otherF
    }else{
      otherFs<-otherF
      otherFl<-gsub(pattern = " ",replacement = "",x = otherF)
      otherFl<-unlist(strsplit(x = otherFl,split = ","))
    }
  }
  
  if(!(length(unique(c(otherFl,facetFl,xx,groupF,DV,subF)))==length(c(otherFl,facetFl,xx,groupF,DV,subF)))) stop("DV,facS and otherF not mutually exclusive")
  if(!all(c(otherFl,facetFl,xx,groupF,DV,subF) %in% names(dataT))) stop("DV,facS and otherF are not all column names")
  out<-effSizeVLDT(dataT = dataT,otherF = otherFl,facS = c(xx,facetFl,groupF),DV = DV,subF = subF,alpha = alpha)
  if(!is.null(facetFl)){
    if(length(facetFl)>1){
      strstr<-paste0("out[,interF:=interaction(",paste0(facetFl,collapse=","),")]")
      eval(parse(text=strstr))
    }else{
      strstr<-paste0("out[,interF:=",facetFl,"]")
      eval(parse(text=strstr))
    }
  }
  
  if(!is.null(groupF)){
    graOut<-ggplot(out,mapping=aes_string(x=xx,y="Mean",ymin="Mean-StE",ymax="Mean+StE",group=groupF))+
      geom_line(mapping=aes_string(linetype=groupF),position=position_dodge(width = dWidth))+
      geom_errorbar(width=eWidth,position=position_dodge(width = dWidth))+
      geom_point(mapping=aes_string(shape=groupF),position=position_dodge(dWidth),fill=pFill,size=pSize)
  }else{
    graOut<-ggplot(out,mapping=aes_string(x=xx,y="Mean",ymin="Mean-StE",ymax="Mean+StE",group="1"))+
      geom_line()+
      geom_errorbar(width=eWidth)+
      geom_point(fill=pFill,size=pSize)
  }
  if(!is.null(facetF)){
    graOut<-graOut+facet_wrap(facets=~interF)
  }
  return(graOut)
}

pairedCVL<-function(xx,yy,paired=T){
  if(paired){
    if(!(length(xx)==length(yy))) stop("length(xx) != length(yy)")
    DiffV<-xx-yy
    NN<-length(DiffV)
    mDiff<-mean(DiffV)
    sdDiff<-sd(x = DiffV)
    rr<-cor(x = xx,y = yy)
    semDiff<-sdDiff/sqrt(NN)
    tt<-sqrt(NN)*mDiff/sdDiff
    pval<-2*(1-pt(q=abs(tt),df=NN-1))
    dz<-tt/sqrt(NN)
    drm<-dz*sqrt(2*(1-rr))
    grm<-drm*(1-(3/(8*NN-9)))
    dav<-2*mDiff/(sd(xx)+sd(yy))
    gav<-dav*grm/drm
    dout<-dav
    retL<-list(NN=NN,mDiff=mDiff,semDiff=semDiff,tt=tt,rr=rr,pval=pval,dz=dz,drm=drm,grm=grm,dav=dav,gav=gav,dout=dout)
  }else{
    N1<-length(xx)
    N2<-length(yy)
    NN<-N1+N2
    sd1<-sd(xx)
    sd2<-sd(yy)
    Mdiff<-mean(xx)-mean(yy)
    Sx1x2<-sqrt(((N1-1)*sd1^2+(N2-1)*sd2^2)/(NN-2))
    SEMdiff<-Sx1x2*sqrt((1/N1)+(1/N2))
    tt<-Mdiff/(SEMdiff)
    pval<-2*(1-pt(q=abs(tt),df=NN-2))
    SEM_W<-sqrt((sd1^2/N1)+(sd2^2/N2))
    ttWelsh<-Mdiff/SEM_W
    dfW<-((sd1^2/N1)+(sd2^2/N2))^2/(((sd1^2/N1)^2/(N1-1))+((sd2^2/N2)^2/(N2-1)))
    pvalW<-2*(1-pt(q=abs(ttWelsh),df=dfW))
    ds<-tt*sqrt((1/N1)+(1/N2))
    rr<-ds/sqrt(ds^2+(NN^2-2*NN)/(N1*N2))
    gs<-ds*(1-(3/(4*(N1+N2)-9)))
    dout<-ds
    retL<-list(N1=N1,N2=N2,NN=N1+N2,mDiff=Mdiff,semDiff=SEMdiff,tt=tt,pval=pval,SEM_W=SEM_W,ttWelsh=ttWelsh,dfW=dfW,pvalW=pvalW,ds=ds,rr=rr,gs=gs,dout=dout)
  }
  return(retL)
}

expandVL<-function(x){
  xx<-as.character(x)
  zz<-c()
  yy<-c()
  counter<-1
  for(i in 1:(length(xx)-1)){
    for(j in (i+1):(length(xx))){
      zz[counter]<-xx[i]
      yy[counter]<-xx[j]
      counter<-counter+1
    }
  }
  returndt<-data.table(Var1=zz,Var2=yy)
  return(returndt)
}

internalPaired<-function(DT,DV,facF,paired){ 
  outttt<-expandVL(unique(DT[,facF,with=F])[[1]])
  strstr<-paste0("{xx<-DT[",facF,"==Var1,list(",DV,")][[1]];yy<-DT[",facF,"==Var2,list(",DV,")][[1]];pairedCVL(xx=xx,yy=yy,paired=",paired,")}")
  outttt[,eval(parse(text=strstr)),by="Var1,Var2"]
}

#' Paired comparisons on data.tables
#' 
#' Calculates paired comparisons for a factor on a data.table
#' 
#' @param dataT The data table
#' @param facF The name of the column containing the factor on which the comparisons are operated.
#' @param otherF Any other factors in the ANOVA design. Useful for simple effects. Vector of column names or comma separated string of column names.
#' @param aggF Factors to aggregate on. Useful when decomposing main effects. Vector of column names or comma separated string of column names.
#' @param DV Name of the column containing the dependant variable.
#' @param subF Name of the column containing the subject code.
#' @param aovv (optional) aov object
#' @param tDec Number of decimals for the t statistic
#' @param pDec Number of decimals for the p value(s)
#' @param pCut Cutting point for the p value
#' @param dDec Number of decimals for Cohen's d
#' @return Augmented data.table with the means, t tests and effect sizes.
#' @section References: 
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: a practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4. 
#' @examples
#' data("donn_ag",package="muCoVL")
#' dataT<-donn_ag
#' facF<-"Trial_Block"
#' otherF<-"Group,dist"
#' aggF<-"dur"
#' DV<-"relE"
#' subF<-"sub"
#' pairedCVLDT(dataT,facF,otherF,aggF,DV,subF)
#' @export

pairedCVLDT<-function(dataT,facF,otherF=NULL,aggF=NULL,DV,subF,aovv=NULL,tDec=2,pDec=3,pCut=.001,dDec=3){
  if(!(is.data.table(dataT))) stop("Invalid dataT parameter")
  if(!(is.character(facF))) stop("Invalid facF parameter")
  if(length(facF)>1) stop("Invalid facF parameter")
  if(!is.null(otherF)){
    if(!(is.character(otherF))) stop("Invalid otherF parameter")
  }
  if(!is.null(aggF)){
    if(!(is.character(aggF))) stop("Invalid aggF parameter")
  }
  if(!(is.character(DV))) stop("Invalid DV parameter")
  if(length(DV)>1) stop("Invalid DV parameter")
  if(!(is.character(subF))) stop("Invalid subF parameter")
  if(length(subF)>1) stop("Invalid subF parameter")
  if(is.null(otherF)){
    otherFs<-NULL
    otherFl<-NULL
  }else{
    if(length(otherF)>1){
      otherFs<-paste0(otherF,collapse=T)
      otherFl<-otherF
    }else{
      otherFs<-otherF
      otherFl<-gsub(pattern = " ",replacement = "",x = otherF)
      otherFl<-unlist(strsplit(x = otherFl,split = ","))
    }
  }
  if(is.null(aggF)){
    aggFs<-NULL
    aggFl<-NULL
  }else{
    if(length(aggF)>1){
      aggFs<-paste0(aggF,collapse=T)
      aggFl<-aggF
    }else{
      aggFs<-aggF
      aggFl<-gsub(pattern = " ",replacement = "",x = aggF)
      aggFl<-unlist(strsplit(x = aggFl,split = ","))
    }
  }
  if(!(length(unique(c(otherFl,aggFl,facF,DV,subF)))==length(c(otherFl,aggFl,facF,DV,subF)))) stop("otherFl,aggFl,facF,DV,subF not mutually exclusive")
  if(!all(c(otherFl,aggFl,facF,DV,subF) %in% names(dataT))) stop("otherFl,aggFl,facF,DV,subF missing not column names.")
  facPa<-T
  strstr1<-paste0("length(unique(dataT[,list(",subF,",",facF,")])[[1]])")
  strstr2<-paste0("length(unique(dataT[,list(",subF,")])[[1]])")
  
  if(eval(parse(text=strstr1))==eval(parse(text=strstr2))) facPa<-F
  if(!is.null(aggFl)){
    aggPa<-rep(T,length.out = length(aggFl))
    count<-1
    for(ii in aggFl){
      strstr1<-paste0("length(unique(dataT[,list(",subF,",",ii,")])[[1]])")
      strstr2<-paste0("length(unique(dataT[,list(",subF,")])[[1]])")
      if(eval(parse(text=strstr1))==eval(parse(text=strstr2))) aggPa[count]<-F
      count<-count+1
    }
  }else{
    aggPa<-F
  }
  if(any(aggPa)){
    strstr<-parse(text=paste0("list(",DV,"=mean(",DV,"))"))
    out<-dataT[,eval(strstr),by=c(facF,otherFl,aggFl[!aggPa],subF)]
  }else{
    out<-dataT
  }
  setkeyv(out,cols = facF)
  if(is.null(otherFl)){
    out2<-out[,internalPaired(DT=.SD,DV=DV,facF=facF,paired=facPa)]
    #setkey(out2,Var1,Var2)
  }else{
    out2<-out[,internalPaired(DT=.SD,DV=DV,facF=facF,paired=facPa),by=otherFl]
    #setkeyv(out2,c(otherFl,"Var1","Var2"))
  }
  if(!is.null(aovv)){
    refg<-ref.grid(aovv)
    if(is.null(otherF)){
      outlsm<-lsmeans(refg,specs=facF)
    }else{
      outlsm<-lsmeans(refg,specs=facF,by=otherFl)
    }
    outpa<-pairs(outlsm)
    out2[,ptuk:=summary(outpa)$p.value]
    out2[,txxtHTML:=paste0("<i>t</i>(",NN-1,") = ",roundVL2(tt,tDec,cutoff=0),", <i>p</i> ",ifelse(test = pval<pCut,yes = "< ",no = "= "),roundVL2(pval,pDec,remZero=T,cutoff=pCut),", <i>p<sub>Tukey</sub></i> ",ifelse(test = ptuk<pCut,yes = "< ",no = "= "), roundVL2(ptuk,pDec,remZero=T,cutoff=pCut),", <i>d<sub>C</sub></i> = ",roundVL2(dout,dDec,cutoff=0))]
  }else{
    out2[,txxtHTML:=paste0("<i>t</i>(",NN-1,") = ",roundVL2(tt,tDec,cutoff=0),", <i>p</i> ",ifelse(test = pval<pCut,yes = "< ",no = "= "),roundVL2(pval,pDec,remZero=T,cutoff=pCut),", <i>d<sub>C</sub></i> = ",roundVL2(dout,dDec,cutoff=0))]
  }
  return(out2)
}

roundVL<-function (x, digits = 2,remZero = F) 
{
  y <- rep("cc", times = length(x))
  for (ii in 1:length(x)) {
    if (abs(x[ii]) < 0.1) {
      y[ii] <- sprintf(paste0("%.", digits, "g"), x[ii])
    }
    else {
      if (trunc(x[ii]) == x[ii]) {
        y[ii] <- paste0(x[ii])
      }
      else {
        y[ii] <- sprintf(paste0("%.", digits, "f"), x[ii])
      }
    }
    if (getOption("OutDec") == ",") {
      y[ii] <- gsub("\\.", ",", y[ii])
    }
    if(remZero && (x[ii]<1) && (substring(y[ii],first = 1,last = 1)=="0")){
      y[ii]<-substring(y[ii],2)
    }
  }
  return(y)
}

roundVL2<-function(x,digits=3,cutoff=.001,remZero=F){
  textOut<-rep("cc",times=length(x))
  for(iii in 1:length(x)){
    expO<-F
    nega<-F
    if(x[iii]<0){
      nega<-T
      x[iii]<- (-1)*x[iii]
    }
    if(x[iii]<cutoff) x[iii]<-cutoff
    eee<-as.character(round(x[iii],digits))
    if(grepl("e",eee)){
      remZero<-F
      expO<-T
      expoVal<-strsplit(x = eee,split = "e",fixed = T)[[1]]
      expoVal<-as.numeric(expoVal[length(expoVal)])
      x[iii]<-x[iii]*10^(-expoVal)
      eee<-as.character(round(x[iii],digits))
    }
    ppp<-strsplit(x = eee,split = getOption("OutDec"),fixed = T)[[1]]
    if(length(ppp)==2){
      digitsafter0<-nchar(ppp[2])
      if(is.na(digitsafter0)) digitsafter0<-0
    }else{
      digitsafter0<-0
    }
    if(is.na(ppp[2])) ppp[2]<-""
    if(digitsafter0<digits){
      ppp[2]<-paste0(c(ppp[2],rep("0",times=digits-digitsafter0)),collapse = "")
    }
    if(digits>0){
      textOut[iii]<-paste0(getOption("OutDec"),ppp[2])
    }else{
      textOut[iii]<-""
    }
    
    if(!remZero||round(x[iii],digits)>=1) textOut[iii]<-paste0(ppp[1],textOut[iii])
    if(expO) textOut[iii]<-paste0(textOut[iii]," &times; 10<sup>",expoVal,"</sup>")
    if(nega) textOut[iii]<-paste0("-",textOut[iii])
  }
  
  return(textOut)
}

repANhelp2<-function(data,eff,stat,digits=2,cutoff=.001){
  if(!is.null(data$`Mauchly's Test for Sphericity`)){
    isCorr<-eff %in% data$`Mauchly's Test for Sphericity`$Effect
  }else{
    isCorr<-F
  }
  if(isCorr){
    Mrow<-match(eff,data$`Mauchly's Test for Sphericity`$Effect)
  }
  ANOrow<-match(eff,data$ANOVA$Effect)
  if(isCorr){
    spherow<-match(eff,data$`Sphericity Corrections`$Effect)
    corrRow<-match(eff,data$corrDF$Effect)
  }
  switch(stat,
         DFn={
           if(isCorr){
             if(data$`Mauchly's Test for Sphericity`[Mrow,"p"]<0.05){
               returnV<-roundVL2(x=data$corrDF[corrRow,DFnGG],digits=digits)
             }else{
               returnV<-roundVL2(x=data$corrDF[corrRow,DFn],digits=0)
             }
           }else{
             returnV<-roundVL2(x=data$ANOVA[ANOrow,DFn],digits=0)
           }
         },DFd={
           if(isCorr){
             if(data$`Mauchly's Test for Sphericity`[Mrow,"p"]<0.05){
               returnV<-roundVL2(data$corrDF[corrRow,DFdGG],digits=digits)
             }else{
               returnV<-roundVL2(data$corrDF[corrRow,DFd],digits=0)
             }
           }else{
             returnV<-roundVL(data$ANOVA[ANOrow,DFd],digits=0)
           }
         },p={
           if(isCorr){
             if(data$`Mauchly's Test for Sphericity`[Mrow,"p"]<0.05){
               returnV<-roundVL2(data$`Sphericity Corrections`[spherow,"p[GG]"],digits=digits,remZero=T,cutoff=cutoff)
               eqsign<-ifelse(data$`Sphericity Corrections`[spherow,"p[GG]"]<cutoff,"<","=")
             }else{
               returnV<-roundVL2(data$ANOVA[ANOrow,p],digits=digits,remZero=T,cutoff=cutoff)
               eqsign<-ifelse(data$ANOVA[ANOrow,p]<cutoff,"<","=")
             }
           }else{
             returnV<-roundVL2(data$ANOVA[ANOrow,p],digits=digits,remZero=T,cutoff=cutoff)
             eqsign<-ifelse(data$ANOVA[ANOrow,p]<cutoff,"<","=")
           }
           returnV<-paste0(eqsign," ",returnV)
         },F={
           returnV<-roundVL2(data$ANOVA[ANOrow,F],digits=digits,remZero=F,cutoff=cutoff)
         },e2p={
           eqsign<-ifelse(data$ANOVA[ANOrow,e2p]<cutoff,"<","=")
           returnV<-paste0(eqsign," ",roundVL2(data$ANOVA[ANOrow,e2p],digits=digits,remZero=T,cutoff=cutoff))
         },ges={
           eqsign<-ifelse(data$ANOVA[ANOrow,ges]<cutoff,"<","=")
           returnV<-paste0(eqsign, " ", roundVL2(data$ANOVA[ANOrow,ges],digits=digits,remZero=T,cutoff=cutoff))
         },epsi={
           if(isCorr){
             if(data$`Mauchly's Test for Sphericity`[Mrow,"p"]<0.05){
               temp<-roundVL2(data$`Sphericity Corrections`[spherow,"GGe"],digits=digits,remZero=T,cutoff=cutoff)
               eqsign<-ifelse(data$`Sphericity Corrections`[spherow,"GGe"]<cutoff,"<","=")
               returnV<-paste0(", &epsilon;<sub>GG</sub> ",eqsign," ",temp)
             }else{
               returnV<-""
             }
           }else{
             returnV<-""
           }
         })
  return(returnV)
}

#' Augment ezANOVA output
#' 
#' This function augments the output from an ezANOVA for easier pasting into a manuscript.
#' 
#' @param ezOut ezANOVA output.
#' @param digits significant digits.
#' @param genEs Whether to print the generalized eta squared or not
#' @return augmented list. The txxthtml is a printable an HTML table. Use in conjunction with xtable.
#' @examples
#' data("donn_ag",package="muCoVL")
#' donn_ag[,durF:=factor(dur)]
#' outt<-ezA_VL(ezANOVA(data = donn_ag,dv = .(relE),wid = .(sub),within = .(dist,durF),between = .(Trial_Block,Group),type = 3,return_aov = T))
#' @export 

ezA_VL<-function(ezOut,digits=2,genEs=F){
  if(!is.null(ezOut$ANOVA)){
    ANOVAt<-data.table(ezOut$ANOVA,key="Effect")
    ANOVAt[,e2p:=(F*DFn/(F*DFn+DFd))]
    ezOut$ANOVA<-ANOVAt
    if(!is.null(ezOut$`Sphericity Corrections`)){
      corrdt<-data.table(ezOut$`Sphericity Corrections`,key="Effect")
      temp<-ANOVAt[corrdt]
      temp[,DFnGG:=ifelse(test = GGe<1,yes = GGe,no = 1)*DFn]
      temp[,DFdGG:=ifelse(test = GGe<1,yes = GGe,no = 1)*DFd]
      temp[,DFnHF:=ifelse(test = HFe<1,yes = HFe,no = 1)*DFn]
      temp[,DFdHF:=ifelse(test = HFe<1,yes = HFe,no = 1)*DFd]
      ezOut$corrDF<-temp
    }
    txtAn<-ANOVAt[,list(Effect)]
    ezOut$tabl<-txtAn[,list(DFn=repANhelp2(ezOut,Effect,"DFn",digits=DoFd),DFf=repANhelp2(ezOut,Effect,"DFd",digits=DoFd),Ff=repANhelp2(ezOut,Effect,"F",digits=Fd),pp=repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),e2p=repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),e2g= repANhelp2(ezOut,Effect,"ges",digits=e2d,cutoff=e2c)),by="Effect"]
    ezOut$txxt<-txtAn[,{
      txtout<-paste0("F(",repANhelp2(ezOut,Effect,"DFn",digits=DoFd),", ",repANhelp2(ezOut,Effect,"DFd",digits=DoFd),") = ", repANhelp2(ezOut,Effect,"F",digits=Fd), ", p ", repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),", e2p ", repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),", e2g ", repANhelp2(ezOut,Effect,"ges",digits=e2d,cutoff=e2c))
      list(txtout)
    },by="Effect"]
    if(genEs){
      ezOut$txxtHTML<-txtAn[,{
        txtout<-paste0("<em>F</em>(",repANhelp2(ezOut,Effect,"DFn",digits=DoFd),", ",repANhelp2(ezOut,Effect,"DFd",digits=DoFd),") = ", repANhelp2(ezOut,Effect,"F",digits=Fd), ", <em>p</em> ", repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),", <em>&eta;<sup>2</sup><sub>p</sub></em> ", repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),", <em>&eta;<sup>2</sup><sub>g</sub></em> ", repANhelp2(ezOut,Effect,"ges",digits=e2d,cutoff=e2c),repANhelp2(ezOut,Effect,"epsi",digits=epd,cutoff=epc))
        list(txtout)
      },by="Effect"]
    }else{
      ezOut$txxtHTML<-txtAn[,{
        txtout<-paste0("<em>F</em>(",repANhelp2(ezOut,Effect,"DFn",digits=DoFd),", ",repANhelp2(ezOut,Effect,"DFd",digits=DoFd),") = ", repANhelp2(ezOut,Effect,"F",digits=Fd), ", <em>p</em> ", repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),", <em>&eta;<sup>2</sup><sub>p</sub></em> ", repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),repANhelp2(ezOut,Effect,"epsi",digits=epd,cutoff=epc))
        list(txtout)
      },by="Effect"]
    }
    
  }
  return(ezOut)
}


#' Augment ezANOVA output
#' 
#' This function augments the output from an ezANOVA for easier pasting into a manuscript. Unlike ezA_VL, it invokes ezANOVA directly
#' 
#' @param dataT Data table to input.
#' @param dv Name of the column in data that contains the dependent variable. Values in this column must be numeric.
#' @param wid Name of the column in data that contains the variable specifying the case/Ss identifier. This should be a unique value per case/Ss.
#' @param within Names of columns in data that contain predictor variables that are manipulated (or observed) within-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param within_full Same as within, but intended to specify the full within-Ss design in cases where the data have not already been collapsed to means per condition specified by within and when within only specifies a subset of the full design.
#' @param within_covariates Names of columns in data that contain predictor variables that are manipulated (or observed) within-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param between Names of columns in data that contain predictor variables that are manipulated (or observed) between-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param between_covariates Names of columns in data that contain predictor variables that are manipulated (or observed) between-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param type Numeric value (either 1, 2 or 3) specifying the Sums of Squares “type” to employ when data are unbalanced (eg. when group sizes differ). type = 2 is the default because this will yield identical ANOVA results as type = 1 when data are balanced but type = 2 will additionally yield various assumption tests where appropriate. When data are unbalanced, users are warned that they should give special consideration to the value of type. type=3 will emulate the approach taken by popular commercial statistics packages like SAS and SPSS, but users are warned that this approach is not without criticism.
#' @param digits significant digits. Deprecated.
#' @param genEs Whether to print the generalized eta squared or not
#' @param DoFd decimal place for degrees of freedom
#' @param Fd decimal place for F stat
#' @param pd decimal place for p value
#' @param pc cutoff value for p value. (i.e. lower limit)
#' @param e2d decimal place for eta squared 
#' @param e2c cutoff value for eta squared. (i.e. lower limit)
#' @param epd decimal place for epsilon
#' @param epc cutoff value for epsilon
#' @return augmented list. The txxthtml is a printable an HTML table. Use in conjunction with xtable.
#' @examples
#' data("donn_ag",package="muCoVL")
#' donn_ag[,durF:=factor(dur)]
#' outt<-ezA_VL_Direct(data = donn_ag,dv = .(relE),wid = .(sub),within = .(dist,durF),between = .(Trial_Block,Group))
#' @export 

ezA_VL_Direct<-function(dataT,dv,wid,within=NULL,within_full=NULL,within_covariates=NULL,between=NULL,between_covariates=NULL,type=3,digits=2,genEs=F,DoFd=2,Fd=2,pd=3,pc=.001,e2d=3,e2c=.001,epd=3,epc=.001){
  uu<-try(eval(substitute(ezOut<-ezANOVA(data = dataT,dv = ddv,wid = wwid,within = wwithin,within_full = wwithin_full,within_covariates = wwithin_covariates,between = bbetween,between_covariates = bbetween_covariates,observed = NULL,diff = NULL,reverse_diff = NULL,type = type,detailed = T,return_aov = T),list(ddv=dv,wwid=wid,wwithin=within,wwithin_full=within_full,wwithin_covariates=within_covariates,bbetween=between,bbetween_covariates=between_covariates))))
  if(inherits(uu,"try-error")) stop("error in ezANOVA")
  ANOVAt<-data.table(ezOut$ANOVA,key="Effect")
  ANOVAt[,e2p:=(F*DFn/(F*DFn+DFd))]
  ezOut$ANOVA<-ANOVAt
  if(!is.null(ezOut$`Sphericity Corrections`)){
    corrdt<-data.table(ezOut$`Sphericity Corrections`,key="Effect")
    temp<-ANOVAt[corrdt]
    temp[,DFnGG:=ifelse(test = GGe<1,yes = GGe,no = 1)*DFn]
    temp[,DFdGG:=ifelse(test = GGe<1,yes = GGe,no = 1)*DFd]
    temp[,DFnHF:=ifelse(test = HFe<1,yes = HFe,no = 1)*DFn]
    temp[,DFdHF:=ifelse(test = HFe<1,yes = HFe,no = 1)*DFd]
    ezOut$corrDF<-temp
  }
  txtAn<-ANOVAt[,list(Effect)]
  ezOut$tabl<-txtAn[,list(DFn=repANhelp2(ezOut,Effect,"DFn",digits=DoFd),DFf=repANhelp2(ezOut,Effect,"DFd",digits=DoFd),Ff=repANhelp2(ezOut,Effect,"F",digits=Fd),pp=repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),e2p=repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),e2g= repANhelp2(ezOut,Effect,"ges",digits=e2d,cutoff=e2c)),by="Effect"]
  ezOut$txxt<-txtAn[,{
    txtout<-paste0("F(",repANhelp2(ezOut,Effect,"DFn",digits=DoFd),", ",repANhelp2(ezOut,Effect,"DFd",digits=DoFd),") = ", repANhelp2(ezOut,Effect,"F",digits=Fd), ", p ", repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),", e2p ", repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),", e2g ", repANhelp2(ezOut,Effect,"ges",digits=e2d,cutoff=e2c))
    list(txtout)
  },by="Effect"]
  if(genEs){
    ezOut$txxtHTML<-txtAn[,{
      txtout<-paste0("<em>F</em>(",repANhelp2(ezOut,Effect,"DFn",digits=DoFd),", ",repANhelp2(ezOut,Effect,"DFd",digits=DoFd),") = ", repANhelp2(ezOut,Effect,"F",digits=Fd), ", <em>p</em> ", repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),", <em>&eta;<sup>2</sup><sub>p</sub></em> ", repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),", <em>&eta;<sup>2</sup><sub>g</sub></em> ", repANhelp2(ezOut,Effect,"ges",digits=e2d,cutoff=e2c),repANhelp2(ezOut,Effect,"epsi",digits=epd,cutoff=epc))
      list(txtout)
    },by="Effect"]
  }else{
    ezOut$txxtHTML<-txtAn[,{
      txtout<-paste0("<em>F</em>(",repANhelp2(ezOut,Effect,"DFn",digits=DoFd),", ",repANhelp2(ezOut,Effect,"DFd",digits=DoFd),") = ", repANhelp2(ezOut,Effect,"F",digits=Fd), ", <em>p</em> ", repANhelp2(ezOut,Effect,"p",digits=pd,cutoff=pc),", <em>&eta;<sup>2</sup><sub>p</sub></em> ", repANhelp2(ezOut,Effect,"e2p",digits=e2d,cutoff=e2c),repANhelp2(ezOut,Effect,"epsi",digits=epd,cutoff=epc))
      list(txtout)
    },by="Effect"]
  }
  

  return(ezOut)
}

putInChunk<-function(strstr,cname="",options="None"){
  paste0("```{r ",cname,ifelse(test = options=="None",yes = "",no = paste0(",",options)),"}\n",strstr,"\n```\n\n")
}

#' Generate knitR script for descriptive statistics
#' 
#' This helper function prints the knitR code necessary to print the descriptive stats for all the specified factors in a data.table.
#' 
#' @inheritParams pairedCVLDT
#' @param levelV Heading level.A number between 2 and 5
#' @param file Name of the file to output to. If the file already exists, it will append _1 to the name.
#' @param alpha Significance level.
#' @return Prints the knitR code in the specified file.
#' @examples
#' data("donn_ag")
#' dataT<-donn_ag
#' facF<-"Trial_Block,Group,dist,dur"
#' levelV<-2
#' file<-""
#' DV<-"relE"
#' subF<-"sub"
#' alpha<-.05
#'
#' scriptDS(dataT = dataT,facF = facF,DV = DV,subF = subF,levelV = levelV,file=file)
#' @export

scriptDS<-function(dataT,facF,DV,subF,levelV=2,file="",alpha=.05){
  if(!(is.data.table(dataT))) stop("error: dataT not data.table")
  dataN<-deparse(substitute(dataT))
  if(!(is.character(subF))) stop("error: subject not character")
  if(!(length(subF)==1)) stop("error: subject length != 1")
  if(!(is.character(DV))) stop("error: DV not character")
  if(!(length(DV)==1)) stop("error: DV length !=1")
  if(!(is.character(facF))) stop("error: factors not character")  
  if(length(facF)==1){
    facFs<-facF
    facFl<-gsub(pattern = " ",replacement = "",x = facF)
    facFl<-unlist(strsplit(x = facFl,split = ","))
  }else{
    facFs<-paste0(facF,collapse=",")
    facFl<-facF
  }
  if(!(length(unique(c(facFl,DV,subF)))==length(c(facFl,DV,subF)))) stop("DV,facF and subF not mutually exclusive")
  if(!all(c(facFl,DV,subF) %in% names(dataT))) stop("DV,facF and subF are not all column names")
  fileC<-file
  count<-1
  if(!(file=="")){
    while(file.exists(fileC)){
      fileL<-unlist(strsplit(x = file,split = "."))
      if(length(fileL)==1) stop("Bad file name.")
      fileC<-paste0(fileL[-length(fileL)],"_",count,fileL[length(fileL)],collapse=".")
      count<-count+1
    }
  }
  file<-fileC
  cat("SCRIPT\n",file=file)
  for(i in 1:length(facFl)){
    factComb<-combn(x = facFl,m = i,simplify = F)
    for(j in length(factComb):1){
      facIn<-factComb[[j]]
      facOut<-facFl[!(facFl %in%  factComb[[j]] )]
      if(levelV<4){
        tee<-rep("#",times=levelV)
        tee<-paste0(tee,collapse = "")
        strstr<-paste0(tee," ",paste0(facIn,collapse = " and "),"\n\n")
      }else{
        strstr<-paste0("<h",levelV,">",paste0(facIn,collapse = " and "),"</h",levelV,">\n\n")
      }
      cat(strstr,file=file,append = T)
      strstr<-paste0("out",DV,i,"_",j,"<-effSizeVLDT(dataT=",dataN,",facS=\"",paste0(facIn,collapse=","),"\",DV=\"",DV,"\",subF=\"",subF,"\"",ifelse(test=length(facOut)>0,paste0(",otherF=\"",paste0(facOut,collapse=","),"\""),""),",alpha=",alpha,")\n")
      strstr<-paste0(strstr,"xtable(","out",DV,i,"_",j,")")
      strstr<-putInChunk(strstr = strstr,cname=paste0("ES",DV,"_",paste0(facIn,collapse = "_")),options = "results=\'asis\'")
      cat(strstr,file=file,append=T)
      if(levelV+1<4){
        tee<-rep("#",times=levelV+1)
        tee<-paste0(tee,collapse = "")
        strstr<-paste0(tee," Graph\n\n")
      }else{
        strstr<-paste0("<h",levelV+1,">","Graph","</h",levelV+1,">\n\n")
      }
      
      cat(strstr,file=file,append=T)
      xxG<-paste0(",xx=\"",facIn[1],"\"")
      groupFG<-NULL
      if(length(facIn)>1) groupFG<-paste0(",groupF=\"",facIn[2],"\"")
      facetFG<-NULL
      if(length(facIn)>2) facetFG<-paste0(",facetF=\"",paste0(facIn[-c(1,2)],collapse=","),"\"")
      otherFG<-NULL
      if(length(facOut)>0) otherFG<-paste0(",otherF=\"",paste0(facOut,collapse=","),"\"")
      DVG<-paste0(",DV=\"",DV,"\"")
      subFG<-paste0(",subF=\"",subF,"\"")
      strstr<-paste0("gra",DV,i,"_",j,"<-graDescVL(dataT=",dataN,xxG,groupFG,facetFG,otherFG,DVG,subFG,")\nprint(","gra",DV,i,"_",j,")\n")
      cat(putInChunk(strstr = strstr,cname=paste0("gra",DV,"_",paste0(facIn,collapse = "_")),options = "dev='svg'"),file=file,append=T)
    }
  }
  return(T)
}

#' Complete SPSS-like output for ANOVA
#' 
#' This function prints and returns the complete results from an ANOVA (ANOVA results, descriptive stats and test of simple effects)
#' 
#' @param dataT Data table to input.
#' @param dv Name of the column in data that contains the dependent variable. Values in this column must be numeric.
#' @param wid Name of the column in data that contains the variable specifying the case/Ss identifier. This should be a unique value per case/Ss.
#' @param within Names of columns in data that contain predictor variables that are manipulated (or observed) within-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param within_full Same as within, but intended to specify the full within-Ss design in cases where the data have not already been collapsed to means per condition specified by within and when within only specifies a subset of the full design.
#' @param within_covariates Names of columns in data that contain predictor variables that are manipulated (or observed) within-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param between Names of columns in data that contain predictor variables that are manipulated (or observed) between-Ss. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param between_covariates Names of columns in data that contain predictor variables that are manipulated (or observed) between-Ss and are to serve as covariates in the analysis. If a single value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param type Numeric value (either 1, 2 or 3) specifying the Sums of Squares “type” to employ when data are unbalanced (eg. when group sizes differ). type = 2 is the default because this will yield identical ANOVA results as type = 1 when data are balanced but type = 2 will additionally yield various assumption tests where appropriate. When data are unbalanced, users are warned that they should give special consideration to the value of type. type=3 will emulate the approach taken by popular commercial statistics packages like SAS and SPSS, but users are warned that this approach is not without criticism.
#' @param genEs Whether to print the generalized eta squared or not
#' @param DoFd decimal place for degrees of freedom
#' @param Fd decimal place for F stat
#' @param pd decimal place for p value
#' @param pc cutoff value for p value. (i.e. lower limit)
#' @param e2d decimal place for eta squared 
#' @param e2c cutoff value for eta squared. (i.e. lower limit)
#' @param epd decimal place for epsilon
#' @param epc cutoff value for epsilon
#' @param td decimal place for the t statistics
#' @param dd decimal place for Cohen's d
#' @param DescDigits decimal place for the descriptive statistics
#' @param Eng whether the output is in English (true) or French (false)
#' @param Std whether to report the standard deviation (true) or the standard error (false)
#' @param Unit measurement unit of the DV
#' @param printOut if True, print the results in the HTML format.
#' @param headerL header level for the printout. 
#' @return a list containing the results from the ezANOVA call as well as data tables that can be printed by kable
#' @export


anova_SPSS<-function(dataT,dv,wid,within=NULL,within_full=NULL,within_covariates=NULL,between=NULL,between_covariates=NULL,type=3,genEs=F,DoFd=2,Fd=2,pd=3,pc=.001,e2d=3,e2c=.001,epd=3,epc=.001,td=2,dd=3,DescDigits=2,Eng=T,Std=T,Unit=NULL,printOut=T,headerL=1){
  withins<-as.character(within)
  within_fulls<-as.character(within_full)
  betweens<-as.character(between)
  bbcov<-as.character(between_covariates)
  dddv<-as.character(dv)
  wwwid<-as.character(wid)
  
  uu<-try(eval(substitute(ezOut<-ezA_VL_Direct(data = dataT,dv = ddv,wid = wwid,within = wwithin,within_full = wwithin_full,within_covariates = wwithin_covariates,between = bbetween,between_covariates = bbetween_covariates,type = type,genEs=genEs,DoFd=DoFd,Fd=Fd,pd=pd,pc=pc,e2d=e2d,e2c=e2c,epd=epd,epc=epc),list(ddv=dv,wwid=wid,wwithin=within,wwithin_full=within_full,wwithin_covariates=within_covariates,bbetween=between,bbetween_covariates=between_covariates))))
  if(inherits(uu,"try-error")) stop("error in ezANOVA")
  if(printOut){
    if(headerL>5){
      headerL<-5
    }
    if(headerL<1){
      headerL<-1
    }
    cat(paste0(paste0(rep("#",times=headerL),collapse="")," ANOVA\n"))
    print(kable(ezOut$txxtHTML))
    cat("\n\n")
  }
  
  outObj<-list(ANOVA=ezOut$txxtHTML)
  caplon<-ezOut$tabl[!J("(Intercept)"),on="Effect"][,ppnum:=as.numeric(substring(pp,first = 2))][ppnum<.05]
  sigMains<-ezOut$tabl[!J("(Intercept)"),on="Effect"][,ppnum:=as.numeric(substring(pp,first = 2))][!grepl(":",Effect)]$Effect
  sigInts<-unique(unlist(strsplit(caplon[grepl(":",Effect)]$Effect,split = ":")))
  globDesc<- muCoVL::effSizeVLDT(dataT = dataT,DV = dddv,subF = wwwid,facS = paste0(c(betweens,withins),collapse = ","),Eng = Eng,Std = Std,Unit = Unit)
  if(length(sigMains)>0){
    descMains<-vector("list",length(sigMains))
    names(descMains)<-sigMains
    pairedMains<-vector("list",length(sigMains))
    names(pairedMains)<-sigMains
    for(iii in 1:(length(sigMains))){
      oothers<-setdiff(c(withins,betweens),sigMains[iii])
      descMains[[iii]]<-effSizeVLDT(dataT = dataT,DV = dddv,subF = wwwid,facS = sigMains[iii],otherF = paste0(oothers,collapse=","),Eng = Eng,Std = Std,Unit = Unit,digits = DescDigits)
      if(printOut){
        cat(paste0(paste0(rep("#",times=headerL),collapse="")," Descriptive Statistics on ",sigMains[iii],"\n"))
        print(kable(descMains[[iii]]))
        cat("\n\n")
      } 
      
      pairedMains[[iii]]<-pairedCVLDT(dataT=dataT,facF=sigMains[iii],aggF = paste0(oothers,collapse=","),subF = wwwid,DV = dddv,aovv = ezOut$aov,tDec=td,pDec=pd,pCut=pc,dDec=dd)
      if(printOut){
        cat(paste0(paste0(rep("#",times=headerL),collapse="")," Paired Comparison on ",sigMains[iii],"\n"))
        print(kable(pairedMains[[iii]][,list(Var1,Var2,NN,mDiff,semDiff,text=txxtHTML)]))
        cat("\n\n")
      } 
    }
    outObj$Descriptive<-descMains
    outObj$Paired<-pairedMains
  }
  if(length(sigInts)>0){
    for(iii in 1:(length(sigInts))){
      if(printOut){
        cat(paste0(paste0(rep("#",times=headerL),collapse="")," Simple Effects on ",sigInts[iii],"\n\n"))
      }
      levelsSE<-unique(dataT[[sigInts[iii]]])
      SEout<-vector("list",length(levelsSE))
      for(jjj in 1:(length(levelsSE))){
        if(printOut){
          cat(paste0(paste0(rep("#",times=headerL+1),collapse="")," ",sigInts[iii]," = ",levelsSE[jjj],"\n\n"))
        }
        dataT2<-dataT[J(levelsSE[jjj]),on=sigInts[iii]]
        if(is.element(sigInts[iii],withins)){
          withins2<-setdiff(withins,sigInts[iii])
          if(length(withins2)>0){
            code<-paste0(withins2,collapse=",")
            code<-paste0("within2=.(",code,")")
            eval(parse(text = code))
          }else{
            within2<-NULL
          }
          between2<-between
        }
        if(is.element(sigInts[iii],betweens)){
          betweens2<-setdiff(betweens,sigInts[iii])
          if(length(betweens2)>0){
            code<-paste0(betweens2,collapse=",")
            code<-paste0("between2=.(",code,")")
            eval(parse(text = code))
          }else{
            between2<-NULL
          }
          within2<-within
        }
        ww<-try(eval(substitute(ezOut2<-anova_SPSS(data = dataT2,dv = ddv,wid = wwid,within = wwithin,within_full = wwithin_full,within_covariates = wwithin_covariates,between = bbetween,between_covariates = bbetween_covariates,type = type,genEs=genEs,DoFd=DoFd,Fd=Fd,pd=pd,pc=pc,e2d=e2d,e2c=e2c,epd=epd,epc=epc,td=td,dd=dd,DescDigits=DescDigits,Eng=Eng,Std=Std,Unit=Unit,printOut=printOut,headerL=headerL+2),list(ddv=dv,wwid=wid,wwithin=within2,wwithin_full=within_full,wwithin_covariates=within_covariates,bbetween=between2,bbetween_covariates=between_covariates))))
        if(inherits(ww,"try-error")) stop("error in ezANOVA")
        SEout[[jjj]]<-ezOut2
        names(SEout)[jjj]<-paste0(sigInts[iii],"_",levelsSE[jjj])
      }
      outObj[[length(outObj)+1]]<-SEout
      names(outObj)[length(outObj)]<-paste0("SimpleEff_",sigInts[iii])
    }
  }
  
  return(outObj)
}


#' Create APA style tables
#' 
#' This function formats a table in HTML format that is ready for publication.
#' 
#' @param DataT Data.table containing the data
#' @param bys vector of the names of the columns in DataT that will form the row names. Slowest to fastest varying columns.
#' @param DVcol string variable containing the name of the columns in DataT that contain the DVs. Single string of names separated by commas.
#' @param nameIV string variable containing the name of the independant Variable.
#' @param nameslvl1 vector of string variables giving the label of each column. If two column label cells are merged, then put NA for the second column's label. 
#' @param nameslvl2 vector of string variables giving the label of each column at the second level of headings. If two column label cells are merged, then put NA for the second column's label. (optional)
#' @param nameslvl3 vector of string variables giving the label of each column at the third level of headings. If two column label cells are merged, then put NA for the second column's label. (optional)
#' @param tablew width of the table, either in pixels.
#' 
#' @return string variable containing the html code for the table.
#' 
#' @export

tableAPA<-function(DataT,bys,DVcol,nameIV,nameslvl1,nameslvl2=NULL,nameslvl3=NULL,tablew="100%"){
  bys<-bys[length(bys):1]
  if(length(DVcol)>1){
    DVc<-c()
    nn<-1
    for(i in 1:(length(DVcol)-1)){
      DVc<-c(DVc,DVcol[nn])
      nn<-nn+1
      DVc<-c(DVc,"\"\"")
    }
    DVc<-c(DVc,DVcol[nn])
  }else{
    DVc<-DVcol
  }
  if(!is.null(nameslvl1)){
    if(length(nameslvl1)>1){
      nl1<-c()
      for(i in 1:(length(nameslvl1)-1)){
        nl1<-c(nl1,nameslvl1[i])
        if(!is.na(x = nameslvl1[i+1])){
          nl1<-c(nl1,"")
        }else{
          nl1<-c(nl1,NA)
        }
      }
      nl1<-c(nl1,nameslvl1[length(nameslvl1)])
      nn<-1
      nNA<-0
      nl1p<-c()
      for(i in 1:(length(nl1)-1)){
        if(is.na(nl1[i+1])){
          nNA<-nNA+1
        }else{
          nl1p<-c(nl1p,nNA+1)
          nNA<-0
        }
      }
      nl1p<-c(nl1p,nNA+1)
    }else{
      nl1<-nameslvl1
      nl1p<-1
    }
    
  }
  nl1<-nl1[!is.na(nl1)]
  
  if(!is.null(nameslvl2)){
    if(length(nameslvl2)>1){
      nl2<-c()
      for(i in 1:(length(nameslvl2)-1)){
        nl2<-c(nl2,nameslvl2[i])
        if(!is.na(x = nameslvl2[i+1])){
          nl2<-c(nl2,"")
        }else{
          nl2<-c(nl2,NA)
        }
      }
      nl2<-c(nl2,nameslvl2[length(nameslvl2)])
      nn<-1
      nNA<-0
      nl2p<-c()
      for(i in 1:(length(nl2)-1)){
        if(is.na(nl2[i+1])){
          nNA<-nNA+1
        }else{
          nl2p<-c(nl2p,nNA+1)
          nNA<-0
        }
      }
      
      nl2p<-c(nl2p,nNA+1)
      
    }else{
      nl2<-nameslvl2
      nl2p<-1
    }
    nl2<-nl2[!is.na(nl2)]
    nl2<-c("","",nl2)
    nl2p<-ifelse(nl2p>1,yes = paste0(" colspan=\"",nl2p,"\" style=\"border-bottom: 1px solid black\""),no = "")
    nl2p<-c(paste0(" colspan=\"",length(bys),"\"",collapse=""),"",nl2p)
  }
  
  
  if(!is.null(nameslvl3)){
    if(length(nameslvl3)>1){
      nl3<-c()
      for(i in 1:(length(nameslvl3)-1)){
        nl3<-c(nl3,nameslvl3[i])
        if(!is.na(x = nameslvl3[i+1])){
          nl3<-c(nl3,"")
        }else{
          nl3<-c(nl3,NA)
        }
      }
      nl3<-c(nl3,nameslvl3[length(nameslvl3)])
      nn<-1
      nNA<-0
      nl3p<-c()
      for(i in 1:(length(nl3)-1)){
        if(is.na(nl3[i+1])){
          nNA<-nNA+1
        }else{
          nl3p<-c(nl3p,nNA+1)
          nNA<-0
        }
      }
      
      nl3p<-c(nl3p,nNA+1)
      
    }else{
      nl3<-nameslvl3
      nl3p<-1
    }
    nl3<-nl3[!is.na(nl3)]
    nl3<-c("","",nl3)
    nl3p<-ifelse(nl3p>1,yes = paste0(" colspan=\"",nl3p,"\" style=\"border-bottom: 1px solid black\""),no = "")
    nl3p<-c(paste0(" colspan=\"",length(bys),"\"",collapse=""),"",nl3p)
  } 
  nl1<-c(nameIV,"",nl1)
  nl1cp<-sum(nl1p)
  nl1p<-ifelse(nl1p>1,yes = paste0(" colspan=\"",nl1p,"\" style=\"border-bottom: 1px solid black\""),no = "")
  nl1p<-c(paste0(" colspan=\"",length(bys),"\"",collapse=""),"",nl1p)
  
  
  
  strout<-paste0("<table width=\"",tablew,"\" style=\"border-bottom: 1px solid black;border-top: 1px solid black\"><colgroup><col span =\"",collapse="")
  strout<-paste0(strout,length(bys)+1,"\" style=\"text-align: left\"><col span =\"", nl1cp,"\" style=\"text-align: center\"></colgroup>",collapse="")
  if(!is.null(x = nameslvl3)){
    strout<-paste0(strout,"<tr>",paste0("<th",nl3p,">",nl3,"</th>",collapse = ""),"</tr>",collapse="")
  }
  if(!is.null(x = nameslvl2)){
    strout<-paste0(strout,"<tr>",paste0("<th",nl2p,">",nl2,"</th>",collapse = ""),"</tr>",collapse="")
  }
  strout<-paste0(strout,"<tr style =\"border-bottom: 1px solid black\">",paste0("<th",nl1p,">",nl1,"</th>",collapse = ""),"</tr>",collapse="")
  
  
  if(length(bys)==3){
    bys1<-bys[1]
    bys2<-bys[2]
    bys3<-bys[3]
    ccolspr<-c(" colspan=\"3\"","",rep("",times = length(DVc)))
    cccolspr<-c(""," colspan=\"2\"","",rep("",times = length(DVc)))
    ccolsline<-c(bys[3],"\"\"",rep("\"\"",times = length(DVc)))
    ccolslinep<-paste0("{ccols<-c(",paste0("as.character(",ccolsline,")",collapse = ","),")",collapse = "")
    ccolslinep<-c(ccolslinep,"ttemp<-paste0(\"<td\",ccolspr,\">\",ccols,\"</td>\",collapse = \"\")")
    ccolslinep<-c(ccolslinep,"strout<<-paste0(strout,\"<tr>\",ttemp,\"</tr>\",collapse=\"\")")
    ccolsline2<-c("\"\"",bys[2],"\"\"",rep("\"\"",times = length(DVc)))
    ccolslinep<-c(ccolslinep,paste0(".SD[,{cccols<-c(",paste0("as.character(",ccolsline2,")",collapse = ","),")",collapse = ""))
    ccolslinep<-c(ccolslinep,"tttemp<-paste0(\"<td\",cccolspr,\">\",cccols,\"</td>\",collapse = \"\")")
    ccolslinep<-c(ccolslinep,"strout<<-paste0(strout,\"<tr>\",tttemp,\"</tr>\",collapse=\"\")")
    ccolsline3<-c("\"\"","\"\"",bys[1],"\"\"",DVc)
    ccolslinep<-c(ccolslinep,paste0(".SD[,{ccccols<-c(",paste0("as.character(",ccolsline3,")",collapse = ","),")",collapse = ""))
    ccolslinep<-c(ccolslinep,"ttttemp<-paste0(\"<td>\",ccccols,\"</td>\",collapse = \"\")")
    ccolslinep<-c(ccolslinep,"strout<<-paste0(strout,\"<tr>\",ttttemp,\"</tr>\",collapse=\"\")},by=bys1]},by=bys2]}")
    ccolslinep<-paste0(ccolslinep,collapse = ";")
    DataT[,eval(parse(text=ccolslinep)),by=bys3]
  }else if(length(bys)==2){
    bys1<-bys[1]
    bys2<-bys[2]
    
    ccolspr<-c(" colspan=\"2\"","",rep("",times = length(DVc)))
    ccolsline<-c(bys[2],"\"\"",rep("\"\"",times = length(DVc)))
    ccolslinep<-paste0("{ccols<-c(",paste0("as.character(",ccolsline,")",collapse = ","),")",collapse = "")
    ccolslinep<-c(ccolslinep,"ttemp<-paste0(\"<td\",ccolspr,\">\",ccols,\"</td>\",collapse = \"\")")
    ccolslinep<-c(ccolslinep,"strout<<-paste0(strout,\"<tr>\",ttemp,\"</tr>\",collapse=\"\")")
    ccolsline3<-c("\"\"",bys[1],"\"\"",DVc)
    ccolslinep<-c(ccolslinep,paste0(".SD[,{ccccols<-c(",paste0("as.character(",ccolsline3,")",collapse = ","),")",collapse = ""))
    ccolslinep<-c(ccolslinep,"ttttemp<-paste0(\"<td>\",ccccols,\"</td>\",collapse = \"\")")
    ccolslinep<-c(ccolslinep,"strout<<-paste0(strout,\"<tr>\",ttttemp,\"</tr>\",collapse=\"\")},by=bys1]}")
    ccolslinep<-paste0(ccolslinep,collapse = ";")
    DataT[,eval(parse(text=ccolslinep)),by=bys2]
  }else if(length(bys)==1){
    bys1<-bys[1]
    ccolsline3<-c(bys[1],"\"\"",DVc)
    ccolslinep<-paste0("{ccccols<-c(",paste0("as.character(",ccolsline3,")",collapse = ","),")",collapse = "")
    ccolslinep<-c(ccolslinep,"ttttemp<-paste0(\"<td>\",ccccols,\"</td>\",collapse = \"\")")
    ccolslinep<-c(ccolslinep,"strout<<-paste0(strout,\"<tr>\",ttttemp,\"</tr>\",collapse=\"\")}")
    ccolslinep<-paste0(ccolslinep,collapse = ";")
    DataT[,eval(parse(text=ccolslinep)),by=bys1]
  }else{
    
  }
  
  strout<-paste0(strout,"</table>")
  return(strout)
}

#' Uneven polynomial contrasts
#' 
#' Returns a matrix of polynomial contrasts in accordance with a set of weights.
#' 
#' @param w vector of weights for the contrasts
#' @param scaled Whether the output should be scaled or not (boolean).
#' 
#' @return A matrix of polynomial contrasts.
#' 
#' @export

VL_unEvenPoly<-function(w,scaled=T){
  jj<-length(w)
  matt<-matrix(nrow=jj-1,ncol=jj,dimnames=list(paste0("x",1:(jj-1)),c()))
  for(ii in 1:(jj-1)){
    matt[ii,]<-w^ii
  }
  matt[1,]<-lm(matt[1,]~1)$res
  for(kk in 2:(jj-1)){
    fstr<-paste0("matt[",kk,",]<-lm(matt[",kk,",]~1+",paste0("matt[",1:(kk-1),",]",collapse="+"),")$res")
    eval(parse(text=fstr))
  }
  if(scaled){
    matt<-matt/(rowSums(x=abs(matt))/2)
  }
  return(matt)
}

#' Test of contrasts
#' 
#' Statistical test for a set of contrasts decomposing a main effect in an ANOVA.
#' 
#' @param aovl aov or aovlist object returned from an anova.
#' @param mEff name of the main effect on which the contrast is operated.
#' @param contt matrix for the contrast to be tested.
#' 
#' @return a data table including the results of the test on contrasts
#' @section References:
#' Baguley, T. (2012). Contrasts. Serious Stats: A guide to advanced statistics for the behavioral sciences. Palgrave MacMillan.
#' 
#' @export

vl_repCont2<-function (aovl, mEff, contt) 
{
  if (is.matrix(x = contt)) {
    NRR <- nrow(contt)
  }
  else {
    contt <- matrix(data = contt, nrow = 1, dimnames = list("x1", 
                                                            c()))
    NRR <- 1
  }
  dfC <- model.frame(aovl)
  dtC <- data.table(dfC)
  stopifnot(mEff %in% names(dfC))
  DVf <- names(dfC)[1]
  setkeyv(dtC,mEff)
  Fmeans <- dtC[, list(MeanF = colMeans(.SD)), by = mEff, .SDcols = DVf][, 
                                                                         MeanF]
  CCC <- contt %*% as.matrix(Fmeans)
  ngroups <- length(unique(dtC[, mEff, with = F])[[1]])
  errSt <- labels(terms(aovl))[grepl(pattern = "Error", x = labels(terms(aovl)), 
                                     fixed = T)]
  if (length(errSt) > 0) {
    if (grepl(pattern = "/", x = errSt, fixed = T)) {
      errSt2 <- strsplit(x = errSt, fixed = T, split = "/")[[1]][2]
      errSt <- strsplit(x = errSt, fixed = T, split = "/")[[1]][1]
    }
    posi <- rep(F, length(names(dfC)))
    posi2 <- posi
    posi3 <- posi
    for (i in 2:length(names(dfC))) {
      posi[i] <- grepl(pattern = names(dfC)[i], x = errSt, 
                       fixed = T)
    }
    for (i in 2:length(names(dfC))) {
      posi2[i] <- grepl(pattern = names(dfC)[i], x = errSt2, 
                        fixed = T)
    }
    posi3[1] <- T
    namS <- names(dfC)[posi]
    namRep <- names(dfC)[posi2]
    namNRep <- names(dfC)[!(posi | posi2 | posi3)]
    if (mEff %in% namRep) {
      SSerr <- sum(resid(aovl[names(aovl) == paste0(namS, 
                                                    ":", mEff)][[1]])^2)
      dfErr <- aovl[names(aovl) == paste0(namS, ":", mEff)][[1]]$df.residual
      numS <- length(unique(dtC[, namS, with = F])[[1]])
      SScont <- CCC^2 * numS/rowSums(contt^2)
      MSErr <- SSerr/dfErr
      Fcont <- SScont/MSErr
    }
    else {
      namNrepS <- c(namS, namNRep)
      eval(parse(text = paste0("dtCm<-dtC[,list(", DVf, 
                               "=mean(", DVf, ")),by=namNrepS]")))
      eval(parse(text = paste0("Fsd<-dtCm[,list(SdF=sd(", 
                               DVf, ")),by=mEff][,SdF]")))
      Nj <- dtCm[, .N, by = mEff][, N]
      Fcont <- CCC^2/rowSums(contt^2 * matrix(rep(Fsd^2/Nj, 
                                                  times = NRR), nrow = NRR, byrow = T))
      dfErr <- rowSums(contt^2 * matrix(rep(Fsd^2/Nj, times = NRR), 
                                        nrow = NRR, byrow = T))^2/rowSums(contt^4 * matrix(rep(Fsd^4/((Nj - 
                                                                                                         1) * Nj^2), times = NRR), nrow = NRR, byrow = T))
      numS <- length(unique(dtC[, namS, with = F])[[1]])/length(unique(dtC[, 
                                                                           mEff, with = F][[1]]))
    }
  }
  else {
    numS <- nrow(dtC)/length(unique(dtC[, mEff, with = F][[1]]))
    eval(parse(text = paste0("Fsd<-dtC[,list(SdF=sd(", DVf, 
                             ")),by=mEff][,SdF]")))
    Nj <- dtC[, .N, by = mEff][, N]
    Fcont <- CCC^2/rowSums(contt^2 * matrix(rep(Fsd^2/Nj, 
                                                times = nrow(contt)), nrow = nrow(contt), byrow = T))
    dfErr <- rowSums(contt^2 * matrix(rep(Fsd^2/Nj, times = nrow(contt)), 
                                      nrow = nrow(contt), byrow = T))^2/rowSums(contt^4 * 
                                                                                  matrix(rep(Fsd^4/((Nj - 1) * Nj^2), times = nrow(contt)), 
                                                                                         nrow = nrow(contt), byrow = T))
  }
  pcomp <- 1 - pf(Fcont, 1, dfErr)
  qobs <- sqrt(2 * Fcont)
  CohensD <- sqrt(2 * Fcont/numS)
  ptk <- 1 - ptukey(qobs, ngroups, (ngroups - 1) * (numS - 
                                                      1))
  r2al <- cor(x = t(contt), y = as.matrix(Fmeans))^2
  txtt <- paste0("C = ", roundVL(CCC), ", t(", roundVL(dfErr), 
                 ") = ", roundVL(sign(x = CCC) * sqrt(x = Fcont)), ", p = ", 
                 roundVL(pcomp,remZero=T), ", dC = ", roundVL(CohensD), ", r2al = ", 
                 roundVL(r2al))
  txttHTML <- paste0("<em>C</em> = ", roundVL(CCC), ", <em>t</em>(", 
                     roundVL(dfErr), ") = ", roundVL(sign(x = CCC) * sqrt(x = Fcont)), 
                     ", <em>p</em> = ", roundVL(pcomp,remZero=T), ", <em>d<sub>C</sub></em> = ", 
                     roundVL(CohensD), ", <em>r<sup>2</sup><sub>al</sub></em> = ", 
                     roundVL(r2al,remZero=T))
  out <- data.table(cName = dimnames(contt)[[1]], CCC, Fcont, 
                    dfErr, pcomp, ptk, CohensD, r2al, txtt, txttHTML)
  setnames(out, c("Cnames", "Cvalue", "Fstat", "DFe", "pval", 
                  "pvalTuk", "dC", "r2al", "textt", "txttHTML"))
  return(out)
}




#' Example data for muCoVL
#' 
#' A data set used for examples.
#' 
#' \itemize{
#' \item sub. Participant code.
#' \item Trial_Block. factor indicating whether the participant is in the Block or Trail condition
#' \item Group. Language group. French or Italian
#' \item dist. Number distribution. 3 for (1,2,3), 6 for (4,5,6) and 9 for (7,8,9)
#' \item dur. Target duration. 3000, 7000 or 10000 ms.
#' \item relE. Relative error.
#' \item cov. Coefficient of Variation.
#' \item CEr. Constant Error.
#' \item AEr. Absolute Error.
#' }
#' @docType data
#' @name donn_ag
#' @usage data("donn_ag")
#' @format data.table with 711 observations of 9 variables.
#' 

NULL

#' @import data.table
#' @import ggplot2
#' @import ez
#' @import lsmeans
#' @import plyr

NULL
