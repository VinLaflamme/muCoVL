% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/muCoVL.R
\name{vl_repCont2}
\alias{vl_repCont2}
\title{Test of contrasts}
\usage{
vl_repCont2(aovl, mEff, contt)
}
\arguments{
\item{aovl}{aov or aovlist object returned from an anova.}

\item{mEff}{name of the main effect on which the contrast is operated.}

\item{contt}{matrix for the contrast to be tested.}
}
\value{
a data table including the results of the test on contrasts
}
\description{
Statistical test for a set of contrasts decomposing a main effect in an ANOVA.
}
\section{References}{

Baguley, T. (2012). Contrasts. Serious Stats: A guide to advanced statistics for the behavioral sciences. Palgrave MacMillan.
}

