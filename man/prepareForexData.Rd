% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataPrepare.R
\name{prepareForexData}
\alias{prepareForexData}
\title{Prepare all required datasets for FX daily Modeling}
\usage{
prepareForexData(ib.duration = "1 Y", ib.barsize = "1 day", Cur1 = "USD",
  Cur2 = "CAD", oanda.granularity = "D", QuandlSymbol1 = "CAN",
  QuandlSymbol2 = "USA", ibAcc = "paper")
}
\arguments{
\item{ib.duration}{length of data to retreive from Interactive Broker}

\item{ib.barsize}{the barsize of IB data}

\item{Cur1}{currency one}

\item{Cur2}{currency two}

\item{oanda.granularity}{barsize of Oanda data}

\item{QuandlSymbol1}{currency one for Quandl}

\item{QuandlSymbol2}{currency two for Quandl}

\item{ibAcc}{account type of Interactive Brokers (live or paper)}
}
\value{
A \code{list} of \code{xts}
}
\description{
Prepare all required datasets for FX daily Modeling.
}
\examples{
res = prepareForexData(ib.duration = "1 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD",
oanda.granularity = 'D', QuandlSymbol1 = 'CAN', QuandlSymbol2 = 'USA')

}
