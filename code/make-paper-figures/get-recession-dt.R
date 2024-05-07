require(quantmod)
getSymbols("USREC", src = "FRED")

start <- zoo::index(USREC[which(diff(USREC$USREC)==1)])
end   <- zoo::index(USREC[which(diff(USREC$USREC)==-1)-1])

recessions <- data.table(start = start, end = end[-1])
