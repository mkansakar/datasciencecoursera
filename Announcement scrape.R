library(XML)
library(xts)

frm_ann <- 24893
to_ann  <- 26392
fl <- paste("announce_",frm_ann,"_to_",to_ann,".csv",sep="")
#c_name <- c("Symbol","Fiscal_Year","Announc_Detail","Announ_Date","Agenda","Bookclose","Cash_Dividend","Bonus_Share","Right_Ratio","Date","Venue","Time")
#write.table(c_name, fl, row.names = F, quote = F, sep = ",")
for (a in frm_ann:to_ann) { 
  tryCatch({
    url        <- paste("http://merolagani.com/AnnouncementDetail.aspx?id=",a,sep="")
    doc        <- htmlParse(url)
    tableNodes <- getNodeSet(doc, "//table")
    d1         <- readHTMLTable(tableNodes[[1]], header = F, which = 1)
    print(url)
    d1_t       <- data.frame(t(d1))
    d1_t       <- tail(d1_t,1)
    d1_t$SNo   <- a
    d1_t$X1    <- as.character(lapply(strsplit(as.character(d1_t[1,1])," "), "[", 1))
    d1_t$X2    <- as.character(gsub(","," ",d1_t$X2))
    d1_t$X3    <- as.character(gsub(","," ",d1_t$X3))
    d1_t$X4    <- as.character(gsub(","," ",d1_t$X4))
    d1_t$X6    <- as.character(gsub(","," ",d1_t$X6))
    d1_t$X7    <- strsplit(as.character(d1_t$X7)," .*")
    d1_t$X8    <- as.character(gsub(","," ",d1_t$X8))
    d1_t$X9    <- as.character(gsub(","," ",d1_t$X9))
    d1_t$X10   <- as.character(gsub(","," ",d1_t$X10))
    d1_t$X11   <- as.character(gsub(","," ",d1_t$X11))
    d1_t$X12   <- as.character(gsub(","," ",d1_t$X12))
    
    write.table(tb, fl, col.names = F,append = T, row.names=FALSE, na="NA",quote = F, sep = ",") 
    Sys.sleep(1)
  }, error=function(e){})
}
