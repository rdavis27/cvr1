library(tidyverse)
library(shiny)
library(datasets)
library(ggplot2)
library(plotly)
ballottypediv <- 1
tabulatordiv <- 1
precinctdiv <- 1

# Replace 'server <- function(input, output) {'...'}' with
#       'shinyServer(function(input, output) {'...'})' if in separate server.R file
shinyServer(function(session, input, output) {
    getcvr <- reactive({
        if (!exists("cvr")){
            print("START read_csv")
            uu <- unzip("data/24G_CVRExport_NOV_Final_Confidential.zip",exdir = "data")
            cc <<- read_csv("data/24G_CVRExport_NOV_Final_Confidential.csv",skip = 3)
            print("STOP read_csv")
            cc$CvrNumber <- gsub("=","",cc$CvrNumber)
            cc$CvrNumber <- gsub("\"","",cc$CvrNumber)
            cc$CvrNumber <- as.numeric(cc$CvrNumber)
            cc$TabulatorNum <- gsub("=","",cc$TabulatorNum)
            cc$TabulatorNum <- gsub("\"","",cc$TabulatorNum)
            cc$TabulatorNum <- as.numeric(cc$TabulatorNum)
            cc$BallotType <- as.numeric(str_match(cc$BallotType,"\\d+")[,1])
            # cc$BallotType <- gsub("=","",cc$BallotType)
            # cc$BallotType <- gsub("\"","",cc$BallotType)
            cc$BallotType <- as.numeric(cc$BallotType)
            cc$BatchId <- gsub("=","",cc$BatchId)
            cc$BatchId <- gsub("\"","",cc$BatchId)
            cc$BatchId <- as.numeric(cc$BatchId)
            cc$RecordId <- gsub("=","",cc$RecordId)
            cc$RecordId <- gsub("\"","",cc$RecordId)
            cc$RecordId <- as.numeric(cc$RecordId)
            cc$ImprintedId <- gsub("=","",cc$ImprintedId)
            cc$ImprintedId <- gsub("\"","",cc$ImprintedId)
            names(cc)[which(names(cc) == "PrecinctPortion")] <- "Precinct" #rename PrecinctPortion
            cc$Precinct <- as.numeric(str_match(cc$Precinct,"\\d+")[,1])
            cc <- cc[,seq(1,26)]
            names(cc)[17:21] <- c("DEM","LPN","IAP","REP","OTH")
            cc$DEM <- as.numeric(cc$DEM)
            cc$LPN <- as.numeric(cc$LPN)
            cc$IAP <- as.numeric(cc$IAP)
            cc$REP <- as.numeric(cc$REP)
            cc$OTH <- as.numeric(cc$OTH)
            cc$DEM[is.na(cc$DEM)] <- 0
            cc$LPN[is.na(cc$LPN)] <- 0
            cc$IAP[is.na(cc$IAP)] <- 0
            cc$REP[is.na(cc$REP)] <- 0
            cc$OTH[is.na(cc$OTH)] <- 0
            #vote counts for Senator
            names(cc)[22:26] <- c("REP2","LPN2","IAP2","DEM2","OTH2")
            cc$DEM2 <- as.numeric(cc$DEM2)
            cc$LPN2 <- as.numeric(cc$LPN2)
            cc$IAP2 <- as.numeric(cc$IAP2)
            cc$REP2 <- as.numeric(cc$REP2)
            cc$OTH2 <- as.numeric(cc$OTH2)
            cc$DEM2[is.na(cc$DEM2)] <- 0
            cc$LPN2[is.na(cc$LPN2)] <- 0
            cc$IAP2[is.na(cc$IAP2)] <- 0
            cc$REP2[is.na(cc$REP2)] <- 0
            cc$OTH2[is.na(cc$OTH2)] <- 0
            write_csv(cc,"cvr1.csv")
            cvr <<- cc
            dd <- cc %>% group_by(cc$Precinct) %>%
                summarise(DEM=sum(DEM),LPN=sum(LPN),IAP=sum(IAP),REP=sum(REP),OTH=sum(OTH)) %>%
                gather(key = "Party",value = "Votes",DEM,LPN,IAP,REP,OTH)
            names(dd) <- c("precinct","party","votes")
            dd$county <- "Clark"
            #dd$precinct <- str_match(dd$precinct,"\\d+")[,1]
            dd$office <- "President of the US"
            dd$district <- ""
            dd$candidate <- "Other"
            dd$candidate[dd$party == "DEM"] <- "Harris"
            dd$candidate[dd$party == "REP"] <- "Trump"
            ee <- dd[,c(4,1,5,6,2,7,3)]
            ddpres <- ee
            
            dd <- cc %>% group_by(cc$Precinct) %>%
                summarise(DEM=sum(DEM2),LPN=sum(LPN2),IAP=sum(IAP2),REP=sum(REP2),OTH=sum(OTH2)) %>%
                gather(key = "Party",value = "Votes",DEM,LPN,IAP,REP,OTH)
            names(dd) <- c("precinct","party","votes")
            dd$county <- "Clark"
            #dd$precinct <- str_match(dd$precinct,"\\d+")[,1]
            dd$office <- "US Senator"
            dd$district <- ""
            dd$candidate <- "Other"
            dd$candidate[dd$party == "DEM"] <- "Rosen"
            dd$candidate[dd$party == "REP"] <- "Brown"
            dd <- dd[,c(4,1,5,6,2,7,3)]
            ee <- rbind(ee,dd)
            write_csv(ee,"20241105__nv__general__precinct.csv")

            dd <- ddpres                        
            dd$xx <- 0
            dd$yy <- 0
            dd$allreg <- 0
            aa <- read_csv("aareg25h.csv")
            aa$xx <- 100 * aa$REP / (aa$DEM+aa$REP)
            for (i in 1:NROW(dd)){
                dd$xx[i] <- aa$xx[dd$precinct[i] == aa$Precinct]
                dd$allreg[i] <- aa$TOTAL[dd$precinct[i] == aa$Precinct]
            }
            dd$yy <- 100 * dd$votes / dd$allreg
            write_csv(dd,"lutz.csv")
            
            #aa$ZOTH <- 100 * (dd$LPN + dd$IAP + dd$OTH) / dd$TOT
            bb <- aa %>% gather(key = "Party", value = "Vote", DEM, REP, NP, LPN, IAP, OTH)
            bb$county <- "Clark"
            bb$office <- "Registered"
            bb$district <- ""
            bb$candidate <- bb$Party
            bb <- bb[,c(6,1,7,8,4,9,5)]
            names(bb) <- c("county","precinct","office","district","party","candidate","votes")
            write_csv(bb,"20241105__nv__GEreg__precinct.csv")
            
            dd <- cc %>% group_by(cc$TabulatorNum) %>%
                summarise(DEM=sum(DEM),LPN=sum(LPN),IAP=sum(IAP),REP=sum(REP),OTH=sum(OTH),n=n())
            dd$dem_per <- 100*dd$DEM/(dd$DEM+dd$REP)
            dd$rep_per <- 100*dd$REP/(dd$DEM+dd$REP)
            write_csv(dd,"tabulator_table.csv")
        }
        return(cvr)
    })
    getdata <- reactive({
        cvr <- getcvr()
        cc <- cvr
        choices <- c("(all)",sort(unique(cc$CountingGroup)))
        selected <- choices[2]
        updateSelectInput(session = session,"voteType",choices = choices,selected = selected)
        #cc$BallotType <- as.numeric(str_match(cc$BallotType,"\\d+")[,1])
        choices <- sort(unique(cc$BallotType))
        choices <- gsub("=","",choices)
        choices <- gsub("\"","",choices)
        choices <- sort(unique(trunc(as.numeric(choices)/ballottypediv)))
        choiceslo <- c("(all)",choices)
        choiceshi <- c(" ",choices)
        selected <- "(all)"
        updateSelectInput(session = session,"ballottypelo",choices = choiceslo,selected = selected)
        updateSelectInput(session = session,"ballottypehi",choices = choiceshi,selected = " ")
        #choices <- c("(all)",sort(unique(cc$TabulatorNum)))
        choices <- sort(unique(cc$TabulatorNum))
        choices <- gsub("=","",choices)
        choices <- gsub("\"","",choices)
        choices <- sort(unique(trunc(as.numeric(choices)/tabulatordiv)))
        choiceslo <- c("(all)",choices)
        choiceshi <- c(" ",choices)
        selected <- "(all)"
        updateSelectInput(session = session,"tabulatorlo",choices = choiceslo,selected = selected)
        updateSelectInput(session = session,"tabulatorhi",choices = choiceshi,selected = " ")
        
        #cc$Precinct <- str_match(cc$PrecinctPortion,"\\d+")[,1]
        choices <- sort(unique(cc$Precinct))
        #choices <- gsub("=","",choices)
        #choices <- gsub("\"","",choices)
        choices <- sort(unique(trunc(as.numeric(choices)/precinctdiv)))
        choiceslo <- c("(all)",choices)
        choiceshi <- c(" ",choices)
        selected <- "(all)"
        updateSelectInput(session = session,"precinctlo",choices = choiceslo,selected = selected)
        updateSelectInput(session = session,"precincthi",choices = choiceshi,selected = " ")
        zdd1 <<- as.data.frame(cvr)
        return(cvr)
    })
    orderdf <- function(dd, sortcol, sortdesc){
        #DEBUG_FIX 250210 - fix by column name
        if (sortcol != 0){
            if (!sortdesc){
                dd <- dd[order(dd[[sortcol]]),] #DEBUG_FIX 250210
            }
            else{
                if (class(dd[[sortcol]]) == "numeric"){ #DEBUG_FIX 250210
                    dd <- dd[order(-dd[[sortcol]]),] #DEBUG_FIX 250210
                }
                else{
                    dd <- dd[order(dd[[sortcol]]),] #DEBUG_FIX 250210
                    dd <- dd %>% arrange(desc(row_number()))
                }
            }
        }
        return(dd)
    }
    addScales <- function(gg, xscale, yscale){
        xx <- NULL
        yy <- NULL
        if(xscale != ""){
            sxx <- unlist(strsplit(xscale, ","))
            xx <- as.numeric(sxx)
            if (length(sxx) == 3){
                gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                              minor_breaks = seq(xx[1],xx[2],xx[3]))
            }
            else if (length(sxx) == 4){
                gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                              minor_breaks = seq(xx[1],xx[2],xx[4]))
            }
        }
        if(yscale != ""){
            syy <- unlist(strsplit(yscale, ","))
            yy <- as.numeric(syy)
            if (length(syy) == 3){
                gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                              minor_breaks = seq(yy[1],yy[2],yy[3]))
            }
            else if (length(syy) == 4){
                gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                              minor_breaks = seq(yy[1],yy[2],yy[4]))
            }
        }
        if (length(xx) >= 2){
            if (length(yy) >= 2){
                gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
            }
            else{
                gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
            }
        }
        else if (length(yy) >= 2){
            gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
        }
        return(gg)
    }
    prepData <- function(dd){
        dd$Group0 <- 0
        if (!("(all)" %in% input$voteType)){
            dd <- dd[dd$CountingGroup %in% input$voteType,]
        }
        if (!("(all)" %in% input$ballottypelo)){
            lolimit <- as.numeric(input$ballottypelo)*ballottypediv
            if (input$ballottypehi == " "){
                hilimit <- (as.numeric(input$ballottypelo)+1)*ballottypediv
            }
            else{
                hilimit <- (as.numeric(input$ballottypehi)+1)*ballottypediv
            }
            dd <- dd[dd$BallotType >= lolimit & dd$BallotType < hilimit,]
        }
        if (!("(all)" %in% input$tabulatorlo)){
            lolimit <- as.numeric(input$tabulatorlo)*tabulatordiv
            if (input$tabulatorhi == " "){
                hilimit <- (as.numeric(input$tabulatorlo)+1)*tabulatordiv
            }
            else{
                hilimit <- (as.numeric(input$tabulatorhi)+1)*tabulatordiv
            }
            zlolimit <<- lolimit
            zhilimit <<- hilimit
            zdd0 <<- dd
            dd <- dd[dd$TabulatorNum >= lolimit & dd$TabulatorNum < hilimit,]
        }
        if (!("(all)" %in% input$precinctlo)){
            lolimit <- as.numeric(input$precinctlo)*precinctdiv
            if (input$precincthi == " "){
                hilimit <- (as.numeric(input$precinctlo)+1)*precinctdiv
            }
            else{
                hilimit <- (as.numeric(input$precincthi)+1)*precinctdiv
            }
            zlolimit <<- lolimit
            zhilimit <<- hilimit
            zdd0 <<- dd
            dd <- dd[dd$Precinct >= lolimit & dd$Precinct < hilimit,]
        }
        if (NROW(dd) > 0) dd$CvrNumber <- seq(1,NROW(dd)) #DEBUG_FIX 250222
        if (input$xlabel == "Bin"){
            if (input$xvar == "BallotType"){
                dd <- dd[with(dd, order(BallotType,CvrNumber)),]
            }
            else if (input$xvar == "Tabulator"){
                dd <- dd[with(dd, order(TabulatorNum,CvrNumber)),]
            }
            else if (input$xvar == "Precinct"){
                dd <- dd[with(dd, order(Precinct,CvrNumber)),]
            }
            else if (input$xvar == "TabulatorSum"){
                tdem <- 0
                trep <- 0
                utabs <- unique(dd$TabulatorNum)
                print("BEFORE for") #DEBUG_RM
                for (i in 1:length(utabs)){
                    tt <- dd[dd$TabulatorNum == utabs[i],]
                    sdem <- tt$DEM
                    srep <- tt$REP
                    ltdem <- length(tdem)
                    lsdem <- length(sdem)
                    if (ltdem < lsdem){
                        tdem <- c(tdem,rep(0,lsdem-ltdem))
                    }
                    else if (ltdem > lsdem){
                        sdem <- c(sdem,rep(0,ltdem-lsdem))
                    }
                    tdem <- tdem + sdem
                    
                    ltrep <- length(trep)
                    lsrep <- length(srep)
                    if (ltrep < lsrep){
                        trep <- c(trep,rep(0,lsrep-ltrep))
                    }
                    else if (ltrep > lsrep){
                        srep <- c(srep,rep(0,ltrep-lsrep))
                    }
                    trep <- trep + srep
                }
                ztdem <<- tdem
                ztrep <<- trep
                #dd <- dd[with(dd, order(TabulatorNum,CvrNumber)),]
                nn <- seq(1,length(tdem))
                dd <- data.frame(nn,tdem,trep)
                names(dd) <- c("NN","DEM","REP")
                dd$IAP <- 0
                dd$LPN <- 0
                dd$OTH <- 0
            }
            else{
                dd <- dd[with(dd, order(CvrNumber)),]
            }
            dd$Group <- seq(1:NROW(dd)) - 1
            dd$Group <- trunc(dd$Group/input$binsize) + 1
            dd$Group0 <- dd$Group
            zdd2 <<- as.data.frame(dd)
            # print(paste0("input$binsize=",input$binsize))
            # dd$Group <- trunc((seq(1:NROW(dd)) - 1)/input$binsize) + 1
        }
        else{
            if (input$xvar == "BallotType"){
                dd$Group <- dd$BallotType
            }
            else if (input$xvar == "Tabulator"){
                dd$Group <- dd$TabulatorNum
            }
            else if (input$xvar == "Precinct"){
                dd$Group <- dd$Precinct
            }
            else{
                dd$Group <- dd$CvrNumber
            }
            dd$Group0 <- dd$Group
            zdd2 <<- as.data.frame(dd)
        }
        dd9 <<- dd #DEBUG_RM
        dd <- dd %>% group_by(Group) %>%
            summarise(DEM=sum(DEM),REP=sum(REP),LPN=sum(LPN),IAP=sum(IAP),OTH=sum(OTH),
                      MIN0=min(Group0),MAX0=max(Group0),N=n())
        if (input$xlabel == "Index"){
            dd <- dd[with(dd, order(Group)),]
            if (NROW(dd) > 0) dd$Group <- seq(1,NROW(dd)) #DEBUG_FIX 250222
        }
        else if (input$xlabel == "N"){
            dd <- dd[with(dd, order(Group)),]
            ddpren <<- dd #DEBUG_TMP
            if (NROW(dd) > 0) dd$Group <- dd$N
        }
        ###dd$BallotType <- seq(1:NROW(dd)) # DEBUG_RENUM
        ee <<- dd #DEBUG_RM
        dd$TOT <- dd$DEM + dd$REP + dd$LPN + dd$IAP + dd$OTH
        dd$DEM <- 100 * dd$DEM / dd$TOT
        dd$REP <- 100 * dd$REP / dd$TOT
        if (input$incl_oth){
            dd$ZOTH <- 100 * (dd$LPN + dd$IAP + dd$OTH) / dd$TOT
            dd <- dd %>% gather(key = "Party", value = "Vote", DEM, REP, ZOTH)
        }
        else{
            dd <- dd %>% gather(key = "Party", value = "Vote", DEM, REP)
        }
        zdd3 <<- as.data.frame(dd)
        return(dd)
    }
    doHist <- function(){
        dd <- getdata()
        dd <- prepData(dd)
        hist(dd$Vote, col="red", breaks=seq(input$xminHist,input$xmaxHist,input$xstepHist),
             main="Vote Share by Frequency", xlab="Vote Share")
    }
    doPlot <- function(){
        dd <- getdata()
        dd <- prepData(dd)
        gg <- ggplot(data=dd, aes_string(x="Group",y="Vote"))
        #gg <- gg + geom_point(data=dd , aes_string(color="Party",shape="Party"),size=3, alpha=0.7)
        gg <- gg + geom_point(data=dd , aes_string(color="Party",size="TOT"), alpha=0.4)
        #gg <- gg + geom_point(data=dd ,aes_string(color="fcyl",shape="fcyl"), size=3, alpha=0.7)
        #gg <- gg + scale_size_continuous(breaks = seq(min(dd$TOT),max(dd$TOT),length.out=10))
        if (input$xlabel == "Bin"){
            sbinsize <- paste0(", binsize=", input$binsize)
            xlab <- paste0(input$xvar," (binned)")
        }
        else if (input$xlabel == "Index"){
            sbinsize <- ""
            xlab <- paste0(input$xvar," in ID order")
        }
        else if (input$xlabel == "N"){
            sbinsize <- ""
            xlab <- paste0(input$xvar," Count")
        }
        else{
            sbinsize <- ""
            xlab <- input$xvar
        }
        stabulator <- ""
        if (input$xvar == "Tabulator"){
            if (input$tabulatorhi == " "){
                stabulator <- paste0("Tabulator ",input$tabulatorlo)
            }
            else{
                stabulator <- paste0("Tabulator ",input$tabulatorlo,"-",input$tabulatorhi)
            }
        }
        sprecinct <- ""
        if (input$xvar == "Precinct"){
            if (input$precincthi == " "){
                sprecinct <- paste0("Precinct ",input$precinctlo)
            }
            else{
                sprecinct <- paste0("Precinct ",input$precinctlo,"-",input$precincthi)
            }
        }
        if (input$xlabel == "N") nplus <- " Count"
        else nplus <- ""
        title <- paste0("Clark County, NV: 2024 CVR: ",input$voteType,", ",
                        stabulator, sprecinct, sbinsize, ", order by ",input$xvar,nplus)
        gg <- gg + ggtitle(title)
        gg <- gg + xlab(xlab) + ylab("Vote Margin (percent)")
        xclr <- unlist(strsplit(input$xcolor,","))
        if (length(xclr) > 1){
            gg <- gg + scale_color_manual(values=xclr)
        }
        else{
            gg <- gg + scale_color_brewer(palette = xclr)
        }
        gg <- addScales(gg,input$xscale,input$yscale)
        gg <- gg + theme(axis.text=element_text(size = 12),
                         axis.title=element_text(size = 12,face = "bold"),
                         text = element_text(size = 12,face = "bold"))
        return(gg)
    }
    output$myPlot <- renderPlot({
        doPlot()
    },height = 600,width = 1500)
    output$myPlotly <- renderPlotly({
        doPlot()
    })
    output$myHist <- renderPlot({
        doHist()
    },height = 600,width = 1200)
    output$myVText <- renderPrint({
        cc <- getdata()
        dd <- as.data.frame(prepData(cc))
        dd <- orderdf(dd,input$xsortcolText,input$xsortdescText)
        return(dd)
    })
    output$myUsage <- renderUI({
        includeHTML("https://econdataus.com/voting_area_NV24.htm")
    })
}
)
