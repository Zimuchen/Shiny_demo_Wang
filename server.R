library(shiny)

function(input, output) {
  # if (input$dist != 'U2N'){
    output$plot <- renderPlot({
    if (input$dist == 'norm'){
      minx = do.call(paste("q",input$dist,sep=""),c(.0001,list(0,1)))
      maxx = do.call(paste("q",input$dist,sep=""),c(.9999,list(0,1)))
      x = seq(from=minx,to=maxx,length=1000)
      hx = x
      
      for(k in 1:1000){  
        hx[k] = do.call(paste("d",input$dist,sep=""),c(x[k],list(0,1)))
      }
      
      miny = 0
      miny = 0
      if (is.infinite(max(hx)) || max(hx)>1)
      {
        maxy = 1
      }else{
        maxy = round(max(hx),digits=2)      
      }
      
      plot(x,hx,type="n",xlab="X",ylab="Density",
           main="Probability Density",axes=FALSE,ylim=c(miny,maxy))
      lines(x,hx)
        # axis(1,pos=0,col.axis="grey",col.ticks="grey",col="grey")
      axis(1,pos=0)
      axis(2,at=round(seq(from=miny,to=maxy,length=5),digits=3),pos=minx)
      pval = do.call(paste("q",input$dist,sep=""),c(input$p,list(0,1)))
      if(input$sides=="left"){
        i = x<=pval
        polygon(c(minx,x[i],pval),c(0,hx[i],0),col="deepskyblue3")
        area = do.call(paste("p",input$dist,sep=""),c(pval,list(0,1)))
        result = paste("P(X<",signif(pval,digits=4),")=",signif(area,digits=3))
        mtext(result,3)
        axis(1,at=pval,pos=0,col.ticks="red",col.axis="red",lwd.ticks = 2,cex.axis=2)
      }else if(input$sides=="right"){
        i = x>=-pval
        polygon(c(-pval,x[i],maxx),c(0,hx[i],0),col="deepskyblue3")
        area = do.call(paste("p",input$dist,sep=""),c(pval,list(0,1)))
        result = paste("P(X>",signif(-pval,digits=4),")=",signif(area,digits=3))
        mtext(result,3)
        axis(1,at=-pval,pos=0,col.ticks="red",col.axis="red",lwd.ticks = 2,cex.axis=2)
      }else if(input$sides=="both"){
        tempval = do.call(paste("q",input$dist,sep=""),c(input$p/2,list(0,1)))
        i = (x<=tempval)
        j = (x>=-tempval)
        polygon(c(minx,x[i],tempval),c(0,hx[i],0),col="deepskyblue3")
        polygon(c(-tempval,x[j],maxx),c(0,hx[j],0),col="deepskyblue3")
      
      # area = (1-do.call(paste("p",input$dist,sep=""),c(input$p/2,list(0,1))))+
        # do.call(paste("p",input$dist,sep=""),c(input$p/2,list(0,1)))
        area = do.call(paste("p",input$dist,sep=""),c(pval,list(0,1)))
        result = paste("P(X < ",signif(tempval,digits=4)," or X > ",signif(-tempval,digits=4), ")=",signif(area,digits=3))
        mtext(result,3)
        axis(1,at=c(-tempval,tempval),pos=0,col.ticks="red",col.axis="red",lwd.ticks = 2,cex.axis=2)
      }
    } else if (input$dist == "t") {
      minx = do.call(paste("q","norm",sep=""),c(.0001,list(0,1)))
      maxx = do.call(paste("q","norm",sep=""),c(.9999,list(0,1)))
      x = seq(from=minx,to=maxx,length=1000)
      hx = x
      
      for(k in 1:1000){  
        hx[k] = do.call(paste("d","norm",sep=""),c(x[k],list(0,1)))
      }
      
      miny = 0
      miny = 0
      if (is.infinite(max(hx)) || max(hx)>1)
      {
        maxy = 1
      }else{
        maxy = round(max(hx),digits=2)      
      }
      
      plot(x,hx,type="n",xlab="X",ylab="Density",
           main="Probability Density",axes=FALSE,ylim=c(miny,maxy))
      lines(x,hx)
      axis(1,pos=0)
      axis(2,at=round(seq(from=miny,to=maxy,length=5),digits=3),pos=minx)
      # pval = do.call(paste("q",input$dist,sep=""),c(input$p,list(0,1)))


      mintx = do.call(paste("q","t",sep=""),c(.0001,list(input$t.df)))
      maxtx = do.call(paste("q","t",sep=""),c(.9999,list(input$t.df)))
      tx = seq(from=mintx,to=maxtx,length=1000)
      thx = tx
      
      for(k in 1:1000){  
        thx[k] = do.call(paste("d","t",sep=""),c(tx[k],list(input$t.df)))
      }
      
      tminy = 0
      tminy = 0
      if (is.infinite(max(thx)) || max(thx)>1)
      {
        tmaxy = 1
      }else{
        tmaxy = round(max(hx),digits=2)      
      }
      
      lines(tx,thx,col="blue")
    } ## end else if t
    })
  # } else if (input$type == "U2N") {
    output$plotunif <- renderPlot({
      unifdata <- runif(input$n, 0, 1)
      hist(unifdata, main=paste('runif(', input$n, ')', sep=''))
    })
    output$plotnorm <- renderPlot({
      unifdata <- runif(input$n, 0, 1)
      normdata <- qnorm(unifdata)
      hist(normdata, main=paste('rnorm(', input$n, ')', sep=''))
    })
  # }
}