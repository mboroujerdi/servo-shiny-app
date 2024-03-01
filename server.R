##shiny app for insulin regime By Massoud Boroujerdi (c)
##############################
rm(list=(ls(all=TRUE)))
library(lattice)
library(gridExtra)
library(deSolve)
#########################################################
server <- function(input, output) {

  ###################################################
  simulate<-function(mcr,zeta){
    library(deSolve)
    library(lattice)

    ####################################
    ############################
    ren <- function(x, y, z) {
      zz <- deparse(substitute(z))
      names(z)[match(x, names(z))] <- y
      assign(zz, z, env = parent.frame())
      invisible(NULL)
    }

    ##############################

      Mymodel <- c(
        ym <- 0,
        forcing<-1.0,
        input <- forcing,
        vs<-45.5,  ## accesible volume in ml/kg
        cr<-mcr/vs,
        omegan<-2*zeta*cr,
        k2<-2*zeta*omegan,
        k3<-omegan,
        k4<-k3,
        R1<-ym*cr
      )
      observtimes <- c(seq(from = 0, to = 200, by = 1))
      ###############################################################################
      # User specified function: Initialize the differential equations
      iniy <- function(parms) {
        # initialize
        with(as.list(parms), {
          y1 <- ym
          y2 <- y1
          y3<-y1
          y4<-y3*k4/k2
          # store results in ystart
          ystart <- c(y1,y2,y3,y4)

          return(ystart)
        })
      }
      ###################
      model <- function(t, y, parms) {
        y1 <- y[1]
        y2 <- y[2]
        y3 <- y[3]
        y4 <- y[4]
        with(as.list(parms), {
          ####################################################################
          ####################################################################
          ####### derivatives#################################################

          if (t > 100) input <- 0
          dy2dt <- -2 * zeta * omegan * y2 - omegan * omegan * y1 + input*omegan^2.0
          dy1dt <- y2
          dy3dt<- -k3*y4+R1+input*cr
          dy4dt<- k4*y3 -k2*y4
          res <- c(dy1dt,dy2dt,dy3dt,dy4dt)
          list(res)
        })
      }
      ###################

      out <- lsoda(
        y = iniy(Mymodel),
        times = observtimes,
        func = model,
        parms = Mymodel, hmax = 0.5
      )

      d2 <- as.data.frame(out) # Usage: ren("old_name", "new_name",dataframe_name)
      names1 <- colnames(d2)
      ren(names1[2], "y1", d2)
      ren(names1[3], "y2", d2)
      ren(names1[4], "y3", d2)
      ren(names1[5], "y4", d2)

    ####################################
    ####################################
    ###################################################
    pan2 <- function(x, y, groups, subscripts, ...) {
      panel.superpose(x, y,
                      col = c("blue"),
                      groups = groups, subscripts = subscripts, ...
      )

      ###########################
      panel.xyplot(d2$time, d2$y1, type = "l", col = "blue", lwd = 3,lty=1)
      panel.xyplot(d2$time, d2$y3, type = "l", col = "black", lwd = 3,lty=3)
      panel.xyplot(d2$time, d2$y4, type = "l", col = "black", lwd = 3,lty=4)

    }


    plotresponse<- xyplot(0 ~ 0,
                   panel = pan2, xlab = "Time", ylab = "Response", subscripts = TRUE,
                   groups = 2, type = "p", xlim = c(-2, 200), ylim = c(-1, 1.5),
                   key=list(text=list(c("Servo position So","Aaccessible compartment (S)",
                                        "Not accessible compartment (C)"),col=(c("blue","black","black"))),
                            lines=list(lty=c(1,2,4), lwd=3,col=c("blue","black","black"),
                                       divide=1, columns=2)))
    ############################################
    return(plotresponse)
  }

  ##########################################

  #######################################
  data1<-reactive({input$mcr})
  data2<-reactive({input$zeta})

 output$responseplot <- renderPlot({simulate(data1(),data2())})

  }
#########################################################

