### abstract Graphic


library(xkcd)
library(lubridate)
library(dplyr)
library(extrafont)
library(ggplot2)

x <- 10:50

caseLine <- function(m,x,b ){
  
  err <- rnorm(n = length(x),mean = 0.1,sd = .05)
  
  value <- x^(m+err)+b
  return(value)
}


recorededCases <- caseLine(m = 1.2,x = x,b = 20)

x2 <- 50:90

predictedCases <- caseLine(m = 1.2,x = x2,b = 20)


rcDF <- tibble(x = as.Date((today()-40):today(),origin = "1970-01-01"), Cases =  recorededCases, type = "recorded" )


pcDF <- tibble(x = as.Date(today():(today()+40),origin = "1970-01-01"),Cases = predictedCases, type = "predicted")

simDF <- rbind(rcDF,pcDF)

xrange <- range(simDF$x)

yrange <- range(simDF$Cases)

rectX<- c(as.numeric(xrange),rev(as.numeric(xrange)))

rectY <- c(40,40,100,100,100,100,200,200,200,200,300,300,300,300,400,400)

rectDF <- tibble(x = rep(rectX,4),y = rectY, Burden = factor(rep(c("Low","Moderate","High","Very High"), each = 4),levels =c("Low","Moderate","High","Very High"))   )



toyPlot <- simDF %>% 
  ggplot(aes(x = as.numeric(x), y = Cases)) +
  geom_polygon(data = rectDF, aes(x = x, y= y, group = Burden, fill = Burden),position=position_jitter(w=0.02, h=0), alpha = .5) +
  scale_fill_brewer(palette = "YlOrRd") + 
  geom_point( aes(color = type), size = 3) +
    xkcdaxis(xrange = as.numeric(xrange), yrange = yrange) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = c(18550,18575,18600), labels = c("past","present","future")) +
  xlab("") 
 
toyPlot

ggsave(plot = toyPlot,filename = "./toyPlot.png",device = "png",width = 9, height = 3,bg = "transparent" )

# extrafont::font_import(paths = getwd(), pattern = "[X/x]kcd", prompt=FALSE)
# 
#  
#    extrafont::font_import(pattern = "",prompt = F)
# 
#  font_import(paths = "C:/Windows/Fonts",pattern = "xk")
