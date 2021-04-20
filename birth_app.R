require(lubridate)
require(ggplot2)

Birth <- data.frame(YEAR =	as.factor(c(2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019)), julian_date =	c(30,	31,	43,	29,	30,	13,	35,	27,	30,	23,	27,	11,	55,	19,	22,	17,	29,	12,	50,	52,	52,	64,	64,	64,	12,	37,	39,	50,	36,	37,	27,	42,	41,	40,	27,	31,	18,	62,	26,	29,	24,	33,	19,	57,	59,	59,	72,	72,	71,	20))

r <- data.frame(x1 = c(-10),
                x2 = c(20),
                y1 = c(-1),
                y2 = c(0),
                t="grey")

arcData = 
  data.frame(des = format(seq(as.Date("2000/1/1"),
                              by = "month", length.out = 12), "%b"),
             start = c(0,as.integer(yday(seq(as.Date("2000/2/1"), 
                                             by = "month", 
                                             length.out = 11)))),
             end = c(as.integer(yday(seq(as.Date("2000/2/1"), 
                                         by = "month", length.out = 11))),
                     366), y1 =c(.11), y2=0.095,
             Color = rep(c("white","grey"),6),
             chr = as.factor(c("years")))
arcData2 = 
  data.frame(des = c("Baja","North","Bering",
                     "South","Baja"),
             start = as.integer(yday(as.Date(c("2000/1/1", "2000/4/1", "2000/05/31","2000/10/15","2000/12/15")))),
             end = as.integer(yday(as.Date(c("2000/04/01","2000/06/01","2000/10/15","2000/12/15","2000/12/31")))),
             y1 =c(.085), y2=0.07, 
             Angle = c(320,235,145,45,0),
             Color = c("#FF7F0E","#FCD975","#FF7F0E","#FCD975","#FF7F0E"),
             chr = c("years"),
             stringsAsFactors = FALSE)

arcData3 = 
  data.frame(des = c("birth","gestation","birth"),
             start = as.integer(yday(as.Date(c("2000/1/1","2000/02/15","2000/12/15")))),
             end = as.integer(yday(as.Date(c("2000/02/15","2000/12/15","2000/12/31")))),
             y1 =c(-.0), y2=-0.02,
             Color = c("#80DAEB","#1F75FE","#80DAEB"),
             Angle = c(340,150,0),
             chr = c("years"),
             stringsAsFactors = FALSE)

arcData4 = 
  data.frame(des = c("start of","lactation"),
             start = as.integer(yday(as.Date(c("2000/01/01","2000/02/15")))),
             end = as.integer(yday(as.Date(c("2000/02/15","2000/05/31")))),
             y1 =c(-.03), y2=-0.05,
             Color = c("#FDFC74","#FFCF48"),
             Angle = c(340,270),
             chr = c("years"))

arcData5 = 
  data.frame(des = c("repro\nduction"),
             start = as.integer(yday(as.Date(c("2000/1/1","2000/11/30")))),
             end = as.integer(yday(as.Date(c("2000/02/14","2000/12/31")))),
             y1 =c(-.06), y2=-.08,
             Color = c("#EE204D"),
             Angle = c(340),
             chr = c("years"),
             stringsAsFactors = FALSE)

#ggplot object

server <- function(input, output, session) {
  birth <- reactive({
    Birth <- data.frame(YEAR =	as.factor(c(2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019)), julian_date =	c(30,	31,	43,	29,	30,	13,	35,	27,	30,	23,	27,	11,	55,	19,	22,	17,	29,	12,	50,	52,	52,	64,	64,	64,	12,	37,	39,	50,	36,	37,	27,	42,	41,	40,	27,	31,	18,	62,	26,	29,	24,	33,	19,	57,	59,	59,	72,	72,	71,	20))
    (Birth[Birth$YEAR %in% input$YEAR,])
  })
  output$result <- 
    renderPlot(ggplot() + 
                 geom_rect(data=arcData, aes(xmin = start, xmax = end, ymin = y1, ymax = y2, fill=Color), alpha = 0.5) +
                 geom_text(data=arcData, aes(x = start+15, y = 0.102, label = des), size=2.5, angle= rev(seq(15,365+15, length=13)[1:12])) +
                 geom_rect(data=arcData2, aes(xmin = start, xmax = end, ymin = y1, ymax = y2, fill=Color), alpha = 0.5) +
                 geom_text(data=arcData2[1:4,], aes(x = c(45,120,220,315), y = 0.08, label = des), size=2.5, angle= arcData2$Angle[1:4]) +
                 geom_rect(data=arcData3, aes(xmin = start, xmax = end, ymin = y1, ymax = y2, fill=des), alpha = 0.5) +
                 geom_text(data=arcData3[1:2,], aes(x = c(15,215), y = -0.01, label = des), size=2.5, angle= arcData3$Angle[1:2]) +
                 geom_rect(data=arcData4[1:2,], aes(xmin = start, xmax = end, ymin = y1, ymax = y2, fill=Color), alpha = 0.5) +
                 geom_text(data=arcData4, aes(x = c(20,90), y = -0.04, label = des), size=2.5, angle= arcData4$Angle[1:2]) +
                 geom_rect(data=arcData5[1,], aes(xmin = start, xmax = end, ymin = y1, ymax = y2, fill=Color), alpha = 0.5) +
                 geom_text(data=arcData5, aes(x = c(20), y = -0.07, label = des), size=2.5, angle= arcData4$Angle[1]) + 
                 geom_density(data = birth(), aes(x =julian_date, fill=YEAR, group=YEAR), alpha=0.4)+ylim(-.1,0.11)+ 
                 xlim(0,366)+ scale_fill_brewer(palette = "Paired")+
                 coord_polar()+theme_void()+ theme(legend.position = "none")
               , res = 96)
}
# Run the application 
ui <- fluidPage(
  sidebarPanel(
    checkboxGroupInput("YEAR", 
                       label = "YEAR", unique(Birth$YEAR),
                       selected = unique(Birth$YEAR)),
  ),
  mainPanel(
    plotOutput("result", width = "100%", height = "500px")  
  )
)

shinyApp(ui = ui, server = server)

