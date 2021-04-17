# Marco F.G. Gauger
# Data are taken from a recent publication of Gelippi et al. 2020. There we were able to show how isotop values of recently born grey whale calves changed  between threir strata. We dedused that this change is due to changes in diet. Using an isotopic clock model we were able to estimate the moment when lactating started hence the end of gestation (birth). We ilustrated the density plots of birthdates in two years (2018 & 2019) in a ciclic graph using the R package circlize. Here we adapted this visualization using the new package interacCircos that allows an interaction and included in a Shiny app.
# This app is meant to communicate our findings to a broader audience,  decision makers and other scientist interested in birth of baleen whales in the wild. 
#

Sys.setlocale("LC_ALL","English")

birth = 
  data.frame(YEAR =	c(2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2018,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019,	2019), yday =	c(30,	31,	43,	29,	30,	13,	35,	27,	30,	23,	27,	11,	55,	19,	22,	17,	29,	12,	50,	52,	52,	64,	64,	64,	12,	37,	39,	50,	36,	37,	27,	42,	41,	40,	27,	31,	18,	62,	26,	29,	24,	33,	19,	57,	59,	59,	72,	72,	71,	20))   

l_2019=density(as.numeric(birth$yday[birth$YEAR == "2019"]), na.rm=T)
l_2018=density(as.numeric(birth$yday[birth$YEAR == "2018"]), na.rm=T)

years = list("year" = 365)

require(scales)
require(interacCircos)
arcData = 
  data.frame(des = format(seq(as.Date("2000/1/1"),
                                    by = "month", length.out = 12), "%b"),
             start = c(0,as.integer(yday(seq(as.Date("2000/2/1"), 
                                         by = "month", length.out = 11)))),
             end = c(as.integer(yday(seq(as.Date("2000/2/1"), 
                           by = "month", length.out = 11))),366),
             color = rep(c("#000000","#CCD1D1"),6),
             chr = c("years"),
             stringsAsFactors = FALSE)
arcData2 = 
  data.frame(des = c("N Migration","North Pacific",
                     "S migration","Baja California"),
             start = as.integer(yday(as.Date(c("2000/4/1", "2000/05/31","2000/10/15","2000/12/15")))),
             end = as.integer(yday(as.Date(c("2000/06/01","2000/10/15","2000/12/15","2000/04/01")))+c(0,0,0,365)),
             color = c("#FCD975","#FF7F0E","#FCD975","#FF7F0E"),
             chr = c("years"),
             stringsAsFactors = FALSE)

arcData3 = 
  data.frame(des = c("birth","gestation"),
             start = as.integer(yday(as.Date(c("2000/12/15","2000/02/15")))+
                                  c(-365,0)),
             end = as.integer(yday(as.Date(c("2000/02/15","2000/12/15")))),
             color = c("#80DAEB","#1F75FE"),
             chr = c("years"),
             stringsAsFactors = FALSE)

arcData4 = 
  data.frame(des = c("start of lactation","lactation"),
             start = as.integer(yday(as.Date(c("2000/12/15","2000/02/15")))+
                                  c(-365,0)),
             end = as.integer(yday(as.Date(c("2000/02/15","2000/5/31")))),
             color = c("#FDFC74","#FFCF48"),
             chr = c("years"),
             stringsAsFactors = FALSE)

arcData5 = 
  data.frame(des = c("reproduction"),
             start = as.integer(yday(as.Date(c("2000/11/30")))+
                                  c(-365)),
             end = as.integer(yday(as.Date(c("2000/02/14")))),
             color = c("#EE204D"),
             chr = c("years"),
             stringsAsFactors = FALSE)

legend1 = list(type= "circle", color="#1E77B4",opacity="1.0",
               circleSize="8",text= "C.CK", textSize= "14",textWeight="normal")

str(arcData)
str(arcData2)

line2019 = 
  data.frame(chr = c("years"),
             pos = l_2019$x,
             value= l_2019$y,
             des = "birthdates 2019")
line2018 = 
  data.frame(chr = c("years"),
             pos = l_2018$x,
             value= l_2018$y,
             des = "birthdates 2018")

Circos(moduleList=CircosWig('LINE01', data = line2018,
                             maxRadius=170,minRadius=110,
                            color= alpha("darkgreen",.5),
                             animationDisplay = TRUE,animationTime = 4000,
                             animationDelay = 100) +
         CircosWig('LINE02', data = line2019,
                    maxRadius=170,minRadius=110,
                   color= alpha("darkred",.5),
                    animationDisplay = TRUE,animationTime = 4000,
                    animationDelay = 100)+
         CircosText('text01', '2019',
                    color = "darkred", 
                    x = -20, y = -140)  +
         CircosText('text02', '2018',
                    color = "darkgreen", 
                    x = 100, y = -100)  +
         CircosText('text03', 'lactation',
                    color = "black", 
                    x = -20, y = -75, size="0.8em") +
         CircosText('text04', 'birth',
                    color = "black", 
                    x = -10, y = -115, size="0.8em")  +
         CircosText('text01', 'reproduction',
                    color = "black",
                    x = -35, y = -30, size="0.8em", rotateRate=200) +
         CircosText('text04', 'Baja California',
                    color = "black", 
                    x = -30, y = -195, size="0.8em")  +
         CircosArc('Arc01', outerRadius = 210, 
                   innerRadius = 220, opacity =1,
                   data=arcData)+
         CircosArc('Arc02', outerRadius = 190, 
                   innerRadius = 175, opacity =.5, 
                   data=arcData2)+
         CircosArc('Arc03', outerRadius = 110, 
                   innerRadius = 90, opacity =.5, 
                   data=arcData3)+
         CircosArc('Arc04', outerRadius = 70, 
                   innerRadius = 50, opacity =.5, 
                   data=arcData4)+
         CircosArc('Arc05', outerRadius = 30, 
                   innerRadius = 10, opacity =.5, 
                   data=arcData5),
       genome=list("years" = 365),
       outerRadius = 216,genomeFillColor = c("black"),
       WIGMouseOverDisplay = TRUE,
       WIGMouseOverTooltipsSetting = "style1",WIGMouseOutDisplay = TRUE,
       WIGMouseOutFillColor = NULL,
       ARCMouseOverDisplay = TRUE,ARCMouseOverTooltipsSetting = "style1",
       ARCMouseOutDisplay = TRUE,ARCMouseOutColor = NULL, 
       LINEMouseOverDisplay = TRUE,LINEMouseOverTooltipsSetting = "style2",
       LINEMouseOutDisplay = TRUE,LINEMouseOutLineStrokeColor = NULL)

         