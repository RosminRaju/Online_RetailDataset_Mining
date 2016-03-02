cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cat("++         ASSIGNMENT: Assignment #, Problem 3                 ++\n")
cat("++         AUTHOR: WENYU ZHANG                                 ++\n")
cat("++         SUID: 233508014                                     ++\n")
cat("++         DATE: 2/12/2016                                     ++\n")
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
#Step 1: Load the sales dataset into R
install.packages("plotrix")
require("plotrix")
require("ggplot2")
require("reshape2")
require("scales")
require("plyr")#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 2: Apply visualization technique #1: Pie Charts
product_amount_data <- tapply(sales_data$amount,sales_data$product,sum)
product_unit_data <- tapply(sales_data$unit,sales_data$product,sum)
par(mfrow=c(1,2))
pie3D(product_amount_data,
      col = c("lightblue", "mistyrose", "lightcyan", "lavender","cornsilk"),
      main = "Sales Amount of Products",
      explode = 0.1)
pie3D(product_unit_data,
    col = c("lightblue", "mistyrose", "lightcyan", "lavender","cornsilk"),
    main = "Sales Unit Sum of Products",
    explode = 0.1)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 3: Apply visualization technique #2: Bar plots
#Compare sales amount in different quarters.
quarter_data <- tapply(sales_data$amount/1000000,
    INDEX=list(sales_data$quarter,sales_data$year),
    FUN=sum)
quarter_amount_data <- tapply(sales_data$amount/1000000,sales_data$quarter,FUN=sum)
par(cex.axis=0.8,las = 1)
barplot(quarter_amount_data[order(quarter_amount_data,decreasing = TRUE)],
        horiz = TRUE,
        space = 0,
        col = c("lightblue", "mistyrose", "lightcyan", "lavender"),
        main = "Sales Amount in Different Quarters",
        xlab = "sales amount(Million)")
#Compare the quarter sales amount difference between 2013 and 2014
quarter_amount_2013 <- quarter_data[,"2013"]
quarter_amount_2014 <- quarter_data[,"2014"]
par(mfrow=c(1,2))
barplot(quarter_amount_2013[order(quarter_amount_2013,decreasing = TRUE)],
        horiz = TRUE,
        space = 0,
        col = c("lightblue", "mistyrose", "lightcyan", "lavender"),
        main = "Quarters Sales Amount of 2013",
        xlab = "sales amount(Million)")
barplot(quarter_amount_2014[order(quarter_amount_2014,decreasing = TRUE)],
        horiz = TRUE,
        space = 0,
        col = c("lightblue", "mistyrose", "lightcyan", "lavender"),
        main = "Quarters Sales Amount of 2014",
        xlab = "sales amount(Million)")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 4: Apply visualization technique #3: Line plot
month_data <- tapply(sales_data$amount, 
    INDEX=list(sales_data$mo,sales_data$year), 
    sum)
month_amount_data <- melt(month_data,
    id.var="mo",indices.name = "month", 
    value.name = "amount")
p <- ggplot(data = month_amount_data,
    aes(x=Var1,y=amount,group=Var2,colour=Var2))
(p + geom_point()+geom_line()
+ scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12))
+ xlab("Month") + ylab("Sales Amount") 
+ ggtitle("Sales Amount of Different Month"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 5: Apply visualization technique #4: Heatmap
heatmap_data <- tapply(sales_data$amount/2, 
                       INDEX=list(sales_data$mo,sales_data$product), sum)
heatmap_amount_data <- melt(heatmap_data,id.var="mo")
heatmap <- ddply(heatmap_amount_data, .(Var1), transform,
                 rescale = rescale(value))
(p <- ggplot(heatmap, aes(x=Var1, y=Var2, fill=value)) 
+ geom_tile(aes(fill = rescale),colour = "white") 
+ scale_fill_gradient(low = "white",high = "steelblue")
+ coord_fixed(ratio=1/1)
+ scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12)) 
+ xlab("Month") + ylab("Product") + ggtitle("Heatmap"))









