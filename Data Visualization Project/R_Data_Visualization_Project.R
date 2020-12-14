# Corruption and Human Development
# This assignment aims at using ggplot2 to recreate the "Corruption and Human Development" plot from The Economist (https://www.economist.com/graphic-detail/2011/12/02/corrosive-corruption)

# Import relevant libraries and load the csv file into a dataframe
library(ggplot2)
library(data.table)

df <- fread("Economist_Assignment_Data.csv",drop = 1)

pl <- ggplot(df,aes(x=CPI,y=HDI)) + geom_point(aes(color=factor(Region)),size=5,shape=1)+ geom_smooth(aes(group=1),method = 'lm',formula = y~log(x),se=FALSE,color='red') + scale_shape(solid=FALSE)


points.to.label <- c("Russia","Venezuela", "Iraq", "Myanmar", "Sudan",
                     "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                     "India", "Italy", "China", "South Africa", "Spane",
                     "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                     "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                     "New Zealand", "Singapore")
pl2 <- pl + geom_text(data=subset(df,Country %in% points.to.label,check_overlap=TRUE),aes(label=Country),color="gray20")

pl3 <- pl2 + xlab('CPI') + ylab('HDI') + theme_bw() + scale_x_continuous(name = 'Corruption Perceptions Index, 2011 (10=least corrupt)',limits = c(1,10),breaks = 1:10) + scale_y_continuous(name='Human Development Index, 2011 (1=Best)',limits = c(0.2,1.0))

pl4 <- pl3 + ggtitle('Corruption and Human Development')

print(pl4)

# Plotly
library(plotly)
gpl <- ggplotly(pl4)
print(gpl)
