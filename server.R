#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Downloading data

data(GaltonFamilies)

## Here, we will use the following libraries:

library(shiny)
library(HistData)
library(dplyr)
library(ggplot2)

## Using data

GaltonFamilies <- GaltonFamilies
GaltonFamilies <- GaltonFamilies %>% mutate(father=father*2.54,
                    mother=mother*2.54,
                    childHeight=childHeight*2.54)

## Construction of the linear model

model1 <- lm(childHeight ~ father + mother + gender, data=GaltonFamilies)

shinyServer(function(input, output) {
    output$pText <- renderText({
        paste("Father's height is",
              strong(round(input$inFh, 1)),
              "cm, and mother's height is",
              strong(round(input$inMh, 1)),
              "cm, then:")
    })
    output$pred <- renderText({
        GaltonFamilies <- data.frame(father=input$inFh,
                         mother=input$inMh,
                         gender=factor(input$inGen, levels=levels(GaltonFamilies$gender)))
        childHeight1 <- predict(model1, newdata=GaltonFamilies)
        gender <- ifelse(
            input$inGen=="female",
            "Daugther",
            "Son"
        )
        paste0(em(strong(gender)),
               "'s predicted height is going to be around ",
               em(strong(round(childHeight1))),
               " cm"
        )
    })
    output$Plot <- renderPlot({
        gender <- ifelse(
            input$inGen=="female",
            "Daugther",
            "Son"
        )
        GaltonFamilies <- data.frame(father=input$inFh,
                         mother=input$inMh,
                         gender=factor(input$inGen, levels=levels(GaltonFamilies$gender)))
        childHeight1 <- predict(model1, newdata=GaltonFamilies)
        yvals <- c("Father", gender, "Mother")
        GaltonFamilies <- data.frame(
            x = factor(yvals, levels = yvals, ordered = TRUE),
            y = c(input$inFh, childHeight1, input$inMh))
        ggplot(GaltonFamilies, aes(x=x, y=y, color=c("red", "green", "blue"), fill=c("red", "green", "blue"))) +
            geom_bar(stat="identity", width=0.5) +
            xlab("") +
            ylab("Height (cm)") +
            theme_minimal() +
            theme(legend.position="none")
    })
})

