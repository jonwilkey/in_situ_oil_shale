# Script Info -------------------------------------------------------------
# Name:      ShinyPlot.R (In Situ Oil Shale v2 LHS Shiny Plot)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Description -------------------------------------------------------------

# This is the server script for a Shiny plot of the results from UO_main.R


# 1.1 Paths ---------------------------------------------------------------

# Predefine list object "path" for holding directory path listings
path <- NULL

# Path switch - uncomment and/or replace with the path directory for your local
# copy of the Git repository and Dropbox files.

pwd.drop <- "D:/"                       # Windows
pwd.git  <- "C:/Users/Jon/Documents/R/"
# pwd.drop <- "/Users/john/"              # Mac
# pwd.git  <- "/Users/john/Documents/"
# pwd.drop <- "~/"                        # Linux
# pwd.git  <- "~/Documents/R Projects/"

# Define paths.
# "raw"  is raw data (*.dbf files from DOGM, *.csv files, etc.).
# "data" is prepared data files (typically *.rda).
# "look" is lookup tables.
# "plot" is the directory for saving plot *.pdf files.
# "work" is the working directory where main.R and IO_options.R are located.
# "fun"  is the directory for all *.R functions.
path$raw   <- paste(pwd.drop, "Dropbox/Oil Shale/Raw Data", sep = "")
path$data  <- paste(pwd.drop, "Dropbox/Oil Shale/Prepared Data", sep = "")
path$plot  <- paste(pwd.drop, "Dropbox/Oil Shale/Plots", sep = "")
path$work  <- paste(pwd.git,  "oilshale/", sep = "")
path$fun   <- paste(pwd.git,  "oilshale/Functions", sep = "")
path$BCfig <- paste(pwd.drop, "Dropbox/Oil Shale/Book Chapter/Figures", sep = "")
path$Cdata <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Prepared Data", sep = "")

# Remove temporary path objects
remove(pwd.drop, pwd.git)


# 1.2 Functions -----------------------------------------------------------

# # List of functions used in this script to be loaded here
# flst <- file.path(path$fun, c())
#
# # Load each function in list then remove temporary file list variables
# for (f in flst) source(f); remove(f, flst)


# 1.3 Libraries -----------------------------------------------------------

library(zoo)
library(sqldf)
library(lhs)
library(shiny)


# 1.4 Options -------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# 1.5 Load results --------------------------------------------------------

# Load the results
load(file.path(path$data, "UO_main Results v1.rda"))


# 2.0 Shiny Server --------------------------------------------------------

# Define server logic required to draw scatterplot
shinyServer(function(input, output) {

  output$plot <- renderPlot({

    x <- results[,as.integer(input$select1)]
    y <- results[,as.integer(input$select2)]

    plot(x, y)
  })
})
