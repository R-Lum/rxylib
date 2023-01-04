## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
library(powdR)

#Extract the path of an xy file from the powdR package
file <- system.file("extdata/D5000/xy/D5000_1.xy", package = "powdR")

#Load the file as an object called xy1
xy1 <- read_xy(file)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
#Explore the xy data
summary(xy1)

#check the class of xy data
class(xy1)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
methods(class = "XY")

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
#Extract the path of the file from the powdR package
file <- system.file("extdata/D5000/RAW/D5000_1.RAW", package = "powdR")

#Load the file as an object called xy2
xy2 <- extract_xy(file)

#Summarise the xy data
summary(xy2)

#Check the class of xy2
class(xy2)

## ----xy_list1, message=FALSE, warning=FALSE, cache = TRUE---------------------
#Extract the paths of the files
paths1 <- dir(system.file("extdata/D5000/xy", package = "powdR"),
             full.names = TRUE)

#Now read all files
xy_list1 <- read_xy(paths1)

#Check the class of xy_list1
class(xy_list1)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
#Check the class of each item within the multiXY object
lapply(xy_list1, class)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
#Summarise the data within the first sample:
summary(xy_list1$D5000_1)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
methods(class = "multiXY")

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
#Extract the paths of the files
paths2 <- dir(system.file("extdata/D5000/RAW", package = "powdR"),
              full.names = TRUE)

#Now read all files in the directory
xy_list2 <- extract_xy(paths2)

#Find out what the xy_list2 is
class(xy_list2)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
all.equal(xy_list1, xy_list2)

## ----p1, fig.cap = "An example figure created using the plot method for an XY object.", message=FALSE, warning=FALSE, out.width='80%', fig.asp=.75, fig.align='center', cache = TRUE----
plot(xy1, wavelength = "Co", interactive = FALSE)

## ---- fig.cap = "An example figure created using the plot method for a multiXY object.", message=FALSE, warning=FALSE, out.width='80%', fig.asp=.75, fig.align='center', cache = TRUE----
plot(xy_list1, wavelength = "Co")

## ---- fig.cap = "An example figure created using the plot method for an XY object with normalised count intensities and a restricted x-axis.", message=FALSE, warning=FALSE, out.width='80%', fig.asp=.75, fig.align='center', cache = TRUE----
plot(xy_list1, wavelength = "Co",
     xlim = c(30, 32), normalise = TRUE)

## ---- fig.cap = "A quartz diffractogram with the locations and relative intensities of the quartz peaks identified.", message=FALSE, warning=FALSE, out.width='80%', fig.asp=.75, fig.align='center', cache = TRUE----
#Define the relative intensities of quartz peaks
quartz <- data.frame("tth" = c(24.22, 30.99, 42.61, 46.12,
                               47.06, 49.62, 53.62, 58.86,
                               64.60, 65.18, 70.79, 73.68),
                     "intensity" = c(0.20, 1.00, 0.06, 0.06,
                                     0.03, 0.05, 0.03, 0.11,
                                     0.03, 0.01, 0.07, 0.03))

#Load the ggplot2 package
library(ggplot2)

#Create a plot called p1
p1 <- plot(xy1, wav = "Co", normalise = TRUE) +
           geom_point(data = quartz, aes(x = tth, y = intensity), size = 5,
             shape = 21, colour = "red") +
           ggtitle("A soil with quartz peaks identified") +
           theme_bw()

p1

## ---- eval = FALSE------------------------------------------------------------
#  library(plotly)
#  
#  ggplotly(p1)

## ---- fig.cap = "Diffractograms from two different instruments.", out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, warning=FALSE, cache = TRUE----
two_instruments <- as_multi_xy(list("a" = soils$granite,
                                    "b" = rockjock_mixtures$Mix2))

lapply(two_instruments, summary)

## ---- fig.cap = "Interpolated diffractograms from two different instruments.", out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, warning=FALSE, cache = TRUE----
new_tth <- seq(10, 60, 0.02)

two_instruments_int <- interpolate(two_instruments, new_tth)

lapply(two_instruments_int, summary)

## ---- fig.cap = "Unaligned diffractograms.", out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, warning=FALSE, cache = TRUE----
#Extract the location of the quartz xy file
quartz_file <- system.file("extdata/minerals/quartz.xy", package = "powdR")

#load the file
quartz <- read_xy(quartz_file)

#Plot the main quartz peak for pure quartz and a sandstone-derived soil
plot(as_multi_xy(list("quartz" = quartz,
                      "sandstone" = soils$sandstone)),
     wavelength = "Cu",
     normalise = TRUE,
     xlim = c(26, 27))

## ---- fig.cap = "Aligned diffractograms.", out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, warning=FALSE, cache = TRUE----
#Align the sandstone soil to the quartz pattern
sandstone_aligned <- align_xy(soils$sandstone, std = quartz,
                              xmin = 10, xmax = 60, xshift = 0.2)

#Plot the main quartz peak for pure quartz and a sandstone-derived soil
plot(as_multi_xy(list("quartz" = quartz,
                      "sandstone aligned" = sandstone_aligned)),
     wavelength = "Cu",
     normalise = TRUE,
     xlim = c(26, 27))

## ---- fig.cap = "Unaligned diffractograms in a multiXY object.", out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, warning=FALSE, cache = TRUE----
#Plot the unaligned soils data to show misalignments
plot(soils, wav = "Cu",
     xlim = c(26, 27), normalise = TRUE)

## ---- fig.cap = "Aligned diffractograms in a multiXY object.", out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, warning=FALSE, cache = TRUE----
#Align the sandstone soil to the quartz pattern
soils_aligned <- align_xy(soils, std = quartz,
                          xmin = 10, xmax = 60, xshift = 0.2)

#Plot the main quartz peak for pure quartz and a sandstone-derived soil
plot(soils_aligned,
     wavelength = "Cu",
     normalise = TRUE,
     xlim = c(26, 27))

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
#Convert xy_list1 to a dataframe
xy_df1 <- multi_xy_to_df(xy_list1, tth = TRUE)

#Show the first 6 rows of the derived data frame
head(xy_df1)

## ---- message=FALSE, warning=FALSE, cache = TRUE------------------------------
#Convert xy_df1 back to a list
back_to_list <- as_multi_xy(xy_df1)

#Show that the class is now multiXY
class(back_to_list)

## ---- fig.cap = "Data obtained from Co and Cu X-ray tubes prior to 2theta transformation.", out.width='80%', fig.asp=.75, fig.align='center', message=FALSE, warning=FALSE, cache = TRUE----
#Create a multiXY object for this transform example
transform_eg <- as_multi_xy(list("Co" = xy_list1$D5000_1,
                                 "Cu" = soils$sandstone))

#Plot two patterns recorded using different wavelengths
plot(transform_eg,
     wavelength = "Cu",
     normalise = TRUE,
     interactive = FALSE)

#transform the 2theta of the "Co" sample to "Cu"
transform_eg$Co$tth <- tth_transform(transform_eg$Co$tth,
                                     from = 1.78897,
                                     to = 1.54056)

#Replot data after transformation
plot(transform_eg,
     wavelength = "Cu",
     normalise = TRUE,
     interactive = FALSE)

