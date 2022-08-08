## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
library(powdR)

data(minerals_xrd)

head(minerals_xrd)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
data(minerals_phases)

minerals_phases

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
identical(names(minerals_xrd[-1]),
          minerals_phases$phase_id)

## ----p1, fig.cap = "Plotting selected reference patterns from a powdRlib object.", message = FALSE, warning = FALSE, out.width='80%', fig.asp=.75, fig.align='center', cache = TRUE----
my_lib <- powdRlib(minerals_xrd, minerals_phases)

plot(my_lib, wavelength = "Cu",
     refs = c("ALB", "DOL.1",
              "QUA.1", "GOE.2"),
     interactive = FALSE)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
data(rockjock)

#Have a look at the phase IDs in rockjock
rockjock$phases$phase_id[1:10]

#Remove reference patterns from rockjock
rockjock_1 <- subset(rockjock,
                     refs = c("ALUNITE", #phase ID
                              "AMPHIBOLE", #phase ID
                              "ANALCIME", #phase ID
                              "Plagioclase"), #phase name
                     mode = "remove")

#Check number of reference patterns remaining in library
nrow(rockjock_1$phases)

#Keep certain reference patterns of rockjock
rockjock_2 <- subset(rockjock,
                     refs = c("ALUNITE", #phase ID
                              "AMPHIBOLE", #phase ID
                              "ANALCIME", #phase ID
                              "Plagioclase"), #phase name
                     mode = "keep")

#Check number of reference patterns remaining
nrow(rockjock_2$phases)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
#Load the minerals library
data(minerals)

#Check the number of reference patterns
nrow(minerals$phases)

#Load the rockjock library
data(rockjock)

#Check the number of reference patterns
nrow(rockjock$phases)

#interpolate minerals library onto same 2theta as rockjock
minerals_i <- interpolate(minerals, new_tth = rockjock$tth)

#merge the libraries
merged_lib <- merge(rockjock, minerals_i)

#Check the number of reference patterns in the merged library
nrow(merged_lib$phases)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
#Load the afsis library
data(afsis)

identical(rockjock$tth, afsis$tth)

rockjock_afsis <- merge(rockjock, afsis)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
data(rockjock_mixtures)

fit1 <- fps(lib = rockjock,
            smpl = rockjock_mixtures$Mix5,
            refs = c("ORDERED_MICROCLINE",
                     "Plagioclase",
                     "KAOLINITE_DRY_BRANCH",
                     "MONTMORILLONITE_WYO",
                     "CORUNDUM",
                     "QUARTZ"),
            std = "CORUNDUM",
            std_conc = 20,
            omit_std = TRUE,
            align = 0.3)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
summary(fit1)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
#All phases
fit1$phases

#Phases grouped and summed by the phase name
fit1$phases_grouped

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
sum(fit1$phases$phase_percent, na.rm = TRUE)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
fit1c <- close_quant(fit1)

sum(fit1c$phases$phase_percent, na.rm = TRUE)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
fit2 <- fps(lib = rockjock,
            smpl = rockjock_mixtures$Mix5,
            refs = c("ORDERED_MICROCLINE",
                     "Plagioclase",
                     "KAOLINITE_DRY_BRANCH",
                     "MONTMORILLONITE_WYO",
                     "CORUNDUM",
                     "QUARTZ"),
            std = "CORUNDUM",
            align = 0.3)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
fit2$phases

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
sum(fit2$phases$phase_percent)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
#Create a timestamp
a <- Sys.time()

fit2_n <- fps(lib = rockjock,
              smpl = rockjock_mixtures$Mix5,
              refs = c("ORDERED_MICROCLINE",
                       "Plagioclase",
                       "KAOLINITE_DRY_BRANCH",
                       "MONTMORILLONITE_WYO",
                       "CORUNDUM",
                       "QUARTZ"),
              solver = "NNLS",
              std = "CORUNDUM",
              align = 0.3)

#Calculate computation time
Sys.time() - a

## ---- eval = FALSE, message = FALSE, warning = FALSE, cache = TRUE------------
#  #Produce the fit
#  a_fit1 <- afps(lib = rockjock,
#                 smpl = rockjock_mixtures$Mix5,
#                 std = "CORUNDUM",
#                 align = 0.3,
#                 lod = 1)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
#View the phases of the fit1 output
fit1$phases

#Load the rockjock regrouping structure
data(rockjock_regroup)

#View the first 6 rows
head(rockjock_regroup)

#Regroup the data in a_fit1 using the coarsest description
fit1_rg <- regroup(fit1, rockjock_regroup[c(1,3)])

#Check the regrouped data
fit1_rg$phases_grouped

## ---- fig.cap = "Example output from plotting a powdRfps or powdRafps object.", message = FALSE, warning = FALSE, fig.align='center', cache = TRUE----
plot(fit1, wavelength = "Cu", interactive = FALSE)

## ---- fig.cap = "Plotting a powdRfps or powdRafps object with the reference patterns grouped.", message = FALSE, warning = FALSE, fig.align='center', cache = TRUE----
plot(fit1, wavelength = "Cu",
     group = TRUE,
     interactive = FALSE)

## ---- fig.cap = "Plotting the residuals of a powdRfps or powdRafps object.", message = FALSE, warning = FALSE, fig.align='center', cache = TRUE----
plot(fit1, wavelength = "Cu",
     mode = "residuals",
     interactive = FALSE)

## ---- fig.cap = "Plotting both the fit and residuals of a powdRfps or powdRafps object.", message = FALSE, warning = FALSE, fig.align='center', cache = TRUE----
plot(fit1, wavelength = "Cu",
     mode = "both", xlim = c(20,30),
     interactive = FALSE)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
multi_fit <- lapply(rockjock_mixtures[1:2], fps,
                    lib = rockjock,
                    std = "CORUNDUM",
                    refs = c("ORDERED_MICROCLINE",
                             "Plagioclase",
                             "KAOLINITE_DRY_BRANCH",
                             "MONTMORILLONITE_WYO",
                             "ILLITE_1M_RM30",
                             "CORUNDUM",
                             "QUARTZ"),
                    align = 0.3,
                    std_conc = 20,
                    omit_std = TRUE)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
identical(names(rockjock_mixtures[1:2]),
          names(multi_fit))

## ---- eval = FALSE------------------------------------------------------------
#  #Install the foreach and doParallel package
#  install.packages(c("foreach", "doParallel"))
#  
#  #load the packages
#  library(foreach)
#  library(doParallel)
#  
#  #Detect number of cores on machine
#  UseCores <- detectCores()
#  
#  #Register the cluster using n - 1 cores
#  cl <- makeCluster(UseCores-1)
#  
#  registerDoParallel(cl)
#  
#  #Use foreach loop and %dopar% to compute in parallel
#  multi_fit <- foreach(i = 1:2) %dopar%
#    (powdR::fps(lib = rockjock,
#                 smpl = rockjock_mixtures[[i]],
#                 std = "CORUNDUM",
#                 refs = c("ORDERED_MICROCLINE",
#                          "LABRADORITE",
#                          "KAOLINITE_DRY_BRANCH",
#                          "MONTMORILLONITE_WYO",
#                          "ILLITE_1M_RM30",
#                          "CORUNDUM",
#                          "QUARTZ"),
#                 align = 0.3))
#  
#  #name the items in the aquant_parallel list
#  names(multi_fit) <- names(rockjock_mixtures)[1:2]
#  
#  #stop the cluster
#  stopCluster(cl)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
summarise_mineralogy(multi_fit, type = "grouped", order = TRUE)

## ---- message = FALSE, warning = FALSE, cache = TRUE--------------------------
summarise_mineralogy(multi_fit, type = "grouped", order = TRUE,
                     rwp = TRUE, r = TRUE, delta = TRUE)

