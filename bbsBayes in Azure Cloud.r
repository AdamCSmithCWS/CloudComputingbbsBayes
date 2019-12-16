
#### running a hierarchical Bayesian model in the cloud

#Install devtools package, if not already installed
#install.packages("devtools")

# Install rAzureBatch package
devtools::install_github("Azure/rAzureBatch") 

# Install the doAzureParallel package 
devtools::install_github("Azure/doAzureParallel") 

# Load the doAzureParallel library 
library(doAzureParallel) 


#### go to www.azure.microsoft.com to set up your account
#you'll need a batch account and storage account


### follow the steps to get your credentials on this online tutorial
## and create the credentials.json file in your working directory
## this same tutorial will help you set up the cluster file (below)
### https://docs.microsoft.com/en-us/azure/batch/tutorial-r-doazureparallel


setCredentials("credentials.json")




## the cluster file (below) can be modified to suit, including identifying packages
# "cluster.json"
### from CRAN and GitHub
### defining the number of cores
### type of Virtual Machine,
### and the docker image 

# generate your cluster in the cloud; this takes a few minutes
cluster <- makeCluster("cluster.json") #this .json file currently includes instructions to use a rocker Bayesian image
# and to install bbsBayes

# Register your parallel backend 
registerDoAzureParallel(cluster) 

# Check that the nodes are running 
getDoParWorkers() 


#### analyses of BBS data using hierarchical Bayesian models in package bbsBayes

# devtools::install_github("BrandonEdwards/bbsBayes") #install bbsBayes package
 library(bbsBayes)

####################################
# 1: Fetch Data
####################################

# Fetch data from FTP site the package automatically saves a local copy and remembers its location
# This only needs to be run once on your computer
# you need to type yes after running the next line, to accept the BBS data disclaimer

bbs_data <- fetch_bbs_data()

####################################
# 2: Prepare Data
####################################

# Stratify the data using the CWS standard stratification (province/territory/state by BCR intersections with two modifications: BCR7 is all one stratum and Nova Scotia and PEI are combined into a single stratum)
strat = "bbs_cws"
bbs_strat <- stratify(by = strat)


# save(list = c("bbs_strat","strat"),
#      file = "stratified BBS data 2018 CWS.RData")
# you can optionally save these objects to the working directory, as a shortcut for a future use
# load("stratified BBS data 2018 CWS.RData")

SpeciesToRun = c("Bald Eagle","Canada Warbler",
        "Blackpoll Warbler","Eastern Meadowlark",
        "Lark Bunting","Common Nighthawk",
        "Olive-sided Flycatcher","Wood Thrush",
        "Hermit Thrush","Chestnut-collared Longspur") #species to run


functiontorun = function(species = ""){
  
  jags_data <- bbsBayes::prepare_jags_data(strat_data = bbs_strat,
                                 species_to_run = species,
                                 min_max_route_years = 3,
                                 #min_year = 1998,
                                 #max_year = 2018,
                                 model = "gam",
                                 n_knots = 15) ## prepares the data, here only running the last 21 years
  
  # bbsBayes version of the model
  M <- bbsBayes::run_model(jags_data = jags_data,
                   n_burnin = 20000,
                   n_thin = 10,
                   n_saved_steps=5000,
                   n_adapt = 1000,
                   parameters_to_save = c("n","beta","BETA")) #sets the parameters for the MCMC chains
  
  return(M)
}



opt <- list(wait = TRUE, #you can set this to false if you want to free-up your local machine, but you then have to frequently check back with the cluster to download the results
            enableCloudCombine = TRUE) #this combines the results into a single R-object list before downloading

number_of_species <- length(SpeciesToRun) #setting the number of iterations required, in this case the number of species I want to run

t1 = Sys.time()


results <- foreach(i = 1:number_of_species, 
                   .errorhandling = "pass", #this option passes any errors back to the console
                   .options.azure = opt) %dopar% {
                     
  # This code is executed, in parallel, across your cluster.
                     functiontorun(species = SpeciesToRun[i])
}


#

t2 = Sys.time()
t2-t1
### when finished, be sure to run this stopCluster function, so you avoid paying for idle virtual machines
stopCluster(cluster)



###################plotting


##############
altplot_cont_indices = function (indices = NULL, min_year = NULL, max_year = NULL, species = "", 
                                 title_size = 20, axis_title_size = 18, axis_text_size = 16) 
{
  Year <- NULL
  rm(Year)
  Index <- NULL
  rm(Index)
  Q25 <- NULL
  rm(Q25)
  Q975 <- NULL
  rm(Q975)
  if (!is.null(min_year)) {
    indices <- indices[which(indices$Year >= min_year), ]
  }
  if (!is.null(max_year)) {
    indices <- indices[which(indices$Year <= max_year), ]
  }
  p <- ggplot2::ggplot() + 
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                   axis.line = ggplot2::element_line(colour = "black"), 
                   plot.title = ggplot2::element_text(size = title_size), 
                   axis.title = ggplot2::element_text(size = axis_title_size), 
                   axis.text = ggplot2::element_text(size = axis_text_size)) + 
    ggplot2::labs(title = paste(species, "Continental", sep = ""), x = "Year", y = "Expected Mean Count") + 
    ggplot2::geom_point(data = mncounts, ggplot2::aes(x = year,y = mnobs),colour = grey(0.7))+
    ggplot2::geom_line(data = indices, ggplot2::aes(x = Year, y = Index),colour = "darkgreen") + 
    ggplot2::geom_ribbon(data = indices, ggplot2::aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12,fill = "darkgreen")
  return(p)
}




for(s in 7:number_of_species){
  ss = SpeciesToRun[s]
  
  jags_data <- bbsBayes::prepare_jags_data(strat_data = bbs_strat,
                                           species_to_run = ss,
                                           min_max_route_years = 3,
                                           #min_year = 1998,
                                           #max_year = 2018,
                                           model = "gam",
                                           n_knots = 13) ## prepares the data, here only running the last 21 years
  mnobs = tapply(jags_data$count,jags_data$r_year,mean,na.rm = T)
  mncounts = data.frame(year = as.integer(names(mnobs)),mnobs = mnobs)
cont_indices <- generate_cont_indices(results[[s]])
png(filename = paste0(ss,"continental indices.png"),
    res = 200,
    width = 2000,
    height = 2000)
c_plot <- altplot_cont_indices(cont_indices,species = ss)
print(c_plot)
dev.off()

}




##############
altplot_cont_indices = function (indices = NULL, min_year = NULL, max_year = NULL, species = "", 
          title_size = 20, axis_title_size = 18, axis_text_size = 16) 
{
  Year <- NULL
  rm(Year)
  Index <- NULL
  rm(Index)
  Q25 <- NULL
  rm(Q25)
  Q975 <- NULL
  rm(Q975)
  if (!is.null(min_year)) {
    indices <- indices[which(indices$Year >= min_year), ]
  }
  if (!is.null(max_year)) {
    indices <- indices[which(indices$Year <= max_year), ]
  }
  p <- ggplot2::ggplot() + 
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                                          panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                                          axis.line = ggplot2::element_line(colour = "black"), 
                                          plot.title = ggplot2::element_text(size = title_size), 
                                          axis.title = ggplot2::element_text(size = axis_title_size), 
                                          axis.text = ggplot2::element_text(size = axis_text_size)) + 
    ggplot2::labs(title = paste(species, "Continental", sep = ""), x = "Year", y = "Expected Mean Count") + 
    ggplot2::geom_point(data = mncounts, ggplot2::aes(x = year,y = mnobs),colour = grey(0.7))+
    ggplot2::geom_line(data = indices, ggplot2::aes(x = Year, y = Index),colour = "darkgreen") + 
    ggplot2::geom_ribbon(data = indices, ggplot2::aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12,fill = "darkgreen")
  return(p)
}



