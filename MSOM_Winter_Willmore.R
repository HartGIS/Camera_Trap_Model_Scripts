# Step 1: load these libraries, and install.packages(unmarked), etc. if you don't have them installed.

library(unmarked)

#################################################################

##MSOM

# STEP 2: load matrices and co-variate data, replace the file path with where you have saved these matrices that I will email you.

load(file = 'file location here')
load(file = 'file location here')
load(file = 'file location here')
load(file = 'file location here')


load(file = 'file location here')


# Step 3:
# Create 'y' list for the unmarkedFrameOccuMulti() 
y <- list(
  Fisher.matrix.numeric,
  Martin.matrix.numeric,
  redfox.matrix.numeric,
  Wolverine.matrix.numeric)

# rename these after their animal
names(y) <- c('Fisher','Marten', 'RedFox', 'Wolverine')

# Step 4:
# if you want to use landcover/elevation co-variates, run this. (Ignore warning message)
site.covs <- occ_covs
data <- unmarkedFrameOccuMulti(y=y, siteCovs=site.covs, obsCovs=NULL)

# Step 4:
# if you want to NOT use covariates, run this.
data <- unmarkedFrameOccuMulti(y=y, siteCovs=NULL, obsCovs=NULL)

# summary of data if you want to look at it
summary(data)

# plot data
plot(data)


# Step 5: Choose which 'occForumlas' you want to use, only run this once. Try the first one to start.
# M1 = species occur independently and the occupancy probabilities of each species are a function
#      of only landscape character (landcover).
occFormulas <- c('~LANDCOVER','~LANDCOVER','~LANDCOVER','~LANDCOVER','~0','~0','~0','~0','~0','~0', '~0','~0','~0', '~0', '~0')


# Pairwise Occupancies ** This is from the old matrices, the species and order have changed. **
#       
# Coyote-Redfox
occFormulas <- c('~1','~1','~1','~1','~LANDCOVER','~0','~0','~0','~0','~0','~0','~0','~0', '~0', '~0')
# Coyote-Wolf 
occFormulas <- c('~1','~1','~1','~1','~0','~LANDCOVER','~0','~0','~0','~0','~0','~0','~0', '~0', '~0')
# Coyote-Wolverine
occFormulas <- c('~1','~1','~1','~1','~0','~0','~LANDCOVER','~0','~0','~0','~0','~0','~0', '~0', '~0')
# Redfox-Wolverine
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~LANDCOVER','~0','~0','~0','~0', '~0', '~0')
# Wolf-Wolverine
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~0','~LANDCOVER','~0','~0','~0', '~0', '~0')
# Redfox-Wolf
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~0','~0','~LANDCOVER','~0','~0', '~0', '~0')


# no covariates
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~0','~0', '~0','~0','~0', '~0', '~0')

# Step 6: This only changes if you want to add detection co-variates, otherwise, it will always be the number of species (4 in this case).
#Length should match number/order of species in data@ylist
detFormulas <- c('~1','~1','~1','~1')

# Step 7: Run the OccuMulti function to create the model 'fit'.
# fit model
fit <- occuMulti(detFormulas,occFormulas,data)

#Look at output
fit

plot(fit)


# The stuff below is playing around with the conditional occupany code, we haven't figured out this yet. Remove "head" if you want the prob for all 59 sites.
######################################
summary(fit)
SE(fit)

names(fit)

#conditional occupancy
head(predict(fit,'state',species='Wolf',cond='Coyote')) #wolf | coyote present
head(predict(fit,'state',species='Wolf',cond='-Coyote')) #coyote absent


############# TODO:
# If there ARE covariates, then you need to specify the covariate values at which to make the 
# back transformation. You can do that using the linearComb function (see the unmarked manual here).
#Get the estimates for detection
backTransform(fit['det'])
#Get the estimates for occupancy
backTransform(fit['state'])
