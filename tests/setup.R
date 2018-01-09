if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "ncvs" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available NCVS microdata files
ncvs_cat <-
	get_catalog( "ncvs" ,
		output_dir = file.path( getwd() ) )

# 2015 only
ncvs_cat <- subset( ncvs_cat , year == 2015 )
# download the microdata to your local computer


library(survey)

ncvs_df <- readRDS( file.path( getwd() , "2015 main.rds" ) )

ncvs_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = ncvs_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
ncvs_design <- 
	update( 
		ncvs_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( ncvs_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , ncvs_design , unwtd.count )
svytotal( ~ one , ncvs_design )

svyby( ~ one , ~ ever_smoked_marijuana , ncvs_design , svytotal )
svymean( ~ bmipct , ncvs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , ncvs_design , svymean , na.rm = TRUE )
svymean( ~ q2 , ncvs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , ncvs_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , ncvs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , ncvs_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , ncvs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , ncvs_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , ncvs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	ncvs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	ncvs_design ,
	na.rm = TRUE
)
sub_ncvs_design <- subset( ncvs_design , qn41 == 1 )
svymean( ~ bmipct , sub_ncvs_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , ncvs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		ncvs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ncvs_design )
svyvar( ~ bmipct , ncvs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , ncvs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , ncvs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , ncvs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , ncvs_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	ncvs_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		ncvs_design 
	)

summary( glm_result )
library(srvyr)
ncvs_srvyr_design <- as_survey( ncvs_design )
ncvs_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

ncvs_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_bike_helmet , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_bike_helmet ) ) )
 
svymean( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE , method = "beta" )

