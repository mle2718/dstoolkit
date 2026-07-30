#' Set up and Create, if necessary, Paths and Directories for your project
#'
#' Sets up paths for various developers.
#'
#' @dev_name Developer Initials.
#' @fishery Either flukeRDM or groundfishRDM.
#' @management_year the fishing year in which fishery management actions will take place
#' @details
#'
#' @return A file path to the data storage directory.
#'
#' @examples
#' developer_setup("mlee","groundfishRDM", 2028)#'
#' #> "C:/Users/min-yang.lee/Documents/dstoolkit/Data/2028_mgt_cycle"
#' @export
developer_setup<- function(dev_name, fishery, mangement_year) {

   stopifnot(dev_name %in% c("TP", "LCH", "ML", "KB"))
   stopifnot(fishery %in% c("flukeRDM", "groundfishRDM"))

   subf<-paste0(mangement_year,"_mgt_cycle")

   if (dev_name=="LCH"){
      out<-file.path("E:","Lou_projects",fishery,subf)

   } else if (dev_name %in% c("TP","ML", "KB")){
      out<-here("Data",subf)
   }
	 dir.create(out, showWarnings = TRUE, recursive=TRUE)
	 out
}


