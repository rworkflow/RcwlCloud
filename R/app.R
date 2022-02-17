#' get App details
#' @param project The project ID in the following format: {project_owner}/{project}.
#' @param app_id The ID for the app you are querying.
#' @export
get_app <- function(project, app_id){
    keys <- .check_auth()
    key <- keys[[1]]
    api <- keys[[2]]
    app1 <- GET(paste0("https://", api, "-api.sbgenomics.com"),
                add_headers("X-SBG-Auth-Token" = key,
                            "Content-Type" = "application/json"),
                path = paste("v2/apps", project, app_id, sep = "/"))
    app1 <- content(app1)
    return(responseList(app1))
}

#' add App using sbpack
#'
#' @param project TThe project ID in the following format: {project_owner}/{project}.
#' @param app The app JSON file to upload. Or a `cwlProcess` from `Rcwl`.
#' @param app_id The app ID.
#' @param api The cloud api, cgc or cavatica.
#' @param update_key Whether to update key.
#' @importFrom Rcwl writeCWL
#' @export
add_app <- function(project, app, app_id, api = "cgc", update_key = FALSE){
    keys <- .check_auth()
    key <- keys[[1]]
    api <- keys[[2]]
    
    if(update_key | !file.exists("~/.sevenbridges/credentials")){
        dir.create("~/.sevenbridges", showWarnings = FALSE)
        cre <- c(paste0("[", api, "]"),
                 paste0("api_endpoint = https://", api, "-api.sbgenomics.com/v2"),
                 paste0("auth_token = ", key))
        write(cre, "~/.sevenbridges/credentials", append = TRUE)
    }
    
    if(!file.exists(Sys.which("sbpack"))){
        cl <- basiliskStart(env_sbpack)
        basiliskStop(cl)
    }
    if(is.character(app) && file.exists(app)){
        app_file <- app
    }else if(is(app, "cwlProcess")){
        if(is(app, "cwlWorkflow")){
            app_file <- cwlpack(app, app_id)
        }else{
            app_file <- writeCWL(app, app_id, tempdir())[1]
        }
    }
    re <- system(paste0("sbpack ", api, " ", project, "/", app_id, " ", app_file))
    return(paste0(project, "/", app_id))
}


#' require sbpack to wrap cwl workflow
#' @param cwl The Rcwl object.
#' @param name The 
#' @export
cwlpack <- function(cwl, name = NULL){
    if(!file.exists(Sys.which("cwlpack"))){
        cl <- basiliskStart(env_sbpack)
        basiliskStop(cl)
    }

    if(is.null(name)){
        name = deparse(substitute(cwl))
    }
    app_file <- writeCWL(cwl, prefix = name)[1]
    tmp <- sub(".cwl", "_pack.cwl", app_file)
    re <- system(paste("cwlpack", app_file, ">", tmp))
    stopifnot(re == 0)
    return(tmp)
}
