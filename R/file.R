#' get file
#' @param id The file ID.
#' @export
get_file <- function(id){
    keys <- .check_auth()
    key <- keys[[1]]
    api <- keys[[2]]

    ff <- GET(paste0("https://cgc-api.sbgenomics.com/v2/files/", id),
                add_headers("X-SBG-Auth-Token" = key,
                            "Content-Type" = "application/json"))
    ff <- content(ff)
    return(responseList(ff))
}

#' list folder
#' @param id The folder ID to list.
#' @param project The prject ID.
#' @param recursive Whether to list the folder recursively.
#' @return A data.frame of contents in the folder.
#' @export
list_folder <- function(id = NULL, project = NULL, recursive = FALSE){
    keys <- .check_auth()
    key <- keys[[1]]
    api <- keys[[2]]
    if(!is.null(project) & is.null(id)){
        url  <- paste0("https://", api, "-api.sbgenomics.com/v2/files?project=", project)
    }else{
        url <- paste0("https://", api, "-api.sbgenomics.com/v2/files/", id, "/list")
    }
    ff <- GET(url,
              add_headers("X-SBG-Auth-Token" = key,
                          "Content-Type" = "application/json"),
              query = list(offset = 0, limit = 100))
    ff <- content(ff)
    fcount <- length(ff$items)
    repeat{
        if(fcount > 0 && fcount %% 100 == 0){
            fff <- GET(url,
                       add_headers("X-SBG-Auth-Token" = key,
                                   "Content-Type" = "application/json"),
                       query = list(offset = fcount, limit = 100))
            fff <- content(fff)
            ff$items <- c(ff$items, fff$items)
            fcount  <- length(ff$items)
        }else{
            break
        }
    }
    items <- lapply(ff$items, unlist)
    fd <- data.frame(do.call(rbind, items))
    if(recursive){
        if(nrow(fd) > 0){
            for(i in seq(nrow(fd))){
                if(fd$type[[i]] == "folder"){
                    fd1 <- list_folder(id = fd$id[[i]], recursive = recursive)
                    ## fd$type[[i]] <- "Folder"
                    fd <- rbind(fd, fd1)
                }
            }
        }
    }
    ## fd <- data.frame(apply(fd, 2, unlist))
    return(fd)
}
