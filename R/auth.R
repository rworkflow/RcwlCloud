#' Authorization
#' @param key The Authentication Token of your account from API.
#' @param api The cloud api, such as cgc or cavadica.
#' @import httr
#' @export
auth <- function(key, api = "cgc"){
    url <- paste0("https://", api, "-api.sbgenomics.com")

    a <- GET(paste0(url, "/v2/user"),
             add_headers("X-SBG-Auth-Token" = key,
                         "Content-Type" = "application/json"))
    a <- content(a)
    if("message" %in% names(a) && a$message == "Unauthorized"){
        stop("Unauthorized")
    }else{
        options(s_auth_key = key)
        options(s_api = api)
        return(responseList(a))
    }
}

.check_auth <- function(){
    key <- getOption("s_auth_key")
    api <- getOption("s_api")
    
    if(is.null(key)) stop("Please auth first.")
    return(list(key, api))
}
