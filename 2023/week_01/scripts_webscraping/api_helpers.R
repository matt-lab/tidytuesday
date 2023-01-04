library(httr) # for API help
library(jsonlite) # for JSON files

# Function to query ArkhamDB API
arkhamdb_api <- function(path) {
    url <- modify_url("https://arkhamdb.com", path = path)
    response <- GET(url)
    # Check response is JSON
    if (http_type(response) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }
    data <- fromJSON(content(response, "text"))
    Sys.sleep(2)

    structure(
        list(
            content = data,
            path = path,
            response = response),
        class = "arkhamdb_api")
}
print.arkhamdb_api <- function(x, ...) {
  cat("<ArkhamDB ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

print('arkhamdb_api has loaded')