
all_github_scripts <- function() {
	req <- httr::GET("https://api.github.com/repos/reagro/carob/git/trees/main?recursive=1")
	httr::stop_for_status(req)
	ff <- sapply(httr::content(req)$tree, function(i) i$path)
	ff <- grep("scripts/", ff, value = TRUE)
	ff <- grep("\\.R$", ff, value = TRUE)
	ff <- gsub("scripts/", "", ff)
	ff <- ff[!grepl("_functions.R", ff)]
	fb <- gsub("\\.R$", "", basename(ff))
	fd <- dirname(ff)	
	pd <- grepl("_pending", fd)
	fd <- gsub("_pending/", "", fd)
	data.frame(dataset=fb, group=fd, pending=pd)
}


on_github <- function(uri=NULL) {
	d <- all_github_scripts()
	if (!is.null(uri)) {
		uri <- yuri::simpleURI(uri)
		i <- stats::na.omit(match(uri, d$dataset))
		if (length(i) == 0) return("not found")
		return(d[i,])
	} 
	d <- d[order(d$group), ]
	d <- d[d$group != ".", ]
	d$group[d$group == "_pending"] <- ""
	d
}

