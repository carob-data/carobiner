
all_github_scripts <- function() {
	req <- httr::GET("https://api.github.com/repos/reagro/carob/git/trees/main?recursive=1")
	httr::stop_for_status(req)
	ff <- sapply(httr::content(req)$tree, \(i) i$path)
	ff <- grep("^scripts/", ff, value = TRUE)
	ff <- grep("\\.R$", ff, value = TRUE)
	ff <- ff[!grepl("_functions.R", basename(ff))]
	note <- rep("", length(ff))
	note[grepl("(^|/)_pending/", ff)]  <- "to be completed"
	note[grepl("(^|/)_rejected/", ff)] <- "REJECTED"
	uri <- .github_script_key(ff)
	data.frame(uri=uri, file=ff, note=note)
}


.github_script_key <- function(x) {
	x <- as.character(x)
	x <- gsub(" \\(.*\\)$", "", x)
	x <- gsub("\\.R$", "", x, ignore.case=TRUE)
	is_uri <- grepl("^(doi:|hdl:|https?://)", x, ignore.case=TRUE)
	x[!is_uri] <- basename(x[!is_uri])
	out <- x
	if (any(is_uri)) {
		out[is_uri] <- yuri::simpleURI(x[is_uri], warn=FALSE)
	}
	tolower(out)
}


on_github <- function(uri=NULL) {
	d <- all_github_scripts()
	if (is.null(uri)) {
		return(d[order(d$file), , drop=FALSE])
	}
	q <- .github_script_key(uri)
	i <- match(q, d$uri)
	invalid <- is.na(q)
	data.frame(
		uri = ifelse(invalid, NA_character_, q),
		file = ifelse(invalid | is.na(i), "", d$file[i]),
		note = ifelse(invalid, "not a valid URI",
		              ifelse(is.na(i), "not found", d$note[i]))
	)
}
