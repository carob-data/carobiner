
.github_api_get <- function(url, query=NULL) {
	tok <- Sys.getenv("GITHUB_PAT")
	if (tok == "") tok <- Sys.getenv("GITHUB_TOKEN")
	hdr <- c(Accept = "application/vnd.github+json")
	if (tok != "") {
		hdr <- c(hdr, Authorization = paste("Bearer", tok))
	}
	httr::GET(url, query=query, httr::add_headers(.headers=hdr))
}


.github_pr_files <- function(pr_url) {
	r <- try(.github_api_get(paste0(pr_url, "/files"), query=list(per_page=100)), silent=TRUE)
	if (inherits(r, "try-error") || httr::status_code(r) != 200) {
		return(NULL)
	}
	sapply(httr::content(r), \(x) x$filename)
}


.github_open_pr_ids <- function(repo="carob-data/carob", last_pr=0) {
	prs <- list()
	page <- 1
	repeat {
		req <- try(.github_api_get(
			paste0("https://api.github.com/repos/", repo, "/pulls"),
			query=list(state="open", per_page=100, page=page)
		), silent=TRUE)
		if (inherits(req, "try-error") || httr::status_code(req) != 200) {
			if (page == 1) {
				return(list(ids=character(), last_pr=last_pr))
			}
			break
		}
		x <- httr::content(req)
		if (length(x) == 0 || is.null(x[[1]]$url)) {
			break
		}
		prs <- c(prs, x)
		if (length(x) < 100) {
			break
		}
		page <- page + 1
	}

	n <- sapply(prs, \(p) p$number)
	i <- which(!is.na(n) & n > last_pr)
	i <- i[order(n[i])]
	ids <- character()
	max_pr <- last_pr
	for (k in i) {
		ff <- .github_pr_files(prs[[k]]$url)
		if (is.null(ff)) {
			break
		}
		ff <- grep("^scripts/.*\\.R$", ff, ignore.case=TRUE, value=TRUE)
		ff <- grep("_functions\\.R$", ff, invert=TRUE, ignore.case=TRUE, value=TRUE)
		ids <- c(ids, gsub("\\.r$", "", tolower(basename(ff))))
		max_pr <- n[k]
	}
	list(ids=unique(gsub(" ", "", ids)), last_pr=max_pr)
}


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
	note[grepl("(^|/)_todo/", ff)] <- "to do"
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
