
update_todo <- function(path) {

	ftodo <- file.path(path, "misc", "todo", "to-do.csv")
	if (!file.exists(ftodo)) return(invisible(FALSE))
	
	todo <- utils::read.csv(ftodo)
	todo$uri <- trimws(todo$uri)
	uri <- gsub("https://doi.org/", "doi:", tolower(todo$uri))
	uri <- gsub("https://hdl.handle.net/", "hdl:", uri)
	id <- tolower(sapply(uri, yuri::simpleURI, warn=FALSE, USE.NAMES=FALSE))

	done <- list.files(file.path(path, "scripts"), pattern="^doi|^hdl.*\\.R$", recursive=TRUE, ignore.case=TRUE)
	done <- grep("_draft|_todo|_pending|_AI", done, invert=TRUE, value=TRUE)
	done <- gsub("\\.r$", "", tolower(basename(done)))
	done <- gsub(" ", "", done)
	id <- gsub(" ", "", id)

	i <- which(id %in% done)
	if (length(i) > 0) {
		todo <- todo[-i,]
		utils::write.csv(todo, ftodo, row.names=FALSE)
	}

	ff <- list.files(file.path(path, "scripts", "_todo"), pattern="^doi|^hdl.*\\.R$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
	if (length(ff) > 0) {
		fid <- gsub("\\.r$", "", tolower(basename(ff)))
		fid <- gsub(" ", "", fid)
		j <- fid %in% done
		if (any(j)) {
			file.remove(ff[j])
		}
	}

	invisible(TRUE)
}

