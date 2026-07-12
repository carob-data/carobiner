
update_todo <- function(path) {

	ftodo <- file.path(path, "misc", "todo", "to-do.csv")
	if (!file.exists(ftodo)) return(invisible(FALSE))
	
	todo <- utils::read.csv(ftodo)
	todo$uri <- trimws(todo$uri)
	uri <- gsub("https://doi.org/", "doi:", tolower(todo$uri))
	uri <- gsub("https://hdl.handle.net/", "hdl:", uri)
	id <- tolower(sapply(uri, yuri::simpleURI, USE.NAMES=FALSE))

	done <- list.files(file.path(path, "scripts"), pattern="^doi|^hdl.*\\.R$", recursive=TRUE, ignore.case=TRUE)
	done <- grep("_draft", done, invert=TRUE, value=TRUE)
	done <- gsub("\\.r$", "", tolower(basename(done)))
	done <- gsub(" ", "", done)
	id <- gsub(" ", "", id)

	i <- which(id %in% done)
	if (length(i) > 0) {
		todo <- todo[-i,]
		utils::write.csv(todo, ftodo, row.names=FALSE)
	}
	invisible(TRUE)
}

