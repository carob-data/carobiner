
unlabel <- function(d) {
	for (i in 1:ncol(d)) {
		v <- d[,i,drop=TRUE]
		if (inherits(v, "haven_labelled")) {
			labs <- attr(v, "labels")
			d[i] <- names(labs)[match(as.numeric(v), labs)]
		}
	}
	data.frame(d)
}

read.dta <- function(f, unlabel=TRUE, ...) {
	r <- haven::read_dta(f, ...)
	if (unlabel) {
		unlabel(r)
	} else {
		r
	}
}


eng_months_to_nr <- function(x) {
	mnths1 <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
	mnths2 <- substr(mnths1, 1, 3)

	x <- tolower(x)
	for (i in 1:12) {
		x <- sub(mnths1[i], i, x, ignore.case=TRUE)
		x <- sub(mnths2[i], i, x, ignore.case=TRUE)
	}
	x
}



change_names <- function(x, from, to, must_have=TRUE) {

#	warning("this function will be removed. Please do not use it")

	stopifnot(length(from) == length(to))
	for (i in 1:length(from)) {
		w <- which(colnames(x) == from[i])
		if (length(w) > 1) {
			stop(paste(from[i], "is duplicated"), call.=FALSE)
		} else if (must_have && length(w) == 0) {
			stop(paste(from[i], "is absent"), call.=FALSE)
		}
		names(x)[w] <- to[i]
	}
	x
}



bindr <- function(...) {
	d <- list(...)
	i <- sapply(d, is.null)
	if (all(i)) return(NULL)
	d <- d[!i]
	nms <- unique(unlist(lapply(d, names)))
	out <- lapply(d, 
			function(x) {
				x <- x[, colnames(x)!="", drop=FALSE]
					data.frame(c(x, 
						sapply(setdiff(nms, names(x)), function(y) NA)), check.names=FALSE)
				}
			)
				
	out$make.row.names <- FALSE
	do.call(rbind, out)
}

.binder <- function(ff) {
	#suppress "incomplete final line found by readTableHeader"
	x <- suppressWarnings(lapply(ff, utils::read.csv))
	nr <- sapply(x, nrow)
	x <- x[nr > 0]
	nms <- unique(unlist(lapply(x, names)))
	x <- lapply(x, function(x) data.frame(c(x, sapply(setdiff(nms, names(x)), function(y) NA))))
	x$make.row.names <- FALSE
	do.call(rbind, x)
}

fix_varnames <- function(x) {
	nms <- gsub("%", "pct", x)
	nms <- make.names(nms, unique=TRUE)
	nms <- gsub("\\.\\.\\.\\.", ".", nms)	
	nms <- gsub("\\.\\.\\.", ".", nms)	
	nms <- gsub("\\.\\.", ".", nms)	
	nms <- gsub("\\.$", "", nms)	
	make.names(nms, unique=TRUE)
}



replace_values <- function(x, from, to, must_have=TRUE) {
	stopifnot(length(from) == length(to))
	for (i in 1:length(from)) {
		if (must_have) {
			if (!all(from[i] %in% x)) {
				stop("not all names in 'from' are in 'x'")
			}
		}
		x[x==from[i]] <- to[i]
	}
	x
}


dfput <- function(x, name = NULL, digits=5, indent=4, drop = NULL) {
	stopifnot(is.data.frame(x))
	if (length(drop)) {
		x <- x[, setdiff(names(x), drop), drop = FALSE]
	}
	sp <- paste(rep(" ", indent), collapse = "")
	if (ncol(x) == 0) {
		code <- "data.frame()"
	} else {
		cols <- vapply(names(x), function(nm) {
			paste0(nm, " = ", .format_r_vec(x[[nm]], digits = digits))
		}, character(1), USE.NAMES = FALSE)
		body <- paste0(sp, cols, collapse = paste0(",", "\n"))
		code <- paste0("data.frame(\n", body, "\n)")
	}
	if (!is.null(name)) {
		code <- paste0(name, " <- ", code)
	}
	cat(code, "\n", sep = "")
	invisible(code)
}


.format_r_vec <- function(v, digits = NULL) {
	if (is.factor(v)) v <- as.character(v)
	if (is.character(v)) {
		out <- ifelse(is.na(v), "NA", paste0('"', gsub('"', '\\"', v, fixed = TRUE), '"')
		)
		return(paste0("c(", paste(out, collapse = ", "), ")"))
	}
	if (is.logical(v)) {
		out <- ifelse(is.na(v), "NA", ifelse(v, "TRUE", "FALSE"))
		return(paste0("c(", paste(out, collapse = ", "), ")"))
	}
	if (is.integer(v)) {
		out <- ifelse(is.na(v), "NA", as.character(v))
		return(paste0("c(", paste(out, collapse = ", "), ")"))
	}
	if (is.numeric(v)) {
		if (!is.null(digits)) v <- round(v, digits)
		out <- vapply(v, .format_r_num, character(1))
		return(paste0("c(", paste(out, collapse = ", "), ")"))
	}
	stop("unsupported column type: ", paste(class(v), collapse = "/"), call. = FALSE)
}


.format_r_num <- function(x) {
	if (is.na(x)) return("NA")
	if (abs(x - round(x)) < 1e-8 && abs(x) < 1e15) {
		return(as.character(as.integer(round(x))))
	}
	format(x, scientific = FALSE, trim = TRUE, digits = 15)
}

